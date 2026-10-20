## Context

The embedding path today is `UseCase.Pipeline.Core.generateGraphEmbeddings` / `UseCase.Ingest.generateEmbeddingsForNodes` over `Infrastructure.LLM.Embedding.generateEmbeddings` (native http-client, OpenAI-compatible `/v1/embeddings`). Texts are `label <> " " <> sourceFile`; markdown file nodes carry the full doc body in `nodeExtra.doc_body` but the embed text itself stays short *today* — yet the live run observed 6,628-token inputs reaching a 512-token model (LFM2.5-Embedding-350M on llama-server), so some label/source combinations already blow the limit. `embedChunk` discards an entire 64-text batch on any failure (`Core.hs:129` / `Ingest.hs:298`), which amplified 18 HTTP 500s into ~1,150 lost vectors. The cache key is `SHA256(model <> text)` (`EmbeddingCache.hs:55`) — prefix- and preparation-blind. Concurrency defaults to 1; write-back rewrites cache hits every run (`Core.hs:115`). See proposal.md for the measured numbers and llama-server burst (~343 req/s).

Constraints from the Lean side: the equivalence instrument (`EmbedPipeline.lean`, Lean 4.29.1 core) models the server as a deterministic function of (model, text) and proves dedup+batch+cache+redistribute ≡ sequential baseline via master theorems (`pipeline_eq_baseline`, `runCached_fst`, `runCached_sound`). Any change to the text transform or cache semantics must be expressible as an instantiation/composition of those theorems, under the `lean-proof-methodology` rules (goal-guided invocation, no `by decide` over `List.lookup`).

## Goals / Non-Goals

**Goals:**
- G1: No embed input ever exceeds the effective token limit (no more HTTP 500s from over-limit texts).
- G2: A failing input degrades alone; batch siblings keep their vectors.
- G3: Prefix support for asymmetric bi-encoders (LFM `document:`/`query:`), cache-correct by construction.
- G4: Cache correctness extended to preparation: two texts that prepare identically converge on one entry; hits are never rewritten.
- G5: Machine-checked proof that preparation + misses-only write-back preserve the baseline assignment.
- G6: Defaults (model, concurrency) pinned by a recorded, re-runnable benchmark (`BENCHMARK.md`).

**Non-Goals:**
- No change to the cosine consumption path (`inferSemanticCodeDocEdges`, 10K scale guard) — vectors' *meaning* is unchanged for non-truncated texts.
- No ANN index, no vector-database integration.
- No new sidecar formats; `embeddings.json` / `index.json` contracts unchanged.
- No server-side work: llama-server is treated as a healthy external dependency.
- Token counting is approximate (see D3) — this is a client-side guard, not exact BPE.

## Decisions

### D1 — Preparation strategy: truncate, not chunk-and-pool

> Serves G1.

For texts over the limit, truncate to the effective token budget.

- **Why**: truncation is a pure function `prepare : Text -> Text`, so the Lean equivalence is a one-line instantiation of the master theorem at `f ∘ prepare` — chunk-and-pool introduces a vector-meaning change (mean-pooled chunks ≠ any single-text embedding) that would *break* the "prepared text ≡ what baseline would embed" story and require new semantics the current theorem cannot express. It also keeps one input = one vector, preserving cache shape and `ieSourceHash` semantics. For our embed texts (`label + sourcefile`), the informative head is the label — truncation loses at most a tail path.
- **Alternatives**: (a) chunk-and-pool: better recall for long doc bodies, but changes vector semantics, costs N× API calls per long text, and forfeits the trivial Lean proof; (b) reject-and-skip: no data loss prevention, keeps silent gaps; (c) server-side chunking: llama-server does not offer it for embeddings.

### D2 — Per-input failure isolation: split on error, not on submission

> Serves G2.

When a batch request returns per-input errors (OpenAI-compatible responses do not have a standard per-input error field; llama-server returns HTTP 500 for the whole request), the client cannot always distinguish which input failed. Strategy:

1. First attempt: the batch as assembled.
2. On whole-request failure with a batch of >1: bisect — retry the halves independently (recursion depth ≤ log2(batchSize); worst case degrades to single-text requests, which the existing `PipelineSpec` "failing text lands in its own batch" test already models).
3. On single-text failure: log the text (truncated form, first 200 chars) and the error; that input alone gets no vector.

- **Why**: keeps the common path one request; only failing batches pay the retry cost; the failure domain shrinks monotonically toward the true culprit; no API contract change. The observed 18/10,300 failure rate makes bisection overhead negligible.
- **Alternatives**: (a) always submit one text per request — forfeits the 343 req/s batch gains; (b) parse per-input errors — no such field in the OpenAI embeddings contract; (c) drop the whole batch (status quo) — the bug being fixed.

### D3 — Token counting: conservative character-based estimate, config-pinned model limits

> Serves G1. Budget guard only.

`prepare` uses an approximate token count: `T.length text` ÷ 4 (English/code heuristic ≈ 4 chars/token) with a hard character cap `4 × effectiveTokenLimit` as a belt-and-braces bound. Model limits come from a static table in Domain (nomic-embed: 8192, LFM2.5-Embedding-350M: 512, all-minilm: 256, `unknown`: configurable via `maxTokens`, no default truncation when both are absent).

- **Why**: a real tokenizer (HuggingFace tokenizers, ICU) is a new dependency and Infrastructure-side; the spec requires only that prepared text "MUST NOT exceed the effective limit". Char÷4 under-estimates tokens for CJK/whitespace-dense text, but llama-server rejects on its own tokenizer — and D2's isolation turns any residual 500 into a per-input, not per-batch, loss, making the estimate's precision non-critical.
- **Alternatives**: (a) exact BPE client-side — dependency + perf cost for a guard; (b) server-reported usage fields — not exposed for embeddings; (c) no client guard — the defect.

### D4 — Prefixes and cache key: prepare once, hash what was sent

> Serves G3, G4.

Preparation composes: `submitText = docPrefix <> prepare(rawText)` (query-side uses `queryPrefix` at query time — out of this change's pipeline scope but the key design holds). The cache key becomes `SHA256(model <> docPrefix <> submitText)` — i.e., hash the exact string sent to the API, prefixed by model. `ieSourceHash` likewise.

- **Why**: hashing what was sent makes cache soundness definitional: the cache stores `f(submitText)` under `key(model, prefix, submitText)`, matching the existing Lean soundness theorem instantiated at `f` with keys over the submitted text. Prefix changes invalidate for free (key differs). Truncation converges texts that prepare identically (spec: "Identical preparation converges on one cache entry") because the key is over the *prepared* text.
- **Alternatives**: (a) hash raw text + separate prefix field — two sources of truth, divergence risk; (b) per-prefix cache subdirectories — churn; (c) no prefix support — blocks LFM quality.

### D5 — Misses-only write-back

> Serves G4.

`generateGraphEmbeddings` currently writes every vector (hit or fresh) via `saveVector`. Change: write only vectors produced by this run's API calls (misses). Hits are read-only.

- **Why**: a warm re-run goes from ~250k atomic writes to 0. Soundness is untouched — `runCached_sound` instantiates fine: the cache after the run ⊇ cache before (entries only added, never mutated); the Lean artifact gains a lemma that a read-only pass over entries preserves soundness trivially.
- **Alternatives**: (a) mtime refresh on hits — write amplification for zero benefit; (b) status quo — the hotspot.

### D6 — Concurrency default: benchmark-pinned, preliminarily 2

> Serves G6.

Run the benchmark's concurrency sweep (1/2/4/8) on an idle machine; default = lowest level reaching ≥90% of the measured plateau. Preliminary under-load data suggests 2; the recorded idle sweep decides. If no plateau is observed, default stays 1 and `BENCHMARK.md` records why.

- **Why**: the plateau under load (43–48 t/s at 2–8) suggests llama-server saturates near 2 streams on this hardware; a higher default buys nothing and risks memory contention with the 880M iGPU.
- **Alternatives**: (a) hardcode 4 — guesswork; (b) adaptive — complexity with no spec requirement.

### D7 — Benchmark harness: script, not cabal target

A standalone Python script (`openspec/changes/lfm-embedding-optimization/bench/bench.py`, runnable ad hoc, also copied to `scripts/bench-embeddings.py` at implementation time) POSTing to the configured server with node-shaped synthetic texts (label + source path patterns drawn from the repo's own label distribution), sweeping concurrency, and emitting a Markdown table to append to `BENCHMARK.md`.

- **Why**: the benchmark measures *server throughput* (HTTP client behavior), not Haskell code paths — a Python script avoids coupling measurement to the cabal build, matches how the headline 343 req/s was actually measured, and keeps the harness runnable before/after any code changes.
- **Alternatives**: (a) cabal bench target — heavyweight, couples to the code being measured for defaults; (b) shell + curl — weaker corpus shaping, no concurrency orchestration.

### D8 — Where preparation lives: Domain function, Infrastructure transport

`prepare` (prefix + truncate) is a **pure Domain function** (`Domain.Embedding.prepare`, new module) taking config + text. The Infrastructure client calls it before building the JSON payload; UseCase pipelines pass raw texts unchanged and remain preparation-agnostic (they see raw-text dedup; prepared-text convergence happens beneath via the cache).

- **Why**: clean-architecture hard rule (Domain = zero IO); `prepare` must be pure for the Lean artifact to reference its semantics; UseCase staying raw-text-keyed means `generateGraphEmbeddings`'s dedup/redistribution logic is untouched, minimizing proof obligations.
- **Alternatives**: (a) prepare inside Infrastructure transport silently — hides the transform from specs/tests; (b) prepare at UseCase — touches both pipeline entry points and widens the proof delta.

### D9 — Streaming sidecar write: incremental staged append + atomic rename

> Serves crash-resilience and bounded write memory; matches `atomic-output-writes`.

`embedding.streaming` (default `true`). With streaming enabled, `generateGraphEmbeddings` opens a staged sidecar (`embeddings.json.staging-<pid>` style, per the `withStagedOutput` convention) and appends each completed batch's `(NodeId, vector)` entries as they arrive; the staged file is atomically renamed to `embeddings.json` when the pass completes. `streaming: false` retains the current write-at-end `writeEmbeddingsSidecar`.

- **Why**: today ~4 GB of vectors accumulate in a `Map` before one giant atomic write at the end — a crash loses all of it, and the write itself spikes memory. Appending per-batch bounds resident memory to one batch of vectors and makes the pass crash-resilient (prior sidecar intact until rename, per atomic-output-writes). The existing `IncrementalWriter` machinery (`Infrastructure/Export/IncrementalJSON.hs`) demonstrates the JSON-append pattern; the sidecar is a simpler object/map shape so a lightweight writer suffices. The staged+rename step rides `withStagedOutput`/`writeFileAtomic` conventions rather than new infrastructure.
- **Alternatives**: (a) status-quo write-at-end — simplest, but 4 GB peak and total loss on crash; (b) write-through to final path with truncate+rewrite — violates atomic-output-writes (readers can observe partial); (c) per-batch separate files + concat pass — extra I/O and cleanup complexity for no benefit over staged append.
- **Interaction with D5**: the cache write-back (misses-only) is orthogonal — D5 governs the *cache* entries, D9 governs the *sidecar* the graph loader consumes. Both write per-batch; both stay misses-only where applicable.
- **Lean obligation**: streaming equivalence is a list-append lemma — `concat (map entriesOf batches)` under staged append equals the write-at-end content (flattening associativity), instantiated against the existing `chunks_flatten` pattern; no new theorem shape.


## Risks / Trade-offs

- [Truncation loses tail information for long doc-body texts] → Accepted for now: embed texts are `label + sourcefile` (short); the long-text case comes from pathological labels, where the head carries the label. Chunk-and-pool remains a future change with its own proof obligations. `BENCHMARK.md`'s oversized probe documents the truncation behavior.
- [Char÷4 token estimate under-counts CJK/compact scripts] → D2 isolation contains the damage to per-input loss; `maxTokens` lets users tighten. If a corpus is known-CJK-heavy, the user can set `maxTokens` below the model limit.
- [Bisection retries add requests on flaky servers] → Bounded by log2(64) ≈ 6 extra requests per failing batch; failure rate observed at 0.17%.
- [LFM prefix mismatch (docs embedded without `document:`, queries without `query:`)] → Prefix defaults are empty; users opt in per model. `docs/embedding-models.md` documents the LFM prefix requirement next to the model row.
- [Cache growth from prepared-text keys] → Old entries (raw-text keys) become dead weight, not incorrect: keys differ, so stale entries are never served. A future GC pass can prune; not in scope.
- [Benchmark measured on developer hardware may not generalize] → The capability records hardware in every row; defaults cite the row. Users on different hardware re-run the same script and override.
- [llama-server model limit knowledge is a static table] → `maxTokens` overrides; unknown models default to no truncation (status quo risk) — mitigated by D2 isolation when the server rejects.

## Migration Plan

1. Ship preparation (D1, D3, D4-prefix, D8) + isolation (D2) with prefixes defaulting to empty and `maxTokens` absent → behavior for existing nomic users is byte-identical (texts under limit pass through; over-limit texts get truncated instead of 500ing).
2. Pin the benchmark harness output and flip defaults in the same release: model → LFM2.5 GGUF reference, concurrency → recorded value (D6).
3. Cache migration: none needed — keys extend (model <> prefix <> prepared), old entries simply never hit for changed configs and continue to hit for unchanged ones.

Rollback: revert the default-model config commit; the nomic path is unchanged, so reverting restores exact previous behavior (same keys, same transport).

## Open Questions

- None blocking. (Query-time `queryPrefix` application — semantic search — is the memory-agent's territory; its absence here is a scope boundary, not an unknown.)