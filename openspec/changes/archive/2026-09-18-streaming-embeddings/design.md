## Context

`Infrastructure/LLM/Embedding.hs` embeds one text per call: writes the JSON payload to the fixed path `/tmp/graphos-embed-payload.json`, forks `curl` via `readProcessWithExitCode`, parses a single vector. `UseCase/Pipeline/Core.hs:generateGraphEmbeddings` and `UseCase/Ingest.hs:generateEmbeddingsForNodes` call it via `LLMPort.lpGenerateEmbedding` once per node, sequentially. The embedded text is `nodeLabel <> " " <> nodeSourceFile`; at function granularity thousands of nodes share a label, so most API calls compute identical vectors. `graphos.cabal` already depends on `http-client`, `async`, `cryptohash-sha256`, `vector`; `Infrastructure/FileSystem/Cache.hs` already establishes the `graphos-out/cache/` + SHA256 + atomic-write pattern. The Lean 4 artifact `lean/EmbedPipeline.lean` (already drafted in this change) states the equivalence theorems the implementation must satisfy.

## Goals / Non-Goals

**Goals:**
- API call count proportional to unique texts per run, not node count (109k → ~1–2k requests; near-zero on unchanged re-runs).
- Kill the per-call process fork and the shared temp file (correctness bug, not just perf).
- Keep the node→vector assignment provably identical to the current sequential loop, including per-text failure behavior.
- Keep `graph.json` / `embeddings.json` / `index.json` formats compatible.

**Non-Goals:**
- Node text enrichment (signatures, neighbor labels, Leiden community context) — quality lever, separate change.
- `search_document:` / `search_query:` nomic prefixes, embedding propagation over the graph, `Data.Vector.Unboxed` migration of stored vectors.
- Streaming/NDJSON response handling; the OpenAI-compat batch endpoint returns one JSON body per request.
- Changing `EmbeddingConfig` provider semantics (still Ollama-local only).

## Decisions

| # | Decision | Rationale | Alternatives |
|---|----------|-----------|--------------|
| D1 | `http-client` with one shared `Manager` created once per pipeline run and threaded through the port call | Keep-alive removes the TCP handshake per call; `http-client` is already a dependency and already used in `UseCase/Ingest.hs:downloadFile`. Zero new deps | (a) keep curl but pool processes — still forks, still a race surface; (b) `http-conduit` — heavier, no added value for a single POST endpoint |
| D2 | Batch API: `generateEmbeddings :: Manager -> EmbeddingConfig -> [Text] -> IO (Either Text [[Double]]]`, `input: [t…]` JSON array, response `data` array ordered by `index` | OpenAI-compat contract; one HTTP round-trip amortizes connection + GPU dispatch over up to 128 texts | (a) one text per keep-alive request — 109k round-trips remain; (b) NDJSON streaming — not offered by the `/v1/embeddings` compat endpoint |
| D3 | Dedup in UseCase: `uniqueTexts = Set.toList (Set.fromList texts)`, embed uniques in chunks of `embBatchSize`, redistribute via `Map Text [Double]` | Pure UseCase logic, no IO; the Lean theorem `pipeline_eq_baseline` (EmbedPipeline.lean §4) proves assignment equivalence for any chunk size and any duplicate-handling order | Server-side dedup — the API has no such contract; dedup in Infrastructure — would hide the invariant from the port boundary |
| D4 | Cache: `graphos-out/cache/embeddings/<sha256hex(model <> text)>.json` storing `{vector: [Double]}`, written atomically (existing `writeFileAtomic`), read-only API on run | Content-addressed key reuses the `FileSystem/Cache.hs` pattern; model in the key gives free model-change invalidation; `runCached_sound` (§5) guarantees the accumulated cache stays sound across runs, `runCached_fst` guarantees the assignment is baseline-equal | (a) key by source-file hash — wrong unit (text, not file, is what's embedded); (b) single JSON map file — O(all-texts) load, worse concurrency; (c) SQLite — new dependency, no payoff |
| D5 | Concurrency: `mapConcurrently`-style worker pool over chunks, limit `embConcurrency` (default 1 = sequential) | After batching, 2–4 in-flight batches hide per-request latency; theorem `pipeline_keys_irrelevant` shows order/duplicates cannot change the assignment; default stays gentle on local Ollama | Unbounded concurrency — overwhelms local GPU server, exactly the regression the old comment warned about |
| D6 | Port extension: add `lpGenerateEmbeddings :: EmbeddingConfig -> [Text] -> IO (Either Text [[Double]]])` to `LLMPort`; keep `lpGenerateEmbedding` (used by tests + external callers) delegating to a batch of one | Port-only access keeps UseCase clean (usecase-ports capability); plural/singular pair mirrors the memory-agent proposal precedent | Change the existing field type — breaks every port instance including test doubles |
| D7 | `ieSourceHash` := `sha256hex(model <> embeddedText)` at write time | Spec says content-derived; old value (file path) is retained nowhere that load logic depends on; loaders treat it as opaque text, so no format migration | Keep path — blocks any content-based invalidation, and spec now forbids it |
| D8 | `EmbeddingConfig` + `embBatchSize :: Int` (default 64) and `embConcurrency :: Int` (default 1), parsed with `.:? … .!=`, `batchSize < 1` rejected in the config loader with a named error | YAML stays optional-backward-compatible; validation at load time per spec | CLI flags — duplicates config, complicates watch mode |
| D9 | Manager threading: `EmbeddingConfig`-bound `Manager` created lazily once per pipeline run in `Wiring.hs` (`productionLLMPort`), not per call | One TLS/TCP pool per process; avoids leaking a Manager per request | Per-call `newManager` — recreates the connection pool every batch, half the win lost |
| D10 | Lean artifact: `lean/EmbedPipeline.lean` checked by `lake build` / `lean` in CI-equivalent verification during apply; theorems pinned in tasks' check criteria, each maps to a theorem name (`chunks_flatten`, `pipeline_eq_baseline`, `pipeline_keys_irrelevant`, `runCached_sound`, `runCached_fst`) | Machine-checked proof replaces hand-waving that "batching + dedup + cache is just an optimization"; generic in `f` and `β` so per-text failure (`β := Option V`) is covered by the same statements | Mathlib-backed model — heavier toolchain than needed; TLA+ — no Lean toolchain synergy with AVI-618 precedent |

## Risks / Trade-offs

- [Local Ollama batch timeout: 128 texts × slow model may exceed one-text timeout] → keep per-request timeout generous (existing 30s scaled ×batch), make chunk size configurable (D8); default 64 is conservative.
- [Server returns `data` out of order] → sort response items by the OpenAI `index` field before zipping; reject a response whose indices are not a permutation of `[0..n-1]`.
- [Cache poisoning by truncated writes] → only ever `writeFileAtomic` (same guarantee as the extraction cache); readers treat undecodable entries as a miss.
- [Two Graphos processes sharing `graphos-out/cache/embeddings/`] → content-addressed keys are write-idempotent (same key ⇒ same value under the determinism assumption); atomic rename makes concurrent writers benign, unlike the temp-file race it replaces.
- [`Vector`-vs-`[Double]` memory untouched] → out of scope (Non-Goal); the sidecar format stays list-encoded.
- [Per-text failure semantics shift: one failed batch now withholds up to 128 texts instead of 1] → theorem guarantees equality *with the baseline where those calls also fail*; on failure the pipeline logs the chunk and stores metadata-only entries exactly as today.
- [Lean toolchain not present on all dev machines] → `lean/` artifact verification is an explicit apply-phase check task, not a `cabal test` dependency; the Haskell side carries Hspec coverage for observable behavior.

## Migration Plan

1. Land the batched client alongside the singular one; wire `lpGenerateEmbeddings`; switch `generateGraphEmbeddings` and `generateEmbeddingsForNodes` to dedup+batch(+cache) behind default config (batchSize 64, concurrency 1) — behavior-equivalent by D3/D10 theorems.
2. Flip `ieSourceHash` to content hash (D7). `index.json` consumers treat it as opaque; no loader change.
3. Rollback: revert the two UseCase call sites to `lpGenerateEmbedding`; the new port field is additive.

## Open Questions

- Should the cache directory live under the existing `graphos-out/cache/` flat namespace (sharing it with extraction entries) or a dedicated `embeddings/` subdirectory? Leaning subdirectory; decided in task 1 without spec impact.