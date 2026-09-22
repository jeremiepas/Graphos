## Why

The re-measured deployment overturns the original diagnosis: the model and transport are **not** the bottleneck. The live `--embed` run (`graphos . --no-cluster --embed --update`) drives a standalone llama-server on `:8080` serving LFM2.5-Embedding-350M at **~343 req/s burst** (≈10,300 requests in ~30s) — more than 4× the sequential nomic baseline. The actual defect is input contract: Graphos sends embed texts of **arbitrary length** to a model whose document limit is **512 tokens**; llama-server rejected 18 inputs with HTTP 500 (observed >1024 tokens, largest **6,628**) — and because `embedChunk` discards the whole batch on failure (`Pipeline/Core.hs:129`), each oversized text silently withholds vectors for up to 63 innocent batch siblings (~1,150 nodes lost in the observed run). Compounding model-independent hotspots remain: `embConcurrency` defaults to 1, and every run rewrites all cache-hit vectors to disk (`Pipeline/Core.hs:115`).

Measured facts grounding this change (24-core CPU, 62 GB RAM, AMD 880M iGPU, no discrete GPU):

| Configuration                                        | Result                                      |
|------------------------------------------------------|---------------------------------------------|
| nomic-embed-text (Ollama :11434), batch 64, idle      | 82 texts/s                                  |
| nomic-embed-text, concurrency 2–8, under load         | 43–48 texts/s aggregate (plateau)            |
| LFM2.5-Embedding-350M (llama-server :8080), burst     | ~343 req/s (10,300 req / ~30s)               |
| LFM2.5 inputs >1024 tokens (largest 6,628)            | HTTP 500; whole 64-text batch loses vectors  |

## What Changes

- **Token-bounded input preparation (the fix)**: before any `/v1/embeddings` call, each embed text SHALL be fit to the model's token limit — either truncated, or split into ≤512-token chunks whose vectors are pooled into one (design decides; truncation is the trivially-safe default). The limit SHALL default from model metadata with a config override (`maxTokens`). A text that still fails SHALL degrade alone: per-input failure isolation, so one oversized/rejected input can no longer discard its batch siblings.
- Add a reproducible embedding benchmark harness measuring texts/s per (model, server, concurrency) on a node-shaped corpus, with results recorded in the change directory (`BENCHMARK.md`); the headline result is already recorded above — the harness makes it re-runnable and gates future model/default decisions.
- Raise default `embedding.concurrency` from 1 to the benchmark-pinned value (preliminary data points to 2–4); assignment-invariance under concurrency is already machine-checked (`pipeline_keys_irrelevant`).
- Restrict cache write-back to misses: vectors served from the cache are no longer rewritten to disk on every run.
- Add asymmetric prefix support — optional `docPrefix`/`queryPrefix` config keys, empty defaults preserve today's behavior — required by LFM-class bi-encoders whose quality depends on `document:`/`query:` prefixes.
- Extend the cache key to incorporate the configured prefix (and count token-fit preparation as part of the transform), so a prefix or limit change invalidates for free, mirroring the existing model-name invalidation design.
- Adopt LFM2.5-Embedding-350M as the default local model now that throughput is proven; the recorded benchmark keeps the decision auditable.
- **Incremental (streaming) sidecar write**: vectors append to a staged `embeddings.json` as batches complete (mirroring the `graph.json` incremental writer), atomically renamed into place at the end; `embedding.streaming` config key (default `true`) falls back to today's write-at-end when `false`. Crash-resilient, bounded write memory, and consistent with the atomic-output-writes capability.
- New Lean 4 verification artifact (`lean/EmbedPerf.lean`, Lean 4.29.1 core only, following `lean-proof-methodology` rules: goal-guided invocation of cache-bearing theorems, no `by decide` over `List.lookup`, clean `lake clean && lake build` recorded in `VERIFICATION.md`) proving the algorithm changes preserve the baseline assignment: token-fit truncation (master equivalence instantiated at `f ∘ truncate`), misses-only write-back (cache stays sound and assignment-equal), prefix composition, and streaming-write equivalence (per-batch append then rename produces the same sidecar content as write-at-end).

## Capabilities

### New Capabilities
- `embedding-benchmark`: reproducible local benchmark of embedding throughput per model, server, and concurrency on a node-shaped corpus, with recorded results gating model and default decisions.

### Modified Capabilities
- `embedding`: the embedding client gains token-bounded input preparation (model-metadata default limit, `maxTokens` override, truncation or chunk+pool per design) and per-input failure isolation; optional asymmetric `docPrefix`/`queryPrefix` text transforms with empty defaults; cache keys and `ieSourceHash` incorporate the prefix alongside the model name.
- `embedding-throughput`: the concurrency default changes from 1 to the benchmark-pinned value; cache write-back becomes misses-only; the cache key incorporates the configured preparation (prefix, token-fit); the Lean 4 equivalence requirement extends to cover the truncation transform, write-back skip, and prefix composition.

## Impact

- **Infrastructure**: `LLM/Embedding.hs` (token counting/fit, prefix application, per-input error reporting), `LLM/EmbeddingCache.hs` (cache key includes preparation parameters).
- **UseCase**: `Pipeline/Core.hs` (`generateGraphEmbeddings`: misses-only write-back, prepared texts, per-input isolation) and `Ingest.hs` (`generateEmbeddingsForNodes`: same); batch assembly should bin prepared texts so no oversized input shares a batch.
- **Domain**: `EmbeddingConfig` gains `embMaxTokens` and `embDocPrefix`/`embQueryPrefix` (empty defaults preserve behavior).
- **Config**: `graphos.yaml` `embedding:` gains `maxTokens`/`docPrefix`/`queryPrefix` keys; `concurrency` default re-pinned from the recorded benchmark; `base_url` documentation reflects both Ollama and standalone llama-server deployments.
- **Benchmark harness**: standalone script or cabal bench target (design decides); results checked in as `BENCHMARK.md` under the change directory.
- **Lean**: new `lean/EmbedPerf.lean` + `VERIFICATION.md` in the change directory, building on `EmbedPipeline.lean`'s master theorems (`pipeline_eq_baseline`, `runCached_fst`, `runCached_sound`).
- **Docs**: `docs/embedding-models.md` gains the LFM2.5 row (512-token limit), the token-fit requirement, and a pointer to the recorded benchmark.
- **No format break**: `graph.json` / `embeddings.json` / `index.json` contracts unchanged; cache invalidation is automatic via key extension. The cosine-consumption path (`inferSemanticCodeDocEdges`) is out of scope — guarded by the existing 10K scale guard.