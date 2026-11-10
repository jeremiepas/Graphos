## MODIFIED Requirements

### Requirement: Bounded optional concurrency

The system SHALL support processing multiple batches concurrently with a configurable limit. The default concurrency SHALL be the value pinned by the recorded benchmark (preliminary plateau at 2–4; exact value recorded in `BENCHMARK.md`), instead of the historical default of 1. A concurrency limit above 1 MUST NOT change the resulting node-to-vector assignment.

#### Scenario: Concurrent completion is order-independent
- **WHEN** batches complete out of order under a concurrency limit of 4
- **THEN** the final node-to-vector assignment equals the assignment produced with limit 1

#### Scenario: Default concurrency comes from the benchmark
- **WHEN** `graphos.yaml` contains `embedding: {enabled: true}` with no `concurrency` key
- **THEN** the effective concurrency is the benchmark-pinned value recorded in `BENCHMARK.md`, not 1

### Requirement: Persistent content-addressed embedding cache

The system SHALL persist embeddings in a cache under the output cache directory keyed by a SHA-256 digest of the model name, the effective query/document prefixes, and the prepared text actually submitted to the API. On a run whose cache already contains the key, the system MUST reuse the cached vector and MUST NOT call the API for that text, and MUST NOT rewrite the cached entry to disk. The cache MUST be sound: a cache entry for key `k` SHALL only exist if the stored vector is the embedding the API returns for the corresponding prepared text, model, and prefixes.

#### Scenario: Second run hits the cache
- **WHEN** a run completes and a second run processes the same texts with the same model, prefixes, and preparation
- **THEN** the second run issues zero API calls for those texts and produces the same node-to-vector assignment as the first run

#### Scenario: Cache hits are not rewritten
- **WHEN** a fully warm run re-embeds nothing and the cache directory is intact
- **THEN** no cache entry file is created, modified, or renamed during the run (no atomic-write churn on hits)

#### Scenario: Model change invalidates entries
- **WHEN** the same text is embedded with a different model than the cached entry
- **THEN** the cache key differs and the text is re-embedded rather than served a stale vector

#### Scenario: Fresh rebuild still benefits from the cache
- **WHEN** the pipeline runs with `--fresh` and the cache directory is intact
- **THEN** the graph is rebuilt from scratch but unchanged prepared texts are served from the cache instead of being re-embedded

#### Scenario: Prefix change invalidates entries
- **WHEN** a run completes with empty prefixes and a second run uses a `docPrefix`
- **THEN** the cache key differs for every text and the second run re-embeds rather than serving stale vectors

### Requirement: Sequential-equivalence of the optimized pipeline

Under the assumption that the embedding API is a deterministic function of (model, prepared text), the deduplicate + prepare + batch + cache + redistribute pipeline SHALL produce, for every node, exactly the assignment the current sequential one-text-per-call loop produces, including which nodes receive a vector when individual texts fail. The change SHALL include a machine-checked Lean 4 proof of this equivalence covering chunking, deduplication, batching, distribution, cache soundness, the token-fit preparation transform (instantiating the master equivalence at `f ∘ prepare`), the misses-only write-back, and the streaming sidecar equivalence (per-batch staged append then atomic rename produces the same sidecar content as write-at-end).

#### Scenario: Optimized pipeline matches baseline
- **WHEN** the embedding API is a fixed deterministic function and a graph is embedded both by the sequential loop and by the optimized pipeline
- **THEN** both produce the same node-to-vector assignment, and the Lean 4 proof artifact compiles with zero errors

#### Scenario: Preparation preserves equivalence
- **WHEN** texts are truncated (or chunk-pooled) before submission and the same preparation is applied in the sequential baseline
- **THEN** the pipeline assignment equals the baseline assignment, as proved by instantiating the master theorem at the preparation function

#### Scenario: Per-text failure is isolated
- **WHEN** the API fails for one batch and succeeds for the others
- **THEN** only the nodes whose texts belong to the failed batch lack vectors, exactly as with sequential processing where those calls fail

#### Scenario: Reproducible machine check
- **WHEN** `lake clean && lake build` is run in the change's `lean/` directory with the pinned Lean 4.29.1 core toolchain
- **THEN** the command exits 0 with zero errors and zero warnings, and `VERIFICATION.md` records the command and the run log