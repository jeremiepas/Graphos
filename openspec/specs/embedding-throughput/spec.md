# embedding-throughput Specification

## Purpose

Batched, deduplicated, and content-cached embedding generation over a persistent HTTP connection, so that embedding a large graph costs a number of API calls proportional to the number of unique texts rather than the number of nodes, and repeated runs re-embed only what changed.

## Requirements

### Requirement: Batched embedding transport over a persistent connection

The system SHALL send embedding requests through a native HTTP client holding a shared connection pool (keep-alive) and SHALL accept an array of texts per request, using the OpenAI-compatible `input: [text, …]` payload. The system MUST NOT fork one process per text and MUST NOT write request payloads to a shared fixed temporary file. A batch request SHALL return vectors in an order matching the order of the submitted texts.

#### Scenario: Batch request returns one vector per text in order
- **WHEN** the system submits a batch of 3 texts `[t1, t2, t3]` to the embedding API
- **THEN** it receives exactly 3 vectors `[v1, v2, v3]` such that `vi` is the embedding of `ti`

#### Scenario: No temp file race
- **WHEN** two embedding requests run concurrently against the same API
- **THEN** neither writes a request payload to a shared fixed temporary file, and both requests succeed independently

#### Scenario: Batch larger than configured chunk size
- **WHEN** 300 unique texts are submitted with a configured chunk size of 128
- **THEN** the system issues at least 3 requests covering disjoint, order-preserving chunks whose concatenation is the original 300 texts

### Requirement: Text deduplication before embedding

The system SHALL embed only unique texts within a run and SHALL redistribute the resulting vector to every node whose embedded text is equal. Two nodes with the same embedded text MUST receive equal vectors.

#### Scenario: Repeated labels embed once
- **WHEN** a graph contains 5,000 nodes whose embedded text is identical and 100 other unique texts
- **THEN** the system submits at most 101 distinct texts to the API and every one of the 5,000 nodes receives the same vector as the first

#### Scenario: Deduplication preserves per-node assignment
- **WHEN** nodes n1 and n2 share text `tA` while node n3 has text `tB`
- **THEN** the final node-to-vector assignment equals the assignment a sequential one-text-per-call loop would produce

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

### Requirement: Bounded optional concurrency

The system SHALL support processing multiple batches concurrently with a configurable limit. The default concurrency SHALL be the value pinned by the recorded benchmark (preliminary plateau at 2–4; exact value recorded in `BENCHMARK.md`), instead of the historical default of 1. A concurrency limit above 1 MUST NOT change the resulting node-to-vector assignment.

#### Scenario: Concurrent completion is order-independent
- **WHEN** batches complete out of order under a concurrency limit of 4
- **THEN** the final node-to-vector assignment equals the assignment produced with limit 1

#### Scenario: Default concurrency comes from the benchmark
- **WHEN** `graphos.yaml` contains `embedding: {enabled: true}` with no `concurrency` key
- **THEN** the effective concurrency is the benchmark-pinned value recorded in `BENCHMARK.md`, not 1

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

### Requirement: Configurable batch size and concurrency

The embedding configuration SHALL accept optional `batchSize` and `concurrency` keys with defaults of 64 and 1 respectively. Absent keys MUST fall back to the defaults. The batch size MUST be at least 1; a value below 1 MUST be rejected at load time.

#### Scenario: Defaults when keys absent
- **WHEN** `graphos.yaml` contains `embedding: {enabled: true}` with no `batchSize` or `concurrency` keys
- **THEN** the effective batch size is 64 and the effective concurrency is 1

#### Scenario: Invalid batch size rejected
- **WHEN** `graphos.yaml` contains `embedding: {batchSize: 0}`
- **THEN** configuration loading fails with an error naming the invalid key