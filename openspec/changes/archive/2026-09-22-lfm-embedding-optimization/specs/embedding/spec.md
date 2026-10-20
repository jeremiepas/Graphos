## ADDED Requirements

### Requirement: Token-bounded input preparation

Before any embedding request is submitted, each input text SHALL be prepared to fit the model's token limit: the default limit SHALL come from the model's documented context, configurable via an `embedding.maxTokens` key (0 or absent = model default). Preparation SHALL apply either truncation or chunk-and-pool (design decides); a prepared text MUST NOT exceed the effective token limit, and the original unprepared text MUST NOT be sent to the API.

#### Scenario: Oversized document is truncated to the limit
- **WHEN** a node's embed text is 6,628 tokens and the effective model limit is 512 tokens
- **THEN** the text submitted to the embedding API is at most 512 tokens, and the request is accepted

#### Scenario: Normal texts pass through preparation unchanged
- **WHEN** a node's embed text is 20 tokens and the effective limit is 512 tokens
- **THEN** the text submitted to the API is byte-identical to the input text (no prefixes, no truncation when prefix config is empty)

#### Scenario: maxTokens override wins over model default
- **WHEN** `graphos.yaml` sets `embedding.maxTokens: 256` for a model whose documented limit is 512
- **THEN** every prepared text is at most 256 tokens

### Requirement: Per-input failure isolation

When the embedding API rejects or fails an individual input within a batch, the failure SHALL be isolated to that input: every other text in the batch MUST still receive its vector, and the failing input MUST be reported (logged or counted) rather than silently dropped. A whole-batch transport failure (connection failure, non-2xx for the entire request) MAY withhold vectors for the whole batch, but per-input errors returned by a completed request MUST NOT.

#### Scenario: One oversized input does not lose siblings' vectors
- **WHEN** a batch of 64 texts contains one input the server rejects (for example an over-limit or malformed text) and 63 accepted texts
- **THEN** all 63 accepted texts receive their vectors and only the rejected input lacks one, with a log line naming the rejected input

#### Scenario: Whole-batch transport failure remains batch-scoped
- **WHEN** the embedding server is unreachable
- **THEN** all texts in the in-flight batches lack vectors, no exception escapes the pipeline, and other batches are unaffected

### Requirement: Asymmetric query and document prefixes

The embedding configuration SHALL accept optional `docPrefix` and `queryPrefix` keys (defaults: empty). The `docPrefix` SHALL be prepended to every embedded text sent to the embedding API; the `queryPrefix` SHALL be prepended to query-time text when the embedding API is used for search or similarity. Empty prefixes MUST reproduce today's unprefixed behavior byte-for-byte, and both prefixes SHALL participate in the cache key and persisted source-hash alongside the model name.

#### Scenario: Empty prefixes preserve current behavior
- **WHEN** `graphos.yaml` contains `embedding: {enabled: true}` with no `docPrefix`/`queryPrefix` keys
- **THEN** the texts sent to the API are byte-identical to today's pipeline output

#### Scenario: LFM-style prefixes applied to documents
- **WHEN** `docPrefix: "document: "` is configured
- **THEN** every API payload text begins with `"document: "` and cache keys differ from the unprefixed run's

#### Scenario: Prefix change invalidates the cache
- **WHEN** a run completes with empty prefixes and a second run uses `docPrefix: "document: "`
- **THEN** the second run re-embeds the texts rather than serving vectors cached under the unprefixed keys

### Requirement: Prepared text participates in the content hash

Each persisted embedding record's source-hash and each cache key SHALL be computed over the model name, the effective prefixes, and the **prepared** text actually submitted to the API. Two nodes whose texts prepare identically MUST produce equal source-hashes; two nodes whose raw texts differ but prepare identically MUST hit the same cache entry.

#### Scenario: Identical preparation converges on one cache entry
- **WHEN** two doc-body texts differ only beyond the 512-token truncation point
- **THEN** they share one cache entry and one API call, and both nodes receive the same vector

#### Scenario: Source hash reflects the prepared text
- **WHEN** a record is persisted for text `t` prepared under model `m` with prefix `p`
- **THEN** its source-hash equals the hash the current content-hash rule produces for `(m, p, prepare(t))`, and differs from any hash computed over raw `t`

### Requirement: Streaming sidecar write

The embedding configuration SHALL accept a `streaming` key (default `true`). When enabled, embedding vectors SHALL be written to a staged `embeddings.json` incrementally — each completed batch's entries append to the staged file — and the staged file SHALL be atomically renamed into place when the embedding pass completes. When `streaming` is `false`, the sidecar SHALL be written once at the end of the pass (today's behavior). An interrupted run MUST NOT leave a partially written `embeddings.json` at the final path.

#### Scenario: Interrupted streaming run leaves prior sidecar intact
- **WHEN** a run with `streaming: true` is interrupted mid-embedding-pass
- **THEN** the previous `embeddings.json` remains valid and complete, and no partially written file exists at the final path

#### Scenario: Streaming writes complete sidecar atomically
- **WHEN** the embedding pass completes with `streaming: true`
- **THEN** `embeddings.json` is atomically renamed into place and its content equals a write-at-end run's content

#### Scenario: Opt-out preserves today's write-at-end behavior
- **WHEN** `embedding.streaming: false` is configured
- **THEN** the sidecar is written once at the end of the pass, and no staged embedding file remains in the output directory

### Requirement: Default model is LFM2.5-Embedding-350M

The default value of `embedding.model` SHALL change to `hf.co/LiquidAI/LFM2.5-Embedding-350M-GGUF:Q4_K_M` (or its locally installed alias), with `dimension: 0` (auto-detect) retained, citing the recorded benchmark showing ≥4× sequential throughput over nomic-embed-text on the target hardware. Existing configs naming `nomic-embed-text` explicitly MUST continue to work unchanged.

#### Scenario: Fresh config gets the new default
- **WHEN** a user initializes a new `graphos.yaml` without an `embedding.model` key
- **THEN** the effective model is the LFM2.5-Embedding-350M GGUF reference

#### Scenario: Explicit nomic config unchanged
- **WHEN** `graphos.yaml` explicitly sets `embedding.model: nomic-embed-text`
- **THEN** embeddings are generated with nomic-embed-text and the cache keys match the nomic-based entries from previous runs