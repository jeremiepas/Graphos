## MODIFIED Requirements

### Requirement: EmbeddingConfig headers field

The system SHALL add `embHeaders :: Map String String` to `EmbeddingConfig` with default value `Map.empty`. The `FromJSON` instance SHALL parse a `headers` key from YAML as a string-to-string mapping, defaulting to empty map when absent. Custom headers SHALL be passed to every embedding HTTP request, including batched requests.

#### Scenario: Embedding with custom headers in YAML
- **WHEN** graphos.yaml contains `embedding: {enabled: true, headers: {X-API-Key: "my-key"}}`
- **THEN** every embedding request includes the `X-API-Key: my-key` header

#### Scenario: Embedding without custom headers
- **WHEN** graphos.yaml contains `embedding: {enabled: true}` with no `headers` key
- **THEN** embedding requests are sent with only the `Content-Type: application/json` header (no extra headers)

## ADDED Requirements

### Requirement: Batched embedding API surface

The embedding client SHALL expose a batched operation taking a list of texts and returning either an error or one vector per input text, in input order. The single-text operation SHALL remain available and MUST be equivalent to a batch of one.

#### Scenario: Single text via batched API
- **WHEN** the batched operation is called with exactly one text
- **THEN** the result is the same vector the single-text operation returns for that text

### Requirement: Embedding records carry a content hash

Each persisted embedding record SHALL store, in its source-hash field, the SHA-256 digest of the model name concatenated with the embedded text — not the source file path. Two records with the same text and model MUST have equal source-hash values.

#### Scenario: Source hash is content-derived
- **WHEN** two nodes from different source files share the same embedded text and model
- **THEN** their embedding records have equal source-hash values, and the value differs from any file path

### Requirement: HTTP transport without process spawning

The embedding client SHALL perform HTTP requests in-process via a native Haskell HTTP client with a shared connection manager, and MUST NOT spawn external processes or write payloads to fixed temporary files.

#### Scenario: Embedding runs without curl
- **WHEN** the embedding pipeline runs on a machine where the `curl` binary is absent
- **THEN** embeddings are still generated successfully