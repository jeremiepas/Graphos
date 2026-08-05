## MODIFIED Requirements

### Requirement: EmbeddingConfig headers field

The system SHALL add `embHeaders :: Map String String` to `EmbeddingConfig` with default value `Map.empty`. The `FromJSON` instance SHALL parse a `headers` key from YAML as a string-to-string mapping, defaulting to empty map when absent. Custom headers SHALL be passed to the embedding curl call.

#### Scenario: Embedding with custom headers in YAML
- **WHEN** graphos.yaml contains `embedding: {enabled: true, headers: {X-API-Key: "my-key"}}`
- **THEN** `generateEmbedding` includes `-H "X-API-Key: my-key"` in the curl request

#### Scenario: Embedding without custom headers
- **WHEN** graphos.yaml contains `embedding: {enabled: true}` with no `headers` key
- **THEN** `generateEmbedding` makes the curl request with only `Content-Type: application/json` (no extra headers)