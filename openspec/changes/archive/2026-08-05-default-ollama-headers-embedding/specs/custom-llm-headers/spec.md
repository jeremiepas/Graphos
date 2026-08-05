## ADDED Requirements

### Requirement: Custom HTTP headers on LLM API calls

The system SHALL support a `headers` field on `LabelingConfig`, `EmbeddingConfig`, and `VisionConfig` of type `Map String String`. When non-empty, each key-value pair SHALL be sent as an HTTP header (`-H "Key: Value"`) in the corresponding curl call.

#### Scenario: Custom auth header for labeling
- **WHEN** `labeling.headers` contains `{"X-API-Key": "my-token"}` and `labeling.apiKey` is empty
- **THEN** the curl call to the LLM API includes `-H "X-API-Key: my-token"` and no `Authorization` header

#### Scenario: Custom headers override Authorization
- **WHEN** `labeling.apiKey` is `"sk-123"` and `labeling.headers` contains `{"Authorization": "Token abc"}`
- **THEN** the curl call uses `Authorization: Token abc` (headers override the Bearer token)

#### Scenario: Custom headers on embedding calls
- **WHEN** `embedding.headers` contains `{"X-API-Key": "${MY_KEY}"}` and `embedding.baseUrl` points to a custom gateway
- **THEN** the embedding curl call includes `-H "X-API-Key: <resolved-value>"` with env var expansion

#### Scenario: Custom headers on vision calls
- **WHEN** `vision.headers` contains `{"X-Tenant-ID": "acme"}`
- **THEN** the vision curl call includes `-H "X-Tenant-ID: acme"`

### Requirement: Headers field in config types

The system SHALL define `labelingHeaders :: Map String String` on `LabelingConfig`, `embHeaders :: Map String String` on `EmbeddingConfig`, and `vcHeaders :: Map String String` on `VisionConfig`. All three SHALL default to `Map.empty`. The `FromJSON` instances SHALL parse a `headers` key as a YAML mapping, defaulting to empty. The `ToJSON` instances SHALL serialize headers as a JSON object.

#### Scenario: FromJSON with headers
- **WHEN** graphos.yaml contains `labeling: {provider: ollama, headers: {X-API-Key: my-token}}`
- **THEN** `labelingHeaders` is `Map.fromList [("X-API-Key", "my-token")]`

#### Scenario: FromJSON without headers
- **WHEN** graphos.yaml contains `labeling: {provider: ollama}` with no `headers` key
- **THEN** `labelingHeaders` is `Map.empty`

#### Scenario: Config merge preserves explicit headers
- **WHEN** global config has `headers: {X-API-Key: global-key}` and project config has `headers: {X-Tenant: proj-tenant}`
- **THEN** merged config has `headers: {X-Tenant: proj-tenant}` (project wins, not union — same pattern as other scalar sections)

### Requirement: Env var expansion in header values

The system SHALL apply `resolveEnvVars` to all header values before use, supporting the `${VAR}` pattern. This enables referencing secrets from environment variables in custom headers.

#### Scenario: Env var in header value
- **WHEN** `headers` contains `{"X-API-Key": "${MY_API_KEY}"}` and environment variable `MY_API_KEY` is `"secret123"`
- **THEN** the resolved header value is `"secret123"`

### Requirement: Auth header merge strategy

The system SHALL compose HTTP auth headers in this order: (1) if `provider /= "ollama"` and `apiKey /= ""`, add `Authorization: Bearer <apiKey>`; (2) merge custom `headers` map entries as `-H "Key: Value"`; (3) on key collision between (1) and (2), custom `headers` SHALL override.

#### Scenario: Ollama with no apiKey and no headers
- **WHEN** `provider` is `"ollama"`, `apiKey` is `""`, and `headers` is empty
- **THEN** no auth header is included in the curl call

#### Scenario: OpenAI with apiKey and no headers
- **WHEN** `provider` is `"openai"`, `apiKey` is `"sk-123"`, and `headers` is empty
- **THEN** the curl call includes `-H "Authorization: Bearer sk-123"`

#### Scenario: Ollama with custom header
- **WHEN** `provider` is `"ollama"`, `apiKey` is `""`, and `headers` contains `{"X-Auth": "token"}`
- **THEN** the curl call includes `-H "X-Auth: token"` and no `Authorization` header