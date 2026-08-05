## MODIFIED Requirements

### Requirement: Default labeling provider is Ollama

The system SHALL default `LabelingConfig` to `provider: "ollama"`, `model: "llama3.2"`, `apiKey: ""`, `baseUrl: "http://localhost:11434/v1"`, `batchSize: 20`. When no labeling section is provided in graphos.yaml, these defaults SHALL be used.

#### Scenario: No labeling config in graphos.yaml
- **WHEN** no `labeling` section exists in graphos.yaml
- **THEN** `LabelingConfig` defaults to `{provider: "ollama", model: "llama3.2", apiKey: "", baseUrl: "http://localhost:11434/v1", batchSize: 20, headers: Map.empty}`

#### Scenario: Explicit OpenAI config still works
- **WHEN** graphos.yaml contains `labeling: {provider: openai, model: gpt-4o-mini, apiKey: "${OPENAI_API_KEY}", baseUrl: "https://api.openai.com/v1"}`
- **THEN** the system uses OpenAI for labeling with Bearer token auth

## ADDED Requirements

### Requirement: LabelingConfig headers field

The system SHALL add `labelingHeaders :: Map String String` to `LabelingConfig` with default value `Map.empty`. The `FromJSON` instance SHALL parse a `headers` key from YAML as a string-to-string mapping, defaulting to empty map when absent.

#### Scenario: Labeling with custom headers in YAML
- **WHEN** graphos.yaml contains `labeling: {provider: litellm, baseUrl: "http://proxy:4000/v1", headers: {X-API-Key: "${LITELLM_KEY}"}}`
- **THEN** `callLLM` includes `-H "X-API-Key: <resolved>"` in the curl request