## ADDED Requirements

### Requirement: Vision configuration in graphos.yaml
The system SHALL support a `vision` section in graphos.yaml with fields: `enabled` (bool, default false), `model` (string, default "qwen3.6-moe"), `apiKey` (string, default "${OPENAI_API_KEY}"), `baseUrl` (string, default "http://localhost:11434/v1"), `maxTokens` (int, default 1000), `batchSize` (int, default 5). When `vision.apiKey` or `vision.baseUrl` are not explicitly set, the system SHALL inherit from `labeling.apiKey` and `labeling.baseUrl`.

#### Scenario: Default config with vision disabled
- **WHEN** no vision section is in graphos.yaml
- **THEN** vision defaults to `{enabled: false, model: "qwen3.6-moe", apiKey: "${OPENAI_API_KEY}", baseUrl: "http://localhost:11434/v1", maxTokens: 1000, batchSize: 5}` and no LLM calls are made for images

#### Scenario: Explicit vision config overrides defaults
- **WHEN** graphos.yaml contains `vision: {enabled: true, model: "gpt-4o", apiKey: "${OPENAI_API_KEY}"}`
- **THEN** the system uses gpt-4o for vision calls with the specified API key

#### Scenario: Vision inherits from labeling config
- **WHEN** graphos.yaml contains `labeling: {apiKey: "sk-123", baseUrl: "https://api.openai.com/v1"}` and `vision: {enabled: true, model: "gpt-4o"}`
- **THEN** vision uses `apiKey: "sk-123"` and `baseUrl: "https://api.openai.com/v1"` inherited from labeling, with model gpt-4o

### Requirement: VisionConfig type in Domain
The system SHALL define a `VisionConfig` data type in `Graphos.Domain.Config` with fields: `vcEnabled`, `vcModel`, `vcApiKey`, `vcBaseUrl`, `vcMaxTokens`, `vcBatchSize`. It SHALL have `ToJSON`/`FromJSON` instances with sensible defaults and environment variable resolution (`${VAR}` pattern) for apiKey.

#### Scenario: VisionConfig FromJSON with partial override
- **WHEN** graphos.yaml contains `vision: {enabled: true, model: "claude-sonnet-4-20250514"}`
- **THEN** VisionConfig parses as `{vcEnabled: True, vcModel: "claude-sonnet-4-20250514", vcApiKey: "${OPENAI_API_KEY}", vcBaseUrl: "http://localhost:11434/v1", vcMaxTokens: 1000, vcBatchSize: 5}` (defaults for missing fields)

### Requirement: PipelineConfig integration
The system SHALL add a `cfgVision` field to `PipelineConfig` sourced from `VisionConfig`. The `--vision` CLI flag SHALL enable vision analysis (setting `vcEnabled = True`). The `--no-vision` flag SHALL disable it.

#### Scenario: --vision flag enables image analysis
- **WHEN** the CLI is invoked with `--vision` flag
- **THEN** `cfgVision.vcEnabled` is set to True and images are analyzed

#### Scenario: Default without flag
- **WHEN** the CLI is invoked without --vision or --no-vision
- **THEN** `cfgVision.vcEnabled` follows the config file (default: false)