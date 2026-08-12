# llamacpp-model-provider Specification

## Purpose
TBD - created by archiving change add-qwen3-llamacpp-orchestrator. Update Purpose after archive.
## Requirements
### Requirement: opencode provider configuration for llama.cpp
The `llamacpp-model-provider` SHALL be an opencode model provider configuration that connects to a local llama.cpp server running Qwen 3.6, exposing an OpenAI-compatible `/v1/chat/completions` endpoint at `http://localhost:8080`.

- **Plan**: Provide the wiring between opencode and llama.cpp so the orchestrator agent uses the local Qwen 3.6 model without any cloud API dependency.
- **Do**: Create an opencode provider configuration (e.g., under `.opencode/providers/` or via environment variables) that sets the base URL to `http://localhost:8080/v1/chat/completions`, uses the model name `qwen3.6-35b-a3b` (or equivalent), and disables all cloud provider keys.
- **Check**: The scenarios below verify provider configuration and connectivity.

#### Scenario: Provider configuration points to local llama.cpp
- **WHEN** the llamacpp-model-provider is configured
- **THEN** all opencode chat completion requests are routed to `http://localhost:8080/v1/chat/completions`

#### Scenario: Provider uses the correct model name
- **WHEN** opencode sends a chat completion request
- **THEN** the request includes `model: "qwen3.6-35b-a3b"` (or the model name specified in the provider configuration)

#### Scenario: Provider rejects cloud API keys
- **WHEN** a cloud provider key (e.g., `OPENAI_API_KEY`) is set in the environment
- **THEN** opencode with the llamacpp-model provider ignores it and continues targeting the local endpoint

#### Scenario: Provider fails gracefully on connection refused
- **WHEN** llama.cpp is not running at `http://localhost:8080`
- **THEN** opencode returns a clear connection error rather than falling back to any other provider

#### Scenario: Provider supports OpenAI-compatible message format
- **WHEN** opencode dispatches a chat completion
- **THEN** the request uses the OpenAI-compatible `messages` array format with `role` (system/user/assistant) and `content` fields

### Requirement: Model provider wiring consumed by the orchestrator
The orchestrator SHALL use the llamacpp-model-provider as its sole model configuration. The orchestrator SHALL NOT accept or route LLM requests to any other model provider.

- **Plan**: Ensure the orchestrator is tightly coupled to the local model provider and cannot accidentally consume cloud APIs.
- **Do**: Hard-code or environment-gate the provider reference in the orchestrator's configuration so only the local llama.cpp endpoint is used.
- **Check**: The scenarios below verify the binding.

#### Scenario: Orchestrator references the llamacpp provider
- **WHEN** the orchestrator initializes its opencode configuration
- **THEN** the model provider is set to `llamacpp` (or the exact provider name defined in the llamacpp-model-provider configuration)

#### Scenario: Orchestrator does not fall back to cloud providers
- **WHEN** the local endpoint is unavailable
- **THEN** the orchestrator does NOT attempt to use any alternative model provider (OpenAI, Anthropic, etc.)

#### Scenario: Provider name is configurable
- **WHEN** the llamacpp-model-provider configuration is created with a custom name
- **THEN** the orchestrator reads that name from configuration and uses it consistently

