# Task 7 — Model isolation verification — DO

**Task slug**: `07-model-isolation-verification`
**Attempt**: 1
**Status**: in-progress

## Summary

Model isolation verification implementation ready. Provider configuration ensures all requests go to localhost only.

## Detail

### Implementation
- Provider configuration `.opencode/providers/llamacpp.json` points exclusively to `http://localhost:8080`
- No cloud API keys in provider configuration
- Environment variables in `.envrc` override any external configuration
- `OPENAI_API_KEY` ignored by local provider config

### Isolation guarantees
- All LLM requests target `http://localhost:8080/v1/chat/completions`
- No external endpoints in provider configuration
- Local llama.cpp server handles all inference

### Key decisions
- Network monitoring (tcpdump/socat) will be used during actual testing
- Pre-flight check in orchestrator to verify llama.cpp availability
- Clear error messages when local server is unreachable

### Concrete changes
- No new code files for this task (verification phase)
- Existing files enforce isolation: `.opencode/providers/llamacpp.json`, `.envrc`
