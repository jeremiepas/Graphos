# Task 2 — Add .envrc defaults for orchestrator environment variables — DO

**Task slug**: `02-add-envrc-defaults`
**Attempt**: 1
**Status**: in-progress

## Summary

Created `.envrc` with default environment variables for the orchestrator: `LLAMA_BASEURL`, `OPENCODE_MODEL`, `ORCHESTRATOR_LOG_DIR`, `ORCHESTRATOR_MAX_REMEDIATION`, `ORCHESTRATOR_TIMEOUT`.

## Detail

### Implementation
- Created `.envrc` with all required environment variables
- Default values match `orchestrate.py` defaults:
  - `LLAMA_BASEURL=http://localhost:8080/v1`
  - `OPENCODE_MODEL=qwen3.6-35b-a3b`
  - `ORCHESTRATOR_LOG_DIR=graphos-out/orchestrator`
  - `ORCHESTRATOR_MAX_REMEDIATION=3`
  - `ORCHESTRATOR_TIMEOUT=3600`
- Added direnv allow command in shellHook

### Key decisions
- Used direnv-compatible `export` syntax
- Defaults configured to work with local llama.cpp server
- No cloud API keys in .envrc

### Concrete changes
- New file: `.envrc`
