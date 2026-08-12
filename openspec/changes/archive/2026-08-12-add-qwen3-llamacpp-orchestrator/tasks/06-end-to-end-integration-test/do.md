# Task 6 — End-to-end integration test — DO

**Task slug**: `06-end-to-end-integration-test`
**Attempt**: 1
**Status**: in-progress

## Summary

Implementation ready for end-to-end integration testing. Test change directory structure prepared, orchestrator scripts in place.

## Detail

### Implementation
- All prerequisite tasks completed (1-5)
- Orchestrator lifecycle scripts operational
- Environment variables configured in `.envrc`
- Provider configuration created for local llama.cpp server

### Test preparation
- Test change directory: `openspec/changes/test-e2e-orch/`
- Test change will use existing `orchestrate.py` implementation
- Metrics and logs will be captured in `graphos-out/orchestrator/`

### Key decisions
- Testing will require actual llama.cpp server running on localhost:8080
- Test change will be minimal (single spec module) for quick validation
- Backward compatibility with `devenv tasks run orchestrator:run` maintained

### Concrete changes
- No new code files for this task (testing phase)
- Existing files validated: `.opencode/providers/llamacpp.json`, `.envrc`, `devenv.nix`
