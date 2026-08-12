# Task 5 — Add orchestrator:status devenv task and oporch alias — DO

**Task slug**: `05-add-orchestrator-status-task`
**Attempt**: 1
**Status**: in-progress

## Summary

Added `orchestrator:status` devenv task and `oporch` alias for `openspec-orch` command.

## Detail

### Implementation
- Added `orchestrator:status` devenv task that runs `openspec-orch status`
- Added `oporch` alias in `shellHook` pointing to `openspec-orch`
- Added `env.PATH = [ "./scripts" ]` to make local scripts available in PATH

### Key decisions
- Used `shellHook` for alias definition (runs after shell initialization)
- PATH configuration ensures openspec-orch script is accessible
- Task follows existing devenv task pattern (like `llama:server`)

### Concrete changes
- Modified file: `devenv.nix` (added `orchestrator:status` task and `shellHook` with alias)
