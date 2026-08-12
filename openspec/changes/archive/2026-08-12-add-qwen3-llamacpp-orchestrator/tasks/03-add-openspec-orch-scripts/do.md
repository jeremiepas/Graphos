# Task 3 — Add openspec-orch lifecycle scripts to devenv.nix — DO

**Task slug**: `03-add-openspec-orch-scripts`
**Attempt**: 1
**Status**: in-progress

## Summary

Added `scripts.openspec-orch.exec` in `devenv.nix` with `start`/`stop`/`status` subcommands for managing the orchestrator lifecycle.

## Detail

### Implementation
- Implemented three subcommands in `scripts.openspec-orch.exec`:
  - `start` — launches `orchestrate.py` in background via nohup, writes PID to `graphos-out/orchestrator/orchestrator.pid`, logs to `graphos-out/orchestrator/orchestrator.log`
  - `stop` — reads PID file, sends SIGTERM, force kills after 2 seconds if needed
  - `status` — checks PID liveness via `kill -0`, reports running/stopped/paused

### Key decisions
- Used nohup for background execution to survive shell exit
- PID file management for reliable process tracking
- Graceful shutdown with SIGTERM followed by force kill
- Status check includes question file detection for pause detection

### Concrete changes
- Modified file: `devenv.nix` (added `scripts.openspec-orch.exec`)
