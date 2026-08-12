# Task 3 — Add openspec-orch lifecycle scripts to devenv.nix — PLAN

**Task slug**: `03-add-openspec-orch-scripts`
**Attempt**: 1
**Status**: pending

## Summary

Add `scripts.openspec-orch.exec` in `devenv.nix` with `start`/`stop`/`status` subcommands for managing the orchestrator lifecycle. `start` launches `orchestrate.py` in background via nohup, writes PID to `graphos-out/orchestrator/orchestrator.pid`, logs to `graphos-out/orchestrator/*.log`. `stop` reads PID and sends SIGTERM. `status` checks PID liveness and question file state.

## Detail

### Scope
- Add `scripts.openspec-orch.exec` in `devenv.nix`
- Implement three subcommands:
  - `start` — launch `orchestrate.py` with nohup, capture PID, log to `graphos-out/orchestrator/`
  - `stop` — read PID file, send SIGTERM, clean up PID file
  - `status` — check PID liveness via `kill -0`, inspect question files in `questions/`
- Follow existing pattern of `scripts.orchestrator.exec` already in devenv.nix

### Check Criteria

1. **Test**: `openspec-orch start` creates PID file and starts process
   - **PASS**: `openspec-orch start ; test -f graphos-out/orchestrator/orchestrator.pid && echo "PID_FILE_EXISTS"` outputs "PID_FILE_EXISTS"
2. **Test**: `openspec-orch stop` terminates the process
   - **PASS**: `openspec-orch stop ; pid=$(cat graphos-out/orchestrator/orchestrator.pid 2>/dev/null); kill -0 $pid 2>/dev/null || echo "PROCESS_STOPPED"` outputs "PROCESS_STOPPED"
3. **Test**: `openspec-orch status` reports correct state
   - **PASS**: After stop, `openspec-orch status` reports "stopped" (case-insensitive check)
4. **Test**: Log files appear under `graphos-out/orchestrator/`
   - **PASS**: `ls graphos-out/orchestrator/*.log 2>/dev/null | grep -c '\.log'` ≥ 1
5. **Spec scenarios**: `devenv-shell` — "devenv exposes openspec-orch script"
   - **PASS**: spec requirement 3.1 (script is available), 3.2 (logs to graphos-out/orchestrator/)

### Fail conditions
- PID file not created after start → FAIL
- Process does not respond to SIGTERM → FAIL
- Status output does not indicate correct state → FAIL
- No log files produced → FAIL

### Affected modules
- Modified file: `devenv.nix` (add `scripts.openspec-orch.exec`)

### Prerequisites
- Task 4 must complete first (or be combined): `graphos-out/orchestrator/` directory exists
- `orchestrate.py` exists at project root

### Risks
- Script must handle edge cases: missing PID file, stale PID, concurrent starts
- May need to combine with task 4 if directory creation is a hard prerequisite
