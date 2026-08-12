# Task 5 — Add orchestrator:status devenv task and oporch alias — PLAN

**Task slug**: `05-add-orchestrator-status-task`
**Attempt**: 1
**Status**: pending

## Summary

Add a `orchestrator:status` devenv task that runs `openspec-orch status`, and add `oporch` as an alias for `openspec-orch` on the devenv shell PATH.

## Detail

### Scope
- Register `orchestrator:status` task in `devenv.nix`
- Create `oporch` alias pointing to `openspec-orch`
- Place alias in `env.sh` or equivalent profile script

### Check Criteria

1. **Test**: `oporch` runs successfully from the devenv shell
   - **PASS**: `bash -c "source <(grep -A3 'oporch' devenv.nix | tail -n+2); oporch --help 2>&1" | grep -c "openspec-orch"` ≥ 1
2. **Test**: `devenv tasks run orchestrator:status` outputs state
   - **PASS**: `devenv tasks run orchestrator:status` returns 0 and outputs one of: "running", "stopped", "paused"
3. **Spec scenarios**: `devenv-shell` — "oporch alias and orchestrator:status task available"
   - **PASS**: spec requirement 3.4, 3.5

### Fail conditions
- `oporch` not found in shell → FAIL
- `orchestrator:status` task does not output valid state → FAIL

### Affected modules
- Modified file: `devenv.nix` (add task and alias)

### Prerequisites
- Task 3 must complete: `openspec-orch` script exists

### Risks
- Alias may conflict with existing commands; verify no name collision
