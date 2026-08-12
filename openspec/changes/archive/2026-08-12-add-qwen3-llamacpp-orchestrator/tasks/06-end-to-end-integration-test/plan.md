# Task 6 — End-to-end integration test — PLAN

**Task slug**: `06-end-to-end-integration-test`
**Attempt**: 1
**Status**: pending

## Summary

Create a minimal test change in `openspec/changes/` with a trivial spec, then run the full orchestrator loop (start → artifact advance → validate → archive → stop) end-to-end. Also verify `devenv tasks run orchestrator:run` still works.

## Detail

### Scope
- Create a test change directory `openspec/changes/test-e2e-orch/` with one spec
- Run `openspec-orch start test-e2e-orch` and observe full PDCA cycle
- Verify logs in `graphos-out/orchestrator/*.log` contain artifact timestamps
- Verify `metrics.jsonl` records events
- Verify exit code 0 on clean archive or 10 on question pause
- Run `devenv tasks run orchestrator:run` for backward compatibility

### Check Criteria

1. **Test**: Full PDCA cycle completes for test change
   - **PASS**: `openspec-orch start test-e2e-orch` → exit code 0 within 300s
2. **Test**: Logs contain artifact timestamps
   - **PASS**: `grep -c "artifact" graphos-out/orchestrator/*.log` ≥ 1
3. **Test**: `metrics.jsonl` records at least one event per phase
   - **PASS**: `grep -c '"phase"' graphos-out/orchestrator/metrics.jsonl` ≥ 7 (proposal through act)
4. **Test**: `openspec-orch stop` works after archive
   - **PASS**: `openspec-orch stop` returns 0
5. **Test**: `devenv tasks run orchestrator:run` still works
   - **PASS**: `devenv tasks run orchestrator:run --dry-run` returns 0
6. **Spec scenarios**: `openspec-orchestrator` — "orchestrator drives change through all PDCA artifacts"
   - **PASS**: spec requirement 1.1, 1.2, 2.1, 3.1

### Fail conditions
- Orchestrator crashes during artifact advance → FAIL
- No metrics events recorded → FAIL
- Exit code non-zero and not 10 (question pause) → FAIL
- `orchestrator:run` devenv task no longer works → FAIL

### Affected modules
- New test change: `openspec/changes/test-e2e-orch/` (created for testing, archived after)
- Existing: `orchestrate.py` (may need debugging)

### Prerequisites
- Tasks 1–5 must complete: provider config, envrc, lifecycle scripts, directory, status task
- llama.cpp server running on `localhost:8080`

### Risks
- Test change may need multiple iterations if artifacts stall
- llama.cpp server availability is an external dependency; test may fail if server is down
