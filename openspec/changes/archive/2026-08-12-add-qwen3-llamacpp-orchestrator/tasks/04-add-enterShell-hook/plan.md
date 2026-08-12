# Task 4 — Add enterShell hook to create orchestrator output directory — PLAN

**Task slug**: `04-add-enterShell-hook`
**Attempt**: 1
**Status**: pending

## Summary

Add a devenv `enterShell` hook that runs `mkdir -p graphos-out/orchestrator` to ensure the log directory exists before the orchestrator service writes any output.

## Detail

### Scope
- Add `enterShell` hook in `devenv.nix`
- Runs `mkdir -p graphos-out/orchestrator` on shell activation
- Placed near existing setup hooks for consistency

### Check Criteria

1. **Test**: Activating devenv shell creates `graphos-out/orchestrator/`
   - **PASS**: `rm -rf graphos-out/orchestrator ; bash -c "source <(grep -A5 'enterShell' devenv.nix | tail -n+2)" ; test -d graphos-out/orchestrator && echo "DIR_CREATED"` outputs "DIR_CREATED"
2. **Test**: Directory persists across shell restarts
   - **PASS**: After two activations, `test -d graphos-out/orchestrator` returns 0
3. **Spec scenarios**: `devenv-shell` — "orchestrator log directory exists"
   - **PASS**: spec requirement 3.3 (directory created on shell activation)

### Fail conditions
- Directory does not exist after shell activation → FAIL
- Hook conflicts with existing hooks causing error → FAIL

### Affected modules
- Modified file: `devenv.nix` (add `enterShell` hook)

### Prerequisites
- None (standalone task)

### Risks
- Low risk: `mkdir -p` is idempotent; safe to run even if directory exists
