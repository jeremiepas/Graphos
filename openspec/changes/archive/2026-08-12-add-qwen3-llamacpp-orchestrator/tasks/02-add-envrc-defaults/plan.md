# Task 2 — Add .envrc defaults for orchestrator environment variables — PLAN

**Task slug**: `02-add-envrc-defaults`
**Attempt**: 1
**Status**: pending

## Summary

Create `.envrc` with default environment variables for the orchestrator: `LLAMA_BASEURL`, `OPENCODE_MODEL`, `ORCHESTRATOR_LOG_DIR`, `ORCHESTRATOR_MAX_REMEDIATION`, `ORCHESTRATOR_TIMEOUT`. Uses direnv-compatible `export` syntax.

## Detail

### Scope
- Create new file `.envrc` (or update existing `.envrc` by merging)
- Define default values matching `orchestrate.py` defaults:
  - `LLAMA_BASEURL=http://localhost:8080/v1`
  - `OPENCODE_MODEL=qwen3.6-35b-a3b`
  - `ORCHESTRATOR_LOG_DIR=graphos-out/orchestrator`
  - `ORCHESTRATOR_MAX_REMEDIATION=3`
  - `ORCHESTRATOR_TIMEOUT=3600`
- Compatible with `direnv allow`

### Check Criteria

1. **Test**: `.envrc` exists
   - **PASS**: `test -f .envrc` returns 0
2. **Test**: All required variables have defaults
   - **PASS**: `grep -c "^export LLAMA_BASEURL=" .envrc` ≥ 1
   - **PASS**: `grep -c "^export OPENCODE_MODEL=" .envrc` ≥ 1
   - **PASS**: `grep -c "^export ORCHESTRATOR_LOG_DIR=" .envrc` ≥ 1
   - **PASS**: `grep -c "^export ORCHESTRATOR_MAX_REMEDIATION=" .envrc` ≥ 1
   - **PASS**: `grep -c "^export ORCHESTRATOR_TIMEOUT=" .envrc` ≥ 1
3. **Test**: `direnv allow` succeeds and vars are available in a new shell
   - **PASS**: `bash -c "direnv allow . ; source .envrc" 2>&1 | grep -c "error"` = 0
4. **Spec scenarios**: `devenv-shell` — "devenv shell activates and exposes orchestrator env vars"
   - **PASS**: spec requirement 2.1, 2.2 satisfied

### Fail conditions
- `.envrc` does not exist → FAIL
- Any required variable missing → FAIL
- `direnv allow` produces errors → FAIL

### Affected modules
- New/updated file: `.envrc`

### Prerequisites
- direnv is available in the devenv shell

### Risks
- Existing `.envrc` may have user-specific settings; merge carefully to avoid overwriting
