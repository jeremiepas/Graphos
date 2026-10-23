# Task 2 — Verify the `devenv-shell` spec delta captures the invariant — PLAN

**Task slug**: `02-verify-devenv-shell-spec-delta`
**Attempt**: 1
**Status**: pending

## Summary

Confirm that the ADDED requirement in `specs/devenv-shell/spec.md` (within the `fix-ci-missing-hackage-package-list` change) correctly codifies the "CI build task refreshes the Hackage package index before configure" invariant. Validate that the delta uses proper OpenSpec schema: `## ADDED Requirements`, `#### Scenario:` headers (exactly 4 hashtags), SHALL/MUST language, and that WHEN/THEN clauses are present. Run `openspec validate --changes --json` and fix any schema errors.

## Detail

### Scope

- Review and validate `openspec/changes/fix-ci-missing-hackage-package-list/specs/devenv-shell/spec.md`.
- No code changes — spec text review and schema validation only.
- If `openspec validate` reports errors, fix the spec text (headers, sections, scenario format).

### Check Criteria (defined BEFORE code)

| Criterion | Test/Gate | Spec Scenarios | PASS Condition |
|-----------|-----------|----------------|----------------|
| C1 | `openspec validate --changes --json` returns valid with no errors | Schema validation | Exit 0, JSON output shows `valid: true` (or equivalent passing status) |
| C2 | The ADDED requirement block exists with correct header level | *(self-evident from file content)* | `## ADDED Requirements` header present, followed by `### Requirement:` sub-header |
| C3 | Every scenario uses exactly 4 hashtags (`#### Scenario:`) | *(self-evident from file content)* | `grep -c "^#### Scenario:"` returns 4 (the 4 new scenarios) |
| C4 | No scenario uses 3 hashtags (`### Scenario:`) | *(self-evident from file content)* | `grep -c "^### Scenario:"` returns 0 |
| C5 | The ADDED requirement text explicitly mentions `cabal update` before `cabal configure` | *(self-evident from file content)* | Requirement body contains both "cabal update" and "cabal configure" |

**Exact test commands**:
```bash
# C1: OpenSpec schema validation
openspec validate --changes --json

# C2: Check ADDED Requirements header
grep -c "^## ADDED Requirements" specs/devenv-shell/spec.md
# Expected: 1

# C3: Count scenarios (4 hashtags only)
grep -c "^#### Scenario:" specs/devenv-shell/spec.md
# Expected: >= 4

# C4: No 3-hashtag scenarios
grep -c "^### Scenario:" specs/devenv-shell/spec.md
# Expected: 0

# C5: Requirement mentions cabal update
grep "cabal update" specs/devenv-shell/spec.md
# Expected: at least one match
```

**FAIL boundaries**:
- FAIL if `openspec validate --changes --json` returns non-zero exit code
- FAIL if any scenario header is `### Scenario:` (3 hashtags — silently fails validation)
- FAIL if `## ADDED Requirements` is missing entirely
- FAIL if a scenario is missing a WHEN/THEN clause

### Affected Modules

| Module | Change |
|--------|--------|
| `openspec/changes/fix-ci-missing-hackage-package-list/specs/devenv-shell/spec.md` | Spec text review and fix (if validation errors found) |

### Prerequisites

- `openspec` CLI available and working.
- The spec delta file already exists (created in the same change).

### Risks

| Risk | Mitigation |
|------|------------|
| Scenario headers use 3 hashtags instead of 4 | OpenSpec validation will catch this; fix by upgrading to `####` |
| Baseline requirement incorrectly placed under MODIFIED | If the baseline requirement's content truly hasn't changed, it should NOT be in MODIFIED — remove it to avoid losing detail at archive time |
| Schema validation errors from missing WHEN/THEN | Add WHEN/THEN to each scenario if missing |

### Dependency graph

- **No dependencies on other tasks** (spec validation is independent of code changes).
- **Can run in parallel with Task 1** (spec validation doesn't depend on devenv.nix being correct).
