<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 12 — Final verification and CI gate — PLAN

**Task slug**: `12-final-verification-ci-gate`
**Attempt**: 1
**Status**: pending

## Summary

Comprehensive verification that all architectural constraints are met: zero UseCase→Infrastructure imports, no bare IO in UseCase signatures, all modules under size limits, and clean build/test.

## Detail

### Scope

Run a full verification pass across the entire codebase:
1. Verify zero UseCase→Infrastructure imports (excluding Port modules)
2. Verify all modules <300 lines (excluding re-export modules)
3. Verify build passes with `-Wall -Werror`
4. Verify all tests pass with zero failures
5. Verify no bare `IO` in UseCase signatures (only `MonadIO m =>` or port-constrained)

### Spec Scenarios

This task covers the final Check gate from the design verification strategy:
- Gate 1: No UseCase→Infrastructure imports
- Gate 2: All modules <300 lines
- Gate 3: Build passes with warnings-as-errors
- Gate 4: All tests pass
- Gate 5: No bare IO in UseCase signatures

### Affected Modules

- All UseCase modules (verification)
- All Domain/Config modules (verification)
- `app/Main.hs` (verification)
- `.opencode/context/core/standards/code-quality.md` (documentation update)

### Prerequisites

- All Tasks 1-11 complete
- All refactoring done and building

### Risks

- Pre-existing test failures (e.g., SDKSpec flaky failure noted in Task 1) should be documented but not block
- `-Wall -Werror` may surface new warnings from the refactoring
- Bare IO check may find legitimate uses that need port abstraction

## Check Criteria

| # | Criterion | Spec Scenario | PASS Condition | FAIL Condition |
|---|-----------|---------------|----------------|-----------------|
| 1 | Zero UseCase→Infrastructure imports | Design verification | `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/` returns zero (excluding Port modules and re-export modules) | Any Infrastructure import found in UseCase |
| 2 | All modules <300 lines | Design verification | `find src/ -name "*.hs" -exec wc -l {} + \| sort -rn` shows all modules <300 lines (excluding re-exports like Domain.Config.hs, UseCase.Extract.hs, UseCase.Pipeline.hs) | Any non-re-export module ≥300 lines |
| 3 | Build passes with -Wall -Werror | Design verification | `cabal build` succeeds with dev flag (which enables -Wall -Werror) | Build failure or warnings |
| 4 | All tests pass | Design verification | `cabal test` passes with zero failures (note any pre-existing flaky failures) | Any test failure |
| 5 | No bare IO in UseCase | Design verification | `rg ":: .*IO " src/Graphos/UseCase/` shows only `MonadIO m =>` or port-constrained signatures, no bare `IO` return types | Bare `IO` found in UseCase function signatures |