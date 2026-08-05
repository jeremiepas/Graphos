<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 7 — Refactor UseCase.Pipeline to use ports — ACT

**Task slug**: `07-refactor-usecase-pipeline-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. The Do phase has not been executed for this task.

## Detail

### Check Outcome

| # | Criterion | Result | Notes |
|---|-----------|--------|-------|
| 1 | No Infrastructure imports in UseCase.Pipeline | FAIL | 8+ Infrastructure imports remain |
| 2 | `cabal build` succeeds | PASS | Pre-refactoring baseline |
| 3 | `cabal test` passes | PASS | Pre-refactoring baseline |
| 4 | Pipeline functions take AppEnv | FAIL | `runPipeline` does not yet take AppEnv |

### Next Steps

This task requires implementation:
1. Add AppEnv parameter to `runPipeline` and related functions
2. Replace all 8 Infrastructure imports with port method calls
3. Remove suspicious `productionAppEnv` import (Pipeline should receive AppEnv, not create it)
4. Update Main.hs to pass AppEnv
5. Re-run Check criteria

## Result

**NOT OK** — Task 7 requires implementation. See attempt-2/ for the next PDCA cycle.