<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 10 — Split UseCase.Pipeline into focused sub-modules — ACT

**Task slug**: `10-split-usecase-pipeline`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. The Do phase has not been executed. Prerequisite: Task 7 must be completed first (Pipeline uses ports).

## Detail

### Check Outcome

| # | Criterion | Result | Notes |
|---|-----------|--------|-------|
| 1 | UseCase.Pipeline is re-export only (<30 lines) | FAIL | Currently 592 lines with implementation |
| 2 | Sub-modules focused (<300 lines each) | FAIL | Core.hs, Checkpoint.hs, Incremental.hs do not yet exist |
| 3 | `cabal build` succeeds | PASS | Pre-split baseline |
| 4 | `cabal test` passes | PASS | Pre-split baseline |
| 5 | Existing imports still compile | PASS | Pre-split baseline |

### Prerequisites

Task 7 (Pipeline uses ports) must be completed before this task.

## Result

**NOT OK** — Task 10 requires implementation after Task 7. See attempt-2/ for the next PDCA cycle.