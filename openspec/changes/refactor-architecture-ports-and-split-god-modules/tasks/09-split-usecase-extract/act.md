<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 9 — Split UseCase.Extract into focused sub-modules — ACT

**Task slug**: `09-split-usecase-extract`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. The Do phase has not been executed for this task. Prerequisite: Task 6 must be completed first (Extract uses ports).

## Detail

### Check Outcome

| # | Criterion | Result | Notes |
|---|-----------|--------|-------|
| 1 | UseCase.Extract is re-export only (<30 lines) | FAIL | Currently 574 lines with implementation |
| 2 | Sub-modules focused (<300 lines each) | FAIL | Core.hs, LSP.hs, TreeSitter.hs do not yet exist |
| 3 | `cabal build` succeeds | PASS | Pre-split baseline |
| 4 | `cabal test` passes | PASS | Pre-split baseline |
| 5 | Existing imports still compile | PASS | Pre-split baseline |

### Prerequisites

Task 6 (Extract uses ports) must be completed before this task, because:
- Extract sub-modules must use port-delegated IO, not direct Infrastructure imports
- Splitting before port-refactoring would require updating imports twice

## Result

**NOT OK** — Task 9 requires implementation after Task 6. See attempt-2/ for the next PDCA cycle.