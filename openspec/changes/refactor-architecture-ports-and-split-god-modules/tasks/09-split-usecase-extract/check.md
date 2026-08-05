<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 9 — Split UseCase.Extract into focused sub-modules — CHECK

**Task slug**: `09-split-usecase-extract`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. Checking whether the split criteria from plan.md would pass if executed now.

## Detail

### Check Criteria Execution (from plan.md)

| # | Criterion | Spec Scenario | Result | Evidence |
|---|-----------|---------------|--------|----------|
| 1 | UseCase.Extract is re-export only | SC1 | **FAIL** | `src/Graphos/UseCase/Extract.hs` is 574 lines (contains implementation logic). Target: <30 lines with only re-exports. |
| 2 | Sub-modules are focused | SC2, SC3, SC4 | **FAIL** | Sub-modules Core.hs, LSP.hs, TreeSitter.hs do not yet exist. |
| 3 | `cabal build` succeeds | SC5 | **PASS** | `cabal build` succeeds (current state, pre-split) |
| 4 | `cabal test` passes | SC5 | **PASS** | `cabal test` — 200 examples, 0 failures |
| 5 | Existing imports still compile | SC5 | **PASS** | All current imports compile (pre-split) |

## Result

**NOT YET EXECUTED** — Task 9 has not been implemented. Check criteria will be re-run after the Do phase is complete.