<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 6 — Refactor UseCase.Extract to use ExtractionPort — ACT

**Task slug**: `06-refactor-usecase-extract-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. The Do phase has not been executed for this task. Check criteria were recorded as FAIL because the refactoring hasn't been performed yet.

## Detail

### Check Outcome

| # | Criterion | Result | Notes |
|---|-----------|--------|-------|
| 1 | No Infrastructure imports in UseCase.Extract | FAIL | Not yet refactored — UseCase.Extract still imports Infrastructure directly |
| 2 | No Infrastructure imports in UseCase.Extract sub-modules | FAIL | Image.hs, Office.hs, Markdown.hs still import Infrastructure |
| 3 | `cabal build` succeeds | PASS | Pre-refactoring baseline passes |
| 4 | `cabal test` passes | PASS | Pre-refactoring baseline passes |
| 5 | `extractAll` takes AppEnv parameter | FAIL | Not yet refactored |

### Next Steps

This task requires implementation. After the Do phase is completed:
1. Add `AppEnv` parameter to `extractAll` and related functions
2. Replace all Infrastructure imports in UseCase.Extract.* with port method calls
3. Update callers (Pipeline.hs, Main.hs)
4. Re-run Check criteria

## Result

**NOT OK** — Task 6 requires implementation. See attempt-2/ for the next PDCA cycle when implementation begins.