<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 6 — Refactor UseCase.Extract to use ExtractionPort — CHECK

**Task slug**: `06-refactor-usecase-extract-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. Checking whether the refactoring criteria from plan.md would pass if executed now.

## Detail

### Check Criteria Execution (from plan.md)

| # | Criterion | Spec Scenario | Result | Evidence |
|---|-----------|---------------|--------|----------|
| 1 | No Infrastructure imports in UseCase.Extract | "No Domain-to-Infrastructure imports in UseCase.Extract" | **FAIL** | `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/Extract.hs` — **not yet run** (module not yet refactored). Current state: UseCase.Extract.hs still has Infrastructure imports. |
| 2 | No Infrastructure imports in UseCase.Extract sub-modules | "No Domain-to-Infrastructure imports in UseCase.Extract" | **FAIL** | `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/Extract/*.hs` returns matches in Image.hs (Infrastructure.LLM.Vision, Infrastructure.Logging), Office.hs (Infrastructure.FileSystem.OfficeConvert, Infrastructure.Logging), Markdown.hs (Infrastructure.Logging). |
| 3 | `cabal build` succeeds | N/A | **PASS** | `cabal build` succeeds (current state, pre-refactoring) |
| 4 | `cabal test` passes | N/A | **PASS** | `cabal test` — 200 examples, 0 failures (current state, pre-refactoring) |
| 5 | `extractAll` takes AppEnv parameter | "UseCase.Extract delegates to port, not Infrastructure" | **FAIL** | `extractAll` does not yet take an `AppEnv` parameter. |

## Result

**NOT YET EXECUTED** — Task 6 has not been implemented. Check criteria will be re-run after the Do phase is complete. All FAIL criteria are expected to fail at this stage because the refactoring has not been performed yet.