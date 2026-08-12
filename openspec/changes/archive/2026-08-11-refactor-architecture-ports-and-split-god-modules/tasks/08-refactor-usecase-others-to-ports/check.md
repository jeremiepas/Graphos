<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 8 — Refactor UseCase.Export, Ingest, Label, Detect to use ports — CHECK

**Task slug**: `08-refactor-usecase-others-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. Checking whether the refactoring criteria from plan.md would pass if executed now.

## Detail

### Check Criteria Execution (from plan.md)

| # | Criterion | Spec Scenario | Result | Evidence |
|---|-----------|---------------|--------|----------|
| 1 | No Infrastructure imports in UseCase (excluding Port modules) | "No Infrastructure imports in UseCase" | **FAIL** | `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/` returns 25 matches across 10 files (excluding Port/FileSystemPort which has 1 intentional import). Files with violations: Export.hs (5), Ingest.hs (3), IngestIndex.hs (1), Label.hs (1), Detect.hs (2), Extract/Image.hs (2), Extract/Office.hs (2), Extract/Markdown.hs (1), Pipeline.hs (8), Port/FileSystemPort.hs (1). |
| 2 | `cabal build` succeeds | N/A | **PASS** | `cabal build` succeeds (current state, pre-refactoring) |
| 3 | `cabal test` passes | N/A | **PASS** | `cabal test` — 200 examples, 0 failures (current state) |

## Result

**NOT YET EXECUTED** — Task 8 has not been implemented. Check criteria will be re-run after the Do phase is complete.