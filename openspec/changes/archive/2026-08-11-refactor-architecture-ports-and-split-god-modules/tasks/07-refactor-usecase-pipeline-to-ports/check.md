<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 7 — Refactor UseCase.Pipeline to use ports — CHECK

**Task slug**: `07-refactor-usecase-pipeline-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. Checking whether the refactoring criteria from plan.md would pass if executed now.

## Detail

### Check Criteria Execution (from plan.md)

| # | Criterion | Spec Scenario | Result | Evidence |
|---|-----------|---------------|--------|----------|
| 1 | No Infrastructure imports in UseCase.Pipeline | "No Infrastructure.FileSystem imports in UseCase.Pipeline" + general | **FAIL** | `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/Pipeline.hs` returns 8+ matches: Infrastructure.Logging, Infrastructure.Observability.SDK, Infrastructure.FileSystem.Ignore, Infrastructure.FileSystem.Cache, Infrastructure.Export.*, Infrastructure.Wiring |
| 2 | `cabal build` succeeds | N/A | **PASS** | `cabal build` succeeds (current state, pre-refactoring) |
| 3 | `cabal test` passes | N/A | **PASS** | `cabal test` — 200 examples, 0 failures (current state) |
| 4 | Pipeline functions take AppEnv | "Pipeline functions use port constraints, not IO" | **FAIL** | `runPipeline` does not yet take an `AppEnv` parameter. Current signature uses individual IO arguments. |

## Result

**NOT YET EXECUTED** — Task 7 has not been implemented. Check criteria will be re-run after the Do phase is complete.