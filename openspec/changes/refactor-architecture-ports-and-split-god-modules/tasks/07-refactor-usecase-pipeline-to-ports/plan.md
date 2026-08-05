<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 7 — Refactor UseCase.Pipeline to use ports — PLAN

**Task slug**: `07-refactor-usecase-pipeline-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

Replace all Infrastructure imports in `UseCase.Pipeline` with port interfaces. Pipeline currently imports Infrastructure.Logging, Infrastructure.Observability.SDK, Infrastructure.FileSystem.Ignore, Infrastructure.FileSystem.Cache, and multiple Infrastructure.Export modules.

## Detail

### Scope

Refactor `src/Graphos/UseCase/Pipeline.hs` (592 lines) to use AppEnv ports instead of direct Infrastructure imports. Current Infrastructure imports to eliminate:
- `Infrastructure.Logging (LogLevel(..), logInfo, logDebug, logTrace)` → use `loggingPort`
- `Infrastructure.Observability.SDK` → use `observabilityPort`
- `Infrastructure.FileSystem.Ignore (loadIgnorePatterns)` → use `fileSystemPort`
- `Infrastructure.FileSystem.Cache (loadPipelineCheckpoint, savePipelineCheckpoint, clearPipelineCheckpoint)` → use `fileSystemPort`
- `Infrastructure.Export.JSON`, `Infrastructure.Export.CommunityGraph`, `Infrastructure.Export.IncrementalJSON`, `Infrastructure.Export.Neo4j` → use `exportPort`
- `Infrastructure.Wiring (productionAppEnv)` → may need to remain if Pipeline creates its own AppEnv (should be passed in)

All Pipeline functions must accept `AppEnv` and delegate through port methods.

### Affected Modules

- `src/Graphos/UseCase/Pipeline.hs` — add AppEnv parameter, replace all Infrastructure calls
- `app/Main.hs` — update call sites

### Prerequisites

- Tasks 2-5 complete (all ports + AppEnv + Wiring)
- Task 6 complete (Extract refactored — establishes pattern)

### Risks

- Pipeline is the orchestration hub — it calls multiple ports (Export, FileSystem, Logging, Observability). The function signature change from individual IO args to `AppEnv` is the most impactful change in the entire refactor.
- `productionAppEnv` import in Pipeline is suspicious — Pipeline should receive AppEnv, not create it. This circularity must be resolved.

## Check Criteria

| # | Criterion | Spec Scenario | PASS Condition | FAIL Condition |
|---|-----------|---------------|----------------|-----------------|
| 1 | No Infrastructure imports in UseCase.Pipeline | "No Infrastructure.FileSystem imports in UseCase.Pipeline" + general | `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/Pipeline.hs` returns zero | Any Infrastructure import found |
| 2 | `cabal build` succeeds | N/A | Zero compilation errors | Build failure |
| 3 | `cabal test` passes | N/A | All tests pass | Any test failure |
| 4 | Pipeline functions take AppEnv | "Pipeline functions use port constraints, not IO" | `runPipeline` and related functions accept `AppEnv` parameter | Still using individual IO args |