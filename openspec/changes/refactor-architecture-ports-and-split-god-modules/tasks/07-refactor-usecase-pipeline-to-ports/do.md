<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 7 — Refactor UseCase.Pipeline to use ports — DO

**Task slug**: `07-refactor-usecase-pipeline-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. Replace all 8 Infrastructure imports in UseCase.Pipeline with port interfaces. Add AppEnv parameter to `runPipeline` and related functions.

## Detail

### What needs to be implemented

1. **Add `AppEnv` parameter to `runPipeline`** and all pipeline functions
2. **Replace Infrastructure imports** in `UseCase.Pipeline.hs`:
   - `Infrastructure.Logging (LogLevel(..), logInfo, logDebug, logTrace)` → `loggingPort`
   - `Infrastructure.Observability.SDK` → `observabilityPort`
   - `Infrastructure.FileSystem.Ignore (loadIgnorePatterns)` → `fileSystemPort`
   - `Infrastructure.FileSystem.Cache (loadPipelineCheckpoint, savePipelineCheckpoint, clearPipelineCheckpoint)` → `fileSystemPort`
   - `Infrastructure.Export.JSON`, `CommunityGraph`, `IncrementalJSON`, `Neo4j` → `exportPort`
   - `Infrastructure.Wiring (productionAppEnv)` → REMOVE (Pipeline should receive AppEnv, not create it)

3. **Update callers**: `app/Main.hs` must pass `AppEnv` to `runPipeline`

### Current Infrastructure imports in UseCase.Pipeline.hs

```
Infrastructure.Logging (LogLevel(..), logInfo, logDebug, logTrace)
Infrastructure.Observability.SDK
Infrastructure.FileSystem.Ignore (loadIgnorePatterns)
Infrastructure.FileSystem.Cache (loadPipelineCheckpoint, savePipelineCheckpoint, clearPipelineCheckpoint)
Infrastructure.Export.JSON
Infrastructure.Export.CommunityGraph
Infrastructure.Export.IncrementalJSON
Infrastructure.Export.Neo4j
Infrastructure.Wiring (productionAppEnv)  ← SUSPICIOUS: Pipeline should not create AppEnv
```

### Key decisions needed

- **`productionAppEnv` import in Pipeline is circular**: Pipeline imports Wiring which constructs AppEnv, but Pipeline should *receive* AppEnv, not create it. This import must be removed and the AppEnv threaded from Main.hs.
- **LogLevel is now in LoggingPort**: Pipeline uses `LogLevel(..)` for pattern matching — this needs to use `LoggingPort.LogLevel` instead.

### Concrete changes needed

- Modify `src/Graphos/UseCase/Pipeline.hs` — add AppEnv parameter, replace all Infrastructure calls with port calls, remove `productionAppEnv` import
- Update `app/Main.hs` — pass AppEnv to `runPipeline`

## Result

NOT YET IMPLEMENTED — awaiting Do phase.