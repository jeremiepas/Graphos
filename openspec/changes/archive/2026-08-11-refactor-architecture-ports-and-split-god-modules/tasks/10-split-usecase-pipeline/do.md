<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 10 — Split UseCase.Pipeline into focused sub-modules — DO

**Task slug**: `10-split-usecase-pipeline`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. Split `UseCase.Pipeline` (592 lines) into `UseCase.Pipeline.Core` (orchestration), `UseCase.Pipeline.Checkpoint` (checkpoint logic), and `UseCase.Pipeline.Incremental` (incremental logic). Original becomes backward-compatible re-export.

## Detail

### What needs to be implemented

1. **Create `UseCase.Pipeline.Core`** — contains `runPipeline` and core orchestration. Delegates to ports for all IO. No Infrastructure imports.

2. **Create `UseCase.Pipeline.Checkpoint`** — contains checkpoint-related functions:
   - `loadPipelineCheckpoint` — uses `fileSystemPort`
   - `savePipelineCheckpoint` — uses `fileSystemPort`
   - `clearPipelineCheckpoint` — uses `fileSystemPort`

3. **Create `UseCase.Pipeline.Incremental`** — contains incremental pipeline logic:
   - `runIncrementalPipeline`
   - `runSingleFilePipeline`

4. **Convert `UseCase.Pipeline.hs`** to a re-export module (<30 lines):
   ```haskell
   module Graphos.UseCase.Pipeline
     ( module Graphos.UseCase.Pipeline.Core
     , module Graphos.UseCase.Pipeline.Checkpoint
     , module Graphos.UseCase.Pipeline.Incremental
     ) where
   import Graphos.UseCase.Pipeline.Core
   import Graphos.UseCase.Pipeline.Checkpoint
   import Graphos.UseCase.Pipeline.Incremental
   ```

5. **Add new modules to `.cabal` file**

### Prerequisites

- Task 7 must be complete (Pipeline uses ports)

### Concrete changes needed

- Create `src/Graphos/UseCase/Pipeline/Core.hs` — orchestration with port-delegated IO
- Create `src/Graphos/UseCase/Pipeline/Checkpoint.hs` — checkpoint functions
- Create `src/Graphos/UseCase/Pipeline/Incremental.hs` — incremental pipeline
- Convert `src/Graphos/UseCase/Pipeline.hs` to re-export module
- Update `.cabal` file
- Verify all existing imports still compile

## Result

NOT YET IMPLEMENTED — awaiting Do phase.