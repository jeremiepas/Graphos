<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 10 — Split UseCase.Pipeline into focused sub-modules — PLAN

**Task slug**: `10-split-usecase-pipeline`
**Attempt**: 1
**Status**: pending

## Summary

Split `UseCase.Pipeline` (592 lines) into `UseCase.Pipeline.Core` (pure pipeline orchestration), `UseCase.Pipeline.Checkpoint` (checkpoint save/load logic), and `UseCase.Pipeline.Incremental` (incremental pipeline logic). The original becomes a backward-compatible re-export.

## Detail

### Scope

Split `src/Graphos/UseCase/Pipeline.hs` (592 lines) into:
- **UseCase.Pipeline.Core** — `runPipeline` function and core orchestration, delegates to ports for all IO operations
- **UseCase.Pipeline.Checkpoint** — checkpoint-related functions (`loadPipelineCheckpoint`, `savePipelineCheckpoint`, `clearPipelineCheckpoint`), uses `FileSystemPort`
- **UseCase.Pipeline.Incremental** — `runIncrementalPipeline`, `runSingleFilePipeline` and related incremental logic

Original `UseCase.Pipeline.hs` becomes a re-export module (<30 lines).

### Spec Scenarios

- **SC1**: UseCase.Pipeline is a re-export module (<30 lines, no implementation)
- **SC2**: UseCase.Pipeline.Core contains `runPipeline` with port-delegated IO, no Infrastructure imports
- **SC3**: UseCase.Pipeline.Checkpoint contains checkpoint functions using FileSystemPort (<200 lines)
- **SC4**: UseCase.Pipeline.Incremental contains incremental pipeline functions (<300 lines)
- **SC5**: Existing imports still compile
- **SC6**: Pipeline functions use port constraints, not bare IO

### Affected Modules

- `src/Graphos/UseCase/Pipeline.hs` → becomes re-export
- `src/Graphos/UseCase/Pipeline/Core.hs` → new
- `src/Graphos/UseCase/Pipeline/Checkpoint.hs` → new
- `src/Graphos/UseCase/Pipeline/Incremental.hs` → new
- `app/Main.hs` — may need import updates
- `.cabal` file — add new exposed-modules

### Prerequisites

- Task 7 complete (Pipeline uses ports)
- Task 1 complete ✅

### Risks

- Pipeline is the main orchestration module — splitting must preserve the exact call order and checkpointing behavior
- Checkpoint module needs FileSystemPort — ensure port is threaded through correctly
- Circular imports between Core and Checkpoint/Incremental — Core should call sub-modules, not vice versa

## Check Criteria

| # | Criterion | Spec Scenario | PASS Condition | FAIL Condition |
|---|-----------|---------------|----------------|-----------------|
| 1 | UseCase.Pipeline is re-export only | SC1 | `src/Graphos/UseCase/Pipeline.hs` is <30 lines, no implementation logic | More than 30 lines or contains logic |
| 2 | Sub-modules are focused | SC2, SC3, SC4 | Core <300 lines, Checkpoint <200 lines, Incremental <300 lines | Any sub-module exceeds size limit |
| 3 | `cabal build` succeeds | SC5 | Zero compilation errors | Build failure |
| 4 | `cabal test` passes | SC5 | All tests pass | Any test failure |
| 5 | Existing imports still compile | SC5 | All original import paths resolve | Import breakage |