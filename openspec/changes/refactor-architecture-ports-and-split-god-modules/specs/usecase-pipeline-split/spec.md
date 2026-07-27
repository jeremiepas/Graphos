## ADDED Requirements

### Requirement: UseCase.Pipeline split into focused sub-modules

`UseCase.Pipeline` (currently 588 lines) SHALL be split into `UseCase.Pipeline.Core` (pure pipeline orchestration), `UseCase.Pipeline.Checkpoint` (checkpoint save/load logic), and `UseCase.Pipeline.Incremental` (incremental pipeline logic). The original `UseCase.Pipeline` module SHALL become a backward-compatible re-export module.

- **Plan**: Reduce UseCase.Pipeline from 588 lines to a thin re-export (<30 lines), with each sub-module <300 lines.
- **Do**: Extract checkpoint operations into UseCase.Pipeline.Checkpoint, incremental pipeline logic into UseCase.Pipeline.Incremental, and core orchestration into UseCase.Pipeline.Core.
- **Check**: The scenarios verify size, backward compatibility, and port-based dependencies.
- **Act**: Standardize sub-module split pattern for future god modules.

#### Scenario: UseCase.Pipeline is a re-export module
- **WHEN** examining `src/Graphos/UseCase/Pipeline.hs`
- **THEN** it contains only module declaration and re-exports, and is fewer than 30 lines

#### Scenario: UseCase.Pipeline.Core contains pure orchestration
- **WHEN** examining `src/Graphos/UseCase/Pipeline/Core.hs`
- **THEN** it contains `runPipeline` and delegates to ports for all IO operations, and does NOT import `Graphos.Infrastructure.*` directly

#### Scenario: UseCase.Pipeline.Checkpoint contains checkpoint logic
- **WHEN** examining `src/Graphos/UseCase/Pipeline/Checkpoint.hs`
- **THEN** it contains checkpoint-related functions and uses `FileSystemPort` for IO, and is fewer than 200 lines

#### Scenario: UseCase.Pipeline.Incremental contains incremental logic
- **WHEN** examining `src/Graphos/UseCase/Pipeline/Incremental.hs`
- **THEN** it contains `runIncrementalPipeline` and `runSingleFilePipeline`, and is fewer than 300 lines

#### Scenario: Existing imports still compile
- **WHEN** a module imports `Graphos.UseCase.Pipeline (runPipeline, runIncrementalPipeline, PipelineResult(..))`
- **THEN** the code compiles without errors or warnings

#### Scenario: Pipeline functions use port constraints, not IO
- **WHEN** examining the type signatures in `UseCase.Pipeline.Core`
- **THEN** `runPipeline` takes an `AppEnv` parameter and does NOT have a bare `IO` return without port constraint