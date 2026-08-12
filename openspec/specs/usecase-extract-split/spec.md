# usecase-extract-split Specification

## Purpose
TBD - created by archiving change refactor-architecture-ports-and-split-god-modules. Update Purpose after archive.
## Requirements
### Requirement: UseCase.Extract split into focused sub-modules

`UseCase.Extract` (currently 657 lines) SHALL be split into `UseCase.Extract.Core` (pure orchestration), `UseCase.Extract.LSP` (LSP workflow), and `UseCase.Extract.TreeSitter` (TreeSitter fallback workflow). The original `UseCase.Extract` module SHALL become a backward-compatible re-export module that re-exports all public symbols from the sub-modules.

- **Plan**: Reduce UseCase.Extract from 657 lines to a thin re-export (<30 lines), with each sub-module <300 lines.
- **Do**: Extract LSP-specific orchestration into UseCase.Extract.LSP, TreeSitter-specific orchestration into UseCase.Extract.TreeSitter, and core pipeline coordination into UseCase.Extract.Core.
- **Check**: The scenarios verify size, backward compatibility, and module focus.
- **Act**: Apply same pattern to Pipeline and Config splits.

#### Scenario: UseCase.Extract is a re-export module
- **WHEN** examining `src/Graphos/UseCase/Extract.hs`
- **THEN** it contains only module declaration and re-exports (no implementation logic), and is fewer than 30 lines

#### Scenario: UseCase.Extract.Core contains pure orchestration
- **WHEN** examining `src/Graphos/UseCase/Extract/Core.hs`
- **THEN** it contains the `extractAll` function signature and orchestration logic, and does NOT directly import any `Graphos.Infrastructure.*` module (only ports)

#### Scenario: UseCase.Extract.LSP contains LSP workflow
- **WHEN** examining `src/Graphos/UseCase/Extract/LSP.hs`
- **THEN** it contains LSP-specific extraction logic (`extractFilesWithLSP`, `extractWorkspaceSymbols`), and is fewer than 300 lines

#### Scenario: UseCase.Extract.TreeSitter contains TreeSitter workflow
- **WHEN** examining `src/Graphos/UseCase/Extract/TreeSitter.hs`
- **THEN** it contains TreeSitter-specific extraction logic (`extractViaTreeSitterFFI`), and is fewer than 200 lines

#### Scenario: Existing imports still compile
- **WHEN** a module imports `Graphos.UseCase.Extract (extractAll, extractChangedFiles, extractGroup)`
- **THEN** the code compiles without errors or warnings

#### Scenario: God module graph edges redistributed
- **WHEN** rebuilding the Graphos knowledge graph after the split
- **THEN** `UseCase.Extract.Core` has fewer than 100 edges, `UseCase.Extract.LSP` has fewer than 80 edges, and `UseCase.Extract.TreeSitter` has fewer than 40 edges

