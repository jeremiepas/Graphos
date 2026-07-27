## ADDED Requirements

### Requirement: UseCase.Port defines extraction interface

`UseCase.Port.ExtractionPort` SHALL define a record type `ExtractionPort` with fields for LSP extraction, TreeSitter extraction, image extraction, office extraction, markdown extraction, and Haskell stub extraction. UseCase.Extract modules MUST import only `UseCase.Port.ExtractionPort` and Domain types — they SHALL NOT import any `Graphos.Infrastructure.*` module directly.

- **Plan**: Replace 8 direct Infrastructure imports in UseCase.Extract with a single port abstraction, enabling mock-based testing and decoupling from LSP client internals.
- **Do**: Create ExtractionPort record with methods mirroring current Infrastructure.LSP.Client, Infrastructure.Extract.TreeSitter, Infrastructure.LLM.Vision, Infrastructure.FileSystem.OfficeConvert signatures.
- **Check**: The scenarios below verify zero Infrastructure imports and correct delegation.
- **Act**: Standardize port pattern for other UseCase modules.

#### Scenario: No Domain-to-Infrastructure imports in UseCase.Extract
- **WHEN** grepping for `^import.*Graphos\.Infrastructure` in `src/Graphos/UseCase/Extract.hs` and `src/Graphos/UseCase/Extract/*.hs`
- **THEN** zero matches are found

#### Scenario: ExtractionPort record contains all required methods
- **WHEN** examining `UseCase.Port.ExtractionPort`
- **THEN** it contains methods for: `extractViaLSP`, `extractViaTreeSitter`, `extractImageFile`, `extractOfficeFile`, `extractDocFile`, `extractHaskellStub`

#### Scenario: UseCase.Extract delegates to port, not Infrastructure
- **WHEN** `extractAll` is called with an `ExtractionPort` providing mock implementations
- **THEN** the mock implementations are invoked and no Infrastructure code runs

#### Scenario: ExtractionPort wiring delegates to real Infrastructure
- **WHEN** `Infrastructure.Wiring` provides the production `ExtractionPort`
- **THEN** all LSP, TreeSitter, and file extraction calls reach the real Infrastructure implementations

### Requirement: UseCase.Port defines export interface

`UseCase.Port.ExportPort` SHALL define a record type `ExportPort` with fields for HTML, Obsidian, Neo4j, Memgraph, CommunityGraph, JSON, IncrementalJSON, Report, and SVG export. UseCase.Export MUST import only `UseCase.Port.ExportPort` and Domain types.

#### Scenario: No Infrastructure imports in UseCase.Export
- **WHEN** grepping for `^import.*Graphos\.Infrastructure` in `src/Graphos/UseCase/Export.hs`
- **THEN** zero matches are found

#### Scenario: ExportPort record contains all export methods
- **WHEN** examining `UseCase.Port.ExportPort`
- **THEN** it contains methods for each export format (HTML, Obsidian, Neo4j, Memgraph, CommunityGraph, JSON, IncrementalJSON, Report, SVG)

### Requirement: UseCase.Port defines file system interface

`UseCase.Port.FileSystemPort` SHALL define a record type `FileSystemPort` with fields for loading/saving checkpoints, loading ignore patterns, and canonical path resolution. UseCase.Pipeline and UseCase.Detect MUST import `UseCase.Port.FileSystemPort` instead of `Infrastructure.FileSystem.*`.

#### Scenario: No Infrastructure.FileSystem imports in UseCase.Pipeline
- **WHEN** grepping for `^import.*Graphos\.Infrastructure\.FileSystem` in `src/Graphos/UseCase/Pipeline.hs`
- **THEN** zero matches are found

#### Scenario: FileSystemPort contains checkpoint and ignore methods
- **WHEN** examining `UseCase.Port.FileSystemPort`
- **THEN** it contains methods for `loadPipelineCheckpoint`, `savePipelineCheckpoint`, `clearPipelineCheckpoint`, `loadIgnorePatterns`

### Requirement: UseCase.Port defines logging interface

`UseCase.Port.LoggingPort` SHALL define a record type `LoggingPort` with fields for `logInfo`, `logDebug`, `logTrace`, `logWarn`, and `logError`. All UseCase modules that currently import `Infrastructure.Logging` SHALL instead use `LoggingPort`.

#### Scenario: No Infrastructure.Logging imports in UseCase
- **WHEN** grepping for `^import.*Graphos\.Infrastructure\.Logging` in `src/Graphos/UseCase/*.hs` and `src/Graphos/UseCase/Extract/*.hs`
- **THEN** zero matches are found

#### Scenario: LoggingPort provides all log levels
- **WHEN** examining `UseCase.Port.LoggingPort`
- **THEN** it contains `logInfo`, `logDebug`, `logTrace`, `logWarn`, `logError` fields

### Requirement: UseCase.Port defines observability interface

`UseCase.Port.ObservabilityPort` SHALL define a record type `ObservabilityPort` with methods for creating spans, recording metrics, and flushing. UseCase.Pipeline MUST use `ObservabilityPort` instead of importing `Infrastructure.Observability.SDK`.

#### Scenario: No Infrastructure.Observability imports in UseCase.Pipeline
- **WHEN** grepping for `^import.*Graphos\.Infrastructure\.Observability` in `src/Graphos/UseCase/Pipeline.hs`
- **THEN** zero matches are found

### Requirement: UseCase.Port defines LLM interface

`UseCase.Port.LLMPort` SHALL define a record type `LLMPort` with methods for `callLLM`, `parseLabelsFromResponse`, `generateEmbedding`, `analyzeImage`, and `validateUrl`. UseCase.Label, UseCase.Ingest, and UseCase.IngestIndex MUST use `LLMPort` instead of direct Infrastructure.LLM imports.

#### Scenario: No Infrastructure.LLM imports in UseCase
- **WHEN** grepping for `^import.*Graphos\.Infrastructure\.LLM` in `src/Graphos/UseCase/*.hs`
- **THEN** zero matches are found

#### Scenario: LLMPort contains all LLM methods
- **WHEN** examining `UseCase.Port.LLMPort`
- **THEN** it contains `callLLM`, `parseLabelsFromResponse`, `generateEmbedding`, `analyzeImage`, `validateUrl`

### Requirement: UseCase.AppEnv aggregates all ports

`UseCase.AppEnv` SHALL define a record type `AppEnv` containing all port fields (`extractionPort`, `exportPort`, `fileSystemPort`, `loggingPort`, `observabilityPort`, `llmPort`). `Infrastructure.Wiring` SHALL provide a function `productionAppEnv :: IO AppEnv` that wires all ports to real Infrastructure implementations.

#### Scenario: AppEnv contains all port fields
- **WHEN** examining `UseCase.AppEnv`
- **THEN** the `AppEnv` record contains fields for all 6 ports

#### Scenario: Wiring produces production AppEnv
- **WHEN** calling `productionAppEnv` from `Infrastructure.Wiring`
- **THEN** all port fields delegate to their corresponding real Infrastructure implementations

#### Scenario: AppEnv enables mock substitution
- **WHEN** creating an `AppEnv` with mock ports
- **THEN** UseCase functions using that `AppEnv` invoke mock implementations exclusively