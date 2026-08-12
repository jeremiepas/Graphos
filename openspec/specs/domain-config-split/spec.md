# domain-config-split Specification

## Purpose
TBD - created by archiving change refactor-architecture-ports-and-split-god-modules. Update Purpose after archive.
## Requirements
### Requirement: Domain.Config split by concern

`Domain.Config` (currently 677 lines) SHALL be split into `Domain.Config.Core` (GraphosConfig, defaultConfig, config merging), `Domain.Config.Extraction` (ExtractorConfig, ExtractorMode, Granularity, FileExtensionConfig), `Domain.Config.Export` (Neo4jConfig, MemgraphConfig, PushMode, streaming configs), `Domain.Config.Observability` (ObservabilityConfig, OtelConfig), and `Domain.Config.Vision` (VisionConfig, EmbeddingConfig). The original `Domain.Config` module SHALL become a backward-compatible re-export module.

- **Plan**: Reduce Domain.Config from 677 lines to a thin re-export (<30 lines), with each sub-module <200 lines.
- **Do**: Group config types by subsystem. Extraction config with extraction types, export config with export types, etc. Keep all FromJSON instances co-located with their types.
- **Check**: The scenarios verify size, backward compatibility, and Domain purity.
- **Act**: Document config split pattern for future config additions.

#### Scenario: Domain.Config is a re-export module
- **WHEN** examining `src/Graphos/Domain/Config.hs`
- **THEN** it contains only module declaration and re-exports from sub-modules, and is fewer than 30 lines

#### Scenario: Domain.Config.Core contains main config and defaults
- **WHEN** examining `src/Graphos/Domain/Config/Core.hs`
- **THEN** it contains `GraphosConfig`, `defaultGraphosConfig`, `mergeGraphosConfig`, `mergeObservabilityConfig`, and is fewer than 200 lines

#### Scenario: Domain.Config.Extraction contains extraction config
- **WHEN** examining `src/Graphos/Domain/Config/Extraction.hs`
- **THEN** it contains `ExtractorConfig`, `ExtractorMode`, `Granularity`, `FileExtensionConfig`, and their `FromJSON` instances

#### Scenario: Domain.Config.Export contains export config
- **WHEN** examining `src/Graphos/Domain/Config/Export.hs`
- **THEN** it contains `Neo4jConfig`, `MemgraphConfig`, `PushMode`, `Neo4jStreamingConfig`, `Neo4jPushMode`, `MemgraphPushMode`

#### Scenario: Domain.Config.Observability contains observability config
- **WHEN** examining `src/Graphos/Domain/Config/Observability.hs`
- **THEN** it contains `ObservabilityConfig`, `OtelConfig`, and their `FromJSON` instances

#### Scenario: Domain.Config.Vision contains vision and embedding config
- **WHEN** examining `src/Graphos/Domain/Config/Vision.hs`
- **THEN** it contains `VisionConfig`, `EmbeddingConfig`, and their `FromJSON` instances

#### Scenario: Domain purity preserved
- **WHEN** grepping for `^import.*IO|^import.*System\.|^import.*Control\.Concurrent|^import.*Control\.Exception` in all `Domain/Config/*.hs` files
- **THEN** zero matches are found (Domain remains pure)

#### Scenario: Existing imports still compile
- **WHEN** a module imports `Graphos.Domain.Config (GraphosConfig(..), defaultGraphosConfig, Granularity(..))`
- **THEN** the code compiles without errors or warnings

