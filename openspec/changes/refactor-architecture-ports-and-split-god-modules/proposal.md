## Why

Graphos's UseCase layer has 31 direct Infrastructure imports across 7 modules, violating the clean architecture principle that dependencies point inward. The three largest modules — `UseCase.Extract` (657 lines, 229 graph edges), `UseCase.Pipeline` (588 lines, 192 edges), and `Domain.Config` (677 lines) — are god modules that couple orchestration, IO wiring, and configuration in ways that hurt comprehension, testability, and independent evolution.

**Why now?** The graph analysis shows these are the top hub nodes in the entire codebase. Every future feature touches them. Untangling them now prevents exponential coupling growth and makes the planned incremental-pipeline and watch-mode features far easier to implement correctly.

## What Changes

1. **Introduce Port interfaces** in a new `UseCase.Port` namespace — type-class and record-of-functions abstractions that decouple UseCase from Infrastructure. UseCase will depend only on these ports; Infrastructure will provide concrete implementations.

2. **Split god modules** into focused sub-modules:
   - `UseCase.Extract` → `UseCase.Extract.Core` + `UseCase.Extract.LSP` + `UseCase.Extract.TreeSitter` (keeping existing sub-modules)
   - `UseCase.Pipeline` → `UseCase.Pipeline.Core` + `UseCase.Pipeline.Checkpoint` + `UseCase.Pipeline.Incremental`
   - `Domain.Config` → `Domain.Config.Core` + `Domain.Config.Extraction` + `Domain.Config.Export` + `Domain.Config.Observability` + `Domain.Config.Vision`

3. **Push IO signatures out of UseCase** — refactor functions like `extractAll`, `runPipeline`, `exportAll` from `IO`-returning to `MonadIO m =>` or port-constrained signatures, moving concrete IO to Infrastructure adapters.

4. **Thin Main.hs** by introducing `Infrastructure.Wiring` — a single module that wires all ports to concrete implementations, so `app/Main.hs` only parses CLI args and calls wiring + pipeline.

## Capabilities

### New Capabilities
- `usecase-ports`: Port interfaces (ExtractionPort, ExportPort, FileSystemPort, LoggingPort, ObservabilityPort, LLMPort) that decouple UseCase from Infrastructure
- `usecase-extract-split`: Focused sub-modules extracted from the god module UseCase.Extract
- `usecase-pipeline-split`: Focused sub-modules extracted from the god module UseCase.Pipeline
- `domain-config-split`: Domain.Config split into focused configuration modules

### Modified Capabilities
- `pipeline`: Pipeline orchestration changes from direct IO calls to port-constrained calls
- `extraction`: Extraction workflow changes from direct Infrastructure imports to port abstractions
- `export`: Export orchestration changes from direct Infrastructure calls to ExportPort

## Impact

- **Code**: 7 UseCase modules + Domain.Config + Main.hs modified; 6 new Port modules; 1 new Wiring module; 3+ new sub-modules per god module
- **APIs**: Internal module boundaries shift — no public API changes (cabal exposes `Graphos` only)
- **Dependencies**: No new library dependencies
- **Tests**: Existing tests continue to pass (backward-compatible re-exports); new port interfaces enable mock-based testing of UseCase layer
- **Build**: Module split increases file count by ~20 but reduces average module size by ~60%

## PDCA Cycle

- **Plan**: Reduce UseCase→Infrastructure imports from 31 to 0 by introducing 6 port interfaces. Split 3 god modules (avg 640 lines) into focused sub-modules (avg <200 lines). All existing tests pass without modification.
- **Do**: Introduce ports first (backward-compatible re-exports), then split god modules one at a time, then thin Main.hs via Wiring module. Each step verified by `cabal test`.
- **Check**: Measure: (1) zero UseCase imports of Infrastructure, (2) no UseCase function has `IO` in its signature without a port constraint, (3) all modules <300 lines, (4) `cabal test` passes with zero failures, (5) Main.hs <100 lines of actual wiring logic.
- **Act**: Standardize the port pattern in code-quality.md. Add CI check that UseCase modules never import Infrastructure directly. Feed lessons into next cycle for Infrastructure adapter test coverage.