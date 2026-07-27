## Context

Graphos follows clean architecture with three layers: Domain (pure), UseCase (pure orchestration), and Infrastructure (all IO). The current codebase violates this principle in 31 places where UseCase modules import directly from Infrastructure. The three largest modules — `UseCase.Extract` (657 lines, 229 edges), `UseCase.Pipeline` (588 lines, 192 edges), and `Domain.Config` (677 lines) — are god modules that bundle unrelated concerns, making the codebase harder to navigate, test, and evolve independently.

The Graphos knowledge graph confirms these are the top hub nodes, and the `Main.hs` entry point (718 lines, 40+ imports) directly wires Infrastructure implementations to UseCase calls without any abstraction layer.

## Goals / Non-Goals

**Goals:**
- Eliminate all direct UseCase→Infrastructure imports (31 violations → 0)
- Replace IO-constrained UseCase functions with port-constrained signatures
- Split god modules into focused sub-modules (each <300 lines)
- Thin Main.hs by extracting wiring into `Infrastructure.Wiring`
- Maintain 100% backward compatibility via re-export modules
- Enable mock-based testing of UseCase layer

**Non-Goals:**
- No changes to Domain layer logic (only split Domain.Config)
- No changes to Infrastructure implementations (only new adapter modules)
- No changes to public CLI API or output formats
- No new external dependencies
- No changes to test files (existing tests must pass as-is)
- No refactoring of MCP server, export format internals, or LSP client internals

## Decisions

### D1: Record-of-functions ports over type classes

| Aspect | Choice |
|--------|--------|
| **Decision** | Use record-of-functions for port interfaces |
| **Rationale** | Records are explicit, injectable, and trivially mockable. No orphan instance risk. Each port is a plain data type that Infrastructure.Wiring populates with concrete implementations. |
| **Alternatives** | (A) Type classes — more idiomatic Haskell but introduce orphan instance risk and make mocking harder. (B) Free monad effects — powerful but over-engineered for this codebase. (C) mtl-style constraints — composable but add significant boilerplate. |

### D2: Backward-compatible re-export modules

| Aspect | Choice |
|--------|--------|
| **Decision** | Original god modules (UseCase.Extract, UseCase.Pipeline, Domain.Config) become thin re-export modules that re-export all their former contents from the new sub-modules |
| **Rationale** | Preserves backward compatibility. Existing imports `import Graphos.UseCase.Extract (extractAll)` continue to compile. No test changes needed. |
| **Alternatives** | (A) Direct rename — breaks all imports, requires updating every import site. (B) Cabal re-export — only works at package level, not module level. |

### D3: Port module namespace: UseCase.Port

| Aspect | Choice |
|--------|--------|
| **Decision** | All port interfaces live under `Graphos.UseCase.Port.*` |
| **Rationale** | Ports are UseCase-layer abstractions — they define what UseCase needs, not how Infrastructure implements it. Placing them in UseCase keeps the dependency arrow pointing inward (Domain←UseCase←Infrastructure still holds, with UseCase defining the interface). |
| **Alternatives** | (A) `Graphos.Domain.Port` — Domain shouldn't know about UseCase concerns like LSP extraction. (B) `Graphos.Infrastructure.Port` — Infrastructure should implement, not define, ports. |

### D4: Domain.Config split by concern, not by type

| Aspect | Choice |
|--------|--------|
| **Decision** | Split `Domain.Config` into `Core` (GraphosConfig, defaults), `Extraction` (ExtractorConfig, Granularity), `Export` (Neo4j, Memgraph, PushMode), `Observability` (OtelConfig, ObservabilityConfig), `Vision` (VisionConfig) |
| **Rationale** | Config types naturally group by the subsystem they configure. Each sub-module can be imported independently by the subsystem that needs it, reducing unnecessary coupling. |
| **Alternatives** | (A) Split by type vs value — config types and defaults belong together. (B) Keep monolithic — 677 lines is too large, violates SRP. |

### D5: Wiring module in Infrastructure, not Main

| Aspect | Choice |
|--------|--------|
| **Decision** | Create `Graphos.Infrastructure.Wiring` that produces a fully-wired `AppEnv` record containing all port implementations. Main.hs only parses CLI args and calls `runPipeline` with the wired env. |
| **Rationale** | Main.hs should be thin — parse args, call wiring, run. All implementation selection logic lives in Infrastructure.Wiring, which is the only module that knows concrete implementations. |
| **Alternatives** | (A) Wire in Main.hs directly — Main.hs is already 718 lines. (B) Use a DI framework — unnecessary complexity for a CLI tool. |

### D6: AppEnv record as the single dependency injection mechanism

| Aspect | Choice |
|--------|--------|
| **Decision** | Define `data AppEnv = AppEnv { extractionPort :: ExtractionPort, exportPort :: ExportPort, fileSystemPort :: FileSystemPort, loggingPort :: LoggingPort, observabilityPort :: ObservabilityPort, llmPort :: LLMPort }` in a new `UseCase.AppEnv` module |
| **Rationale** | A single record makes it easy to pass all dependencies through ReaderT or as explicit parameters. Mock implementations replace individual fields in tests. |
| **Alternatives** | (A) Separate ReaderT per port — composable but requires mtl constraints. (B) IORef-based registry — anti-pattern, loses type safety. |

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| Re-export modules add indirection layer | Re-export modules are clearly documented; IDEs follow re-exports; minimal performance impact |
| Port abstractions add boilerplate | Each port is a small record (5-10 fields); boilerplate is mechanical and auto-generated; payoff in testability outweighs cost |
| Large change surface (31 imports → 0) | Implemented incrementally: ports first (backward compat), then god module splits, then Main thinning; each step verified by `cabal test` |
| Potential cyclic imports between UseCase.Port and Domain | Ports only reference Domain types; UseCase.Port does not import UseCase.* or Infrastructure.* |
| Mock testing not in scope for this change | Ports enable mocking; actual mock implementations are a follow-up task |
| Domain.Config split may break external config loading | Domain.Config re-exports all sub-module types; `FromJSON` instances remain in Core; config YAML deserialization is unchanged |

## Verification Strategy (Check)

| Gate | Verification | Command |
|------|-------------|---------|
| Build passes | Zero compilation errors after each phase | `cabal build` |
| All tests pass | Existing test suite passes without modification | `cabal test` |
| No UseCase→Infrastructure imports | Grep finds zero `import.*Graphos\.Infrastructure` in `src/Graphos/UseCase/` (excluding Port modules) | `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/` |
| No IO in UseCase signatures | UseCase functions use `MonadIO m =>` or port constraints, never bare `IO` | `rg ":: .*IO " src/Graphos/UseCase/` |
| Module size | All modules <300 lines (excluding re-export modules) | `find src/ -name "*.hs" -exec wc -l {} + \| sort -rn` |
| Main.hs thinned | Main.hs wiring logic <100 lines | `wc -l app/Main.hs` |
| Re-export backward compat | All original import paths still compile | `cabal build` (no import breakage) |

## Iteration & Rollback (Act)

**If Check fails:**
- Build failure → revert the specific module change, fix, re-verify
- Test failure → port abstraction may be incomplete; add missing method to port record
- Import violations remain → identify missed import, add to port, update wiring

**Rollback strategy:** Each phase is a separate commit. Revert to the last passing commit. Re-export modules ensure no external breakage.

**Standardization for next cycle:**
- Add CI check: `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/` must return zero
- Update code-quality.md with port pattern
- Create port-implementation checklist for future Infrastructure modules