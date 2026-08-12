<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 5 — Create UseCase.AppEnv and Infrastructure.Wiring — DO

**Task slug**: `05-usecase-appenv-infrastructure-wiring`
**Attempt**: 1
**Status**: in-progress

## Summary

Created `UseCase.AppEnv` (25 lines) aggregating all 6 ports and `Infrastructure.Wiring` (231 lines) providing production implementations. The wiring module imports all Infrastructure modules and constructs concrete port implementations.

## Detail

### What was implemented

#### AppEnv (`src/Graphos/UseCase/AppEnv.hs`, 25 lines)

**Record type `AppEnv`** with 6 fields:
- `extractionPort :: ExtractionPort`
- `exportPort :: ExportPort`
- `fileSystemPort :: FileSystemPort`
- `loggingPort :: LoggingPort`
- `observabilityPort :: ObservabilityPort`
- `llmPort :: LLMPort`

AppEnv imports only port types (no Infrastructure imports), staying pure in the UseCase layer.

#### Wiring (`src/Graphos/Infrastructure/Wiring.hs`, 231 lines)

**Main entry point**:
- `productionAppEnv :: LogEnv -> ObservabilityEnv -> AppEnv` — takes already-initialized `LogEnv` and `ObservabilityEnv` and wires all 6 ports

**Individual port constructors** (exported for testability):
- `productionLoggingPort :: LogEnv -> LoggingPort`
- `productionObservabilityPort :: ObservabilityEnv -> ObservabilityPort`
- `productionFileSystemPort :: FileSystemPort`
- `productionExtractionPort :: LogEnv -> ExtractionPort`
- `productionExportPort :: LogEnv -> ObservabilityEnv -> ExportPort`
- `productionLLMPort :: LLMPort`

**Notable implementations**:
- **ExtractionPort**: Most complex — wires LSP client lifecycle (find, connect, disconnect, extract), TreeSitter grammar parsing, file-level extraction (delegates to UseCase.Extract sub-modules), office media extraction, and Neo4j streaming
- **ExportPort**: Currently `error "not yet wired"` — will be fully implemented in Task 8 when UseCase.Export is refactored
- **LLMPort**: Includes type conversion functions (`convertImageKind`, `convertEntity`) mapping between Infrastructure and Port types
- **ExtractionPort uses `unsafeCoerce`**: LSP client is stored as `Dynamic` in `LSPHandle` and recovered via `unsafeCoerce` in wiring — safe because only Wiring creates and unwraps the handle

### Key decisions

1. **`productionAppEnv` takes `LogEnv` and `ObservabilityEnv` as parameters, not `GraphosConfig`**: Main.hs initializes logging and observability before creating the AppEnv, so these are passed in as already-initialized resources. This avoids Wiring needing to know about log file paths, OTel config, etc.

2. **`productionExportPort` is a TODO**: The export port's `epExportAll` currently throws `error "not yet wired"`. This is intentional — the export wiring requires UseCase.Export to be refactored first (Task 8), at which point the export function will be wired to call through the UseCase.Export module.

3. **ExtractionPort delegates to UseCase.Extract sub-modules**: `epExtractDocFile`, `epExtractOfficeFile`, `epExtractHaskellStub`, `epExtractImageFile` all delegate to the existing UseCase.Extract.* modules, passing `logEnv` as a parameter. This is a temporary measure — when UseCase.Extract is refactored (Task 6), these will use the logging port instead.

4. **TreeSitter grammar pointer mapping in Wiring**: `getGrammarPtr` maps grammar names to tree-sitter FFI pointers. This stays in Wiring (Infrastructure layer) since it depends on C FFI.

5. **`unsafeCoerce` for LSP handle**: The LSP client is wrapped as `Dynamic` in `LSPHandle` (ExtractionPort). Wiring uses `unsafeCoerce` to recover the concrete `LSPClient` type. This is safe because Wiring is the only place that creates and unwraps `LSPHandle`, and the type is always `LSPClient`.

### Concrete changes

- Created `src/Graphos/UseCase/AppEnv.hs` (25 lines)
- Created `src/Graphos/Infrastructure/Wiring.hs` (231 lines)
- Wiring imports 50+ Infrastructure and Domain modules
- All ports have production implementations except ExportPort (TODO)

## Result

Pending — awaiting Check (Task 5.C).