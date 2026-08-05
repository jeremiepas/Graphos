<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Create UseCase.Port.ExtractionPort — DO

**Task slug**: `02-usecase-port-extractionport`
**Attempt**: 1
**Status**: in-progress

## Summary

Created `ExtractionPort` record type with 16 fields covering LSP lifecycle, LSP extraction, TreeSitter extraction, file-level extraction, office media, Neo4j streaming, and config lookups. Also defined `LSPHandle` (opaque Dynamic wrapper) and `SymbolResult` types in the port module to avoid Infrastructure type leakage.

## Detail

### What was implemented

The `ExtractionPort` module (`src/Graphos/UseCase/Port/ExtractionPort.hs`, 64 lines) was created with:

**Record type `ExtractionPort`** with 16 fields:
- **LSP lifecycle**: `epFindLSPServer`, `epConnectLSP`, `epDisconnectLSP`, `epIsServerConnected`
- **LSP extraction**: `epExtractViaLSP`, `epHasWorkspaceSymbols`, `epExtractWorkspaceSymbols`
- **TreeSitter**: `epParseWithGrammar`
- **File-level extraction**: `epExtractDocFile`, `epExtractOfficeFile`, `epExtractHaskellStub`, `epExtractImageFile`, `epExtractImageFromBytes`
- **Office media**: `epExtractMediaFile`, `epDocxMediaPaths`, `epPptxMediaPaths`
- **Neo4j streaming**: `epPushExtractionStreaming`
- **Config**: `epLanguageServerCommands`

**Supporting types** (defined in the port module to avoid Infrastructure imports):
- `LSPHandle` — opaque wrapper using `Dynamic` to hide `LSP.LSPClient`
- `SymbolResult` — mirrors LSP workspace symbol extraction result using Domain types (`[Node]`, `[Edge]`)

### Key decisions

1. **LSPHandle uses Dynamic + unsafeCoerce**: The LSP client is wrapped as `Dynamic` in the port so UseCase never sees the concrete `LSPClient` type. Wiring uses `unsafeCoerce` to recover it — this is safe because only Wiring creates and unwraps LSPHandle.

2. **16 fields, not 6**: The original plan specified 6 fields, but examining the actual UseCase.Extract code revealed it needs fine-grained primitives (find server, connect, disconnect, check connection, extract file, extract workspace symbols, etc.) rather than coarse-grained operations. This gives UseCase.Extract full orchestration control while staying decoupled from Infrastructure.

3. **No MonadIO constraint**: Fields use bare `IO` return types because this is a record-of-functions — the callers in UseCase.Extract will use the functions as-is. The port pattern itself handles the abstraction; MonadIO constraints are unnecessary at the port boundary.

4. **`epLanguageServerCommands` is a pure Map**: Not an IO action — it's a configuration lookup, included in the port for completeness so UseCase.Extract doesn't need to import LSP server config directly.

5. **`SymbolResult` defined in port module**: Uses Domain types (`Node`, `Edge`) instead of LSP-specific types, so UseCase never imports `Infrastructure.LSP.Protocol`.

### Concrete changes

- Created `src/Graphos/UseCase/Port/ExtractionPort.hs` (64 lines)
- Added `ExtractionPort`, `LSPHandle`, `SymbolResult` to module exports
- Port imports only Domain types (`Extraction`, `Node`, `Edge`, `PipelineConfig`), `Dynamic`, `ByteString`, `Map`, `Text`

## Result

Pending — awaiting Check (Task 2.C).