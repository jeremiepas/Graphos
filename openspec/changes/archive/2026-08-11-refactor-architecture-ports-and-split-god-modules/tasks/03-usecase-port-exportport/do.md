<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Create UseCase.Port.ExportPort — DO

**Task slug**: `03-usecase-port-exportport`
**Attempt**: 1
**Status**: in-progress

## Summary

Created `ExportPort` record type with a single `epExportAll` field that handles all export formats in one operation. Also defined `ExportResult` type in the port module to avoid Infrastructure type leakage.

## Detail

### What was implemented

The `ExportPort` module (`src/Graphos/UseCase/Port/ExportPort.hs`, 27 lines) was created with:

**Record type `ExportPort`** with 1 field:
- `epExportAll :: FilePath -> Analysis -> PipelineConfig -> Detection -> Maybe (Map CommunityId Text) -> IO ExportResult` — single operation that produces all export formats

**Supporting type `ExportResult`**:
- `erReport :: FilePath` — report file path
- `erJSON :: FilePath` — JSON output path
- `erHTML :: Maybe FilePath` — optional HTML output
- `erObsidian :: Maybe FilePath` — optional Obsidian vault
- `erNeo4j :: Maybe FilePath` — optional Neo4j export marker

### Key decisions

1. **Single `epExportAll` field, not 9 separate fields**: The original plan called for 9 separate export format fields (HTML, Obsidian, Neo4j, etc.), but examining the actual UseCase.Export code revealed that `exportAll` is always called as a single orchestration function that produces all formats together. Splitting it into per-format methods would add complexity without benefit — the UseCase never calls individual export formats independently; it always exports everything at once.

2. **`ExportResult` in the port module**: Rather than returning `()` or a simple success/failure, the port returns a structured result type with file paths, matching what `UseCase.Export.exportAll` already produces.

3. **`Detection` and `CommunityId` in signatures**: These Domain types appear in the export orchestration, so they're included in the port signature. No Infrastructure types leak through.

4. **Note on Wiring**: `productionExportPort` in `Infrastructure.Wiring` currently throws `error "not yet wired"` — this is intentional. The export port will be fully wired once UseCase.Export is refactored to use it (Task 8).

### Concrete changes

- Created `src/Graphos/UseCase/Port/ExportPort.hs` (27 lines)
- Defined `ExportPort` record with `epExportAll` field
- Defined `ExportResult` data type with file path results
- Module exports `ExportPort(..)` and `ExportResult(..)`

## Result

Pending — awaiting Check (Task 3.C).