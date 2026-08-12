<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Create UseCase.Port.ExportPort — PLAN

**Task slug**: `03-usecase-port-exportport`
**Attempt**: 1
**Status**: pending

## Summary

Create the `UseCase.Port.ExportPort` record type with fields for all 9 export formats (HTML, Obsidian, Neo4j, Memgraph, CommunityGraph, JSON, IncrementalJSON, Report, SVG). This port decouples UseCase.Export from Infrastructure.Export imports.

## Detail

### Scope

This task creates/verifies a single module: `src/Graphos/UseCase/Port/ExportPort.hs` (currently 26 lines, likely a stub). The ExportPort record must contain methods mapping to each Infrastructure.Export function:
- HTML export — `Infrastructure.Export.HTML`
- Obsidian export — `Infrastructure.Export.Obsidian`
- Neo4j export — `Infrastructure.Export.Neo4j`
- Memgraph export — `Infrastructure.Export.Memgraph`
- CommunityGraph export — `Infrastructure.Export.CommunityGraph`
- JSON export — `Infrastructure.Export.JSON`
- IncrementalJSON export — `Infrastructure.Export.IncrementalJSON`
- Report export — `Infrastructure.Export.Report`
- SVG export — `Infrastructure.Export.SVG` (if present)

### Affected Modules

- `src/Graphos/UseCase/Port/ExportPort.hs` — the port module (currently 26 lines, needs completion)
- `src/Graphos/UseCase/Export.hs` — currently imports 5+ Infrastructure.Export modules (will be refactored in Task 8)

### Prerequisites

- Task 1 complete ✅
- Task 2 (ExtractionPort) should be complete for pattern consistency

### Risks

- Some export functions may have complex signatures (file paths, config params) — ensure port fields capture the full signature
- SVG export may not exist yet as a separate Infrastructure module — verify and add if present, or omit

## Check Criteria

| # | Criterion | Spec Scenario | PASS Condition | FAIL Condition |
|---|-----------|---------------|----------------|-----------------|
| 1 | ExportPort compiles | N/A | `cabal build` succeeds | Build fails |
| 2 | ExportPort has all export method fields | "ExportPort record contains all export methods" | Record has fields for: HTML, Obsidian, Neo4j, Memgraph, CommunityGraph, JSON, IncrementalJSON, Report (and SVG if present) | Missing any required export format field |
| 3 | `cabal build` succeeds | N/A | Zero compilation errors | Any compilation error |