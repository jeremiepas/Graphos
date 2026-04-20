# Graphos Modular Refactoring Proposal

**Date**: 2026-04-20  
**Source**: Graphos knowledge graph analysis (god_nodes, get_neighbors, get_community)  
**Status**: Proposed  

---

## Executive Summary

Graph analysis of the codebase (3,128 nodes, 5,713 edges, 0 bridge nodes) identified **6 coupling hotspots** where single modules carry excessive responsibility. These "God Modules" have degree counts 10-100x higher than the average (3.65), making them hard to test, maintain, and reason about.

This proposal splits each hotspot into focused sub-modules following clean architecture principles, with original modules preserved as re-export hubs for backward compatibility.

---

## Hotspot Analysis

| Priority | Module | Degree | Community Size | Smell |
|----------|--------|--------|---------------|-------|
| P0 | `Infrastructure.Export.HTML` | 436 | 353 | 200+ inline JS, 100+ inline CSS, HTML templates as string literals |
| P1 | `Domain.Types` | 342 | 258 | God Module: all types in one file |
| P1 | `Infrastructure.LSP.Client` | 292 | 211 | 5 responsibilities in one module |
| P2 | `UseCase.Extract` | 254 | — | 4 concerns combined (Haskell/Doc/LSP/concurrency) |
| P2 | `Domain.Graph` | 208 | — | 15+ exported functions, 7+ record fields |
| P2 | Export cross-coupling | — | — | CommunityGraph mediator knows all formats |

---

## Refactoring Plan

### P0: `Infrastructure.Export.HTML` (degree 436 → target < 100)

**Problem**: 436 connections — the most-coupled module in the entire codebase. Contains ~200 inline JavaScript strings, ~100 inline CSS strings, and HTML templates all embedded as Haskell string literals.

**Refactor**:
- Extract `VisNode`/`VisEdge` JSON types → `Domain.Export.Visualization` (domain-level types, not HTML-specific)
- Extract JS → `templates/graph.js`
- Extract CSS → `templates/graph.css`
- Extract HTML shell → `templates/graph.html`
- Split `buildHTML` into: `renderNodesJSON`, `renderEdgesJSON`, `renderPageShell`, `renderSearchUI`, `renderCommunitySidebar`

### P1: `Domain.Types` (degree 342 → target < 50)

**Problem**: God Module containing ALL domain types. 258 nodes in community 2621.

**Refactor** — Split into 5 focused sub-modules:
- `Domain.Types.Node` — `Node`, `NodeId`, `FileType` (Code/Doc/Image/Paper/Video)
- `Domain.Types.Edge` — `Edge`, `EdgeId`, `Relation`, `Confidence`, converters
- `Domain.Types.Graph` — `LabeledGraph`, `GraphDiff`, `Extraction`, `Hyperedge`
- `Domain.Types.Pipeline` — `PipelineConfig`, `defaultConfig`, `Detection` types
- `Domain.Types.Analysis` — `Analysis`, `GodNode`, `SurprisingConnection`, `SuggestedQuestion`
- Keep `Domain.Types` as **re-export hub** for backward compatibility

### P1: `Infrastructure.LSP.Client` (degree 292 → target < 30)

**Problem**: 5 responsibilities in one module (211 community members).

**Refactor** — Split into 4 focused sub-modules:
- `Infrastructure.LSP.ServerMap` — 30+ language→server mappings
- `Infrastructure.LSP.Transport` — JSON-RPC read/write, `connectToLSP`, `disconnectLSP`
- `Infrastructure.LSP.CapabilityParse` — Server capability parsing (merge with existing `Capabilities.hs`)
- `Infrastructure.LSP.Extraction` — `symbolToNodes`, `symbolTreeToEdges`, `workspaceSymbolsToDocumentSymbols`
- Keep `LSP.Client` as thin orchestrator

### P2: `UseCase.Extract` (degree 254 → target < 30)

**Problem**: Combines Haskell parsing, doc extraction, LSP orchestration, and concurrency management.

**Refactor** — Split into 3 focused sub-modules:
- `UseCase.Extract.Haskell` — `parseHaskellImports`, `parseHaskellDecls`, `extractHaskellStub`, `isTopLevelDecl`
- `UseCase.Extract.Markdown` — `parseHeader`, `parseTags`, `parseWikiLinks`, `extractDocFile`
- `UseCase.Extract.LSPOrchestrator` — `doExtractWithSharedLSP`, `groupByLSPServer`
- Keep `UseCase.Extract` as composition orchestrator

### P2: `Domain.Graph` (degree 208 → target < 40)

**Problem**: 15+ exported functions, `Graph` record with 7+ fields. Already has `Domain.Graph.FGL` as a sub-module.

**Refactor** — Extend the existing sub-module pattern:
- `Domain.Graph.Core` — `Graph` type, `buildGraph`, `mergeGraphs`, `mergeExtractions`, field accessors
- `Domain.Graph.Query` — `shortestPath`, `neighbors`, `degree`, BFS, DFS, `subgraph`
- `Domain.Graph.Analysis` — `articulationPoints`, `biconnectedComponents`, `godNodes`, `edgeBetweenness`, `dominators`
- `Domain.Graph.Diff` — `graphDiff`, `LabeledGraph`
- Keep `Domain.Graph` as re-export hub (+ existing `Domain.Graph.FGL`)

### P2: Export Cross-Coupling (CommunityGraph mediator)

**Problem**: `CommunityGraph` has `conceptually_related_to` edges to every export format AND FileSystem modules. Classic Mediator anti-pattern.

**Refactor**:
- Introduce `Domain.Export.Format` type class: `class ExportFormat a where render :: LabeledGraph -> a`
- Each format (HTML, JSON, Neo4j, Obsidian, SVG, GraphML) implements independently
- Reduces CommunityGraph coupling significantly

---

## Execution Plan

### Dependency Graph

```
Batch 1 (PARALLEL): 01-04  Domain.Types sub-modules
         ↓
Batch 2 (SEQUENTIAL): 05   Update imports + cabal test
         ↓
Batch 3 (PARALLEL):  06,08,09,11  Graph.Core, Export.Visualization, Export.Format, LSP.ServerMap+Transport
         ↓
Batch 4 (SEQUENTIAL): 07,10,12    Graph.Query+Analysis+Diff, Export.HTML, LSP.CapabilityParse+Extraction
         ↓
Batch 5 (SEQUENTIAL): 13          UseCase.Extract.Haskell + Markdown
         ↓
Batch 6 (SEQUENTIAL): 14          UseCase.Extract.LSPOrchestrator
         ↓
Batch 7 (SEQUENTIAL): 15          Final integration + ExportFormat + full test suite
```

### 15 Subtasks

| # | Title | Depends On | Parallel |
|---|-------|-----------|----------|
| 01 | Create Domain.Types.Node | — | Yes |
| 02 | Create Domain.Types.Edge | — | Yes |
| 03 | Create Domain.Types.Graph | — | Yes |
| 04 | Create Domain.Types.Pipeline + Analysis | — | Yes |
| 05 | Update all imports after Types split | 01-04 | No |
| 06 | Create Domain.Graph.Core | 05 | No |
| 07 | Create Domain.Graph.Query + Analysis + Diff | 06 | No |
| 08 | Create Domain.Export.Visualization types | 05 | Yes |
| 09 | Create Domain.Export.Format type class | 05 | Yes |
| 10 | Refactor Export.HTML — extract templates | 08,09 | No |
| 11 | Split LSP.Client — ServerMap + Transport | 05 | Yes |
| 12 | Split LSP.Client — CapabilityParse + Extraction | 11 | No |
| 13 | Split UseCase.Extract — Haskell + Markdown | 12 | No |
| 14 | Split UseCase.Extract — LSPOrchestrator | 13 | No |
| 15 | Final integration — ExportFormat + full tests | 07,10,12,14 | No |

---

## Constraints

- **Clean architecture**: No IO in Domain or UseCase layers
- **Backward compatibility**: Original modules become re-export hubs
- **Incremental**: Each subtask must compile and pass tests independently
- **Haskell conventions**: PascalCase types, camelCase functions, explicit exports, type signatures on all top-level definitions
- **Graph resilience**: 0 bridge nodes means no single module is critical — safe to refactor

## Exit Criteria

- [ ] All 6 hotspot modules split into focused sub-modules
- [ ] Each new sub-module has explicit exports and < 100 lines (ideally < 50)
- [ ] Original modules serve as re-export hubs where applicable
- [ ] Clean architecture boundaries respected
- [ ] `cabal build` succeeds after each and all subtasks
- [ ] `cabal test` passes after each and all subtasks
- [ ] Graph degree for former hotspot modules significantly reduced

---

## Expected Impact

| Module | Before | After (est.) | Reduction |
|--------|--------|-------------|-----------|
| Export.HTML | 436 | ~80 | -82% |
| Domain.Types | 342 | ~40 (hub) + 60 sub-modules | -88% |
| LSP.Client | 292 | ~25 (hub) + 55 sub-modules | -91% |
| UseCase.Extract | 254 | ~20 (orchestrator) + 50 sub-modules | -80% |
| Domain.Graph | 208 | ~30 (hub) + 40 sub-modules | -86% |