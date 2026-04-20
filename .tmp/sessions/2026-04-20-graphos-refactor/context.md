# Task Context: Graphos Modular Refactoring

Session ID: 2026-04-20-graphos-refactor
Created: 2026-04-20T00:00:00Z
Status: in_progress

## Current Request

Refactor the 6 identified coupling hotspots in the Graphos Haskell codebase. All findings came from Graphos graph analysis (god_nodes, get_neighbors, get_community). The goal is to split oversized modules into focused sub-modules following clean architecture principles while maintaining backward compatibility via re-export hubs.

## Context Files (Standards to Follow)

- .opencode/context/core/standards/code-quality.md — Haskell patterns, clean architecture, module structure, anti-patterns
- .opencode/context/core/standards/test-coverage.md — Hspec/QuickCheck testing patterns

## Reference Files (Source Material)

These are the 6 modules identified as refactoring targets, ordered by priority:

### P0: Infrastructure.Export.HTML (degree 436 — #1 hotspot)
- src/Graphos/Infrastructure/Export/HTML.hs — Contains ~200+ inline JS strings, ~100+ inline CSS strings, HTML templates, VisNode/VisEdge JSON types, search UI, community sidebar, all embedded as Haskell string literals
- src/Graphos/Infrastructure/Export/CommunityGraph.hs — Hub that knows all export formats (cross-coupling)

### P1: Domain.Types (degree 342 — #2 hotspot, community 2621 with 258 members)
- src/Graphos/Domain/Types.hs — God Module containing: Node, Edge, Relation, Confidence, FileType, PipelineConfig, Detection, Analysis, GraphDiff, Hyperedge, LabeledGraph, ToJSON/FromJSON instances, relationToText/textToRelation converters, defaultConfig

### P1: Infrastructure.LSP.Client (degree 292 — #3 hotspot, community 2040 with 211 members)
- src/Graphos/Infrastructure/LSP/Client.hs — Single module handling: 30+ hardcoded language→server mappings (languageServerCommands), process management (connectToLSP, disconnectLSP), JSON-RPC protocol (readLSPMessage, sendLSPMessage), capability parsing (parseServerCapabilities, lookupBoolCaps), symbol extraction (extractDocumentSymbols, extractCallHierarchy, extractWorkspaceSymbols), data conversion (symbolToNodes, symbolTreeToEdges, workspaceSymbolsToDocumentSymbols)
- src/Graphos/Infrastructure/LSP/Capabilities.hs — Partial capability handling already exists

### P2: UseCase.Extract (degree 254 — #4 hotspot)
- src/Graphos/UseCase/Extract.hs — Combines: Haskell stub parsing (parseHaskellImports, parseHaskellDecls, extractHaskellStub, isTopLevelDecl), doc extraction (parseHeader, parseTags, parseWikiLinks, extractDocFile), LSP orchestration (doExtractWithSharedLSP, groupByLSPServer), thread pool management (QSemN concurrency)

### P2: Domain.Graph (degree 208 — #6 hotspot)
- src/Graphos/Domain/Graph.hs — 15+ exported functions: buildGraph, shortestPath, articulationPoints, biconnectedComponents, godNodes, mergeGraphs, graphDiff, subgraph, neighbors, degree, breadthFirstSearch, depthFirstSearch, dominators, edgeBetweenness, isFileNode, isConceptNode + Graph record with 7+ fields + FGL bridge
- src/Graphos/Domain/Graph/FGL.hs — Already a sub-module, extend this pattern

### P2: Export cross-coupling
- src/Graphos/Infrastructure/Export/CommunityGraph.hs — conceptually_related_to edges to every export format AND FileSystem modules. Mediator anti-pattern.

## Components

### P0: Export.HTML Refactor
- Extract JS/CSS/HTML into template files (templates/graph.html, templates/graph.js, templates/graph.css)
- Extract VisNode/VisEdge JSON types into Domain.Export.Visualization (domain-level types, not HTML-specific)
- Split buildHTML into: renderNodesJSON, renderEdgesJSON, renderPageShell, renderSearchUI, renderCommunitySidebar

### P1: Domain.Types Split
- Domain.Types.Node — Node, NodeId, file types (Code, Doc, Image, Paper, Video)
- Domain.Types.Edge — Edge, EdgeId, Relation, Confidence, relationToText/textToRelation converters
- Domain.Types.Graph — LabeledGraph, GraphDiff, Extraction, Hyperedge types
- Domain.Types.Pipeline — PipelineConfig, defaultConfig, Detection types
- Domain.Types.Analysis — Analysis, GodNode, SurprisingConnection, SuggestedQuestion
- Keep Domain.Types as re-export hub for backward compatibility

### P1: LSP.Client Split
- Infrastructure.LSP.ServerMap — 30+ languageServerCommands mapping
- Infrastructure.LSP.Transport — JSON-RPC read/write, connectToLSP, disconnectLSP
- Infrastructure.LSP.CapabilityParse — Server capability parsing (merge with existing Capabilities.hs)
- Infrastructure.LSP.Extraction — symbolToNodes, symbolTreeToEdges, workspaceSymbolsToDocumentSymbols conversions
- Keep LSP.Client as thin orchestrator

### P2: UseCase.Extract Split
- UseCase.Extract.Haskell — parseHaskellImports, parseHaskellDecls, extractHaskellStub, isTopLevelDecl
- UseCase.Extract.Markdown — parseHeader, parseTags, parseWikiLinks, extractDocFile
- UseCase.Extract.LSPOrchestrator — doExtractWithSharedLSP, groupByLSPServer
- Keep UseCase.Extract as composition orchestrator

### P2: Domain.Graph Split
- Domain.Graph.Core — Graph type, buildGraph, mergeGraphs, mergeExtractions, field accessors
- Domain.Graph.Query — shortestPath, neighbors, degree, BFS, DFS, subgraph
- Domain.Graph.Analysis — articulationPoints, biconnectedComponents, godNodes, edgeBetweenness, dominators
- Domain.Graph.Diff — graphDiff, LabeledGraph
- Keep Domain.Graph as re-export hub
- Domain.Graph.FGL already exists — extend this pattern

### P2: Export Type Class
- Introduce Domain.Export.Format type class: class ExportFormat a where render :: LabeledGraph -> a
- Reduces CommunityGraph hub coupling

## Constraints

- Haskell project: cabal build, nix-shell, Hspec/QuickCheck tests
- Clean architecture: Domain has NO IO, UseCase has NO IO implementation, Infrastructure handles all side effects
- Module naming: Graphos.{Domain|UseCase|Infrastructure}.{SubModule}
- PascalCase types, camelCase functions, explicit exports, type signatures on all top-level definitions
- Backward compatibility: original modules become re-export hubs where possible
- Tests must pass after each subtask (cabal test)
- Each refactoring step must be independently compilable and testable
- Graph has 0 bridge nodes = safe to refactor (no single point of failure)

## Exit Criteria

- [ ] All 6 hotspot modules split into focused sub-modules
- [ ] Each new sub-module has explicit exports and < 100 lines ideally
- [ ] Original modules serve as re-export hubs where applicable
- [ ] Clean architecture boundaries respected (no IO in Domain/UseCase)
- [ ] cabal build succeeds after each and all subtasks
- [ ] cabal test passes after each and all subtasks
- [ ] Graph degree for former hotspot modules significantly reduced

## Progress

- [ ] Session initialized
- [ ] Tasks created by TaskManager
- [ ] Implementation complete
- [ ] All tests pass