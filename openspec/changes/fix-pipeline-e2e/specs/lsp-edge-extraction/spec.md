## MODIFIED Requirements

### Requirement: Infrastructure.LSP.Extraction — extract reference and call edges (was: only Contains edges)

Module `Graphos.Infrastructure.LSP.Extraction` SHALL extract three types of edges from LSP:

1. **Contains edges** (existing): file→symbol and parent→child hierarchy edges
2. **References edges** (NEW): After extracting document symbols, send `textDocument/references` requests for up to 10 top-level symbols per file (sorted by kind priority: Class=5, Function=12, Method=6, then others). Each reference response SHALL produce a `References` edge from the referencing node to the referenced node, with `edgeConfidence = Confidence 0.8`.
3. **Calls edges** (NEW via call hierarchy): If `scpCallHierarchyProvider` is true, send `callHierarchy/incomingCalls` for top-5 symbols per file. Each incoming call SHALL produce a `Calls` edge with `edgeConfidence = Confidence 0.9`.

All edges SHALL have unique `EdgeId` composed as `EdgeId (source <> "->" <> target <> ":" <> relationToText relation)`. (PRD §6.1)

#### Scenario: References extraction produces cross-file edges
- **GIVEN** an LSP server connected to a Haskell project
- **WHEN** `extractViaLSP` processes `Client.hs`
- **THEN** the extraction SHALL contain `References` edges for each symbol that references `Client.hs` symbols from other files

#### Scenario: References request skipped when capability unavailable
- **GIVEN** an LSP server with `referencesProvider = false`
- **WHEN** `extractViaLSP` processes a file
- **THEN** no `references` requests SHALL be sent; only `Contains` edges SHALL be produced

#### Scenario: Top-10 symbols limit per file
- **GIVEN** a file with 50 top-level symbols
- **WHEN** `extractViaLSP` processes it
- **THEN** references SHALL be requested for at most 10 symbols, prioritized by kind (Class > Function > Method > others)

#### Scenario: Unique EdgeId for every edge
- **GIVEN** two Contains edges from different parent symbols to the same child
- **WHEN** edges are added to an Extraction
- **THEN** each SHALL have a unique EdgeId based on source, target, and relation

### Requirement: Infrastructure.LSP.Client — full call hierarchy extraction (was: stub)

`extractCallHierarchy` SHALL be fully implemented: it SHALL send `callHierarchy/incomingCalls` requests for the given symbol, parse the response into `[CallHierarchyIncomingCall]`, and convert each incoming call to a `Calls` edge. If the server does not support `callHierarchyProvider`, the function SHALL return `[]` without sending requests. (PRD §6.1)

#### Scenario: Call hierarchy extraction returns incoming calls
- **GIVEN** an LSP server with `callHierarchyProvider = true`
- **WHEN** `extractCallHierarchy` is called for symbol "main"
- **THEN** it SHALL return a list of incoming calls as `Calls` edges

#### Scenario: Call hierarchy skipped when capability unavailable
- **GIVEN** an LSP server with `callHierarchyProvider = false`
- **WHEN** `extractCallHierarchy` is called
- **THEN** it SHALL return `[]` without sending any requests