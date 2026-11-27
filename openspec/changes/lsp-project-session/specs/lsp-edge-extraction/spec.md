## MODIFIED Requirements

### Requirement: Infrastructure.LSP.Extraction — extract reference and call edges (was: only Contains edges)

Module `Graphos.Infrastructure.LSP.Extraction` SHALL extract three types of edges from LSP:

1. **Contains edges** (existing): file→symbol and parent→child hierarchy edges
2. **References edges**: After extracting document symbols, send `textDocument/references` requests for up to 10 top-level symbols per file (sorted by kind priority: Class=5, Function=12, Method=6, then others). Each reference response SHALL produce a `References` edge from the referencing node to the referenced node, with `edgeConfidence = Confidence 0.8`.
3. **Calls edges** (via call hierarchy): When call hierarchy is enabled for the session (see the next requirement), send `textDocument/prepareCallHierarchy` then `callHierarchy/incomingCalls` for the first `call_hierarchy_max_symbols` symbols of the file in kind-priority order (default 5; 0 means every symbol). Each incoming call SHALL produce a `Calls` edge with `edgeConfidence = Confidence 0.9`, whose target is the node of the asked symbol and whose source is the node id built from the file named by the incoming call's `from.uri` and the name in `from.name`. The source file MUST NOT be assumed to be the file being extracted. When `from.uri` is absent or is not a `file://` URI, no edge SHALL be produced for that call.

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

#### Scenario: Cross-file caller is attributed to its own file
- **GIVEN** symbol `verdict` defined in `/work/p/src/retry.ts` and an incoming call whose `from` is `{ name: "wrap", uri: "file:///work/p/src/factory.ts" }`
- **WHEN** the incoming calls response is converted to edges while extracting `src/retry.ts`
- **THEN** the `Calls` edge source SHALL be the node id of `wrap` in `src/factory.ts` and its target SHALL be the node id of `verdict` in `src/retry.ts`

#### Scenario: Incoming call without a file URI produces no edge
- **GIVEN** an incoming call whose `from` has a name and no `uri`
- **WHEN** the response is converted to edges
- **THEN** no `Calls` edge SHALL be produced for that call

#### Scenario: Symbol cap follows the session option
- **GIVEN** a file with 12 top-level functions and `call_hierarchy_max_symbols = 0`
- **WHEN** `extractViaLSP` processes it with call hierarchy enabled
- **THEN** `prepareCallHierarchy` SHALL be requested for all 12 symbols

### Requirement: Infrastructure.LSP.Client — full call hierarchy extraction (was: stub)

`extractCallHierarchy` SHALL be fully implemented: it SHALL send `textDocument/prepareCallHierarchy` and `callHierarchy/incomingCalls` requests for the given symbol, parse the response into `[CallHierarchyIncomingCall]`, and convert each incoming call to a `Calls` edge. Whether it runs SHALL follow the session's `call_hierarchy` mode:

| Mode | Behaviour |
|---|---|
| `auto` (default) | run only when the server advertised `callHierarchyProvider` |
| `on` | run without consulting the advertised capability; after the first `MethodNotFound` (-32601) answer, stop for the rest of the session and log it once |
| `off` | never run, send no request |

(PRD §6.1)

#### Scenario: Call hierarchy extraction returns incoming calls
- **GIVEN** an LSP server with `callHierarchyProvider = true`
- **WHEN** `extractCallHierarchy` is called for symbol "main"
- **THEN** it SHALL return a list of incoming calls as `Calls` edges

#### Scenario: Call hierarchy skipped when capability unavailable
- **GIVEN** an LSP server with `callHierarchyProvider = false` and `call_hierarchy = auto`
- **WHEN** `extractCallHierarchy` is called
- **THEN** it SHALL return `[]` without sending any requests

#### Scenario: Mode on ignores the advertised capability
- **GIVEN** an LSP server with `callHierarchyProvider = false` that answers call hierarchy requests, and `call_hierarchy = on`
- **WHEN** `extractCallHierarchy` is called for a symbol with one caller
- **THEN** it SHALL send the requests and return one `Calls` edge

#### Scenario: Mode on stops after MethodNotFound
- **GIVEN** `call_hierarchy = on` and a server that answers `prepareCallHierarchy` with error -32601
- **WHEN** two files are extracted in the same session
- **THEN** exactly one `prepareCallHierarchy` request SHALL be sent in that session and one log line SHALL state that call hierarchy was disabled

#### Scenario: Mode off sends nothing
- **GIVEN** an LSP server with `callHierarchyProvider = true` and `call_hierarchy = off`
- **WHEN** `extractCallHierarchy` is called
- **THEN** it SHALL return `[]` without sending any requests
