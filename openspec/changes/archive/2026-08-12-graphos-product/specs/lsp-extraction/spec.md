## ADDED Requirements

### Requirement: Infrastructure.LSP.ServerMap — 30+ language → server mappings
Module `Graphos.Infrastructure.LSP.ServerMap` SHALL export `defaultServerMap :: Map Text LSPServerConfig` containing mappings for at least 30 file extensions (`.hs`, `.py`, `.ts`, `.js`, `.go`, `.rs`, `.java`, `.c`, `.cpp`, `.rb`, `.ex`, `.scala`, `.kt`, `.cs`, `.swift`, `.lua`, `.r`, `.php`, `.el`, `.clj`, `.dart`, `.zig`, `.nim`, `.v`, `.odin`, `.f90`, `.pl`, `.sh`, `.dockerfile`, `.yaml`). Each mapping SHALL include `lspCommand`, `lspArgs`, and `lspLanguageId`. Users SHALL override mappings in `graphos.yaml` under `lsp:` section. Setting `lspCommand: ""` SHALL explicitly disable LSP for that extension. (PRD §6.3)

#### Scenario: ServerMap contains 30+ entries
- **WHEN** `defaultServerMap` is queried
- **THEN** it SHALL contain at least 30 file extension keys with non-empty `lspCommand` values

#### Scenario: User override disables LSP for extension
- **WHEN** `graphos.yaml` sets `lsp: { ".nix": { command: "", language_id: nix } }`
- **THEN** the merged config SHALL have empty `lspCommand` for `.nix`, triggering fallback

### Requirement: Infrastructure.LSP.Transport — JSON-RPC 2.0 over stdio with Content-Length framing
Module `Graphos.Infrastructure.LSP.Transport` SHALL implement JSON-RPC 2.0 message framing over stdin/stdout using `Content-Length` headers. Functions: `sendMessage :: Handle -> Value -> IO ()`, `readMessage :: Handle -> IO (Either Text Value)`. Messages SHALL use `Content-Length: <n>\r\n\r\n<json>` framing per LSP base protocol. (PRD §6.1)

#### Scenario: Send and receive JSON-RPC message
- **WHEN** `sendMessage` writes a JSON-RPC request to a handle
- **THEN** the framing SHALL include `Content-Length` header; `readMessage` on the other end SHALL parse it correctly

### Requirement: Infrastructure.LSP.Protocol — initialize/initialized handshake, documentSymbol, references, callHierarchy
Module `Graphos.Infrastructure.LSP.Protocol` SHALL export: `sendInitialize :: Handle -> IO InitializeResult`, `sendInitialized :: Handle -> IO ()`, `sendDidOpen :: Handle -> Uri -> Text -> IO ()`, `requestDocumentSymbol :: Handle -> Uri -> IO [SymbolInformation]`, `requestReferences :: Handle -> Uri -> Position -> IO [Location]`, `requestCallHierarchy :: Handle -> Uri -> Position -> IO [CallHierarchyItem]`, `shutdownServer :: Handle -> IO ()`. Handshake sequence: `initialize` → wait for result → `initialized` notification → ready for requests. (PRD §6.1)

#### Scenario: Complete LSP handshake
- **WHEN** `sendInitialize` then `sendInitialized` are called
- **THEN** the server SHALL respond with capabilities including `documentSymbolProvider`, `referencesProvider`, `callHierarchyProvider` flags

#### Scenario: Request document symbols
- **WHEN** `requestDocumentSymbol` is called for a file URI
- **THEN** the server SHALL return a list of `SymbolInformation` including function names, types, and their ranges

### Requirement: Infrastructure.LSP.Client — spawn one server per language, extraction lifecycle
Module `Graphos.Infrastructure.LSP.Client` SHALL export `data LSPClient = LSPClient { lspHandle :: Handle, lspConfig :: LSPServerConfig, lspCapabilities :: ServerCapabilities }` and functions: `connectToLSP :: LSPServerConfig -> IO (Either Text LSPClient)`, `disconnectLSP :: LSPClient -> IO ()`, `isServerConnected :: LSPClient -> IO Bool`, `extractViaLSP :: LSPClient -> FilePath -> IO Extraction`. The client SHALL spawn ONE server process per language group (shared across files), perform the full handshake, extract per file, then shutdown + exit. (PRD §6.1)

#### Scenario: Extract Haskell file via LSP
- **WHEN** `extractViaLSP` processes a `.hs` file
- **THEN** it SHALL call `didOpen`, `documentSymbol`, `references`, `callHierarchy`, and return an `Extraction` with nodes and edges

#### Scenario: LSP server crash during extraction
- **WHEN** an LSP server process dies mid-extraction
- **THEN** `extractViaLSP` SHALL return `Left errorMessage` and the caller SHALL fall back to tree-sitter or stub extraction

### Requirement: Infrastructure.LSP.Capabilities — check server capabilities
Module `Graphos.Infrastructure.LSP.Capabilities` SHALL export `checkCapabilities :: InitializeResult -> ServerCapabilities` that parses server capabilities to determine which LSP methods are supported: `documentSymbolProvider`, `referencesProvider`, `callHierarchyProvider`, `workspaceSymbolProvider`. (PRD §6.1)

#### Scenario: Detect partial capabilities
- **WHEN** a server supports `documentSymbol` and `references` but not `callHierarchy`
- **THEN** `checkCapabilities` SHALL return `ServerCapabilities` with `callHierarchyProvider = False`

### Requirement: Infrastructure.LSP.CapabilityParse — parse LSP JSON responses
Module `Graphos.Infrastructure.LSP.CapabilityParse` SHALL parse LSP JSON-RPC responses into typed Haskell values: `DocumentSymbol`, `SymbolInformation`, `Location`, `CallHierarchyItem`, `CallHierarchyIncomingCall`. These SHALL be convertible to Domain `Node` and `Edge` types via `Graphos.Infrastructure.LSP.Extraction`. (PRD §6.1)

#### Scenario: Parse DocumentSymbol response
- **WHEN** an LSP server returns a `textDocument/documentSymbol` response
- **THEN** `CapabilityParse` SHALL decode it into `[SymbolInformation]` with name, kind, range, and children

### Requirement: Infrastructure.LSP.Extraction — convert LSP results to Domain Extraction
Module `Graphos.Infrastructure.LSP.Extraction` SHALL export: `symbolsToNodes :: [DocumentSymbol] -> FilePath -> Map NodeId Node`, `referencesToEdges :: [Location] -> NodeId -> Map EdgeId Edge`, `callHierarchyToEdges :: [CallHierarchyIncomingCall] -> NodeId -> Map EdgeId Edge`. These SHALL convert LSP typed results into Domain `Node` and `Edge` values with appropriate `Relation` types (`Calls`, `References`, `Contains`). (PRD §6.1)

#### Scenario: Convert document symbols to nodes
- **WHEN** `symbolsToNodes` receives document symbols for `Auth.hs`
- **THEN** each symbol SHALL become a `Node` with `nodeSourceFile = "Auth.hs"`, `nodeKind` set from symbol kind, `nodeLineStart`/`nodeLineEnd` set from range

### Requirement: Infrastructure.Extract.TreeSitter — CLI integration for fallback extraction
`Graphos.Infrastructure.Extract.TreeSitter.Core` SHALL export `extractViaTreeSitter :: FilePath -> IO (Either Text Extraction)`. `Grammar` SHALL map file extensions to tree-sitter grammar names. `Convert` SHALL parse tree-sitter JSON output into Domain `Node`/`Edge`. Tree-sitter extraction SHALL be triggered when no LSP server is available for a given file extension. (PRD §6.2)

#### Scenario: Tree-sitter fallback for Python
- **WHEN** no `pyright-langserver` is installed but `tree-sitter-python` grammar is available
- **THEN** `extractViaTreeSitter` SHALL produce syntax-level nodes (functions, classes, imports) without semantic references

### Requirement: Stub extraction — one node per file
When neither LSP nor tree-sitter is available, the system SHALL create a single `Node` per file with `nodeKind = "file"`, `nodeLabel` = filename, `nodeSignature = Nothing`, and no edges. (PRD §6.2)

#### Scenario: Stub extraction for unknown file type
- **WHEN** processing a `.xyz` file with no LSP server and no tree-sitter grammar
- **THEN** the system SHALL create exactly one `Node` with `nodeKind = Just "file"` and zero edges