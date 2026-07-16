## ADDED Requirements

### Requirement: Workflow 16 — LSP server discovery
CLI `graphos lservers` SHALL: (1) load `defaultServerMap` from `Infrastructure.LSP.ServerMap`, (2) for each mapping, verify if the executable exists in PATH, (3) for each found server, connect via `Infrastructure.LSP.Client.connectToLSP`, send `initialize`, check capabilities via `Infrastructure.LSP.Capabilities.checkCapabilities` (documentSymbolProvider, referencesProvider, workspaceSymbolProvider, callHierarchyProvider), (4) shutdown server, (5) output a table: Language | Server Command | documentSymbol | references | callHierarchy | workspaceSymbol. (PRD §6.1, §13, workflow 16)

#### Scenario: Detect and report available LSP servers
- **WHEN** `graphos lservers` is run with `haskell-language-server` and `pyright-langserver` installed
- **THEN** output SHALL include rows for Haskell and Python showing each server's supported capabilities

#### Scenario: Report missing servers gracefully
- **WHEN** a language has no LSP server in PATH
- **THEN** that language SHALL appear in output with "not found" or empty capability columns