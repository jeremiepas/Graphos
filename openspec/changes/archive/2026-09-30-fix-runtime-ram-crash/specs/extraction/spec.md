## MODIFIED Requirements

### Requirement: LSP extraction concurrency bounded

The extraction pipeline SHALL limit concurrent LSP server processes to a configurable maximum (default 2). File groups for different language servers SHALL be processed in bounded parallel batches rather than all simultaneously via `mapConcurrently`.

Previously: LSP file groups were processed via `mapConcurrently`, spawning one LSP server process per language simultaneously (unbounded concurrency).

- **Plan**: Cap LSP server concurrency to prevent multi-GB subprocess accumulation.
- **Do**: Replace `mapConcurrently (extractGroup ...)` with a bounded pool that processes at most N LSP groups concurrently, where N defaults to 2 and is configurable via `--lsp-concurrency`.
- **Check**: On a 5-language codebase, at most 2 LSP server processes are alive at any time. Peak memory stays bounded.
- **Act**: If extraction throughput is significantly impacted, increase default cap or make it adaptive based on available RAM.

#### Scenario: Concurrent LSP extraction with cap
- **WHEN** extracting from a codebase with 5 different language types (Haskell, TypeScript, Python, Go, Rust)
- **THEN** at most N LSP server processes are alive simultaneously (where N = `--lsp-concurrency` value, default 2)
- **AND** each LSP server is disconnected after its file group completes extraction

#### Scenario: LSP server lifecycle
- **WHEN** a language server's file group extraction completes
- **THEN** `disconnectLSP` is called to terminate the server process
- **AND** the server process handle is cleaned up
- **AND** the server's memory is reclaimable by the OS

#### Scenario: Configurable concurrency
- **WHEN** `--lsp-concurrency 4` is specified on the CLI
- **THEN** at most 4 LSP server processes run concurrently during extraction
- **AND** when `--lsp-concurrency` is not specified, the default is 2