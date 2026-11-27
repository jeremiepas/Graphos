## ADDED Requirements

### Requirement: Initialize identifies the Graphos process

The `initialize` request built by `Graphos.Infrastructure.LSP.Protocol` SHALL carry the
operating-system process id of the running Graphos process in `processId`. It MUST NOT
carry a constant. The builder SHALL take the process id as a parameter so that it stays
a pure function; `Graphos.Infrastructure.LSP.Transport` SHALL supply the value when it
connects.

#### Scenario: Initialize carries the supplied process id

- **WHEN** the initialize request is built for process id `31337` and root `/work/p`
- **THEN** its params SHALL contain `processId = 31337` and `rootUri = "file:///work/p"`

#### Scenario: A server with a parent watchdog stays connected

- **WHEN** Graphos connects to a server that exits when its client process id is not alive, and no request is sent for 10 seconds
- **THEN** `isServerConnected` SHALL still return `True`

### Requirement: Initialize declares the client capabilities Graphos uses

The `initialize` request SHALL declare, under `capabilities.textDocument`,
`documentSymbol` with `hierarchicalDocumentSymbolSupport = true`, `references` and
`callHierarchy`, each with `dynamicRegistration = false`, and under
`capabilities.workspace`, `symbol` with `dynamicRegistration = false`. A capability
Graphos sends requests for MUST be declared.

#### Scenario: Call hierarchy and references are declared

- **WHEN** the initialize request is built
- **THEN** `capabilities.textDocument.callHierarchy.dynamicRegistration` and `capabilities.textDocument.references.dynamicRegistration` SHALL both be present and `false`

#### Scenario: A server that advertises on declaration is seen as capable

- **WHEN** a server answers `initialize` with `callHierarchyProvider = true` only because the client declared the capability
- **THEN** the parsed `ServerCapabilities` SHALL have `scpCallHierarchyProvider = True`

### Requirement: Document URIs are absolute

Every `textDocument` URI Graphos sends SHALL be an absolute `file://` URI. This covers
`didOpen`, `didClose`, `documentSymbol`, `references` and `prepareCallHierarchy`. A relative
path SHALL be resolved against the session root and normalised (no `.` or `..`
segment) before the URI is built.

#### Scenario: Relative path is resolved against the session root

- **WHEN** the session root is `/work/p` and the file path is `./src/a.ts`
- **THEN** the URI sent SHALL be `file:///work/p/src/a.ts`

#### Scenario: Absolute path is kept

- **WHEN** the file path is `/work/p/src/a.ts`
- **THEN** the URI sent SHALL be `file:///work/p/src/a.ts`

### Requirement: Per-server session options in graphos.yaml

Each entry of the `lsp:` section of `graphos.yaml` SHALL accept these optional keys in
addition to `command`, `args` and `language_id`:

| Key | Type | Default | Effect |
|---|---|---|---|
| `initialization_options` | object | absent | sent verbatim as `initializationOptions` in `initialize` |
| `timeout_seconds` | integer > 0 | 300 | initialize timeout for this server |
| `call_hierarchy` | `auto`, `on`, `off` | `auto` | see the `lsp-edge-extraction` capability |
| `call_hierarchy_max_symbols` | integer >= 0 | 5 | symbols per file asked for incoming calls; 0 means no cap |
| `max_restarts` | integer >= 0 | 1 | restarts allowed for this server in one build |

An entry without these keys SHALL behave as if each default were written. An unknown
`call_hierarchy` value, a non-positive `timeout_seconds` or a negative integer SHALL be
a configuration validation error that names the extension and the key. When several
extensions share one server command, the options of the first extension in the file
group SHALL apply to the session.

#### Scenario: Options are parsed from YAML

- **WHEN** `graphos.yaml` sets `lsp: { ".ts": { command: typescript-language-server, args: ["--stdio"], language_id: typescript, call_hierarchy: on, call_hierarchy_max_symbols: 0, timeout_seconds: 60, initialization_options: { tsserver: { maxTsServerMemory: 6144 } } } }`
- **THEN** the parsed entry SHALL have `call_hierarchy = on`, no symbol cap, a 60 second timeout, and the initialize request for that server SHALL contain `initializationOptions.tsserver.maxTsServerMemory = 6144`

#### Scenario: Omitted options keep the defaults

- **WHEN** an `lsp:` entry has only `command`, `args` and `language_id`
- **THEN** the session SHALL use `call_hierarchy = auto`, a cap of 5 symbols, a 300 second timeout, one restart, and no `initializationOptions` key in `initialize`

#### Scenario: Invalid value is rejected at validation

- **WHEN** `graphos.yaml` sets `call_hierarchy: always` for `.ts`
- **THEN** configuration validation SHALL fail with a message that names `.ts` and `call_hierarchy`

### Requirement: Server death is reported and recovered within a bound

When the server process exits during a build, Graphos SHALL log its exit status and the
file that was being extracted. While the number of restarts used is below
`max_restarts`, Graphos SHALL start a new session with the same options and extract
that file again. When no restart remains, every file not yet extracted SHALL carry an
`ExtractionError` with source `lsp`, and the build SHALL log one summary line with the
server command, the last exit status and the number of files not extracted.

#### Scenario: One death is recovered by a restart

- **WHEN** the server exits while file 3 of 10 is extracted and `max_restarts` is 1
- **THEN** a new session SHALL be started, files 3 to 10 SHALL be extracted through it, and no file SHALL carry an `ExtractionError`

#### Scenario: Restarts exhausted

- **WHEN** the server exits a second time while file 5 of 10 is extracted and `max_restarts` is 1
- **THEN** files 5 to 10 SHALL each carry an `ExtractionError` with source `lsp`, and the summary line SHALL report 6 files not extracted with the server's exit status

#### Scenario: Restarts disabled

- **WHEN** `max_restarts` is 0 and the server exits while file 3 of 10 is extracted
- **THEN** no new session SHALL be started and files 3 to 10 SHALL each carry an `ExtractionError`

### Requirement: Connect-time capability log is complete

On a successful connection Graphos SHALL log one line that states the four parsed
capabilities: `documentSymbol`, `references`, `callHierarchy` and `workspaceSymbol`.

#### Scenario: All four capabilities are logged

- **WHEN** Graphos connects to a server that supports document symbols and references only
- **THEN** the log line SHALL contain `documentSymbol=True`, `references=True`, `callHierarchy=False` and `workspaceSymbol=False`
