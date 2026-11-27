## Context

### How this was studied

Two sources, both on `origin/main` at `a131ddc` (2026-11-27):

- **A Graphos graph of Graphos itself** (`--granularity function`, tree-sitter Haskell):
  22,647 nodes, 26,597 edges (23,020 `contains`, 3,383 `imports`, 194 `references`).
  Module-to-module edges below come from its `imports` edges, with the target module
  name mapped back to its source file by a script. The graph has no `calls` edges for
  Haskell, so function-level statements in this document come from reading the code.
- **A standalone JSON-RPC client** against `typescript-language-server` 5.1.3 on a real
  TypeScript worktree, to separate what the server does from what Graphos asks.

### The LSP layer, as the graph shows it

| Module | Layer | in | out | Importers |
|---|---|---|---|---|
| `Infrastructure/LSP/Protocol.hs` | Infrastructure | 5 | 0 | CapabilityParse, Extraction, Transport, Wiring, TransportSpec |
| `Infrastructure/LSP/Transport.hs` | Infrastructure | 3 | 2 | Client, Extraction, TransportSpec |
| `Infrastructure/LSP/Extraction.hs` | Infrastructure | 1 | 4 | Client |
| `Infrastructure/LSP/Client.hs` | Infrastructure | 3 | 4 | Capabilities, Wiring, ClientSpec |
| `Infrastructure/LSP/CapabilityParse.hs` | Infrastructure | 2 | 1 | Client, Transport |
| `Infrastructure/LSP/ServerMap.hs` | Infrastructure | 3 | 0 | Config, Client, Extraction |
| `UseCase/Extract/LSP.hs` | UseCase | 2 | 5 | Extract, Extract/Core |
| `UseCase/Port/ExtractionPort.hs` | UseCase | 5 | 5 | Wiring, AppEnv, Extract/Core, Extract/LSP, Extract/TreeSitter |
| `Domain/Config/Extraction.hs` | Domain | 12 | 0 | Config and 7 of its sub-modules, Extract/Command, Extract/Pdf, UseCase/Ingest, ExtractionPort |

The LSP modules are reached from the rest of the system through one place:
`Infrastructure/Wiring.hs` builds `ExtractionPort` from `LSP.Client`. That keeps the
blast radius of this change small. The two wide files are `Domain/Config/Extraction.hs`
(12 importers) and the port (5 importers), so the changes there are additive.

### What already exists

- One server per language group, shared by all its files and rooted at the scanned
  directory (`doExtractWithSharedLSP`). The session is already per project.
- Server stderr is drained to the log with an `[lsp:<command>]` prefix.
- A dead server stops the per-file loop: remaining files get an `ExtractionError`
  (`extractFilesWithLSP`). There is no exit status in the log and no restart.

### Measurements

| # | Observation | Evidence |
|---|---|---|
| M1 | `processId: 42` makes the server exit with status 1 after 3.0 s | probe with pid 42, where pid 42 is a kernel thread; with the client's own pid the server was alive after 12 s |
| M2 | Without a declared `callHierarchy` client capability the server returns no `callHierarchyProvider`; with it, `true` | two probes, same server, same root |
| M3 | Undeclared, the server still answers `prepareCallHierarchy` and `incomingCalls` correctly | probe returned the one real caller of a function with one caller, and 18 callers for a factory function |
| M4 | `parseIncomingCall` builds the caller id with the callee's `filePath` | `Infrastructure/LSP/Extraction.hs`, `parseIncomingCall` |
| M5 | Document URIs are `"file://" ++ filePath` with the path as given | `Infrastructure/LSP/Protocol.hs`, 7 sites; the build log shows `./src/...` paths |
| M6 | An LSP-mode build of a 435-file TypeScript subset ended with 683 nodes, 95 `contains` edges, exit status 0 | build log: 3 files extracted, then `server process is no longer running` |

M1 explains M6. The effect of M5 on cross-file results was not measured: the server
died before any reference or call request could be compared. Task 1.3 measures it.

Issues #293 to #295 were written before M1 and M2 were known. #293 proposed a probe as
the fix; M2 shows that declaring the capability is the fix and the probe is a fallback.
#294 asked for one long-lived server, which exists. #295 said stderr was not surfaced;
it is, and what is missing is the exit status, a restart and a summary.

## Goals / Non-Goals

**Goals:**

- An LSP-mode build of a TypeScript project keeps its server alive for the whole build.
- That build emits function-to-function `calls` edges whose source node is in the
  caller's file.
- A project sets its session options in `graphos.yaml`, per server.
- A dead server is visible in the log with its exit status, and one death does not
  empty the graph.

**Non-Goals:**

- Outgoing calls. Incoming calls on every asked symbol already cover each call once.
- Changing the build's exit status when LSP extraction fails. Today it is 0, and it
  stays 0; the summary line and `ExtractionError` records are the signal.
- Call edges in the `workspace/symbol` path. That path yields symbols only, and a file
  it covers is not extracted per file. Extending it is a separate change.
- Tree-sitter call edges, and any change to the tree-sitter extractors.
- A new top-level `lsp_session:` section. The existing `lsp:` entries carry the options.

## Decisions

### D1 — Send the real process id; keep the builder pure

`lspInitialize` (Infrastructure) takes the process id as an argument.
`connectToLSP` (Infrastructure, IO) reads it once and passes it.

| Alternative | Why rejected |
|---|---|
| Send `processId: null` | Legal, and it switches the watchdog off, but a server orphaned by a crashed Graphos would then never exit. |
| Keep 42 and send a heartbeat | The watchdog checks the pid, not traffic (M1). |
| Read the pid inside the builder | Makes a pure JSON builder do IO, and the handshake test could no longer assert the value. |

### D2 — Declare the capabilities, keep a mode for servers that under-advertise

The initialize builder declares `references` and `callHierarchy` (M2). The gate in
`extractViaLSP` reads a session mode: `auto` (advertised capability, the default), `on`
(ask regardless, stop on the first MethodNotFound), `off`.

| Alternative | Why rejected |
|---|---|
| Probe only, no declaration (first idea in #293) | Treats the symptom. A conforming server is allowed to hide a provider the client did not declare. |
| Declaration only, no mode | Leaves no way out for a server that answers without advertising (M3), and no way to switch an expensive request off. |
| Environment variable for the mode | Not recorded with the project, so two builds of one project could differ silently. |

### D3 — Absolute, normalised URIs built in one place

One URI builder in `Infrastructure/LSP/Protocol.hs` replaces the 7 inline
concatenations. It takes the session root and a path. The session root is already an
absolute path in `LSPClientConfig`.

| Alternative | Why rejected |
|---|---|
| Make paths absolute in the UseCase before the port | Node ids are built from the path as given; changing it there would rename every node of every LSP graph. |
| Leave relative URIs | A URI such as `file://./src/a.ts` puts `.` in the authority position, so the document is not the project's file on disk. |

Node ids keep using the path as given. Only the wire URI changes.

### D4 — Options live on `LSPServerConfig`, and reach the transport through one record

`LSPServerConfig` (Domain, `Domain/Config/Extraction.hs`) gains optional fields with
defaults applied at parse time. `epConnectLSP` (UseCase port) takes a session-options
record instead of `command`, `args` and `root` as three positional strings. The port
has 3 users (`Wiring`, `ExtractionPort`, `UseCase/Extract/LSP`). `Domain/Config/Validation`
rejects bad values.

| Alternative | Why rejected |
|---|---|
| New top-level `lsp_session:` section keyed by language | A second place to describe one server; the two could disagree. |
| Fields on `ExtractorConfig` (the `extractors:` entries) | That record selects the extraction mode per extension; server settings already live under `lsp:`. |
| Add positional parameters to `epConnectLSP` | Five more positional values of similar types in a port with 3 call sites. |

`initialization_options` is carried as an opaque JSON value: the Domain does not
interpret it, so no server-specific type enters the Domain.

### D5 — Caller attribution from `from.uri`

`parseIncomingCall` reads `from.uri`, converts it to a path relative to the session
root in the same form the extractor uses for that file's own nodes, and builds the
source node id from it. The session root is passed down from the client.

| Alternative | Why rejected |
|---|---|
| Keep the callee's file and fix ids in a later resolve pass | The information is in the response; dropping it and guessing later is strictly worse. |
| Use `from.uri` as an absolute path | The id would not match the node the caller's own file produces, leaving a dangling edge. |

The id form must be identical to `symbolToNodes` for the caller's file. Task 4.1 pins
that with a test that extracts two files and checks the edge joins two existing nodes.

### D6 — Bounded restart in the UseCase loop, exit status from the transport

`Transport` exposes the exit status of a dead server. `extractFilesWithLSP` (UseCase)
asks the port to reconnect when the server is dead and restarts remain, then retries
the file in flight. The restart counter is per session group.

| Alternative | Why rejected |
|---|---|
| Restart inside `Transport`, invisible to the UseCase | The UseCase owns which file is in flight and which files remain; hiding the restart loses the summary. |
| Unbounded restarts | A server that dies on one particular file would loop for ever. |
| Fall back to tree-sitter for the remaining files | Mixes two id schemes and two edge vocabularies in one graph without the user asking. |

### Verification strategy

- `cabal build --flag dev` with `-Werror` after each task group.
- `cabal test`: pure Hspec for the initialize builder, URI builder, YAML parsing,
  validation, incoming-call conversion and mode logic. The restart loop is tested
  against a fake `ExtractionPort` whose server dies on a chosen file.
- One integration spec per live-server scenario, skipped with a pending message when
  the server binary is not on `PATH`, so `cabal test` stays green offline.
- A before/after build of one TypeScript fixture: node count, `calls` edge count, and
  the share of `calls` edges whose two ends are existing nodes.

## Risks / Trade-offs

- **Build time grows with `call_hierarchy_max_symbols`.** Two requests per symbol, with
  a 5 s timeout each. → The default stays 5. The before/after build in task 6.2 records
  the wall-clock time for the cap at 5 and at 0.
- **Declaring `references` could change what a server returns.** → Task 1.3 compares
  the `references` edge count of a Haskell fixture before and after.
- **LSP graph node ids change for cross-file callers.** A consumer that stored an old
  caller id will not find it. → Only `calls` edges are affected, and LSP graphs had none
  on the measured project. The changelog states it.
- **A restart re-indexes the project.** → `max_restarts` defaults to 1; 0 disables it.
- **`on` mode hides a real incapacity for one request.** → It stops after the first
  MethodNotFound and logs once.

## Migration Plan

No migration. Every new key is optional. A project that wants call edges from a server
that under-advertises sets `call_hierarchy: on` on its `lsp:` entry. Rollback is a
revert: no persisted format changes. Cached extractions are keyed by
`extractionConfigFingerprint` (`UseCase/Extract/Core.hs`); task 5.4 checks whether that
fingerprint covers the `lsp:` entries and adds the session options if it does not, so a
changed option does not serve stale cached files.

## Open Questions

- Should a build whose LSP extraction lost files exit non-zero under a strict flag?
  Deferred: it does not change these specs.
- Should the `workspace/symbol` path also extract references and calls? Deferred to a
  follow-up change (see Non-Goals).
