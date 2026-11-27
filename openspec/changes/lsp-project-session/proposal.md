## Why

`mode: lsp` cannot produce a usable graph for a TypeScript project today, and it yields
no function-to-function `calls` edges on any project. Measured on 2026-11-27 with
`typescript-language-server` 5.1.3 against `origin/main` (`a131ddc`):

1. **The server kills itself 3 seconds after connecting.** `lspInitialize` sends the
   constant `processId: 42` (`Infrastructure/LSP/Protocol.hs:178`). The server's
   parent-process watchdog checks that pid, finds it is not its client, and exits with
   status 1 after 3.0 s. Sending the real client pid keeps the server alive (12 s probe).
   The build went on, extracted 3 files, logged `server process is no longer running`
   and finished with exit status 0 and a 683-node graph with `contains` edges only.
2. **Call hierarchy is never requested.** `initialize` declares no
   `textDocument.callHierarchy` client capability, so the server omits
   `callHierarchyProvider` and the gate at `Infrastructure/LSP/Extraction.hs:76` stays
   closed. Declaring the capability makes the same server advertise it, and
   `prepareCallHierarchy` / `incomingCalls` then return correct callers.
3. **The call edges that would be produced are wrong.** `parseIncomingCall` builds the
   caller's node id from the *callee's* file path and ignores `from.uri`, so every
   cross-file caller is attributed to the wrong file. Only the top 5 symbols of a file
   are asked, with no way to raise the cap.
4. **A project cannot tune its session.** The `lsp:` section of `graphos.yaml` carries
   only `command`, `args` and `language_id`. The 300 s timeout is hard-coded in
   `Infrastructure/Wiring.hs`, and nothing passes `initializationOptions` (for example
   the TypeScript server memory limit).

The session is already project-level in one sense: one server per language group,
rooted at the scanned directory (`UseCase/Extract/LSP.hs`). What is missing is a correct
handshake, correct call edges, and project-level control over that session.

## What Changes

- `initialize` sends the real Graphos process id, never a constant.
- `initialize` declares the client capabilities Graphos uses: document symbols,
  references, call hierarchy and workspace symbols.
- Every `textDocument` URI is an absolute `file://` URI resolved against the session
  root (today the path is used as given, for example `file://./src/x.ts`).
- The `lsp:` entries of `graphos.yaml` gain optional session options:
  `initialization_options`, `timeout_seconds`, `call_hierarchy` (`auto` | `on` | `off`),
  `call_hierarchy_max_symbols` and `max_restarts`. Omitted, each keeps today's value.
- Call edges take the caller's file from `from.uri`, so a cross-file caller points at
  the node of the file that contains it. The per-file symbol cap becomes configurable.
- A server that dies is reported with its exit status, restarted up to `max_restarts`
  times, and the build ends with a summary of the files it could not extract.
- The connect-time log lists all four capabilities, not two.

No breaking change: every new key is optional, and a `graphos.yaml` without them
behaves as today except for the two handshake fixes.

## Capabilities

### New Capabilities

- `lsp-session`: the contract of one LSP session for a project — handshake identity and
  declared client capabilities, absolute document URIs, per-server session options in
  `graphos.yaml`, and reporting and bounded restart when the server dies.

### Modified Capabilities

- `lsp-edge-extraction`: `Calls` edges resolve the caller from the incoming call's
  `from.uri`; the symbol cap is configurable; the capability gate follows the
  `call_hierarchy` mode.

## Impact

- **Code** (Infrastructure, plus one Domain record and one UseCase port):
  `Infrastructure/LSP/Protocol.hs` (initialize builder, URI builder),
  `Infrastructure/LSP/Extraction.hs` (gate, caller attribution, cap),
  `Infrastructure/LSP/Transport.hs` (capability log, exit status, restart),
  `Infrastructure/Wiring.hs` (session options into the port),
  `Domain/Config/Extraction.hs` (`LSPServerConfig` optional fields),
  `Domain/Config/Validation.hs`, `UseCase/Port/ExtractionPort.hs` and
  `UseCase/Extract/LSP.hs` (`epConnectLSP` takes the session options).
- **Config**: new optional keys under `lsp:` in `graphos.yaml`; the generated config
  template documents them.
- **Graph output**: LSP-mode graphs gain `calls` edges, and their node ids for callers
  change from "callee file + caller name" to "caller file + caller name". Tree-sitter
  graphs are unchanged.
- **Docs**: the `lsp:` description in `docs/explanation/architecture.md`, the generated
  `graphos.yaml` template and `CHANGELOG.md`.
- **Issues**: #293, #294, #295. Their first analysis is superseded by the measurements
  above (see `design.md`, Context).
