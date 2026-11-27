## 1. Baseline and check criteria (before any code)

- [ ] 1.1 Add a TypeScript fixture under `test-corpus/` with 3 files: a function defined in one file, called from the two others, plus one same-file call. Check: the fixture has a `tsconfig.json` and its expected call pairs are listed in a comment at the top of the spec that will use it.
- [ ] 1.2 Record the baseline on `main`: build the fixture with `.ts` in `mode: lsp` and note node count, edge count per relation, the last `[lsp]` log lines and the exit status in the change's `README.md`. Check: the note shows the server death and zero `calls` edges, or states what was seen instead.
- [ ] 1.3 Record the `references` edge count of an existing Haskell fixture in LSP mode on `main`, in the same `README.md`. Check: one number with the command that produced it. If `haskell-language-server` is not installed, say so and mark the comparison in 6.2 as not run.
- [ ] 1.4 Write the failing specs first, each pending or red on `main`: initialize builder (process id, declared capabilities, `initializationOptions`), URI builder (relative, absolute, `.` and `..` segments), YAML parsing and validation of the 5 keys, incoming-call conversion (cross-file caller, missing `uri`), mode logic (`auto`, `on`, `off`, MethodNotFound), restart loop (recovered, exhausted, disabled). Check: `cabal test` lists them and none passes by accident.

## 2. Handshake: identity, capabilities, URIs

- [ ] 2.1 `lspInitialize` takes the process id and optional initialization options; `connectToLSP` supplies the real process id. Check: the builder spec from 1.4 passes; no literal `42` remains in `Infrastructure/LSP/Protocol.hs`.
- [ ] 2.2 Declare `textDocument.references` and `textDocument.callHierarchy` in the initialize capabilities. Check: capability spec passes.
- [ ] 2.3 Replace the 7 inline `"file://" ++ filePath` concatenations with one URI builder that takes the session root. Check: URI spec passes; `grep -n '"file://" ++' src/Graphos/Infrastructure/LSP/` returns only the builder.
- [ ] 2.4 Log `documentSymbol`, `references`, `callHierarchy` and `workspaceSymbol` on connect. Check: capability-log spec passes.
- [ ] 2.5 Integration spec: connect to `typescript-language-server`, wait 10 s, assert `isServerConnected`. Pending when the binary is absent. Check: passes locally with the server installed, pending without it.

## 3. Session options in `graphos.yaml`

- [ ] 3.1 Add the optional fields to `LSPServerConfig` with defaults at parse time, and keep the JSON round-trip. Check: parsing specs pass; a config without the keys parses to the defaults.
- [ ] 3.2 Add the validation rules to `Domain/Config/Validation.hs`. Check: each invalid value from the spec yields an error naming the extension and the key.
- [ ] 3.3 Replace the three positional arguments of `epConnectLSP` with a session-options record; update `Wiring`, `UseCase/Extract/LSP` and the test fakes. Check: `cabal build --flag dev` is warning-free; the hard-coded `lspTimeout = 300` in `Wiring.hs` is gone.
- [ ] 3.4 Pass `initialization_options` and `timeout_seconds` through to `initialize`. Check: builder spec shows `initializationOptions` only when configured.
- [ ] 3.5 Document the keys in the generated `graphos.yaml` template and in `docs/explanation/architecture.md`. Check: the docs drift guard passes.

## 4. Call edges

- [ ] 4.1 `parseIncomingCall` builds the source node id from `from.uri` relative to the session root, in the same form `symbolToNodes` uses. Check: conversion specs pass, including a two-file case where the edge joins two nodes that both exist in the merged extraction.
- [ ] 4.2 Apply `call_hierarchy_max_symbols` in `extractCallHierarchy` (0 means all). Check: cap spec passes.
- [ ] 4.3 Implement the `call_hierarchy` mode at the gate in `extractViaLSP`, with a per-session flag set by the first MethodNotFound. Check: the five mode scenarios pass.

## 5. Liveness

- [ ] 5.1 Expose the exit status of a dead server from `Transport` and log it with the file in flight. Check: a spec with a process that exits with status 3 sees status 3 in the result.
- [ ] 5.2 Bounded restart in `extractFilesWithLSP`: reconnect through the port while restarts remain and retry the file in flight. Check: the three restart scenarios pass against the fake port.
- [ ] 5.3 Log the summary line when files were not extracted. Check: the exhausted scenario asserts the server command, exit status and count.
- [ ] 5.4 Read `extractionConfigFingerprint`; if it does not cover the `lsp:` entries, add the session options to it. Check: a spec shows two configs differing only by `call_hierarchy` give different fingerprints, or the task note states why no change was needed.

## 6. Verification and close-out

- [ ] 6.1 `cabal build --flag dev` and `cabal test` pass. Check: paste the test summary line in `README.md`.
- [ ] 6.2 Rebuild the fixture from 1.1 in LSP mode with `call_hierarchy_max_symbols` at 5 and at 0. Record node count, `calls` edge count, the share of `calls` edges whose two ends are existing nodes, wall-clock time, and the Haskell `references` count against 1.3. Check: every expected call pair from 1.1 is present at cap 0, and the share is 100%.
- [ ] 6.3 Add the `CHANGELOG.md` entry, including the caller node id change for LSP graphs. Check: entry present under Unreleased.
- [ ] 6.4 Comment on issues #293, #294 and #295 with the measured outcome and close the ones this change resolves. Check: each issue links the merged pull request.
