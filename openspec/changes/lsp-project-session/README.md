# lsp-project-session

Make `mode: lsp` produce a usable graph for TypeScript projects: a real client
process id in `initialize`, declared client capabilities, absolute `file://`
document URIs, call-hierarchy attribution, and references + calls edges, plus
project-level session options in `graphos.yaml`.

This change is spec-driven. The two failing specs
(`specs/lsp-session`, `specs/lsp-edge-extraction`) document the contract the code
below implements. Run `openspec instructions apply --change lsp-project-session`
and complete `tasks.md` in order (task group 1 first); tick a task only when its
Check holds.

```console
$ openspec validate lsp-project-session --strict
Change 'lsp-project-session' is valid
```

## Baseline (on `main`, before any code)

Recorded to establish the `before` state for tasks 1.2 and 1.3. Both baselines
were reproduced with `cabal exec -- graphos .` run from the fixture directory.
The CLI cannot run from the prebuilt `~/.local/bin/graphos` binary
(interpreter-hash / glibc mismatch); use `cabal exec`.

### 1.2 — TypeScript, `mode: lsp` (baseline: server death, no `calls`)

**Command** (run from `test-corpus/ts-sample/`):

```console
$ export PATH="/tmp/tsbin:$PATH"          # typescript-language-server 5.3.0 (not on PATH)
$ cabal exec -- graphos .
$ echo $?                                 # 0
```

Fixture: `test-corpus/ts-sample/` (`a.ts`, `b.ts`, `c.ts`, `tsconfig.json`,
`graphos.yaml` with `.ts → { mode: lsp, language_id: typescript }`). Expected call
pairs (listed in the spec that uses the fixture):

| defined in | called by | relation |
|---|---|---|
| `a.ts` (`verdict`) | `a.ts` (`sameFileCaller`) | same-file call |
| `a.ts` (`verdict`) | `b.ts` (`wrap`) | cross-file call |
| `a.ts` (`verdict`) | `c.ts` (`main`) | cross-file call |

**What was seen** — the server kills itself ~3 s after connecting, then the build
finishes with exit status 0:

```
[lsp] Initialize successful
[lsp] Server capabilities: documentSymbol=True workspaceSymbol=True
[lsp] Connected to /tmp/tsbin/typescript-language-server
[lsp] Server supports workspace/symbol — using project-level extraction
[lsp] Extracting: ./c.ts
[lsp] Got 1 symbols from ./c.ts
[lsp] Extracting: ./b.ts
[lsp] Got 1 symbols from ./b.ts
[lsp] Extracting: ./a.ts
[lsp] Warning: server process is no longer running
[lsp] Warning: could not send documentSymbol request for ./a.ts (server disconnected?)
[lsp] Got 0 symbols from ./a.ts
```

Per-file extraction, then everything dropped at the build step:

```
[lsp] ./c.ts → 1 nodes, 2 edges
[lsp] ./b.ts → 1 nodes, 2 edges
[lsp] ./a.ts → 0 nodes, 0 edges          # server already dead
[ INFO] [graphos] Extracted 3 nodes, 4 edges
[ INFO] [graphos] Build drops: 4 edges with unknown endpoints, 0 collapsed duplicate
[ INFO] [graphos] Nodes: 3 / Edges: 0
```

- **Server death reproduced** (M1): `typescript-language-server` exits after ~3 s
  because `initialize` sends the constant `processId: 42`
  (`Infrastructure/LSP/Protocol.hs:178`). The build extracts `c.ts` and `b.ts`,
  then loses the server on `a.ts` and finishes.
- **`calls` edges: 0.** The server never advertises `callHierarchyProvider`
  (capabilities were `documentSymbol=True workspaceSymbol=True`), so the gate at
  `Infrastructure/LSP/Extraction.hs:76` stays closed and no `prepareCallHierarchy`
  / `incomingCalls` requests are sent.
- **Edge relations extracted:** `2 Contains + 2 References + 0 Calls`. The two
  `Contains` edges are file→symbol; the two `References` edges are the
  `symbolTreeToEdges` per-file edges. All four are dropped during the build
  because their endpoints (the module nodes) do not exist in this 3-node graph.
- **Final graph:** 3 nodes, 0 edges (`graph.json` `edges: []`).
- **Exit status:** `0`. Full log: `/tmp/ts-baseline.log`.

### 1.3 — Haskell, `mode: lsp` (baseline: `references` = 0)

**Command** (run from `test-corpus/haskell-decls/`):

```console
$ cabal exec -- graphos .
$ echo $?            # 0
```

Fixture: `test-corpus/haskell-decls/Decls.hs` (a standalone module) plus a new
`graphos.yaml` (`.hs → { mode: lsp, language_id: haskell }`).

`haskell-language-server` **2.13.0.0** (GHC 9.10.3) starts and initialises, but
returns **0 symbols** for the file:

```
[lsp] Initialize successful
[lsp] Server capabilities: documentSymbol=True workspaceSymbol=True
[lsp] Extracting: ./Decls.hs
[lsp:/nix/store/.../haskell-language-server-2.13.0.0/bin/haskell-language-server]
   Warning | No [cradle] found for Decls.hs.
   Proceeding with [implicit cradle].
   invoking build tool to determine build flags (this may take some time)
   cabal v2-repl --keep-temp-files --with-repl .../Decls.hs
[lsp] Got 0 symbols from ./Decls.hs
[DEBUG] [graphos]   [haskell-stub] LSP gave 0 symbols for ./Decls.hs, using parser fallback
[DEBUG] [graphos]   [lsp] ./Decls.hs → 0 nodes, 0 edges
[ INFO] [graphos] Extracted 13 nodes, 11 edges     # all from the parser fallback
```

**Root cause:** `Decls.hs` is a standalone file with no cabal package context.
`haskell-language-server`'s implicit cradle runs `cabal v2-repl` to determine
build flags, which cannot resolve a package for a lone `.hs` file, so it returns
0 symbols and graphos falls back to the `haskell-stub` parser.

**`references` edge count: 0** (and 0 `calls` edges). The 13 nodes / 11 edges in
the fallback graph are all `contains` relations (`mod_Decls → <symbol>`); no
`textDocument/references` or `textDocument/callHierarchy` requests are made, so
no `References` or `Calls` edges are produced. Exit status `0`.

Because the LSP produced no symbols, the before/after comparison in **task 6.2**
for Haskell cannot be run here — there are no LSP-sourced `references` edges to
compare against. If `Decls.hs` is given a cabal package context (or the fixture is
placed in a project), `haskell-language-server` will return its document symbols
and the comparison can be completed; that is a follow-up, not part of this change.

## How to reproduce

```bash
# TypeScript baseline/after — needs typescript-language-server on PATH
export PATH="/tmp/tsbin:$PATH"
cd test-corpus/ts-sample && cabal exec -- graphos .

# Haskell baseline — needs haskell-language-server (2.13.0.0)
cd test-corpus/haskell-decls && cabal exec -- graphos .
```

## Close-out checklist (tasks 6.x)

- [ ] 6.1 `cabal build --flag dev` (warning-free) and `cabal test` pass — summary pasted here.
- [ ] 6.2 Rebuild the TS fixture at `call_hierarchy_max_symbols = 5` and `0`; record node
      count, `calls` edge count, the share of `calls` edges whose both ends are existing
      nodes, wall-clock time, and the Haskell `references` count against 1.3 above.
- [ ] 6.3 Add the `CHANGELOG.md` entry (including the caller node-id change for LSP graphs).
- [ ] 6.4 Comment on #293, #294, #295 with the measured outcome; close the ones resolved.
