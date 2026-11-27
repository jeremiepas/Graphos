## Why

A function node tells that a function exists. Getting from the node to its code is
not possible from the CLI, and for Haskell it is not possible at all (issue #298).
Measured on 2026-11-27 with the 0.3.0.0 binary, `--granularity function`:

| Graph | Function and method nodes | with `line_start`, `line_end`, `signature` |
|---|---|---|
| A TypeScript repository | 40,578 | 40,578 |
| Graphos itself (`a131ddc`) | 8,727 | 30, none of them in a `.hs` file |

A Haskell function node holds an id, a label, a kind and a source file, nothing else.
Haskell does not go through the tree-sitter converter: `extractViaTreeSitterFFI` sends
it to a line-based parser (`UseCase/Extract/Haskell.hs`) whose declarations are
`(name, kind)` pairs, and whose nodes are built with `nodeLineStart = Nothing`,
`nodeLineEnd = Nothing` and `nodeSignature = Nothing`.

For every language, once a node is found there is no command that prints its code.
The reader has to open the file by hand, and `graph.json` does not record the directory
the paths are relative to.

Concrete case: the defect behind issue #295 is one literal inside `lspInitialize`. The
graph located the function's file, not its lines, and reading the body took `grep`.

## What Changes

- Haskell declaration nodes carry `line_start`, `line_end` and `signature`.
- New command `graphos source <node>` prints the source lines of a node from disk, with
  `--root` to say where the corpus is, `--context` for surrounding lines, and `--json`.
- The command reports, instead of printing misleading lines: a node without a line
  range, a missing file, and a file whose content no longer matches the node's
  `body_hash` when the node has one.

## Capabilities

### New Capabilities

- `node-source-access`: line range and signature on Haskell declaration nodes, and the
  `graphos source` command.

### Modified Capabilities

None. `node-schema` already defines the three fields as optional, and
`graph-json-contract` already defines `body_hash`; this change fills and reads them.

## Impact

- **Code**: `UseCase/Extract/Haskell.hs` (declaration parser and node builders),
  `UseCase/Query.hs` (node resolution is reused, no change expected), a new source
  reader behind a port in Infrastructure, `CLI/Parser.hs` and `app/Main.hs`.
- **Graph output**: Haskell nodes gain three fields. No schema change, no new node.
- **Out of scope**: storing body text or per-body facts (identifiers, literals) on
  nodes. `notable-string-extraction` already surfaces context-gated strings such as
  environment reads; a general body index is left to a later change (see `design.md`).
- **Issues**: #298.
