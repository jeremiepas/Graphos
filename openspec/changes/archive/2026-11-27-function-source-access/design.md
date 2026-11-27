## Context

Studied on `origin/main` at `a131ddc` with the 0.3.0.0 binary.

### Measurements

| # | Observation | Evidence |
|---|---|---|
| M1 | 8,727 function nodes in the graph of this repository; 30 carry line range and signature (16 `.py`, 12 `.ts`, 2 `.pl`), none in a `.hs` file | counted from `graph.json` |
| M2 | All 40,578 function and method nodes of a TypeScript repository carry the three fields | counted from `graph.json` |
| M3 | A two-file Haskell fixture gives the same result at `function` and `fine`: declaration nodes with no line field | fixture build |
| M4 | Haskell is routed to a line-based parser; `parseHaskellDecls` returns `(name, kind)` pairs; the node builders set the three fields to `Nothing` | `UseCase/Extract/TreeSitter.hs` (`haskell` guard), `UseCase/Extract/Haskell.hs` |
| M5 | `graph.json` metadata has `schemaVersion`, communities, god nodes; no corpus root | read from a built graph |
| M6 | No function node of either graph carried `body_hash` | counted from `graph.json` |

M6 means the stale-file warning will rarely fire today; it costs nothing when the
field is absent and becomes useful as soon as a build fills it.

### Where the code sits

| Concern | Module | Layer |
|---|---|---|
| Haskell declarations | `UseCase/Extract/Haskell.hs` | UseCase (pure parsing) |
| Node fields | `Domain/Types/Node.hs` | Domain |
| Node-argument resolution | `UseCase/Query.hs` (`resolveNodeArg`) | UseCase |
| Reading a file range | new port function, implemented in Infrastructure | UseCase port / Infrastructure |
| Subcommand | `CLI/Parser.hs`, `app/Main.hs` | CLI |

## Goals / Non-Goals

**Goals:**

- Every Haskell declaration node says where its code is.
- One command goes from a node name to its code, for any language whose nodes carry a
  line range.
- The command never prints lines that are not the node's without saying so.

**Non-Goals:**

- Moving Haskell to the tree-sitter converter. That would change node ids (`mod_A`,
  `<bucket>_<name>`) for every Haskell graph.
- Body text, identifiers or literals stored on nodes. See Open Questions.
- Editing or writing source files.
- A corpus root stored in `graph.json`. See D3.

## Decisions

### D1 — Keep the line-based Haskell parser and make it return positions

`parseHaskellDecls` returns, per declaration, its name, kind, first line, last line and
signature text. It stays a pure function over the file content (UseCase). The node
builders copy the values.

| Alternative | Why rejected |
|---|---|
| Route `.hs` through the tree-sitter converter | Changes every Haskell node id and the module node scheme; out of proportion for three fields. |
| Take positions from the LSP path | Needs a running `haskell-language-server`; the default build must work offline. |
| Fill `line_start` only | A range is needed to print a body; the end line is one more pass over the same lines. |

The end line is the last non-blank, non-comment line before the next top-level
declaration. A trailing comment block that documents the *next* declaration therefore
does not extend the previous one.

### D2 — Signature is the type signature when there is one

The type signature is what a reader wants from a Haskell node. It also matches the
open change `extraction-semantic-enrichment`, whose `extraction-quality` delta requires
signatures to be declaration headers without body text.

| Alternative | Why rejected |
|---|---|
| Always the first definition line | `verdict x = helper x + 42` says less than `verdict :: Int -> Int`, and leaks body text. |
| Signature and first line together | Longer labels in every view for little gain. |

### D3 — The corpus root is a flag, not graph data

`source_file` is stored relative to the scanned directory. `graphos source` resolves
it against `--root`, defaulting to the current directory.

| Alternative | Why rejected |
|---|---|
| Store the absolute root in `graph.json` | Makes the graph machine-specific and leaks a local path into a file that is shared and pushed to Neo4j. |
| Guess the root by walking up from the graph file | The output directory is often outside the corpus. |

The error for a missing file names the resolved path and the flag, so a wrong root is
diagnosed in one step.

### D4 — A separate `source` command, not a flag on `explain`

| Alternative | Why rejected |
|---|---|
| `explain --source` | `explain` is a pure function of the graph; adding disk access to it changes its contract and its `--json` shape. |
| Print through `neighbors` or `symbols` | Those return sets of nodes. |

Reading the file is IO, so it sits behind a port function implemented in
Infrastructure; the range selection, numbering and clipping are pure and tested
without disk.

### Verification strategy

- `cabal build --flag dev`, `cabal test`.
- Pure specs: declaration positions and signatures on Haskell snippets (the five
  scenarios), range selection and context clipping, rendering of each outcome.
- One spec with a temporary directory for the port: existing file, missing file,
  short file.
- Rebuild the graph of this repository: count of Haskell function nodes with the three
  fields before and after, and equality of node ids and edges.

## Risks / Trade-offs

- **Line heuristics on unusual layouts** (operators, `where` at column 0, CPP
  directives, literate Haskell). → Task 1.3 measures, on this repository, the share of
  declarations whose range passes a sanity check (start ≤ end, ranges do not overlap,
  the first line contains the name). A declaration that fails keeps its node without
  a range, which the command reports cleanly.
- **Labels and context output grow** with signatures on Haskell nodes. → Capped at
  200 characters, as elsewhere.
- **A stale graph prints the wrong lines** when `body_hash` is absent. → The header
  always shows the file and range, and the docs say to rebuild after edits.

## Migration Plan

Rebuild Haskell graphs to get the fields. Older graphs load unchanged; on them
`graphos source` reports "no line range" for Haskell nodes.

## Open Questions

- A body index (identifiers and literals per function, behind a flag) would answer
  "which functions mention X" as a graph query. Deferred: it is a graph-size decision
  that needs its own measurement, and `graphos source` plus a text search already
  covers the single-function case.
- Should builds fill `body_hash` by default at `function` granularity? Deferred; it
  belongs with the capability that defined the field.
