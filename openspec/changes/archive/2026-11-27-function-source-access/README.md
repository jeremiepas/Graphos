# function-source-access — measurements

Both graphs are built from the same corpus, `git archive f385b0a` (the `main`
tree this branch starts from), with `--granularity function --no-observability`:

```bash
git archive f385b0a | tar -x -C corpus
(cd corpus && graphos-base . -o out-base --granularity function --no-observability)
(cd corpus && graphos-new  . -o out-new  --granularity function --no-observability)
python3 measure.py out-new/graph.json corpus --against out-base/graph.json
```

`measure.py` is in this directory. `graphos-base` is the binary built from `main` (`d468dc9`), `graphos-new` the
one built from this branch. `measure.py` counts nodes whose kind is
`Function`/`Method` and the ones carrying `line_start`, `line_end` and
`signature`; for the sanity check (task 1.3) it reads each ranged Haskell
declaration back from the corpus and checks start ≤ end, no overlap inside a
file, and that the first line contains the label.

## Baseline (task 1.2)

| | `main` (`out-base`) | design M1 (`a131ddc`) |
|---|---|---|
| Nodes / edges | 22,854 / 133,991 | — |
| Function and method nodes | 8,738 | 8,727 |
| … with the three fields | 41 (16 `.py`, 23 `.ts`, 2 `.pl`) | 30 (16 `.py`, 12 `.ts`, 2 `.pl`) |
| Haskell function nodes with the three fields | 0 of 8,697 | 0 |

The difference with M1 is the tree: `main` gained TypeScript fixtures and
skills between `a131ddc` and `f385b0a`. The Haskell result is the one M1
states: no Haskell node carries a location.

## After (task 4.2)

| | `out-new` |
|---|---|
| Nodes / edges | 22,854 / 134,007 |
| Haskell function nodes with the three fields | 2,851 of 8,695 |
| Haskell declaration nodes (Function/Type/Class/Instance) with the three fields | 3,285 of 9,129 |
| Sanity check on those 3,285 ranges | 3,285 pass (100 %) |
| Node ids equal to `out-base` | yes (0 added, 0 removed) |
| Edges equal to `out-base` | **no**: 56 added, 40 removed, all `documents` |

Two runs of `graphos-base` on the corpus give identical edges, so the
difference comes from this change. Most Haskell nodes without a range are not
declarations: the line parser also emits names it finds on indented lines
(local `where` bindings, `case` alternatives, constructor uses). Their ids are
kept, as the spec requires, and they get no range.

### Why edges differ

Nodes of files in one directory share an id prefix, so a name declared in one
file and mentioned on an indented line of another file gives one id from two
files. `Graph.Core.preferDeclared` keeps the node that has a line span when the
per-file extractions are merged. Before this change no Haskell node had a span,
so the first file merged won; now the file that declares the name wins.

28 merged nodes change their `source_file` (and 2 of them their kind) this way,
for example `extractViaTreeSitterFFI` moves from `UseCase/Extract/Core.hs`,
which calls it, to `UseCase/Extract/TreeSitter.hs`, which defines it, and
`PropRef` from a `case` alternative in `Cypher/Parser.hs` (`Function`) to its
`data` declaration in `Cypher/AST.hs` (`Type`). The path-reference pass of
`documents` linking keys on `source_file`, so the 96 differing edges are exactly
the `documents` edges touching those 28 nodes.

Per file, ids and edges are unchanged (spec "Ids are stable", covered by
`HaskellSpec`). Across files, the change corrects which file a colliding id
points at; check 4.2 as written ("ids and edges are equal") does not hold for
edges, and is left for the reviewer to accept or reject.

## Command (task 4.3)

```console
$ graphos source lspInitialize --graph out-new/graph.json --root .
46313_lspInitialize  ./src/Graphos/Infrastructure/LSP/Protocol.hs:173-198
173 | lspInitialize :: FilePath -> JSONRPCRequest
174 | lspInitialize rootPath = JSONRPCRequest
…
178 |       [ "processId" .= (42 :: Int)
…
198 |   }
```

The `processId` literal behind #295 is on line 178 of the output.

## Tests (task 4.1)

`cabal build --flag dev --enable-tests` passes (run in `devenv shell`, which
provides zlib). `graphos-test --skip "satisfies the community invariants on small random graphs"`:
**1993 examples, 0 failures, 4 pending**. The skipped property is the
`BoundedDeltaCommunitySpec` random-graph property that does not finish on
`main` either.
