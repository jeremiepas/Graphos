## Context

Studied on `origin/main` at `a131ddc` with the 0.3.0.0 binary, on a two-file TypeScript
fixture and on a Graphos graph of this repository (22,647 nodes).

### Measurements

| # | Observation | Evidence |
|---|---|---|
| M1 | `function` granularity: 0 `calls` edges; `fine`: 3 | fixture, same command, only `--granularity` differs |
| M2 | At `fine`, the two cross-file calls end on the module node `a.ts` | `wrap → a_Module:a.ts`, `K.run → a_Module:a.ts` |
| M3 | The same-file call is correct at `fine` | `verdict → helper` |
| M4 | `callEdgePairs` only descends when `descendInto gran emitted n` is true; `envRefPairs` ignores that gate | `Infrastructure/Extract/TreeSitter/Convert.hs`, both walks and the comment on `envRefPairs` |
| M5 | No `callers` / `callees` command; `neighbors` has `--depth`, `--edges`, `--path` only | `graphos --help`, `graphos neighbors --help` |
| M6 | Haskell goes to a line-based parser, never to the tree-sitter converter | `UseCase/Extract/TreeSitter.hs`, the `haskell` guard of `extractViaTreeSitterFFI` |

The existing `extraction` requirement says a `calls` edge is emitted "for every
call-denoting node". M1 shows that at the default granularity none inside a body is,
because the walk follows the node-emission gate. The requirement added here states the
granularity behaviour explicitly instead of leaving it implied.

### Where the code sits

| Concern | Module | Layer |
|---|---|---|
| Call edge collection | `Infrastructure/Extract/TreeSitter/Convert.hs` | Infrastructure |
| Merging per-file extractions | the Build use case | UseCase |
| Node-argument resolution, neighbourhood queries | `UseCase/Query.hs` (`resolveNodeArg`, `neighborhoodExpansion`) | UseCase |
| Subcommand parsing and dispatch | `CLI/Parser.hs`, `app/Main.hs` | CLI |
| Per-file result type | `Domain/Types/Graph.hs` (`Extraction`: nodes, edges, errors) | Domain |

`Convert.hs` is one of the ten highest-degree modules of the graph (200 edges), so the
change there is kept to the one walk.

## Goals / Non-Goals

**Goals:**

- A graph built at the default granularity holds function-to-function call edges.
- A cross-file call ends on the called function when the corpus defines it.
- `callers` and `callees` answer the question in one command, by name.

**Non-Goals:**

- Haskell call edges. The Haskell extractor parses lines and the grammar has no call
  node (M6); this needs its own design.
- LSP call hierarchy: change `lsp-project-session`.
- Type-based resolution: receiver types, overloads, dynamic dispatch, re-export chains
  longer than one hop.
- Changing the per-file edge shape defined by the `extraction` capability.
- A relation filter on `neighbors`. The two commands cover the need without widening
  an existing contract.

## Decisions

### D1 — Collect call edges through opaque bodies, emit no node

`callEdgePairs` (Infrastructure) descends into every body, as `envRefPairs` does, and
keeps tracking the nearest emitted definition as the edge source.

| Alternative | Why rejected |
|---|---|
| Tell users to build at `fine` | 12 nodes against 8 on a 5-line fixture; on a repository the `extraction-granularity` capability targets an order of magnitude fewer nodes at `function`. |
| A flag to switch body call edges on | The default graph would stay without call edges, which is the problem being solved. Edges are cheap compared with nodes. |
| Emit call-site nodes at `function` granularity | Contradicts "function bodies are opaque" for nodes. |

The index of definitions used to resolve a callee is the one of the granularity in use,
so an edge never ends on a node that was not emitted.

### D2 — Resolve imported callees in the build stage, from pending call records

The extractor cannot know the definitions of another file. It records, per call bound
by an import, the caller id, the import target and the callee's original name. The
Build use case resolves each record against the merged node set and writes the final
edge. `Extraction` (Domain) gains one field for these records; it is not written to
`graph.json`.

| Alternative | Why rejected |
|---|---|
| Put the callee name in `edgeExtra` and fix targets later | Changes the edge shape the `extraction` capability fixes (`edgeExtra = Nothing`), and leaks a working value into `graph.json`. |
| Compute the target id at the call site from path and name | A node id embeds the definition's kind (`Function:`, `Method:`), unknown at the call site. |
| Resolve at query time | Every consumer of `graph.json` (Neo4j push, HTML view, MCP) would have to repeat it. |

The exact shape of the record and the place of the pass in the Build use case are
fixed in task 3.1, after reading how import targets are merged today.

### D3 — Two commands over one pure query

`UseCase/Query.hs` gains one pure function: a breadth-first traversal over edges of one
relation in one direction, bounded by depth, returning rows with distance and edge
confidence. `callers` and `callees` are two calls of it. Parsing and rendering reuse
the common query options and the not-found and ambiguous renderers.

| Alternative | Why rejected |
|---|---|
| `neighbors --relation calls --direction in` | Widens `neighbor-expansion`, whose ranking and noise controls are tuned for mixed neighbourhoods. |
| Reuse `around` from the open change `explorer-queries` | It returns all in- and out-edges of one node at depth 1 with community data; it is not implemented yet and has no relation filter. |
| Cypher only | The caller has to write the traversal for each depth, and nothing reports that the graph has no call edges. |

### D4 — "No call edges" is an error outcome, an empty list is not

A graph without any `calls` edge cannot answer the question; an empty list would read
as "nothing calls it". The check is one pass over the relation index.

| Alternative | Why rejected |
|---|---|
| Always return an empty list | Indistinguishable from a true negative; this is what made the gap invisible. |
| Warn on stderr only | Lost with `--json`, which agents use. |

### Verification strategy

- `cabal build --flag dev` after each task group.
- `cabal test`: converter specs for D1 on the fixture and on one Java and one Python
  file; a build-stage spec for D2; pure query specs for D3 and D4.
- Before/after build of one real repository at `function` granularity: node count
  (must be equal), `calls` edge count, share of `calls` edges ending on a function or
  method node, and build time.

## Risks / Trade-offs

- **More edges may change clustering.** → Task
  5.2 records modularity and community count before and after on one repository.
- **Name-based resolution can be wrong.** → Ambiguous cases keep the module target;
  confidence stays 0.9 as the `extraction` capability sets it.
- **Extraction time.** The walk now visits every body. → `envRefPairs` already does;
  task 5.2 records the build time.
- **Cached extractions predate the change.** → The extraction fingerprint must change
  so old cache entries are not reused; task 2.3.

## Migration Plan

Rebuild the graph. No schema change; a graph built before the change still loads, and
`callers` reports `no_call_edges` on it when it has none.

## Open Questions

- Should `callers` also follow LSP-sourced `calls` edges differently from name-based
  ones (for example a `--min-confidence` flag)? Deferred until `lsp-project-session`
  produces them.
- Haskell call edges: tree-sitter queries on application nodes, or the LSP path?
  Deferred to a separate change.
