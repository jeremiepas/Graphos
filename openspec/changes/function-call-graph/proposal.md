## Why

"Who calls this function, and what does it call" is the first function-level question
of a code review, and Graphos cannot answer it on the graph most projects build
(issue #297). Measured on 2026-11-27 with the 0.3.0.0 binary on a two-file TypeScript
fixture (`verdict` and `helper` in `a.ts`; `wrap` and a method `K.run` in `b.ts`, both
calling the imported `verdict`):

| Granularity | Nodes | `calls` edges | What they are |
|---|---|---|---|
| `function` (the default) | 8 | 0 | call sites sit inside function bodies, which are opaque at this level |
| `fine` | 12 | 3 | `verdict → helper` (correct); `wrap → module a.ts` and `K.run → module a.ts` (the callee function is lost) |

So three things are missing:

1. **No call edges at `function` granularity.** `callEdgePairs` stops at the opaque
   body, while `envRefPairs` in the same module already walks through it. A real
   TypeScript repository built at this level had 15 `calls` edges for 40,578
   function and method nodes, all from module-level code.
2. **A cross-file call points at the module, not the function.** The callers of
   `verdict` cannot be listed even at `fine`, because the two edges that matter end
   on `a.ts`.
3. **No query for it.** There is no `callers` or `callees` command, and
   `graphos neighbors` has no relation filter and no direction.

## What Changes

- At `function` granularity the tree-sitter extractor emits the same `calls` edges as
  at `fine` for calls located inside the body of an emitted definition. No node is
  added: the body stays opaque for nodes.
- In the built graph, a call to an imported name ends on the definition of that name
  in the imported file when that file is in the corpus and defines it. Otherwise it
  keeps today's target (the import or module node).
- New commands `graphos callers <node>` and `graphos callees <node>`, with the flags
  of the query family, `--depth` and `--path`.
- When the graph holds no `calls` edge at all, both commands say so explicitly
  instead of printing an empty result.

Graph size: more edges at `function` granularity, no additional node.

## Capabilities

### New Capabilities

- `call-graph`: call edges at `function` granularity, cross-file callee resolution in
  the built graph, and the `callers` / `callees` queries.

### Modified Capabilities

None. The per-file edge shape stays as the `extraction` capability defines it; the new
requirements describe the granularity gate and the built graph.

## Impact

- **Code**: `Infrastructure/Extract/TreeSitter/Convert.hs` (`callEdgePairs` walk),
  the build stage that merges extractions (callee retargeting), `UseCase/Query.hs`
  (two pure queries), `CLI/Parser.hs` and `app/Main.hs` (two subcommands), the MCP and
  HTTP query surfaces if they mirror the CLI commands.
- **Graph output**: more `calls` edges at `function` granularity; some `calls` edges
  change target from a module node to a function node. No schema change.
- **Dependencies**: written against the `calls` behaviour of the open change
  `extraction-semantic-enrichment` (per-language call node types), which is implemented
  but not archived.
- **Out of scope**: Haskell (its extractor is a line-based parser and its grammar has
  no call node), LSP call hierarchy (change `lsp-project-session`), and type-based
  method dispatch.
- **Issues**: #297, and the edge side of #78 and #164 section 2.
