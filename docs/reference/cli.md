# CLI Reference

The `graphos` binary has one implicit pipeline command (`graphos [PATH]`) and 22
subcommands. This page is the single source of truth for the command surface; run
`graphos <command> --help` for the complete flag list.

## Pipeline (default command)

```bash
graphos [PATH] [options]
```

Builds the knowledge graph for `PATH` (default: `.`), then clusters, analyzes,
and exports to `graphos-out/`.

| Flag | Default | Description |
|------|---------|-------------|
| `--output, -o DIR` | `graphos-out` | Output directory |
| `--directed` | off | Preserve edge direction |
| `--deep` | off | Deep extraction mode |
| `--no-viz` | off | Skip `graph.html` |
| `--update` | off | Incremental update from the last checkpoint |
| `--cluster-only` | off | Re-run clustering from a checkpoint without re-extracting |
| `--no-cluster` | off | Skip clustering entirely |
| `--fresh`, `--no-checkpoint` | off | Ignore any existing checkpoint and re-extract |
| `--watch` | off | Keep watching and re-run on file changes |
| `--community-graph` | off | Write `community_graph.json` |
| `--obsidian [DIR]` | off | Write an Obsidian vault |
| `--neo4j`, `--neo4j-push URI` | off | Emit Cypher or push to Neo4j |
| `--memgraph`, `--memgraph-push URI` | off | Emit Cypher or push to Memgraph |
| `--label` | off | LLM-label communities |
| `--embed` | off | Generate node embeddings |
| `--svg`, `--graphml`, `--wiki` | off | Additional exports |
| `--mcp GRAPH_JSON` | — | Start the MCP server instead of running the pipeline |
| `--granularity LEVEL` | `function` | `fine`, `function`, or `file` |
| `--threads, -j N` | CPU count | Parallel extraction threads |
| `--edge-density MODE` | `normal` | `sparse`, `normal`, `dense`, `maximum` |
| `--resolution FLOAT` | `1.0` | Leiden resolution (higher = more, smaller communities) |
| `--min-comm-size N` | `3` | Merge communities smaller than this |
| `--max-leiden-iterations N` | `50` | Leiden iteration cap |
| `--cluster-algorithm NAME` | `leiden` | `leiden` or `bunch` |
| `--ignore GLOB` | — | Extra ignore pattern (repeatable) |
| `--no-command-extractors` | command extractors on | Disable external `mode: command` extractors declared in `graphos.yaml` |
| `--verbose, -v`, `--debug` | off | DEBUG / TRACE logging |

## Subcommands

### `query`

`graphos query QUESTION` — keyword/traversal search over node labels with a token
budget. Flags: `--dfs`, `--budget N` (default 2000), `--graph FILE`, `--json`,
`--label-width N`, `--edges MODE`, `--max-nodes N`, `--max-label-chars N`,
`--path GLOB`.

### `cypher`

`graphos cypher QUERY [--write]` — run an openCypher/GQL statement. Reads with
`MATCH`/`WHERE`/`RETURN`; mutations (`CREATE`/`MERGE`/`SET`/`REMOVE`/`DELETE`)
require `--write`. Flags: `--graph FILE`, `--budget N`, `--json`.

### `path`

`graphos path FROM TO` — shortest path between two nodes (exact resolution:
node id or identifier). Flags: `--graph FILE`, `--budget N`, `--json`,
`--label-width N`, `--edges MODE`.

### `explain`

`graphos explain NODE` — show a node and all its connections. Same options as
`path`.

### `symbols`

`graphos symbols NAME` — look up an exact symbol by name. Flags: `--graph FILE`,
`--budget N`, `--json`, `--label-width N`, `--edges MODE`.

### `neighbors`

`graphos neighbors NODE` — expand the neighborhood around a node id or display
name. Flags: `--depth N`, `--graph FILE`, `--budget N`, `--json`,
`--label-width N`, `--edges MODE`.

### `push`

`graphos push` — push an existing `graph.json` to Neo4j without re-extracting.
Flags: `--graph FILE`, `--uri URI`, credentials, push mode.

### `push-memgraph`

`graphos push-memgraph` — push an existing `graph.json` to Memgraph over Bolt.
Same shape as `push`.

### `merge`

`graphos merge A B -o DIR` — merge two `graph.json` files and re-cluster. Flags:
`--edge-density`, `--resolution`, `--min-comm-size`, `--max-leiden-iterations`,
`--directed`.

### `ingest`

`graphos ingest FILE` — ingest a single file or URL into the knowledge graph,
optionally with embeddings. Flags: `--embed`, `--no-embed`, `--output, -o DIR`.

### `subgraph`

`graphos subgraph` — extract a path/taxonomy-driven subgraph from an existing
graph. Flags: `--graph FILE`, `--config CONFIG`, `--seeds FILE`, `--hops N`,
`--out, -o FILE`, `--boundary-hops N`, `--no-derive`.

### `migrate-graph`

`graphos migrate-graph GRAPH` — upgrade a legacy `graph.json` to the JGF
envelope (in place unless `--output`).

### `speccheck`

`graphos speccheck --specs openspec` — verify spec artifacts: dependency cycles,
contradiction candidates, coverage, stale ADRs. Flags: `--json`, `--gate`.

### `intentcheck`

`graphos intentcheck --specs specs.json --graph graph.json` — verify intent
specs against a graph. A violated spec exits non-zero.

### `impact`

`graphos impact --before before.json --after after.json [--gate]` — analyze
change-impact obligations and per-node discharge status.

### `view`

`graphos view changes|artifacts|diff|dashboard` — OpenSpec change status
dashboard, artifact view, spec diff, and aggregate dashboard.

### `clustereval`

`graphos clustereval landscape` — run seeded Bunch searches and report basins,
edge similarity, a random baseline, and the Leiden point. Flags: `--graph`,
`--runs N`, `--random-graph SPEC`, `--json`.

### `lservers`

`graphos lservers` — list language servers detected on `PATH`. This is only
needed for `mode: lsp`; tree-sitter, runtime-grammar, and `mode: command`
extractors work without a language server.

### `serve`

`graphos serve` — serve the HTML viewer and JSON query API. Flags: `--dir DIR`,
`--frontend-dir DIR` (the `graphos-studio` bundle holding the Studio output and
the viewer assets; auto-detected from `graphos-studio/public` or
`../graphos-studio/public` when omitted), `--graph FILE`, `--port N`,
`--api-only`, `--no-api`.

### `init`

`graphos init` — scaffold a documented `graphos.yaml`.

### `install-skill`

`graphos install-skill` — install user-level AI assistant skills (for example
opencode).

### `research`

`graphos research` — multi-query knowledge research with subgraph extraction.
Accepts one or more queries plus the common query options, `--json`, and output
paths.
