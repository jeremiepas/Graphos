# Getting started

This tutorial takes you from installing Graphos to running your first query. It
is a single path: follow it in order. For *why* Graphos works the way it does,
follow the links out at the end.

## 1. Install

```bash
cabal install graphos
```

You need GHC 9.10 and Cabal. Nothing else: extraction is tree-sitter-first, so
there is no language server to install for the first run.

## 2. Run it on a small repository

Pick a repository you know, ideally a few dozen files:

```bash
cd path/to/small-repo
graphos .
```

Graphos detects files, extracts structure, builds the graph, clusters it, and
writes everything to `graphos-out/`. The run prints progress and a summary; the
first run on a small repo takes seconds.

## 3. Tour the output

```bash
ls graphos-out/
```

| File | What it is |
|------|------------|
| `graph.json` | The graph itself, in JSON Graph Format. Query it, migrate it, or read it with any JGF tool. |
| `GRAPH_REPORT.md` | Plain-language summary: extraction coverage, communities, god nodes, bridges, suggested questions. |
| `graph.html` | Interactive viewer. Open it in a browser; skip it with `--no-viz`. |
| `cache/` | Content-hash cache. The next run only re-processes changed files. |

Start with `GRAPH_REPORT.md` — it tells you what Graphos found and what it
could not extract.

## 4. Ask the graph a question

```bash
graphos query "how does authentication work?"
```

The query is a term-indexed BFS/DFS search over node labels, not natural-language
understanding. It returns ranked nodes and edges within a token budget and a
verdict when nothing scores well:

```bash
graphos query "how does authentication work?" --dfs --budget 5000
```

## 5. Follow a path

```bash
graphos path "AuthModule" "Database"
graphos explain "RequestHandler"
```

`path` finds the shortest route between two nodes; `explain` shows one node and
every edge attached to it. Both resolve a node by id or name and accept
`--graph graphos-out/graph.json` when you are not in the output directory.

## 6. Where to go next

- Build with incremental updates: [Workflow 02](../workflows/02-incremental-pipeline.md).
- Expose the graph to an LLM client: [Workflow 08](../workflows/08-mcp-server.md).
- Understand the extraction model: [Architecture](../explanation/architecture.md).
- Look up every command: [CLI reference](../reference/cli.md).
