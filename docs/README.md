# Graphos documentation

This index is the single entry point to the documentation. Everything here is
audience-facing; internal notes and point-in-time snapshots live under
`docs/internal/` and are deliberately not indexed.

For what Graphos is, install, and the 60-second tour, start at the
[README](../README.md).

## Tutorials

Learning-oriented, one path, no alternatives.

- [Getting started](tutorials/getting-started.md) — install, run Graphos on a
  small repository, tour `graphos-out/`, and run your first query.

## How-to guides

Task-oriented workflows. The [workflows index](workflows/README.md) is the
complete map.

- [Workflows index](workflows/README.md)
- [01 — Full pipeline](workflows/01-full-pipeline.md)
- [02 — Incremental pipeline](workflows/02-incremental-pipeline.md)
- [03 — Watch mode](workflows/03-watch-mode.md)
- [04 — Query](workflows/04-query.md)
- [05 — Path](workflows/05-path.md)
- [06 — Explain](workflows/06-explain.md)
- [07 — Context selection](workflows/07-context-selection.md)
- [08 — MCP server](workflows/08-mcp-server.md)
- [09 — Merge](workflows/09-merge.md)
- [10 — Ingest](workflows/10-ingest.md)
- [11 — Community labeling](workflows/11-community-labeling.md)
- [12 — Neo4j push](workflows/12-neo4j-push.md)
- [13 — Memgraph push](workflows/13-memgraph-push.md)
- [14 — Symbols](workflows/14-symbols.md)
- [15 — Neighbors](workflows/15-neighbors.md)
- [16 — Agent scaffolding](workflows/16-agent-scaffolding.md)
- [17 — Cypher](workflows/17-cypher.md)
- [18 — Cypher write](workflows/18-cypher-write.md)

## Reference

Information-oriented, authoritative and exhaustive.

- [CLI reference](reference/cli.md) — every command and its flags.
- [MCP tools](reference/mcp-tools.md) — every tool exposed by the MCP server.
- [Supported file types](reference/supported-file-types.md) — what each
  extension actually extracts.
- [graph.json / JGF contract](reference/graph-json.md) — the on-disk envelope.
- [Release policy](reference/release-policy.md) — versioning, release criteria,
  and cut procedure.

## Explanation

Understanding-oriented background and design rationale.

- [Architecture](explanation/architecture.md) — layers, modules, and data flow.
- [Pipeline flow diagram](explanation/flow-diagram.md) — the seven stages.
- [Decision tree](explanation/decision-tree.md) — which extraction mode to use.
- [Embedding models](explanation/embedding-models.md) — local and hosted options.

## Formal foundations

Mathematical specifications and requirements are kept in two directories and
indexed here as one entry:

- [`docs/math/`](math/) — worked mathematical notes (for example incremental
  edge updates).
- [`docs/math-requirements/`](math-requirements/) — per-issue formalization
  requirements and consolidated requirements.
