# Graphos

**Context graph builder** — turns a folder of code, docs, papers, and images into
a navigable knowledge graph with community detection, so you (or an LLM) can
query a codebase instead of re-reading it.

Graphos extracts structure from a project and builds a graph: nodes are
functions, classes, files, concepts, and documents; edges are `contains`,
`imports`, `calls`, and inferred relationships. It runs Leiden community
detection, then writes an interactive HTML viewer, a queryable `graph.json`, and
a plain-language `GRAPH_REPORT.md`.

Extraction is **tree-sitter-first by default** — no language server to install,
works offline, and covers the languages below out of the box. LSP is an opt-in
override per extension when you want richer semantic information and are willing
to install and trust a language server.

## Install

```bash
cabal install graphos
```

Requires GHC 9.10 and Cabal. `stack install graphos` also works.

## Quickstart

```bash
cd path/to/a/small-repo
graphos .
ls graphos-out/
graphos query "how does authentication work?"
```

The first run builds the graph; open `graph.html` to browse it, read
`GRAPH_REPORT.md` for the summary, and query `graph.json` with the CLI, Cypher,
the MCP server, or any JGF reader.

## What you get

```
graphos-out/
├── graph.json             # JGF-format graph — query it, migrate it, parse it with any JGF tool
├── GRAPH_REPORT.md        # Summary, extraction coverage, communities, god nodes, bridges, questions
├── graph.html             # Interactive viewer (skip with --no-viz)
└── cache/                 # Content-hash cache — reruns only re-process changed files
```

Opt-in outputs: `obsidian/` (`--obsidian`), `graph.cypher` (`--neo4j`),
`memgraph.cypher` (`--memgraph`), `community_graph.json`
(`--community-graph`), `embeddings.json` (`--embed`), and `memory/`
(conversation history via the MCP server).

## Capabilities

| Area | What works today |
|------|------------------|
| Extraction | tree-sitter for Go, Rust, TypeScript/JavaScript, Ruby, Java, Kotlin, PHP, Perl, C#, Python, Haskell, C/C++, JSON/YAML/SQL/Markdown, plus docs, Office, PDF, and optional vision for images. See [supported file types](docs/reference/supported-file-types.md) for the honest per-language edge coverage. |
| Graph | `contains`, `imports`, `calls`, and inferred doc/code edges; name-matched calls are syntactic, not a semantic call graph. |
| Clustering | Leiden community detection with configurable resolution, minimum size, and iteration cap; Bunch modularization is available via `--cluster-algorithm bunch`. |
| Query | `graphos query`, `path`, `explain`, `symbols`, `neighbors`, and a real openCypher/GQL engine (`graphos cypher`, read-only unless `--write`). |
| Integration | stdio MCP server with 13 tools, an HTTP JSON query API, Obsidian/GraphML/SVG/wiki exports, and Neo4j/Memgraph push. |
| Updates | `--update` incremental runs from a SHA256 checkpoint; `--watch` re-extracts on file changes and re-clusters only affected communities. |

## Documentation

The full documentation index is [docs/README.md](docs/README.md).

| Start here | |
|---|---|
| [Tutorial](docs/tutorials/getting-started.md) | Install → run → tour the output → first query |
| [Workflows](docs/workflows/README.md) | All 18 task-oriented guides |
| [Reference](docs/reference/cli.md) | [CLI](docs/reference/cli.md), [MCP tools](docs/reference/mcp-tools.md), [file types](docs/reference/supported-file-types.md), [graph.json](docs/reference/graph-json.md), [release policy](docs/reference/release-policy.md) |
| [Explanation](docs/explanation/architecture.md) | [Architecture](docs/explanation/architecture.md), [pipeline flow](docs/explanation/flow-diagram.md), [decision tree](docs/explanation/decision-tree.md), [embedding models](docs/explanation/embedding-models.md) |

## LSP security notes

Third-party LSP servers may execute project build tools — for example `jdtls`
and `kotlin-language-server` can run Gradle/Maven on startup. Only run Graphos
with LSP servers you trust, against projects you trust. To opt an extension out
of LSP, set its command to an empty string:

```yaml
lsp:
  ".java":
    command: ""
```

Language-server requirements and per-extension setup live in
[Workflow 16](docs/workflows/16-agent-scaffolding.md) and the
[CLI reference](docs/reference/cli.md).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for the development environment, commit
convention, and PR expectations. The branch and merge model is owned by
[GITFLOW.md](GITFLOW.md); releases follow
[docs/reference/release-policy.md](docs/reference/release-policy.md).

## License

MIT
