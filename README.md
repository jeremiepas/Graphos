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

The first run builds the graph; run `graphos serve` and open
`http://localhost:8080/graph.html` to browse it (the viewer assets are served
from the `graphos-studio` frontend bundle, not embedded in the binary), read
`GRAPH_REPORT.md` for the summary, and query `graph.json` with the CLI, Cypher,
the MCP server, or any JGF reader.

## What you get

```
graphos-out/
├── graph.json             # JGF-format graph — query it, migrate it, parse it with any JGF tool
├── GRAPH_REPORT.md        # Summary, extraction coverage, communities, god nodes, bridges, questions
├── graph.html             # Interactive viewer (serve it; skip with --no-viz)
└── cache/                 # Content-hash cache — reruns only re-process changed files
```

Opt-in outputs: `obsidian/` (`--obsidian`), `graph.cypher` (`--neo4j`),
`memgraph.cypher` (`--memgraph`), `community_graph.json`
(`--community-graph`), `embeddings.json` (`--embed`), and `memory/`
(conversation history via the MCP server).

## Capabilities

| Area | What works today |
|------|------------------|
| Extraction | tree-sitter for Go, Rust, TypeScript/JavaScript, Ruby, Java, Kotlin, PHP, Perl, C#, Python, Haskell, C/C++, JSON/YAML/SQL/Markdown, plus docs, Office, PDF, and optional vision for images. Any other language can plug in an external `mode: command` extractor or a runtime tree-sitter grammar. See [supported file types](docs/reference/supported-file-types.md) for the honest per-language edge coverage. |
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

## Custom extractors

Adding a language does not require patching Graphos. Three per-extension
mechanisms are available in `graphos.yaml`:

```yaml
extractors:
  # 1. External command: declared argv (no shell), graph.json-shaped stdout.
  ".pl":
    mode: command
    command: ["my-perl-extractor", "{file}"]   # {files} for batch mode
    batch: false                                # one invocation per file by default
    timeout: 60                                 # seconds per invocation

  # 2. Runtime tree-sitter grammar: a user-supplied .so plus optional .scm queries.
  ".php":
    mode: tree-sitter
    grammar_path: ~/.local/lib/tree-sitter-php.so   # ABI 9-11
    queries: ~/.config/graphos/queries/php/         # definitions/imports/calls.scm

# 3. Declarative import resolution: pick a built-in resolver per extension.
resolve:
  ".java": { kind: package, roots: ["src/main/java"] }
  ".php":  { kind: psr4, roots: ["src"] }
```

A command extractor prints `{"nodes": [...], "edges": [...]}` (or a JGF
envelope); Graphos validates each record, prefixes ids with a per-file bucket,
and merges the result like any other extractor. A non-zero exit, timeout, or
malformed output records a per-file error and falls back to a stub node — one
bad file never aborts the build.

Command extractors run the argv directly, never through a shell, are gated by
the `allow_command_extractors` setting (CLI default on, library default off;
disable with `--no-command-extractors`), and are never spawned for sensitive or
ignored files. Runtime grammars must report tree-sitter ABI 9-11; an unreadable
library or incompatible ABI falls back to the stub with a warning. See
[Workflow 01](docs/workflows/01-full-pipeline.md) and
[supported file types](docs/reference/supported-file-types.md) for details.

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

## Frontend

The frontend is not in this repository. The Elm **Graphos Studio** SPA and the
HTML viewer assets (`viewer.js`, `viewer.css`, the vendored vis-network
renderer) live in the separate `graphos-studio` repository, which owns their Elm
0.19.2 toolchain, build, tests, and CI.

- `graphos serve` reads the built bundle from `--frontend-dir` (defaults to
  `graphos-studio/public` or `../graphos-studio/public`, so a sibling checkout
  works with no flags). No viewer code is compiled into the binary.
- To work on the Studio or the viewer, clone `graphos-studio` beside this
  checkout and follow its README; the local setup is also described in
  [CONTRIBUTING.md](CONTRIBUTING.md#frontend-graphos-studio).
- The two repositories meet only at the `graph-json-contract` shape and the
  `graphos serve` slice/query API.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for the development environment, commit
convention, and PR expectations. The branch and merge model is owned by
[GITFLOW.md](GITFLOW.md); releases follow
[docs/reference/release-policy.md](docs/reference/release-policy.md).

## License

MIT
