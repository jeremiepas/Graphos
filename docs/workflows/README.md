# Graphos Workflows

> Complete reference for every Graphos product workflow — what it does, how to use it, what it produces, and why.

## Index

| # | Workflow | CLI Trigger | One-line Summary |
|---|----------|-------------|-------------------|
| 01 | [Full Pipeline](01-full-pipeline.md) | `graphos <path>` | 7-stage pipeline: detect → extract → build → cluster → infer → analyze → export |
| 02 | [Incremental Pipeline](02-incremental-pipeline.md) | `graphos <path> --update` | SHA256 cache + checkpoint-resume, only changed files re-extracted |
| 03 | [Watch Mode](03-watch-mode.md) | `graphos <path> --watch` | Continuous file watching, auto re-pipeline on change |
| 04 | [Query](04-query.md) | `graphos query <q>` | BFS/DFS graph traversal with token budget |
| 05 | [Path](05-path.md) | `graphos path <from> <to>` | Shortest path between two nodes |
| 06 | [Explain](06-explain.md) | `graphos explain <node>` | Show a node and all its connections |
| 07 | [Context Selection](07-context-selection.md) | `graphos query <q>` (via MCP `select_context`) | LLM context optimization — 5 strategies by complexity |
| 08 | [MCP Server](08-mcp-server.md) | `graphos --mcp <graph.json>` | AI agent integration via 11 tools + chat memory |
| 09 | [Merge](09-merge.md) | `graphos merge <a> <b> -o <dir>` | Combine two knowledge graphs + re-cluster |
| 10 | [Ingest](10-ingest.md) | `graphos ingest <file>` | Single file/URL ingestion + optional embeddings |
| 11 | [Community Labeling](11-community-labeling.md) | `graphos <path> --label` | LLM-based human-readable community names |
| 12 | [Neo4j Push](12-neo4j-push.md) | `graphos <path> --neo4j --neo4j-push` | Graph database push — 3 modes (full/subgraph/community) |
| 13 | [Memgraph Push](13-memgraph-push.md) | `graphos <path> --memgraph --memgraph-push` | In-memory graph database push via Bolt protocol |
| 14 | [Observability](14-observability.md) | `graphos <path> --otel` | OTLP traces + metrics + logs + debug trace |
| 15 | [Config Init](15-config-init.md) | `graphos init` | Bootstrap graphos.yaml with defaults |
| 16 | [LSP Discovery](16-lsp-discovery.md) | `graphos lservers` | Auto-detect installed LSP servers |
| 17 | [Cypher](17-cypher.md) | `graphos cypher "<query>"` | Read-only openCypher/GQL subset (MATCH/WHERE/RETURN) over the property graph |
| 18 | [Cypher Write](18-cypher-write.md) | `graphos cypher "<statement>" --write` | openCypher write subset (CREATE/MERGE/SET/REMOVE/DELETE) with optional graph.json persistence |

## Workflow Dependency Map

```
┌──────────────────────────────────────────────────────────────────────┐
│                                                                      │
│  ┌─────────────────┐     ┌──────────────────┐                       │
│  │ 15. Config Init │────▶│ 01. Full Pipeline│◀──── 16. LSP Discovery│
│  └─────────────────┘     └──┬──┬──┬──┬──┬─┘                       │
│                              │  │  │  │  │                           │
│              ┌───────────────┘  │  │  │  └──────────────┐           │
│              ▼                  │  │  ▼                 ▼           │
│  ┌─────────────────┐   ┌────────┘  │  ┌──────────────┐ ┌──────────┐ │
│  │ 02. Incremental │   │           │  │ 07. Context  │ │11. Comm. │ │
│  │     Pipeline    │   │           │  │   Selection  │ │  Labeling │ │
│  └────────┬────────┘   │           │  └──────────────┘ └──────────┘ │
│           │              │           │                                │
│           ▼              ▼           ▼                                │
│  ┌─────────────────┐   ┌──────────────────┐   ┌──────────────────┐   │
│  │ 03. Watch Mode  │   │ 04. Query        │   │ 12. Neo4j Push │   │
│  └─────────────────┘   ├──────────────────┤   ├──────────────────┤   │
│                        │ 05. Path          │   │ 13. Memgraph   │   │
│                        ├──────────────────┤   │     Push        │   │
│                        │ 06. Explain       │   └──────────────────┘   │
│                        └──────────────────┘                           │
│                                                                       │
│  ┌─────────────────┐   ┌──────────────────┐   ┌──────────────────┐    │
│  │ 08. MCP Server  │   │ 09. Merge        │   │ 10. Ingest     │    │
│  └─────────────────┘   └──────────────────┘   └──────────────────┘    │
│                                                                       │
│  ┌─────────────────┐                                                  │
│  │ 14. Observability│ ← orthogonal, runs alongside any pipeline      │
│  └─────────────────┘                                                  │
│                                                                       │
└──────────────────────────────────────────────────────────────────────┘
```

## Relationship to Outputs

Every workflow writes to `graphos-out/`:

```
graphos-out/
├── graph.json              ← 01, 02, 03, 09 (always produced)
├── graph.html              ← 01, 02, 03, 09 (unless --no-viz)
├── GRAPH_REPORT.md         ← 01, 02, 03, 09 (always produced)
├── community_graph.json    ← 01, 02, 03 (with --community-graph)
├── cache/                  ← 01, 02 (SHA256 cache + checkpoint)
│   ├── graph.checkpoint.json
│   └── *.sha256
├── memory/                 ← 08 (conversation history)
│   └── conv_*.md
├── debug/                  ← 14 (debug trace JSONL)
│   └── *.jsonl
├── index.json              ← 10 (ingest embedding index)
└── obsidian/               ← 01, 02 (with --obsidian)
```