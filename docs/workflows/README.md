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
| 08 | [MCP Server](08-mcp-server.md) | `graphos --mcp <graph.json>` | AI agent integration via 13 tools + chat memory |
| 09 | [Merge](09-merge.md) | `graphos merge <a> <b> -o <dir>` | Combine two knowledge graphs + re-cluster |
| 10 | [Ingest](10-ingest.md) | `graphos ingest <file>` | Single file/URL ingestion + optional embeddings |
| 11 | [Community Labeling](11-community-labeling.md) | `graphos <path> --label` | LLM-based human-readable community names |
| 12 | [Neo4j Push](12-neo4j-push.md) | `graphos <path> --neo4j --neo4j-push` | Graph database push — 3 modes (full/subgraph/community) |
| 13 | [Memgraph Push](13-memgraph-push.md) | `graphos <path> --memgraph --memgraph-push` | In-memory graph database push via Bolt protocol |
| 14 | [Symbols](14-symbols.md) | `graphos symbols <name>` | Exact symbol lookup by name |
| 15 | [Neighbors](15-neighbors.md) | `graphos neighbors <node>` | Expand the neighborhood around a node |
| 16 | [Agent Scaffolding](16-agent-scaffolding.md) | `graphos install-skill`, `graphos init` | Scaffold config and user-level AI assistant skills |
| 17 | [Cypher](17-cypher.md) | `graphos cypher "<query>"` | Read-only openCypher/GQL subset (MATCH/WHERE/RETURN) over the property graph |
| 18 | [Cypher Write](18-cypher-write.md) | `graphos cypher "<statement>" --write` | openCypher write subset (CREATE/MERGE/SET/REMOVE/DELETE) with optional graph.json persistence |

## Workflow Dependency Map

```
┌──────────────────────────────────────────────────────────────────────┐
│                                                                      │
│  ┌─────────────────┐     ┌──────────────────┐                       │
│  │ 15. Neighbors   │     │ 01. Full Pipeline│◀──── 16. Scaffolding  │
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
│  ┌─────────────────┐   ┌──────────────────┐                          │
│  │ 14. Symbols     │   │ 17/18. Cypher    │                          │
│  └─────────────────┘   └──────────────────┘                          │
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
├── debug/                  ← OTLP debug trace JSONL
│   └── *.jsonl
```
