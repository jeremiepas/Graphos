# 08 — MCP Server

> `graphos --mcp graphos-out/graph.json`

Expose the knowledge graph as an MCP (Model Context Protocol) server, allowing AI agents to query, explore, and persist conversation memory via JSON-RPC over stdio.

---

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                    MCP SERVER                                │
│                                                              │
│  Protocol: JSON-RPC 2.0 over stdio                         │
│  Version: 2024-11-05                                       │
│  Transport: stdin/stdout — one JSON object per line         │
│                                                              │
│  Startup:                                                   │
│  1. Load graph.json (nodes, edges, communities, cohesion) │
│  2. Load chat history from graphos-out/memory/             │
│  3. Enrich community map with chat history (community 0)  │
│  4. Run analysis (god nodes, bridges, suggestions)       │
│  5. Enter JSON-RPC request loop                            │
│                                                              │
│  Shutdown:                                                  │
│  - Flush pending conversations to disk                      │
│  - No explicit shutdown message required                     │
└──────────────────────────────────────────────────────────────┘
```

---

## Chat History Architecture

```
┌──────────────────────────────────────────────────────────────┐
│           CONVERSATION MEMORY DESIGN                         │
│                                                              │
│  During Leiden:    Communities 1..N (pure code clusters)    │
│  After Leiden:     Community 0 = synthetic chat community   │
│                                                              │
│  Edges are ONE-WAY:                                        │
│  conversation ──▶ code node  (chat references code)         │
│  code ──/▶▶ conversation  (degree stays pure)              │
│                                                              │
│  select_context:                                            │
│    Default: EXCLUDES community 0 (no pollution)             │
│    include_history=true: INCLUDES community 0               │
│                                                              │
│  Storage: graphos-out/memory/conv_*.md                      │
│  Format: YAML frontmatter + markdown body                  │
└──────────────────────────────────────────────────────────────┘
```

**Why this design**: Code node degrees are never affected by conversations. Community detection is never influenced by chat history. Chat memory is optional and non-polluting.

---

## Tool Inventory (11 Tools)

| # | Tool | Purpose | Key Params |
|---|------|---------|------------|
| 1 | `query_graph` | BFS/DFS graph traversal | question, mode, budget |
| 2 | `get_node` | Node details by ID | node_id |
| 3 | `get_neighbors` | All neighbors of a node | node_id |
| 4 | `get_community` | Community membership | node_id |
| 5 | `god_nodes` | Highest-degree hub nodes | top_n |
| 6 | `graph_stats` | Graph statistics | (none) |
| 7 | `shortest_path` | Path between two nodes | from, to |
| 8 | `bridge_nodes` | Articulation points | (none) |
| 9 | `select_context` | LLM context selection | question, budget, include_history, verbose |
| 10 | `add_conversation` | Persist exchange memory | question, answer_summary, source_nodes |
| 11 | `conversation_history` | Search past exchanges | query, limit |

---

## Tool Groups

### Read-Only Graph Navigation

`query_graph`, `get_node`, `get_neighbors`, `get_community`, `god_nodes`, `graph_stats`, `shortest_path`, `bridge_nodes`

These tools are safe, idempotent, and never modify the graph.

### Context Selection

`select_context` — the core tool for LLM context optimization. See [07 — Context Selection](07-context-selection.md).

### Conversation Memory

`add_conversation`, `conversation_history` — write and read persistent cross-session memory.

---

## Typical Agent Workflow

```
1. User asks: "How does authentication work?"
2. Agent calls select_context(question="how does authentication work")
3. Agent receives compact markdown context (~1500 tokens)
4. Agent includes context in prompt to LLM
5. LLM generates answer using graph context
6. Agent calls add_conversation(question, summary, source_nodes)
7. Next session: conversation_history finds relevant past exchanges
```

---

## Node Metadata

Each node carries metadata from LSP extraction:

| Field | Description | LLM Value |
|-------|-------------|-----------|
| `kind` | Function, Class, Method, Interface, etc. | Changes reasoning completely |
| `line_start` + `line_end` | Exact code range | Enables `read_file("Auth.hs", 42, 58)` |
| `signature` | Type signature | Understand without reading file |
| `community_id` | Which community | "This belongs to Parser community" |
| `degree` | Number of connections | High = important hub |
| `is_bridge` | Connects communities | Cross-cutting concern |

---

## Prerequisite

Requires a pre-built `graph.json`. Run the full pipeline first.