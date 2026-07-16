## ADDED Requirements

### Requirement: Workflow 08 — MCP server startup and JSON-RPC protocol
Module `Graphos.Infrastructure.Server.MCP` SHALL export: `startMCPServer :: LabeledGraph -> CommunityMap -> CohesionMap -> Analysis -> IO ()`, `startMCPServerFromFile :: FilePath -> IO ()`. Protocol: JSON-RPC 2.0 over stdio, one object per line. Version: 2024-11-05. Startup: (1) load graph.json, (2) load conversations from `graphos-out/memory/conv_*.md`, (3) build community 0 from conversations, (4) run analysis, (5) enter request loop. Shutdown: flush pending conversations, no explicit shutdown message. (PRD §8.1, workflow 08)

#### Scenario: MCP server starts and responds to initialize
- **WHEN** client sends JSON-RPC `initialize` request
- **THEN** server SHALL respond with protocol version 2024-11-05 and 11 tool definitions

### Requirement: Workflow 08 — 11 MCP tools
Tool inventory: (1) `query_graph(question, mode, budget)` — BFS/DFS traversal, (2) `get_node(node_id)` — full node metadata, (3) `get_neighbors(node_id)` — all neighbors with relations, (4) `get_community(node_id)` — community ID + members + cohesion + representatives, (5) `god_nodes(top_n)` — top-N by degree, (6) `graph_stats()` — node/edge/community counts + degrees + bridge count, (7) `shortest_path(from, to)` — ordered path or error, (8) `bridge_nodes()` — articulation points, (9) `select_context(question, budget, include_history, verbose)` — compact markdown context, (10) `add_conversation(question, answer_summary, source_nodes)` — persist to community 0, (11) `conversation_history(query, limit)` — search past exchanges. (PRD §8.2, workflow 08)

#### Scenario: select_context via MCP
- **WHEN** MCP client calls `select_context` with `question = "auth flow"`, `budget = 3000`, `include_history = false`
- **THEN** server SHALL classify query, select context, format as markdown, and return within budget; community 0 excluded

#### Scenario: add_conversation creates community 0 node
- **WHEN** MCP client calls `add_conversation` with `source_nodes = ["nodeX"]`
- **THEN** a community 0 ConversationNode SHALL be created with one-way edge to nodeX; nodeX's degree SHALL NOT change; file persisted to `graphos-out/memory/`

#### Scenario: conversation_history searches past exchanges
- **WHEN** MCP client calls `conversation_history` with `query = "auth"`, `limit = 5`
- **THEN** up to 5 matching conversations SHALL be returned sorted by timestamp descending

### Requirement: Workflow 08 — community 0 non-polluting chat memory
Community 0 SHALL be synthetic, created after Leiden. Edges SHALL be one-way: `conversation → code` only. Code node degrees SHALL NOT be affected. `select_context` SHALL exclude community 0 by default; include when `include_history = true`. Conversations persisted to `graphos-out/memory/conv_*.md` (YAML frontmatter + markdown body). (PRD §8.3, workflow 08)

#### Scenario: Code degrees invariant after add_conversation
- **WHEN** a conversation referencing nodeX is added
- **THEN** nodeX's `nodeDegree` in `gNodes` SHALL remain unchanged