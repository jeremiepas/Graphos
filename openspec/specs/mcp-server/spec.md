# mcp-server Specification

## Purpose
TBD - created by archiving change graphos-product. Update Purpose after archive.
## Requirements
### Requirement: Infrastructure.Server.MCP — stdio JSON-RPC 2.0 server
Module `Graphos.Infrastructure.Server.MCP` SHALL export `startMCPServer :: LabeledGraph -> CommunityMap -> CohesionMap -> Analysis -> IO ()` and `startMCPServerFromFile :: FilePath -> IO ()`. Transport: stdin/stdout, one JSON object per line (JSON-RPC 2.0). Protocol version: 2024-11-05. Startup sequence: (1) load graph.json, (2) load conversations from `graphos-out/memory/`, (3) build community 0 from conversations, (4) run analysis, (5) enter request loop. Shutdown: flush pending conversations, no explicit shutdown message required. (PRD §8.1)

#### Scenario: MCP server starts from graph.json
- **WHEN** `startMCPServerFromFile "graphos-out/graph.json"` is called
- **THEN** the server SHALL load the graph, rebuild community 0 from `graphos-out/memory/`, compute analysis, and listen on stdin for JSON-RPC requests

#### Scenario: MCP server responds to initialize
- **WHEN** an MCP client sends a JSON-RPC `initialize` request
- **THEN** the server SHALL respond with protocol version 2024-11-05 and a tools list containing 11 tool definitions

### Requirement: MCP tool query_graph
Tool `query_graph` SHALL accept `question :: String`, `mode :: String` ("bfs"/"dfs"), `budget :: Int`. SHALL perform BFS or DFS on the graph starting from nodes matching the question terms, returning results within the token budget. (PRD §8.2 tool #1)

#### Scenario: BFS query with budget
- **WHEN** client calls `query_graph` with `question="config"`, `mode="bfs"`, `budget=2000`
- **THEN** server SHALL return node list from BFS traversal matching "config" within 2000 tokens

### Requirement: MCP tool get_node
Tool `get_node` SHALL accept `node_id :: String`. SHALL return all metadata for the specified node: id, label, kind, signature, source_file, lines, community_id, degree, is_bridge. (PRD §8.2 tool #2)

#### Scenario: Get node details
- **WHEN** client calls `get_node` with a valid `node_id`
- **THEN** server SHALL return full node metadata as JSON

### Requirement: MCP tool get_neighbors
Tool `get_neighbors` SHALL accept `node_id :: String`. SHALL return all neighboring nodes (both forward and backward edges) with their relations. (PRD §8.2 tool #3)

#### Scenario: Get all neighbors of a node
- **WHEN** client calls `get_neighbors` for a node with 5 connections
- **THEN** server SHALL return 5 neighbor entries with edge relations

### Requirement: MCP tool get_community
Tool `get_community` SHALL accept `node_id :: String`. SHALL return the community ID, member list, cohesion score, and representative nodes for the community containing the specified node. (PRD §8.2 tool #4)

#### Scenario: Get community membership
- **WHEN** client calls `get_community` for a node in community 3
- **THEN** server SHALL return community 3's ID, member count, cohesion, and representative node list

### Requirement: MCP tool god_nodes
Tool `god_nodes` SHALL accept `top_n :: Int` (default 10). SHALL return the N nodes with highest degree sorted descending. (PRD §8.2 tool #5)

#### Scenario: Get top 5 god nodes
- **WHEN** client calls `god_nodes` with `top_n=5`
- **THEN** server SHALL return 5 nodes sorted by descending degree

### Requirement: MCP tool graph_stats
Tool `graph_stats` SHALL accept no parameters. SHALL return: total nodes, total edges, total communities, average degree, max degree, number of bridge nodes. (PRD §8.2 tool #6)

#### Scenario: Get graph statistics
- **WHEN** client calls `graph_stats`
- **THEN** server SHALL return a JSON object with node_count, edge_count, community_count, avg_degree, max_degree, bridge_count

### Requirement: MCP tool shortest_path
Tool `shortest_path` SHALL accept `from :: String`, `to :: String`. SHALL return the shortest path between two named nodes as an ordered list of node IDs, or an error if no path exists. (PRD §8.2 tool #7)

#### Scenario: Shortest path between connected nodes
- **WHEN** client calls `shortest_path` with two connected node names
- **THEN** server SHALL return the ordered path as `[nodeId1, nodeId2, ..., nodeIdN]`

### Requirement: MCP tool bridge_nodes
Tool `bridge_nodes` SHALL accept no parameters. SHALL return all articulation points in the graph (nodes whose removal disconnects the graph). (PRD §8.2 tool #8)

#### Scenario: Get bridge nodes
- **WHEN** client calls `bridge_nodes`
- **THEN** server SHALL return a list of node IDs that are articulation points

### Requirement: MCP tool select_context
Tool `select_context` SHALL accept `question :: String`, `budget :: Int` (default 3000), `include_history :: Bool` (default false), `verbose :: Bool` (default false). SHALL call `UseCase.SelectContext.selectContext` and `UseCase.FormatContext.formatContextForLLM` to produce compact markdown context. (PRD §8.2 tool #9)

#### Scenario: Select context with history excluded
- **WHEN** client calls `select_context` with `include_history=false`
- **THEN** result SHALL NOT include community 0 conversation nodes

#### Scenario: Select context with verbose metadata
- **WHEN** client calls `select_context` with `verbose=true`
- **THEN** result SHALL include per-node metadata (kind, signature, line range, community_id, degree, is_bridge)

### Requirement: MCP tool add_conversation
Tool `add_conversation` SHALL accept `question :: String`, `answer_summary :: String`, `source_nodes :: [String]`. SHALL create a `ConversationNode` in community 0, create one-way edges from conversation to each `source_node`, persist to `graphos-out/memory/conv_<id>.md`. Code node degrees SHALL NOT change. (PRD §8.2 tool #10)

#### Scenario: Add conversation preserves code degrees
- **WHEN** client calls `add_conversation` with `source_nodes=["node_X"]`
- **THEN** node_X's degree SHALL remain unchanged in the graph; a community 0 node SHALL be created with one-way edge to node_X

### Requirement: MCP tool conversation_history
Tool `conversation_history` SHALL accept `query :: String`, `limit :: Int` (default 10). SHALL search past conversation nodes by matching `query` against `convQuestion` and `convSummary` fields. Return matching conversations sorted by `convTimestamp` descending. (PRD §8.2 tool #11)

#### Scenario: Search conversation history
- **WHEN** client calls `conversation_history` with `query="auth"`, `limit=5`
- **THEN** server SHALL return up to 5 conversations whose question or summary matches "auth", newest first

### Requirement: MCP tool cypher_mutate

Tool `cypher_mutate` SHALL accept `query :: String` and `persist :: Bool` (default
false). SHALL parse the query as an openCypher statement, evaluate read statements
normally and mutation statements against the in-memory graph (capability
`cypher-mutation`), and return the mutation summary JSON. When `persist` is true,
the mutated graph SHALL be written back to the loaded `graph.json` with a
timestamped backup. The existing `cypher_query` tool SHALL remain strictly
read-only.

#### Scenario: mutate node property

- **WHEN** client calls `cypher_mutate` with `query="MATCH (n:Function) WHERE n.id = 'f1' SET n.review_status = 'approved'"`
- **THEN** the result contains `summary.properties_set = 1` and the node is queryable by the new property via `cypher_query`

#### Scenario: persist writes back

- **WHEN** client calls `cypher_mutate` with `query="MERGE (n:Module {id: 'm9'})"`, `persist=true`
- **THEN** `graph.json` contains `m9` and a timestamped backup of the original file exists

#### Scenario: write clause rejected on cypher_query

- **WHEN** client calls `cypher_query` with `query="CREATE (n)"`
- **THEN** the tool returns an error naming `CREATE` and pointing at `cypher_mutate`, and the graph is unchanged
