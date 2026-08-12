# 04-query Specification

## Purpose
TBD - created by archiving change graphos-product. Update Purpose after archive.
## Requirements
### Requirement: Workflow 04 — query with BFS/DFS and token budget
Module `Graphos.UseCase.Query` SHALL export: `queryGraph :: LabeledGraph -> Text -> Maybe Int -> Bool -> IO QueryResult` where `data QueryResult = QueryResult { qrNodes :: [Node], qrEdges :: [Edge], qrTraverse :: Text }`. Flow: (1) load graph.json via `UseCase.Load`, (2) normalize query terms via `UseCase.Query.Normalize` (lowercase, tokenize, filter short words), (3) match nodes O(k×log N) via `Domain.Graph.Index` inverted index, take top 5 matches, (4) traverse: BFS (default) or DFS (`--dfs`) with depth limit, (5) enforce `--budget N` (default 2000) token limit. (PRD §13, workflow 04)

#### Scenario: BFS query returns breadth-first results
- **WHEN** `queryGraph` is called with `mode = BFS` and `budget = 2000`
- **THEN** `qrNodes` SHALL contain nodes ordered by distance from matches, within 2000 tokens

#### Scenario: DFS query follows paths deeply
- **WHEN** `--dfs` flag is set
- **THEN** traversal SHALL follow paths deeply before backtracking

#### Scenario: Budget limits result size
- **WHEN** budget is 500 tokens
- **THEN** `qrNodes` SHALL respect the budget; no nodes beyond the token limit included

### Requirement: Workflow 05 — shortest path between two nodes
CLI `graphos path <from> <to>` SHALL load graph.json, match source and target nodes by label/id via inverted index, compute `Domain.Graph.Query.shortestPath` (BFS via FGL `esp`), and return ordered path with node labels, edge relations, and confidence scores. Directed graphs (`--directed`) follow edge direction. (PRD §13, workflow 05)

#### Scenario: Shortest path between connected nodes
- **WHEN** `graphos path "AuthModule" "Database"` is called on a connected graph
- **THEN** result SHALL be ordered list `[Auth, AuthMiddleware, DBPool, Database]` with edge info per hop

#### Scenario: No path exists
- **WHEN** two nodes are disconnected
- **THEN** result SHALL indicate "no path found"

### Requirement: Workflow 06 — explain a node
CLI `graphos explain <node>` SHALL load graph.json, find node by label/id, display full details (kind, signature, location), get all neighbors via `Domain.Graph.Core.neighbors`, get community membership (community ID, cohesion, bridge status), and display all edges with direction, relation, and confidence. (PRD §13, workflow 06)

#### Scenario: Explain shows full node picture
- **WHEN** `graphos explain "RequestHandler"` is called
- **THEN** output SHALL include: node kind, signature, community ID, cohesion, is_bridge, degree, all edges with →/← direction and relation

