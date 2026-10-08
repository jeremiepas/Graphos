## MODIFIED Requirements

### Requirement: Workflow 05 — shortest path between two nodes
CLI `graphos path <from> <to>` SHALL load graph.json, resolve source and target through `resolveNodeArg` (exact id, then exact identifier, then case-insensitive identifier — see `identifier-resolution`), compute `Domain.Graph.Query.shortestPath` (BFS via FGL `esp`), and return ordered path with node labels, edge relations, and confidence scores. Directed graphs (`--directed`) follow edge direction. An endpoint that is not found or is ambiguous SHALL be reported as such and no path SHALL be computed from a fuzzy match. (PRD §13, workflow 05)

#### Scenario: Shortest path between connected nodes
- **WHEN** `graphos path "AuthModule" "Database"` is called on a connected graph
- **THEN** result SHALL be ordered list `[Auth, AuthMiddleware, DBPool, Database]` with edge info per hop

#### Scenario: No path exists
- **WHEN** two nodes are disconnected
- **THEN** result SHALL indicate "no path found"

#### Scenario: Endpoint not found is distinguished from no path
- **WHEN** `graphos path "nosuchnode" "Database"` is called
- **THEN** the output names `nosuchnode` as not found with suggestions and does not print "no path found"

### Requirement: Workflow 06 — explain a node
CLI `graphos explain <node>` SHALL load graph.json, resolve the argument through `resolveNodeArg` (exact id, then exact identifier, then case-insensitive identifier — see `identifier-resolution`), display full details (kind, signature, location), get all neighbors via `Domain.Graph.Core.neighbors`, get community membership (community ID, cohesion, bridge status), and display all edges with direction, relation, and confidence. A node that is not found SHALL be reported with suggestions; an ambiguous argument SHALL list candidates; the command SHALL never explain a node other than the one named. (PRD §13, workflow 06)

#### Scenario: Explain shows full node picture
- **WHEN** `graphos explain "RequestHandler"` is called
- **THEN** output SHALL include: node kind, signature, community ID, cohesion, is_bridge, degree, all edges with →/← direction and relation

#### Scenario: Explain never substitutes a node
- **WHEN** `graphos explain "executeSharedRunCommand"` is called and no node carries that id or identifier
- **THEN** the output is `Node not found: executeSharedRunCommand` with suggestions, verifiable by `cabal test`
