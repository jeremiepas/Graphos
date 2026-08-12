## ADDED Requirements

### Requirement: Workflow 05 — shortest path between two nodes
CLI `graphos path <from> <to>` SHALL: (1) load graph.json via `UseCase.Load`, (2) match source and target nodes by label/id via `Domain.Graph.Index` (O(k×log N)), (3) compute shortest path via `Domain.Graph.Query.shortestPath` (BFS via FGL `esp`), (4) return ordered list with node labels, edge relations, confidence per hop. For directed graphs (`--directed`), paths follow edge direction. Disconnected nodes return "no path found". Flag: `--graph PATH` (default `graphos-out/graph.json`). (PRD §13, workflow 05)

#### Scenario: Shortest path between connected nodes
- **WHEN** `graphos path "AuthModule" "Database"` is called on a connected graph
- **THEN** result SHALL be ordered list like `[Auth, AuthMiddleware, DBPool, Database]` with relation and confidence per hop

#### Scenario: No path exists
- **WHEN** two nodes are disconnected
- **THEN** result SHALL indicate "no path found"