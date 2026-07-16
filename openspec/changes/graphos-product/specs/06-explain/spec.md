## ADDED Requirements

### Requirement: Workflow 06 — explain a node with full connections
CLI `graphos explain <node>` SHALL: (1) load graph.json via `UseCase.Load`, (2) find node by label/id, (3) display full node details (kind, signature, source file, line range), (4) get all neighbors via `Domain.Graph.Core.neighbors` (forward + backward adjacency), (5) get community membership (community ID, cohesion score, bridge status), (6) display all edges with direction (→ outgoing / ← incoming), relation type, and confidence. Output format: node header + community info + edge list. Flag: `--graph PATH` (default `graphos-out/graph.json`). (PRD §13, workflow 06)

#### Scenario: Explain shows complete node neighborhood
- **WHEN** `graphos explain "RequestHandler"` is called on a node with 4 edges
- **THEN** output SHALL show: node kind, signature, community ID, cohesion, is_bridge, degree, and all 4 edges with direction and relation