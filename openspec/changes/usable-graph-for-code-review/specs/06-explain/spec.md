## MODIFIED Requirements

### Requirement: Workflow 06 — explain a node with full connections
CLI `graphos explain <node>` SHALL: (1) load graph.json via `UseCase.Load`, (2) resolve the argument through `Graphos.UseCase.Query.resolveNodeArg` (exact id, then exact identifier, then case-insensitive identifier — see `identifier-resolution`), (3) display full node details (kind, signature, source file, line range), (4) get all neighbors via `Domain.Graph.Core.neighbors` (forward + backward adjacency), (5) get community membership (community ID, cohesion score, bridge status), (6) display all edges with direction (→ outgoing / ← incoming), relation type, and confidence. A `NotFound` resolution SHALL print `Node not found: <arg>` with up to five suggestions and exit non-zero; an `Ambiguous` resolution SHALL print the candidates (id, kind, source file, line) and explain none of them; the command SHALL NOT use tokenized best-match resolution. Output format: node header + community info + edge list. Flag: `--graph PATH` (default `graphos-out/graph.json`). (PRD §13, workflow 06)

#### Scenario: Explain shows complete node neighborhood
- **WHEN** `graphos explain "RequestHandler"` is called on a node with 4 edges
- **THEN** output SHALL show: node kind, signature, community ID, cohesion, is_bridge, degree, and all 4 edges with direction and relation

#### Scenario: Explain by exact id
- **WHEN** `graphos explain "<dirHash>_db_Function:dbGetById"` is called with the id copied from `graph.json`
- **THEN** the output's `ID:` line equals the argument, verifiable by `cabal test`

#### Scenario: Explain refuses an unknown argument
- **WHEN** `graphos explain "dbGetByIdentifier"` is called and no node has that id or identifier
- **THEN** stdout is `Node not found: dbGetByIdentifier` followed by suggestions (for example `dbGetById`), the exit status is non-zero, and no `NODE:` block is printed
