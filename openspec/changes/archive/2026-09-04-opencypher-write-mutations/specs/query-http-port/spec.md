## ADDED Requirements

### Requirement: Cypher mutation HTTP endpoint

The `graphos serve` HTTP server SHALL expose `POST /api/cypher/mutate` with a JSON
body `{"query": <string>, "persist": <bool, default false>}` that evaluates an
openCypher statement against the single in-memory graph loaded at startup and
returns the mutation summary JSON contract
(`{"summary": {...}, "rows": [...], "columns": [...], "truncated": bool}`, see
capability `cypher-mutation`). A read-only statement submitted to this route SHALL
evaluate as a normal read with an all-zero summary. When `persist` is `true`, the
server SHALL write the mutated graph back to the loaded `graph.json` path with a
timestamped backup, following capability `cypher-mutation`'s persistence rules.

#### Scenario: mutate over HTTP

- **WHEN** `POST /api/cypher/mutate` is called with body `{"query": "MERGE (n:Module {id: 'm9'})"}` against a graph without `m9`
- **THEN** the response body parses as JSON with `summary.nodes_created = 1` and the in-memory graph contains `m9` for subsequent `/api/*` reads

#### Scenario: persist flag writes graph.json with backup

- **WHEN** `POST /api/cypher/mutate` is called with body `{"query": "MERGE (n:Module {id: 'm9'})", "persist": true}`
- **THEN** a `graph.json.bak-<timestamp>` copy of the original exists and `graph.json` contains `m9`

#### Scenario: parse error returns 400 with message

- **WHEN** the route receives a body whose `query` does not parse
- **THEN** the response has HTTP status 400 with a JSON body containing the parse error message

#### Scenario: reads served from the mutated in-memory graph

- **WHEN** after a successful mutation `GET /api/neighbors?id=m9` is called
- **THEN** the response reflects the post-mutation in-memory graph state