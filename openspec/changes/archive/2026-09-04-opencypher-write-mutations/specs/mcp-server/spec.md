## ADDED Requirements

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