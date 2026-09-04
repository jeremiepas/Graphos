# cypher-query

Read-only openCypher / GQL (ISO/IEC 39075) subset query over the in-memory
property graph. Provides a `cypher` CLI subcommand and an MCP `cypher_query`
tool that evaluate `MATCH` / `WHERE` / `RETURN` patterns against the graph and
return projected rows, bounded by the shared query result budget.

## Purpose

Give agents and users a familiar, standards-based (openCypher/GQL) way to run
structural queries over the property graph — node and relationship patterns,
property filters, and variable-length paths — without a separate graph database,
while keeping results bounded and the graph read-only.

## Requirements

### Requirement: openCypher/GQL subset pattern matching

The system SHALL evaluate a read-only openCypher / GQL (ISO/IEC 39075) subset —
`MATCH` node and relationship patterns, `WHERE`, and `RETURN` — against the
in-memory property graph, returning the projected rows.

#### Scenario: relationship pattern with label and type
- **WHEN** the query `MATCH (a:Function)-[:Calls]->(b:Function) RETURN a, b` is run against a graph containing a `Calls` edge between two `Function` nodes
- **THEN** the result contains that pair of nodes

#### Scenario: variable-length path
- **WHEN** the query `MATCH (a)-[:Imports*1..3]->(b) RETURN b` is run
- **THEN** the result contains nodes reachable from `a` over 1 to 3 `Imports` edges

### Requirement: Property-graph mapping

The system SHALL map each node's `kind` to its Cypher label, each edge's
`relation` to its relationship type, and the remaining node/edge fields
(including `source_file`, `community`, `weight`, `confidence`) to queryable
properties.

#### Scenario: filter on a node property
- **WHEN** the query `MATCH (n:Function) WHERE n.source_file =~ 'src/services/.*' RETURN n` is run
- **THEN** only `Function` nodes whose `source_file` matches the regex are returned

#### Scenario: unknown property resolves to null
- **WHEN** a `WHERE` clause references a property no node/edge declares
- **THEN** that comparison evaluates as for a null value (the row is excluded), and no error is raised

### Requirement: Read-only subset enforcement

The system SHALL reject any query using a clause outside the supported read-only
subset with a clear error identifying the unsupported construct, and SHALL never
mutate the graph. Write clauses (`CREATE`, `MERGE`, `SET`, `REMOVE`, `DELETE`) are
now recognized grammar: outside a write surface the system SHALL reject them with
an error naming the clause and indicating that write statements require the
mutation surface (`graphos cypher --write`); the loaded graph SHALL be left
unchanged. On the write surface (see capability `cypher-mutation`) write clauses
evaluate.

#### Scenario: write clause rejected

- **WHEN** a query containing `CREATE`, `MERGE`, `SET`, `REMOVE`, or `DELETE` is submitted through a read-only surface (plain `graphos cypher`, MCP `cypher_query`)
- **THEN** the query is rejected with an error naming the clause and stating that write statements require the mutation surface
- **AND** the loaded graph is left unchanged

#### Scenario: out-of-subset construct reports position

- **WHEN** a query uses a construct outside the supported grammar (e.g. `WITH`, `UNWIND`, subqueries)
- **THEN** the error message indicates the unsupported construct and its position

#### Scenario: read behavior unchanged

- **WHEN** existing read-only queries (MATCH/WHERE/RETURN subset) are run after this change
- **THEN** results are byte-identical to the previous read-only engine's output

### Requirement: Bounded results

The system SHALL bound `cypher` results by the shared query result budget, so a
matching query cannot emit the entire graph.

#### Scenario: budget caps a broad match
- **WHEN** `MATCH (n) RETURN n` is run on a graph larger than the budget
- **THEN** the result is truncated to the budget
- **AND** the response indicates that results were truncated
