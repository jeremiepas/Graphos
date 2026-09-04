## MODIFIED Requirements

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