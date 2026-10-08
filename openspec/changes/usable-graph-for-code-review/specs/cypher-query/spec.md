## MODIFIED Requirements

### Requirement: Bounded results

The system SHALL bound `cypher` results by the shared query result budget, so a
matching query cannot emit the entire graph. The budget SHALL apply to rows that satisfy the
`WHERE` predicate: bindings are enumerated lazily, the predicate is evaluated on each binding,
and only matching bindings count against the budget, so a selective filter on a graph larger
than the budget still returns every match up to the budget (PRD §13).

#### Scenario: budget caps a broad match
- **WHEN** `MATCH (n) RETURN n` is run on a graph larger than the budget
- **THEN** the result is truncated to the budget
- **AND** the response indicates that results were truncated

#### Scenario: selective filter is not starved by the cap
- **WHEN** `MATCH (n) WHERE n.source_file = './src/workers/workflow-task-worker.ts' RETURN n.id` is run on a graph of 130,000 nodes whose matching nodes are enumerated after the first 2,000
- **THEN** every matching node is returned (up to the budget) and the response is not marked truncated, verifiable by `cabal test` with a synthetic graph larger than the budget

#### Scenario: equality on id is answered from the node map
- **WHEN** `MATCH (n) WHERE n.id = '<dirHash>_db_Function:dbGetById' RETURN n` is run
- **THEN** the row is returned without enumerating other nodes, verifiable by `cabal test` through the evaluator's candidate count

## ADDED Requirements

### Requirement: Unsupported named paths are rejected by name

A `MATCH` clause that binds a path variable (`MATCH p = (a)-[*1..3]->(b)`) SHALL be rejected
with an error that names the construct ("named path variables are not supported") and suggests
`graphos path <from> <to>` for shortest paths, instead of a generic parse error at the variable
(PRD §13).

#### Scenario: named path error is actionable
- **WHEN** `MATCH p = (a)-[*1..6]->(b) RETURN length(p)` is submitted
- **THEN** the error message contains `named path variables are not supported` and `graphos path`, verifiable by `cabal test` on the parser
