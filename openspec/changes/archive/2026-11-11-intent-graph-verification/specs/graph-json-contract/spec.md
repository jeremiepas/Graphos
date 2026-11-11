# graph-json-contract (delta)

The contract gains the two intent relations and an optional node body hash. Additive:
existing relations, shapes and consumers are untouched.

## ADDED Requirements

### Requirement: Intent relation vocabulary

The edge relation vocabulary SHALL include `tests` (a test exercises its target) and
`maps` (parallel-structure correspondence, e.g. schema field to contract field)
alongside the existing thirteen relations, with the same lowercase text encoding in
JSON, and graph.json documents containing them SHALL load in every graphos consumer.
Consumers built before this change SHALL tolerate the new relations per the
tolerant-decode default (`--strict-graph` reserved for explicit strictness).

- **Plan**: `covered` needs `tests`; `parallel` needs `maps`; the tolerant/strict split
  already exists in the loaders.
- **Do**: Extend `Relation` + `relationToText`/`textToRelation`; round-trip Hspec cases.
- **Check**: Scenario below.
- **Act**: Keep `renderCommandReference` as the single external registry of relations.

#### Scenario: New relations load everywhere

- **WHEN** a graph.json containing `tests` and `maps` edges is queried via
  `graphos cypher` and loaded by the studio
- **THEN** both consumers read the edges with their relation intact and no strict-mode
  failure occurs in tolerant mode

### Requirement: Optional node body hash

The node shape SHALL admit an optional `body_hash` string — the SHA256 of the symbol's
source span. A missing `body_hash` SHALL load as absent, documents written before this
change SHALL load unchanged, and identical span bytes SHALL yield the identical hash
across runs.

- **Plan**: The "body changed, signature stable" discharge rule needs content identity
  at node granularity; the file-level SHA256 cache machinery already exists.
- **Do**: Optional field on `Node` + JSON round-trip; hashing reuses the FileSystem
  cache implementation.
- **Check**: Scenarios below.
- **Act**: If other consumers want span-level dedup, promote the field to the docs of
  the contract rather than inventing a second hash.

#### Scenario: Legacy graphs load without the field

- **WHEN** a graph.json produced before this change is loaded
- **THEN** the load succeeds and every node's body hash is absent

#### Scenario: Hash is deterministic

- **WHEN** two exports run over an unchanged file
- **THEN** every symbol node carries the same `body_hash` in both
