## ADDED Requirements

### Requirement: openCypher write clause subset

The system SHALL evaluate an openCypher write clause subset — `CREATE`,
`MERGE` (with `ON CREATE SET` / `ON MATCH SET`), `SET`, `REMOVE`,
`DELETE` / `DETACH DELETE`, and an optional trailing `RETURN` — optionally
combined with `MATCH` / `WHERE` to select bindings to mutate.

#### Scenario: create nodes and relationship

- **WHEN** the statement `CREATE (n:Module {id: 'm1', label: 'Auth'}), (a:Function {id: 'f1'})-[:Calls]->(n)` is evaluated against a graph without `m1`/`f1`
- **THEN** the graph contains the two new nodes and the `Calls` edge `f1 -> m1`
- **AND** the summary reports `nodes_created = 2` and `rels_created = 1`

#### Scenario: match then set property

- **WHEN** the statement `MATCH (n:Function) WHERE n.source_file =~ 'src/old/.*' SET n.source_file = 'src/new/main.hs'` is evaluated against a graph with three matching functions
- **THEN** all three nodes' `source_file` model fields are updated and the summary reports `properties_set = 3`

#### Scenario: merge upserts on id

- **WHEN** the statement `MERGE (m:Module {id: 'existing'}) ON MATCH SET m.label = 'Touched'` is evaluated against a graph containing module `existing`
- **THEN** no new node is created, the node's label becomes `Touched`, and the summary reports `nodes_created = 0` and `properties_set = 1`

#### Scenario: delete requires detach when relationships remain

- **WHEN** the statement `MATCH (n:Function)-[r]->() DELETE n` is evaluated against a node that still has relationships
- **THEN** evaluation fails with an error naming the node id and the requirement of `DETACH DELETE`

#### Scenario: detach delete removes incident edges

- **WHEN** the statement `MATCH (n:Module) DETACH DELETE n` is evaluated
- **THEN** the matched modules and all their incident edges are removed and the summary reports the deleted counts

#### Scenario: return after mutation

- **WHEN** the statement `MATCH (n:Function) SET n.community = 7 RETURN n.id, n.community` is evaluated
- **THEN** the result rows contain one row per matched node with the updated property values

### Requirement: Write statements are rejected in read-only context

The system SHALL evaluate mutation statements only when the write surface is
explicitly selected (`graphos cypher --write`, the `cypher_mutate` MCP tool, or
the `POST /api/cypher/mutate` HTTP route). In any read-only context, a mutation
statement SHALL be rejected with an error naming the clause and the required
write surface, and the graph SHALL be left unchanged.

#### Scenario: plain CLI cypher rejects mutation

- **WHEN** `graphos cypher "CREATE (n)"` is run without `--write`
- **THEN** the command fails with an error naming `CREATE` and indicating that write statements require `--write`
- **AND** the loaded graph is left unchanged

#### Scenario: read query still evaluates on the write surface

- **WHEN** the statement `MATCH (n:Function) RETURN count(*)` is evaluated on the mutation surface
- **THEN** it evaluates normally as a read query and reports an empty (all-zero) summary

### Requirement: Model reconciliation for mutations

The system SHALL reconcile write statements with the property-graph model:
relationship types SHALL be restricted to the supported relationship vocabulary,
nodes SHALL keep a single primary label with additional labels stored as extra
labels, non-model properties SHALL be stored in the node/edge extension object,
and a `CREATE` relationship between a source/target pair that already has an
edge SHALL upsert the existing edge's properties.

#### Scenario: unknown relationship type is a parse error

- **WHEN** the statement `CREATE (a)-[:Collabs]->(b)` is evaluated
- **THEN** the statement is rejected with an error naming `Collabs` and the supported relationship types

#### Scenario: set label adds an extra label

- **WHEN** the statement `MATCH (n:Module) SET n:Deprecated` is evaluated
- **THEN** `Deprecated` appears in the node's extra labels, the primary kind is unchanged, and a subsequent `MATCH (n:Deprecated)` matches the node

#### Scenario: remove primary label leaves an unlabeled node

- **WHEN** the statement `MATCH (n:Module {id: 'm1'}) REMOVE n:Module` is evaluated
- **THEN** the node's primary label is cleared, `m1` remains present, and `MATCH (n) WHERE n.id = 'm1'` still matches

#### Scenario: non-model property stored in extension object

- **WHEN** the statement `MATCH (n:Function {id: 'f1'}) SET n.review_status = 'approved'` is evaluated
- **THEN** `review_status` is stored on the node's extension object and `MATCH (n) WHERE n.review_status = 'approved'` matches `f1`

#### Scenario: create over existing pair upserts

- **WHEN** the statement `CREATE (a)-[:Calls {weight: 2}]->(b)` is evaluated against a graph that already has a `Calls` edge between `a` and `b`
- **THEN** no parallel edge is created, the existing edge's `weight` becomes `2`, and the summary reports `rels_upserted = 1`

#### Scenario: duplicate node id is an error

- **WHEN** the statement `CREATE (n:Module {id: 'existing'})` is evaluated against a graph that already contains a node with id `existing`
- **THEN** evaluation fails with an error naming the colliding id and no mutation is applied

### Requirement: Mutation summary contract

The system SHALL report every mutation as a summary containing `nodes_created`,
`rels_created`, `rels_upserted`, `properties_set`, `properties_removed`,
`nodes_deleted`, and `rels_deleted` counts, alongside any `RETURN` rows, using
the existing bounded result budget, and the JSON renderer SHALL emit
`{"summary": {...}, "rows": [...], "columns": [...], "truncated": bool}`.

#### Scenario: summary counts reflect applied operations

- **WHEN** the statement `MATCH (n:Deprecated) DETACH DELETE n` deletes two nodes with three edges between them
- **THEN** the summary reports `nodes_deleted = 2` and `rels_deleted = 3`

#### Scenario: summary rendered in text output

- **WHEN** a mutation statement is run through the CLI without `--json`
- **THEN** the text output includes a summary line listing the non-zero counts

### Requirement: Mutation persistence opt-in

The system SHALL apply mutations in memory only by default, and SHALL persist the
mutated graph back to the loaded `graph.json` only when persistence is explicitly
requested (`--write` on the CLI, `persist: true` on MCP/HTTP). Persistence SHALL
copy the previous file to a timestamped backup before writing, SHALL preserve the
versioned `graph.json` schema, and SHALL surface in the summary that re-extraction
overwrites mutations.

#### Scenario: in-memory only by default

- **WHEN** a mutation is evaluated without the persistence opt-in
- **THEN** the loaded graph reflects the mutation for the session but `graph.json` on disk is unchanged

#### Scenario: persist writes with backup

- **WHEN** `graphos cypher "MERGE (n:Module {id: 'm9'})" --write` is run against a graph loaded from `graph.json`
- **THEN** a `graph.json.bak-<timestamp>` copy of the original file exists, `graph.json` contains the mutated graph including node `m9`, and the output states the backup path

#### Scenario: persisted graph reloads with mutations

- **WHEN** a graph persisted with mutations is loaded again by the query family
- **THEN** the mutated nodes, labels, and properties are visible to subsequent queries