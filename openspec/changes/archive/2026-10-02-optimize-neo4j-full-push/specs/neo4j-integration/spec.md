# neo4j-integration Delta Specification

## MODIFIED Requirements

### Requirement: Infrastructure.Export.Neo4j — three push modes

Module `Graphos.Infrastructure.Export.Neo4j` SHALL export: `pushToNeo4j :: Neo4jConfig -> LabeledGraph -> CommunityMap -> CohesionMap -> Analysis -> PushMode -> Int -> IO ()`. Three modes: (1) `FullPush` — push all nodes, edges, community assignments as parameterized UNWIND batches, completing in minutes (< 5 min for a 100k-node graph against a localhost Neo4j). (2) `SubgraphPush` — push community representatives + bridge nodes only. Default 7 representatives per community via `--neo4j-subgraph-size N`. (~30s). (3) `CommunityPush` — push community-level nodes and inter-community edges only. (~5s). Auto-selection: nodes < 10k → FullPush; ≥ 10k → SubgraphPush. Override: `--neo4j-push-mode full|subgraph|community`. (PRD §9.1)

#### Scenario: Auto-select FullPush for small graph
- **WHEN** `pushToNeo4j` is called on a graph with 5000 nodes without explicit mode
- **THEN** the function SHALL use FullPush and generate Cypher for all 5000 nodes + edges

#### Scenario: Auto-select SubgraphPush for large graph
- **WHEN** `pushToNeo4j` is called on a graph with 50,000 nodes without explicit mode
- **THEN** the function SHALL use SubgraphPush with ≤7 representatives per community

#### Scenario: Override to CommunityPush
- **WHEN** `--neo4j-push-mode community` is set
- **THEN** `pushToNeo4j` SHALL generate only community-level nodes and inter-community edges regardless of graph size

### Requirement: Infrastructure.Export.Neo4j — Cypher statement generation

Cypher SHALL use parameterized statements (values passed as JSON parameters, never embedded in strings). Every node row SHALL carry `id_hash = sha1(id)` (lowercase hex over UTF-8 bytes), and all node lookups (node MERGE, edge endpoint MATCH) SHALL key on `id_hash`, never on raw `id` — ids may exceed the RANGE-index key limit (~8 KB), so an index or constraint on raw `id` fails to populate and silently degrades to label scans. Before any data statement, the push SHALL backfill `id_hash` on pre-existing hashless `:Node` rows (batched client-side hashing — vanilla Neo4j has no sha1 function and APOC is not a dependency) and issue `CREATE CONSTRAINT ... IF NOT EXISTS FOR (n:Node) REQUIRE n.id_hash IS UNIQUE` plus the analogous constraint for `:Community(id)`. Data SHALL be sent as `UNWIND $rows AS row` statements — one statement per entity kind per chunk (nodes; edges grouped by relationship type; community assignments), with node chunks capped at 1,000 rows or ~4 MB payload and edge chunks at 5,000 rows. Nodes SHALL be fully written before edges. Transport SHALL be an in-process persistent HTTP connection to the transactional endpoint, reused across the entire push — no per-batch subprocess and no temp-file payloads. If constraint creation fails (e.g. pre-existing duplicate ids), the push SHALL abort with a clear error naming the constraint rather than continuing unindexed. Three entity types: `Node` (code/doc concepts), `Community` (with label + cohesion), `BELONGS_TO` (Node → Community edge). (PRD §9.1)

#### Scenario: Parameterized Cypher avoids injection
- **WHEN** a node label contains special characters like quotes or backslashes
- **THEN** the Cypher SHALL use parameterized `{$param}` syntax, not string interpolation

#### Scenario: UNWIND chunking respected
- **WHEN** pushing 2,600 nodes with default caps (node chunks: 1,000 rows or ~4 MB payload; edge chunks: 5,000 rows)
- **THEN** the nodes SHALL be sent as 3 UNWIND statements (1,000 + 1,000 + 600 rows), not 2,600 individual statements

#### Scenario: Batch size respected
- **WHEN** pushing 10,000 nodes
- **THEN** the node rows SHALL be sent as UNWIND statements of at most 1,000 rows or ~4 MB payload per request (10 batches), not 10,000 individual statements

#### Scenario: Duplicate ids abort with a clear error
- **WHEN** the target database already contains two `:Node` rows with the same `id_hash` and FullPush starts
- **THEN** the push SHALL fail during constraint creation with an error that names the violated constraint, and no data statements SHALL be sent

#### Scenario: Hash parity between pusher and backfill
- **WHEN** a node id is hashed by the pusher and again by the backfill pass on a later push
- **THEN** both SHALL produce the same lowercase hex sha1 over the id's UTF-8 bytes, so re-pushing an already-pushed graph creates no new nodes

#### Scenario: No curl subprocess
- **WHEN** a push runs on a machine without a `curl` binary on PATH
- **THEN** the push SHALL still complete, using the in-process HTTP client
