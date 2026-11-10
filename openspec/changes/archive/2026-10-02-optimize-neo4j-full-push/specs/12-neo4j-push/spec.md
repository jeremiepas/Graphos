# 12-neo4j-push Delta Specification

## MODIFIED Requirements

### Requirement: Workflow 12 — Neo4j push three modes with representative selection

Module `Graphos.Infrastructure.Export.Neo4j` SHALL export: `pushToNeo4j :: Neo4jConfig -> LabeledGraph -> CommunityMap -> CohesionMap -> Analysis -> IO ()`. Three modes: FullPush — all nodes/edges/community assignments, completing in minutes (< 5 min for a 100k-node graph against a localhost Neo4j), SubgraphPush — community representatives + bridges (≤7 per community via `--neo4j-subgraph-size N`, ~30s), CommunityPush — community-level nodes + inter-community edges (~5s). Auto-select: nodes < 10k → FullPush; ≥ 10k → SubgraphPush. Override: `--neo4j-push-mode full|subgraph|community`. Representative selection per community: centroid (highest degree) + top-N by degree + bridge nodes (articulation points) + entry points (file nodes). Every pushed node SHALL carry `id_hash = sha1(id)` (lowercase hex over UTF-8), and before the first data statement the push SHALL backfill `id_hash` on any pre-existing hashless `:Node` rows and create uniqueness constraints on `:Node(id_hash)` and `:Community(id)` using `IF NOT EXISTS`, so every subsequent `MERGE`/`MATCH` lookup is an indexed seek rather than a label scan. Raw `id` MUST NOT be the constraint key: ids can exceed Neo4j's RANGE-index key size limit (~8 KB), which leaves the index FAILED and every lookup unindexed. Cypher: parameterized `UNWIND $rows` statements (no string interpolation), one statement per entity kind per chunk, default chunk size 5,000 rows. Nodes SHALL be fully pushed before edges so edge endpoint lookups always resolve. Three entity types: Node, Community, BELONGS_TO. Streaming: when `--neo4j` during pipeline, push nodes during extraction (per-file batches also use UNWIND), edge repair after. CLI: `--neo4j`, `--neo4j-push <uri>`, `--neo4j-push-mode`, `--neo4j-subgraph-size N`. (PRD §9, workflow 12)

#### Scenario: SubgraphPush selects ≤7 representatives per community
- **WHEN** graph has 50k nodes and SubgraphPush runs
- **THEN** each community SHALL have at most 7 representatives: centroid + top-degree + bridges + entry points

#### Scenario: Parameterized Cypher prevents injection
- **WHEN** a node label contains special characters
- **THEN** Cypher SHALL use `{$param}` syntax, not string interpolation

#### Scenario: Constraints exist before data lands
- **WHEN** FullPush runs against an empty database
- **THEN** uniqueness constraints on `:Node(id_hash)` and `:Community(id)` SHALL be created before the first node row is written

#### Scenario: Oversized ids stay indexable
- **WHEN** the graph contains an `External` node whose id embeds a 50 KB source snippet
- **THEN** the push SHALL succeed and the node SHALL be findable through the `id_hash` index (the constraint key is the 40-character hash, never the raw id)

#### Scenario: FullPush of a large graph completes in minutes
- **WHEN** FullPush runs on a 100k-node / 120k-edge graph against a localhost Neo4j
- **THEN** the push SHALL complete in under 5 minutes

#### Scenario: Edges never dangle
- **WHEN** FullPush pushes nodes and edges
- **THEN** every edge row SHALL be written only after both endpoint nodes are present, and the edge count reported by Neo4j SHALL equal the graph's edge count
