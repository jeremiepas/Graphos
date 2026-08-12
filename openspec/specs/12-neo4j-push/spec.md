# 12-neo4j-push Specification

## Purpose
TBD - created by archiving change graphos-product. Update Purpose after archive.
## Requirements
### Requirement: Workflow 12 — Neo4j push three modes with representative selection
Module `Graphos.Infrastructure.Export.Neo4j` SHALL export: `pushToNeo4j :: Neo4jConfig -> LabeledGraph -> CommunityMap -> CohesionMap -> Analysis -> IO ()`. Three modes: FullPush — all nodes/edges/community assignments (~990k stmts, 2-4h for 100k graph), SubgraphPush — community representatives + bridges (≤7 per community via `--neo4j-subgraph-size N`, ~64k stmts, ~30s), CommunityPush — community-level nodes + inter-community edges (~8k stmts, ~5s). Auto-select: nodes < 10k → FullPush; ≥ 10k → SubgraphPush. Override: `--neo4j-push-mode full|subgraph|community`. Representative selection per community: centroid (highest degree) + top-N by degree + bridge nodes (articulation points) + entry points (file nodes). Cypher: parameterized statements (no string interpolation), batch ≤50. Three entity types: Node, Community, BELONGS_TO. Streaming: when `--neo4j` during pipeline, push nodes during extraction, edge repair after. CLI: `--neo4j`, `--neo4j-push <uri>`, `--neo4j-push-mode`, `--neo4j-subgraph-size N`. (PRD §9, workflow 12)

#### Scenario: SubgraphPush selects ≤7 representatives per community
- **WHEN** graph has 50k nodes and SubgraphPush runs
- **THEN** each community SHALL have at most 7 representatives: centroid + top-degree + bridges + entry points

#### Scenario: Parameterized Cypher prevents injection
- **WHEN** a node label contains special characters
- **THEN** Cypher SHALL use `{$param}` syntax, not string interpolation

