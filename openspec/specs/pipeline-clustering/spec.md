# pipeline-clustering Specification

## Purpose
Define the `Pipeline.hs` clustering and analysis orchestration so that the enriched graph (post edge-inference) is re-clustered, analyzed, and joined to nodes before export. This fulfills the `node-schema` requirement that `nodeCommunityId` is populated and the `html-lod-viewer` requirement that `community_aggregates` is non-empty.
## Requirements
### Requirement: Re-cluster the enriched graph in Step 5

The pipeline SHALL re-cluster the enriched graph (post `inferEdges`) using the same `Resolution` as Step 4, producing `finalComm :: CommunityMap` and `finalCohes :: CohesionMap`. The pipeline SHALL NOT substitute empty maps for the re-cluster result. `analyzeGraph` SHALL run on the enriched graph with the final community and cohesion maps, producing the real `Analysis` (god nodes, articulation points, surprises, bridge classifications).

#### Scenario: Step 5 produces non-empty communities

- **WHEN** the pipeline runs on a graph with 78,529 nodes and 8,519 communities detected in Step 4
- **THEN** Step 5's `finalComm` has the same number of communities (within ±5% to account for inferred-edge topology change) and `analysisGodNodes anal` is non-empty

#### Scenario: Step 5 does not substitute empties

- **WHEN** the pipeline runs on any non-empty graph with clustering enabled (`--no-cluster` not set)
- **THEN** `finalComm` is not `Map.empty` and `anal` is not the empty `Analysis` record

### Requirement: Join communities to nodes before write

The pipeline SHALL call `joinCommunitiesToNodes` on the enriched graph with `finalComm` before `epWriteNodes`, so every node in a community has `nodeCommunityId = Just cid`. The joined graph (not the pre-join enriched graph) SHALL be passed to all downstream exports (HTML, Neo4j, Obsidian).

#### Scenario: Written nodes have community_id

- **WHEN** the pipeline writes `graph.json` for a graph with 78,529 community-assigned nodes
- **THEN** every node record has a non-null `community_id` field

#### Scenario: Downstream export sees joined graph

- **WHEN** Neo4j or Obsidian export reads the graph passed by the pipeline
- **THEN** node records carry `nodeCommunityId` matching the `CommunityMap` (not `Nothing`)

### Requirement: Compute community aggregates from real data

The pipeline SHALL call `computeCommunityAggregates` with the joined graph, `finalComm`, `finalCohes`, articulation points from `analysis`, and the optional LLM labels. The result SHALL be written via `epWriteCommunityAggregates`. The pipeline SHALL NOT write a hardcoded empty list as aggregates.

#### Scenario: Aggregates reflect the community map

- **WHEN** the pipeline runs on a graph producing 8,519 communities
- **THEN** `graph.json` `community_aggregates` has 8,519 entries, each with `member_count` equal to the community size and `cohesion` from the cohesion map

#### Scenario: Aggregates are non-empty when clustering is enabled

- **WHEN** the pipeline runs with `--no-cluster` unset on a non-empty graph
- **THEN** the `community_aggregates` key is present and a non-empty array

### Requirement: Pipeline write order

The incremental JSON write order SHALL be: `writeNodes → writeEdges → writeCommunities → writeCohesion → writeGodNodes → writeCommunityAggregates → writeAnalysisTail`. The community-join pass SHALL occur between re-clustering and `writeNodes`. This ordering is modified from the prior stubbed state where `writeCommunityAggregates` received a hardcoded `[]`.

#### Scenario: Write order is preserved

- **WHEN** the incremental writer serializes `graph.json`
- **THEN** the `community_aggregates` key appears after `god_nodes` and before the analysis tail, and `nodes[].community_id` is non-null for community members

