## MODIFIED Requirements

### Requirement: Re-cluster the enriched graph in Step 5

The pipeline SHALL re-cluster the enriched graph (post `inferEdges`) using the
same `Resolution` as Step 4, producing `finalComm :: CommunityMap` and
`finalCohes :: CohesionMap`. The pipeline SHALL NOT substitute empty maps for
the re-cluster result. When the clustering algorithm is Bunch, the Step-5
`finalComm` SHALL be the Step-4 Bunch partition, joined to the enriched graph
without a second search: the Bunch MDG excludes inferred relations, so it is
identical in both steps. `finalCohes` SHALL be recomputed on the enriched
graph. `analyzeGraph` SHALL run on the enriched graph with the final community
and cohesion maps, producing the real `Analysis` (god nodes, articulation
points, surprises, bridge classifications).

#### Scenario: Step 5 produces non-empty communities
- **WHEN** the pipeline runs on a graph with 78,529 nodes and 8,519 communities detected in Step 4
- **THEN** Step 5's `finalComm` has the same number of communities (within ±5% to account for inferred-edge topology change) and `analysisGodNodes anal` is non-empty

#### Scenario: Step 5 does not substitute empties
- **WHEN** the pipeline runs on any non-empty graph with clustering enabled (`--no-cluster` not set)
- **THEN** `finalComm` is not `Map.empty` and `anal` is not the empty `Analysis` record

#### Scenario: Bunch is not re-searched on inferred edges
- **WHEN** a build runs with `--cluster-algorithm bunch` and edge inference adds `depends_on` edges
- **THEN** the exported partition equals the Step-4 Bunch partition and the Bunch search runs once
