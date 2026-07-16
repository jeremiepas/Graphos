## ADDED Requirements

### Requirement: Domain.Community — Leiden algorithm three phases
Module `Graphos.Domain.Community` SHALL implement `detectCommunities :: LabeledGraph -> CommunityMap` and `detectCommunitiesWithResolution :: LabeledGraph -> Resolution -> CommunityMap`. Phase 1 (Local Moving): iterate over all nodes, compute modularity gain for moving each node to a neighboring community, move if gain > 0, repeat until stable. Phase 2 (Refinement): for each community, check internal subsets, only merge subsets if community cohesion > 0.5. Phase 3 (Aggregation): merge communities into supernodes, build coarser graph, return to Phase 1. Loop until assignment stabilizes or `resMaxIterations` reached. (PRD §5.1)

#### Scenario: Leiden terminates within max iterations
- **WHEN** `detectCommunitiesWithResolution` is called with `resMaxIterations = 10`
- **THEN** the algorithm SHALL complete in at most 10 full iteration cycles

#### Scenario: Refinement prevents poorly-connected communities
- **WHEN** a community has cohesion < 0.5 after local moving
- **THEN** the refinement phase SHALL split it into subsets before aggregation

### Requirement: Domain.Community — Resolution data type and defaults
`data Resolution = Resolution { resGamma :: !Double, resMinSize :: !Int, resMergeInto :: !MergeStrategy, resMaxIterations :: !Int }`. `defaultResolution = Resolution { resGamma = 1.0, resMinSize = 3, resMergeInto = MergeToNeighbor, resMaxIterations = 50 }`. Auto-tuning by graph size: `<1k` → gamma=1.0/minSize=3/maxIter=50; `1k–10k` → 0.8/5/30; `10k–100k` → 0.5/10/20; `100k+` → 0.3–0.5/10–20/10–20. (PRD §5.2, §5.3)

#### Scenario: Auto-tune resolution for 50k-node graph
- **WHEN** `detectCommunities` is called on a graph with 50,000 nodes without explicit resolution
- **THEN** the system SHALL apply `resGamma = 0.5, resMinSize = 10, resMaxIterations = 20`

#### Scenario: CLI override of resolution
- **WHEN** user passes `--resolution 0.8 --min-comm-size 5`
- **THEN** the provided values SHALL override auto-tuning

### Requirement: Domain.Community — mergeSmallCommunities
`mergeSmallCommunities :: LabeledGraph -> CommunityMap -> Int -> MergeStrategy -> CommunityMap`. `data MergeStrategy = MergeToNeighbor`. `MergeToNeighbor` SHALL select the neighboring community with the most shared edges (highest inter-community edge count). Communities below `resMinSize` SHALL be merged. (PRD §5.2)

#### Scenario: Merge 2-node community into best neighbor
- **WHEN** community C1 has 2 nodes, community C2 shares 5 edges with C1, community C3 shares 2 edges with C1, and `resMinSize = 3`
- **THEN** C1 SHALL be merged into C2 (most shared edges)

### Requirement: Domain.Community — cohesionScore and scoreAllCohesion
`cohesionScore :: LabeledGraph -> CommunityMap -> CommunityId -> Double`. Formula: `cohesion(c) = Σ_{i∈c} (neighbors_in_c(i) / total_neighbors(i)) / |c|`. Range [0,1]. `scoreAllCohesion :: LabeledGraph -> CommunityMap -> CohesionMap` SHALL compute cohesion for all communities. Cohesion SHALL be used for representative node selection (highest cohesion nodes = representatives), quality filtering (flag < 0.3), and SubgraphPush mode. (PRD §5.4)

#### Scenario: Highly cohesive community scores near 1.0
- **WHEN** a community has 10 nodes where each node's neighbors are all within the community
- **THEN** `cohesionScore` SHALL return 1.0

#### Scenario: Loosely connected community scores near 0.0
- **WHEN** a community has 10 nodes where each node has only 1 neighbor inside and 9 outside
- **THEN** `cohesionScore` SHALL return approximately 0.1

### Requirement: Community representatives for SubgraphPush
Each community SHALL select representative nodes by: (1) centroid = node with highest degree, (2) top-N by degree, (3) bridge nodes (articulation points within the community). Default: 7 representatives, configurable via `--neo4j-subgraph-size N`. (PRD §9.2)

#### Scenario: Select 7 representatives for a community
- **WHEN** SubgraphPush runs with default `--neo4j-subgraph-size 7`
- **THEN** each community SHALL select up to 7 nodes ranked by: centroid (1) + top-degree (up to N-1-bridges) + bridge nodes