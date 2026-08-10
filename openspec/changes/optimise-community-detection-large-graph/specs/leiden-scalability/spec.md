# Leiden Scalability Capability

## Purpose

Define the performance and data-structure requirements for the Leiden community detection core in `Domain/Community.hs`. The algorithm semantics (Leiden phases, modularity objective, merge strategy, resolution parameter) are unchanged; these requirements govern the *cost model* of the implementation so that 100K-node graphs cluster within the PRD §16.1 target (under 30 seconds).

## MODIFIED Requirements

### Requirement: Constant-time node moves in local moving

The Leiden local moving pass (PRD §5) MUST update community assignments in O(1) per node move, with at most O(n) amortized vector copying per pass (thaw/freeze). It SHALL NOT copy the full assignment vector per individual move.

#### Scenario: Clustering results are unchanged by the optimization

- **WHEN** the deterministic reference graphs from the community test suite are clustered before and after the change
- **THEN** the resulting community maps are identical, verifiable by `cabal test`

#### Scenario: Repository-scale clustering does not regress

- **WHEN** the pipeline runs on the Graphos repository with debug tracing enabled
- **THEN** `span_cluster` duration is at most the pre-change baseline for the same input

#### Scenario: Large graphs cluster within target order of magnitude

- **WHEN** a sparse synthetic graph of at least 50,000 nodes is clustered
- **THEN** clustering completes in seconds (not minutes), consistent with the PRD §16.1 target of under 30 seconds at 100k nodes

### Requirement: Batched refinement updates

The refinement phase MUST apply node reassignments without per-node full-vector copies; updates SHALL be applied in bulk (single copy per pass) or via in-place mutation.

#### Scenario: Refinement preserves assignments semantics

- **WHEN** a graph whose refinement splits a weakly-connected community is clustered
- **THEN** the split result equals the pre-change implementation's output, verifiable by `cabal test`

### Requirement: Incremental merge-phase index

Small-community merging MUST build the node-to-community reverse index once and update it incrementally per merge, rather than rebuilding it for every merged community. Lookups against the index MUST reflect all previously executed merges.

#### Scenario: Merge targets match a recomputed index

- **WHEN** a graph containing several communities below the minimum size is merged
- **THEN** each small community merges into the same target as would be chosen with a freshly recomputed index at each step

### Requirement: Fully forcing LeidenState NFData

The `NFData` instance for the Leiden working state MUST force all fields to normal form so that `deepseq` between iterations prevents thunk accumulation.

#### Scenario: Deep evaluation completes over the whole state

- **WHEN** `rnf` is applied to a constructed Leiden state with non-trivial neighbor and assignment data
- **THEN** evaluation touches all fields and completes without leaving unevaluated structure, verifiable by `cabal test`

## ADDED Requirements

### Requirement: O(N) community member grouping

Community-to-member grouping (used in `leidenStateToCommunityMap` and the refinement phase) MUST build member lists with O(1) per insertion, not O(|list|). It SHALL use `fromListWith (:)` (prepend) or an equivalent O(N) total strategy, not `fromListWith (++)` (append). Member-list order is not semantically significant and MAY differ from the legacy append order.

#### Scenario: Large community grouping is linear

- **WHEN** a graph with a single 2,000-member community (e.g. a god-node neighborhood) is clustered
- **THEN** the `leidenStateToCommunityMap` step completes in time proportional to 2,000 (not 2,000²), verifiable by a microbenchmark or a non-regression check against a 10K-node synthetic graph

#### Scenario: Grouping output is semantically equivalent

- **WHEN** a fixture graph is clustered with the old `(++)` grouping and the new `(:)` grouping
- **THEN** the set of community IDs and the set of members per community are identical (order within a community MAY differ)

### Requirement: One-pass modularity-gain scoring

The local-moving pass MUST compute the edges-to-community counts for a node's neighbors in a single pass over the neighbor vector, producing a `Map CommunityId Int` (or equivalent). It SHALL NOT re-scan the neighbor vector once per candidate community (`VU.filter (== c)` per candidate) and SHALL NOT re-scan for `edgesToOld`/`edgesToNew` after a move. The modularity-gain decision and the `sigmaTot` delta MUST both read from the single precomputed count map.

#### Scenario: Hub-node scoring is linear in degree

- **WHEN** a node with 200 neighbors across 15 distinct communities is scored in the local-moving pass
- **THEN** the scoring work is O(200) (one fold), not O(200 × 15) (scan per candidate), verifiable by a microbenchmark on a synthetic star graph

#### Scenario: Scoring picks the same best community

- **WHEN** a fixture graph is clustered with the old multi-scan scoring and the new one-pass scoring
- **THEN** the chosen `bestComm` for each node is identical on every pass, and the final `CommunityMap` is identical (verifiable by `cabal test` on deterministic fixtures)

#### Scenario: sigmaTot delta is correct after a move

- **WHEN** a node moves from community A to community B with 3 edges to A and 5 edges to B
- **THEN** `sigmaTot[A]` decreases by `ki - 3` and `sigmaTot[B]` increases by `ki - 5`, reading the edge counts from the single precomputed map (not from a re-scan)

### Requirement: CSR adjacency representation

The Leiden working state MUST store neighbor adjacency in CSR (compressed sparse row) form: a single contiguous `VU.Vector Int` of all neighbor indices, plus a `VU.Vector Int` of `N+1` offsets where slice `[offset[i], offset[i+1])` is node `i`'s neighbors. The vector-of-vectors representation (`V.Vector (VU.Vector Int)`) SHALL NOT be used. The CSR MUST be built once in `buildLeidenState` and read via `VU.slice` in the local-moving and refinement passes.

#### Scenario: Neighbor access is contiguous

- **WHEN** the local-moving pass reads neighbors for node `i`
- **THEN** the neighbors occupy a contiguous slice of the adjacency vector, accessed via `VU.slice (offset VU.! i) len adj` (no pointer indirection per node)

#### Scenario: CSR build preserves neighbor sets

- **WHEN** a graph is loaded into `LeidenState`
- **THEN** for every node `i`, the set of indices in its CSR slice equals the set of indices in the legacy `lsNeighbors V.! i` vector (dangling-edge self-loop fallbacks preserved)

#### Scenario: Clustering output is unchanged by the representation swap

- **WHEN** a deterministic fixture is clustered with the vector-of-vectors and the CSR representations
- **THEN** the final `CommunityMap` is identical, verifiable by `cabal test`

### Requirement: Cohesion scoring without per-node neighbor allocation

`scoreAllCohesion` MUST NOT allocate a fresh `Set` per node via `neighbors g nid`. It SHALL read `gAdjFwd`/`gAdjBack` directly, or derive internal-edge counts from the inter-edge count map that `computeCommunityAggregates` already builds (internal edges = edges touching the community − inter-community edges). The O(N) community member set lookup is acceptable; the per-node `Set` allocation is not.

#### Scenario: Cohesion scoring does not allocate per-node Sets

- **WHEN** `scoreAllCohesion` runs on a 78K-node graph
- **THEN** the number of `Set` allocations during the call is O(C) (one member set per community) or zero, not O(sum of degrees) (~470K), verifiable by a heap profile or a non-regression check

#### Scenario: Cohesion values are unchanged

- **WHEN** a fixture is scored with the old per-node-`neighbors` and the new direct-read approaches
- **THEN** the `CohesionMap` is identical, verifiable by `cabal test`