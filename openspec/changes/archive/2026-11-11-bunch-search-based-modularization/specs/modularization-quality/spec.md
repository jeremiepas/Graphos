## ADDED Requirements

### Requirement: Cluster factor and modularization quality

The system SHALL compute, for a graph and a `CommunityMap`, the modularization
quality of Mitchell & Mancoridis (2008) on the undirected projection of the
graph: for each cluster i with intra-edge weight `µ_i` and external weight
`ext_i = Σ_{j≠i}(ε_ij + ε_ji)`, the cluster factor is `CF_i = 0` when
`µ_i = 0` and `CF_i = 2µ_i / (2µ_i + ext_i)` otherwise, and
`MQ = Σ_i CF_i`. Edge weights SHALL default to 1 and SHALL use `edgeWeight`
when weighting is requested. Clusters SHALL be summed in ascending cluster id
so the result is a deterministic function of its inputs. (PRD §5.4 via
community-detection)

#### Scenario: paper fixture
- **WHEN** an 8-module MDG partitioned into 3 subsystems, in which subsystem 2 has 1 intra-edge and 3 inter-edges, is scored
- **THEN** subsystem 2 has `CF = 2·1 / (2·1 + 3) = 0.4` (the paper's Fig. 2 example)

#### Scenario: closed cluster
- **WHEN** a cluster has at least one intra-edge and no edge to any other cluster
- **THEN** its cluster factor is exactly 1

#### Scenario: ranges
- **WHEN** MQ is computed for any graph and any partition into k clusters
- **THEN** every `CF_i` lies in [0, 1] and MQ lies in [0, k]

#### Scenario: weights
- **WHEN** every edge weight is doubled and weighting is requested
- **THEN** MQ is unchanged

### Requirement: Intra-edge fraction

The system SHALL compute the intra-edge fraction of a partition as the total
weight of edges whose endpoints share a cluster divided by the total edge
weight, and SHALL report it together with MQ and the cluster count.

#### Scenario: all edges internal
- **WHEN** every edge has both endpoints in the same cluster
- **THEN** the intra-edge fraction is 1

#### Scenario: singleton partition
- **WHEN** every node is its own cluster and the graph has at least one edge
- **THEN** the intra-edge fraction is 0 and MQ is 0

### Requirement: Lean arm of modularization quality

The formalization companion `formalization/lean` SHALL contain a module
`GraphosFormalization.Bunch.MQ`, built by `lean.yml`, that defines the
cluster factor and MQ as exact fractions (no `Rat`, no `Mathlib`) and proves:
`0 ≤ CF_i ≤ 1` (`cf_le_one`, `cfDen_pos`), `0 ≤ MQ ≤ k` (`mq_le_k`,
`mqDen_pos`), closed cluster ⇒ `CF = 1` (`cf_closed`), the paper's Fig. 2
value `2/5` (`cf_paper_fixture`), intra-edge weight ≤ total weight
(`intra_le_total`), no intra-edge ⇒ `MQ = 0` (`mqNum_eq_zero_of_noIntra`),
and that moving one node leaves `µ`, `ext` and `CF` of every cluster other
than its source and destination unchanged (`mu_moveTo`, `ext_moveTo`,
`cf_moveTo`). The module SHALL compile with zero errors and zero warnings
under the pinned toolchain. (PRD §5.4 via community-detection; AVI-618 §6
posture)

#### Scenario: companion builds green
- **WHEN** `lake build` runs in `formalization/lean` under the pinned toolchain
- **THEN** `GraphosFormalization.Bunch.MQ` compiles with zero errors and zero warnings

#### Scenario: Haskell oracle matches the Lean fixture
- **WHEN** `modularizationQuality` scores the two-triangle fixture `twoTriangles` with the `byTriangle` partition
- **THEN** it returns `84/49` within 1e-12, the value `twoTriangles_mq` proves

### Requirement: Incremental update agrees with full recompute

Any incremental maintenance of MQ across single-node moves SHALL yield the
same value as recomputing MQ from the partition, within floating-point
tolerance 1e-9.

#### Scenario: random move sequence
- **WHEN** a random sequence of single-node moves is applied to a random partition of a random graph with at most 40 nodes
- **THEN** after every move the maintained MQ equals the recomputed MQ within 1e-9
