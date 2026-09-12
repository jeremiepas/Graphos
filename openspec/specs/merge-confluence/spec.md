# merge-confluence Specification

## Purpose
TBD - created by archiving change j2-category-colimit-merge. Update Purpose after archive.
## Requirements
### Requirement: Merge is the colimit of consistent views (M1 — confluence)

The merge stage SHALL realise `merge(V) = colim D` as the **coproduct** of a family
of consistent views, with universal cocone `λᵢ : Dᵢ -> colim D` satisfying the
universal property: for any object `G` and cocone `fᵢ : Dᵢ -> G` there is a
**unique** morphism `⟪μ⟫ : colim D -> G` with `⟪μ⟫ ∘ λᵢ = fᵢ` (unique up to the
NodeId+weight+community iso).

- **WHEN** the input views are consistent (they agree on every shared `NodeId`
  payload and share one directed flag)
- **THEN** for any permutation `σ` of the views, `merge(V) ~= merge(V_σ)` up to the
  NodeId+weight+community iso, because a view permutation is a diagram automorphism
  and colimits are unique up to unique iso
- **AND** the mediating morphism from the merged graph to any cocone is unique up to
  that iso

#### Scenario: two view orders yield isomorphic output
- **WHEN** `mergeGraphs` is folded over consistent views in two different orders
- **THEN** the two `(graph, communityMap)` results are isomorphic via a bijection
  preserving `NodeId`, edge weights `A_uv`, and community labels `c(v)`
- **AND** the unique mediating morphism to any competing output exists and is unique

### Requirement: Cluster output is invariant under view permutation (M2)

The cluster stage SHALL be a deterministic function of the canonical merged graph so
that re-clustering does not depend on view order.

- **WHEN** two consistent views are merged in different orders
- **THEN** `cluster(merge(V)) ~= cluster(merge(V_σ))` up to the community-label iso
- **AND** determinism is grounded in `leidenCore` reading `Map.keys (gNodes g)` in
  ascending key order and relabelling via `leidenStateToCommunityMap` in the same
  order

#### Scenario: community map invariant under view reordering
- **WHEN** the full pipeline runs on consistent views in two orders
- **THEN** both produce isomorphic community maps (same partition; labels related by
  the deterministic relabel)

### Requirement: Deterministic given-order canonical form (M3 — canonical form backing)

Wherever merge/cluster output must be reproducible for a fixed input order, the
output SHALL be the **canonical representative** of the colimit/coproduct: the graph
with nodes and edges keyed by `NodeId`/edge-tuple and hashed by
`computeGraphHash` over sorted keys. Because `computeGraphHash` covers only node ids
and edge endpoint tuples (omitting `edgeWeight`, `edgeRelation`, and community
labels), the canonical oracle SHALL additionally compare edge payloads and the
`CommunityMap`; determinism is well-defined as equality of the full triple
`(gNodes, gEdges, CommunityMap)` (for consistent views) rather than an arbitrary
representative or the hash alone.

- **WHEN** a fixed linear order of the views is given
- **THEN** `mergeGraphs` returns the canonical colimit representative (Map-keyed,
  sorted-hash), independent of which view is treated as `old` vs `new` for consistent
  inputs
- **WHEN** conflicting inputs are given
- **THEN** the returned representative is a deterministic function of the fixed order
  (last-write-wins), so determinism-given-order is well-defined

#### Scenario: canonical form stable for consistent inputs
- **WHEN** `mergeGraphs` folds consistent views in either operand order
- **THEN** `computeGraphHash` of the result is identical in both orders

### Requirement: Last-write-wins conflicts resolve as a coequalizer (M4)

Conflict resolution on shared `NodeId` SHALL be modelled as a **coequalizer**, not a
coproduct: the two conflicting payloads are identified to a single winner (later
write), and divergence between view orders SHALL be quantified by the disagreement
set.

- **WHEN** two views supply differing `Node` payloads for the same `NodeId`
- **THEN** `mergeGraphs` collapses the pair to one node carrying the later payload
  (the coequalizer quotient `q` with `q∘u = q∘v`), never retaining both
- **THEN** the divergence between two view orders is bounded by and reported against
  the disagreement set `Δ = Δ_nodes ∪ Δ_edges`, where
  `Δ_nodes = { n : payloadₐ(n) ≠ payload_b(n) }` and
  `Δ_edges = { (a,b) : edgeWeightₐ(a,b) ≠ edgeWeight_b(a,b) }` (edges keyed by
  `(NodeId,NodeId)` are merged new-wins, so differing weights on a shared tuple also
  diverge); divergence is bounded by `|Δ_nodes| + |Δ_edges|`, not node count alone
- **THEN** for a fixed order the resolved graph is deterministic given that order

#### Scenario: conflict collapses to one node, divergence quantified
- **WHEN** `mergeGraphs` merges two views that disagree on the payload of `NodeId` n
- **THEN** exactly one node labelled `n` exists in the result (last write wins)
- **AND** the reported divergence between the two fold orders is non-zero only on `Δ`

#### Scenario: conflicting orders are not order-independent
- **WHEN** the same two views are merged in opposite orders
- **THEN** results may differ only on `Δ`, and the community move count
  (`countMoves`) across the two compositions is measured but not required to be zero

### Requirement: Merge × cluster is non-commutative under conflicts (CTO note 3, case 3)

The merge (colimit/coequalizer) algebra SHALL NOT be collapsed into the same
commutative/idempotent algebra as cluster; `merge` and `cluster` SHALL not commute in
general, and non-commutativity SHALL be measured rather than assumed absent.

- **WHEN** a conflict exists on a shared `NodeId`
- **THEN** `countMoves(cluster(merge(A,B)), cluster(merge(B,A)))` is measured (not
  asserted zero), because `cluster` is deterministic but not a functor preserving the
  coproduct and LWW makes `merge` order-dependent on `Δ`
- **WHEN** views are consistent
- **THEN** `merge × cluster` is order-independent up to iso (the commutative case),
  and this restriction is stated explicitly

#### Scenario: reordering a conflicting merge changes output
- **WHEN** two conflicting views are merged then clustered in both orders
- **THEN** the resulting community maps are compared with `countMoves` and the
  difference is recorded as the measured non-commutativity bound

### Requirement: Idempoteness asserted separately (M5)

The merge stage SHALL satisfy `mergeGraphs A A ~= A` as an **independent** property,
derived solely from `Map.union` idempotency on identical maps — not from confluence
nor from non-commutativity.

- **WHEN** `mergeGraphs` is applied to the same graph twice
- **THEN** `gNodes A <> gNodes A = gNodes A`, edges and adjacency are unchanged,
  `gDirected` is preserved, and `computeGraphHash` is stable, so the result equals `A`
- **AND** this property is asserted separately from M1/M2/M4 and is not used to
  derive them

#### Scenario: merging a graph with itself is a fixed point
- **WHEN** `mergeGraphs A A` is evaluated
- **THEN** the resulting graph is identical to `A` (up to the deterministic community
  relabel)

