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

### Requirement: Cocone legs are structure-preserving graph morphisms (LG-C1)

Each cocone leg `ι_x : buildGraph d V_x → M` of the merge `M = mergeGraphs A B` SHALL preserve structure: node injection on `keys(gNodes V_x)`, edge incidence (`edgeSource`/`edgeTarget` of every surviving edge), `edgeRelation`, `edgeWeight`, and `edgeConfidence` equal the carried view value, `gAdjFwd/gAdjBack(V_x)(v) ⊆ gAdjFwd/gAdjBack(M)(ι_x v)`, and the directed flag SHALL be `gDirected M = gDirected A || gDirected B` (canonical: directed iff either input is directed; order-independent). (Source: AVI-537 §3 LG-C1, §4; constrains `Domain.Graph.Core.mergeGraphs`, `Domain.Graph.Core.buildGraph`.)

#### Scenario: Edge payloads survive the leg
- **WHEN** random consistent views `A, B` are merged
- **THEN** for every edge `e ∈ gEdges A` surviving in `M`, `edgeRelation`, `edgeWeight`, and `edgeConfidence` equal the values carried by `e` (and symmetrically for `B`)

#### Scenario: Adjacency is preserved per leg and the directed flag is or-composed
- **WHEN** `M = mergeGraphs A B` is computed, including when `gDirected A = True` and `gDirected B = False` (or vice versa, in both orders)
- **THEN** `neighbors_M(ι_A v) ⊇ neighbors_A(v)` for all `v ∈ keys(gNodes A)` (and symmetrically for `B`), and `gDirected M = True` in both orders

### Requirement: Shared keys with identical labels map identically under both legs (LG-C2 — SHALL)

On the agreement region — shared node keys `k ∈ keys(extNodes A) ∩ keys(extNodes B)` carrying **identical** `Node` values, and shared edge keys carrying identical `Edge` values — the two legs SHALL map the shared key to identical entries in `M`: `Map.lookup k (gNodes M) == Map.lookup k (gNodes A) == Map.lookup k (gNodes B)` and the dual statement for edges. This SHALL hold independently of the conflict policy: both legs carry the identical value in this region, so any deterministic winner selection returns that same value — the requirement is scoped to identical payloads precisely to keep it policy-agnostic. (Source: AVI-537 §2.2; constrains `Domain.Graph.Core.mergeGraphs`, `Domain.Types.Node`, `Domain.Types.Graph.extNodes`.)

#### Scenario: Agreeing shared node is equal under both legs
- **WHEN** views `A, B` share a node key `k` with identical `Node` value and `M = mergeGraphs A B` is computed
- **THEN** `Map.lookup k (gNodes M)` equals both view lookups, and the same holds for a shared edge key with identical payload

#### Scenario: Agreement holds under both merge orders
- **WHEN** the agreeing views are merged as `mergeGraphs A B` and as `mergeGraphs B A`
- **THEN** `ι_A(k) = ι_B(k)` in both results on the agreement region (order-independence where values agree)

### Requirement: The merge square is a pullback on labels within the agreement region (LG-C3)

For consistent views (full overlap = agreement), the merge square SHALL be a pullback on labels: the fiber product `{ (a,b) ∈ keys(gNodes A) × keys(gNodes B) : ι_A(a) = ι_B(b) }` SHALL equal the agreement key set on identical-label keys, and the merged node set SHALL satisfy the dedup identity `Map.size (gNodes M) == |keys(gNodes A) ∪ keys(gNodes B)|` — merge introduces no identification beyond shared identical labels and keeps no duplicate copy of an agreed node or edge. (Source: AVI-537 §2.3 LG-PULLBACK, §3 LG-C3, locked by AVI-647; constrains `Domain.Graph.Core.mergeGraphs` and its left-biased `Data.Map.union`.)

#### Scenario: Fiber-product identity on consistent diagrams
- **WHEN** random consistent views with overlapping keys are merged
- **THEN** the fiber-product identity holds (property test) and `Map.size (gNodes M)` equals the union cardinality — no key is duplicated and none is spuriously identified

#### Scenario: Dedup identity
- **WHEN** two single-view graphs sharing `n` agreeing keys are merged
- **THEN** the merged node count is exactly `|A| + |B| − n` (each shared key counted once, INV-MERGE/CT-R8)

### Requirement: Honesty boundary — conflicts over-identify and the square is not a pullback (LG-C4)

Where two views share a key but carry **differing** labels, no value-preserving embedding into both views exists: the merge square SHALL remain a pushout (the merge exists and the shared key resolves to one entry, per `merge-confluence` M4's coequalizer with the fixed-order winner) but SHALL NOT be claimed a pullback — the fiber product contains the conflicting pair's over-identification. This non-pullback SHALL be recorded as the expected behavior (a policy artifact whose divergence is bounded by M4's disagreement set `Δ = Δ_nodes ∪ Δ_edges`), not as an invariant violation. (Source: AVI-537 §3 LG-C4, §6 AQ-LG1; constrains `Domain.Graph.Core.mergeGraphs`, consistent with `merge-confluence` M4.)

#### Scenario: Conflicting shared key over-identifies
- **WHEN** views `A, B` share node key `k` with different `Node` payloads
- **THEN** exactly one node `k` exists in `M` (M4 coequalizer), the square is not a pullback at `(k_A, k_B)`, and the test records this as the *expected non-pullback* boundary — divergence between the two fold orders is non-zero only on `Δ`

### Requirement: The structural projection is natural under merge (LG-C5)

The structural projection `Struct(g) = (gNodes g, gEdges g, gDirected g)` SHALL commute with merge as an isomorphism on keys: `keys(gNodes M) = ⋃_x keys(gNodes V_x)`, `keys(gEdges M) = ⋃_x keys(gEdges V_x)` (after the dangling drop), and `gDirected M = gDirected A || gDirected B` — so the global structure is the union of local structures with no spurious loss (extends `merge-confluence` M3's canonical-form guarantee to an explicit local↔global agreement). (Source: AVI-537 §3 LG-C5, §4; consolidated CT-R9; constrains `Domain.Graph.Core.mergeGraphs`.)

#### Scenario: Structural square commutes
- **WHEN** consistent views `A, B` are merged
- **THEN** `Set.size (keys (gNodes M))` equals the size of the union of node keys, and the node-key sets are exactly the union (naturality property)

#### Scenario: Directed flag agrees with local truth
- **WHEN** the views disagree on directedness (one directed, one undirected)
- **THEN** `gDirected M == gDirected A || gDirected B` in both merge orders (canonical, order-independent)
