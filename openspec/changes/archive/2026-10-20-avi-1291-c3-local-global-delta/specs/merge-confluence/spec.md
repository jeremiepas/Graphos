## ADDED Requirements

### Requirement: Cocone legs are structure-preserving graph morphisms (LG-C1)

Each cocone leg `ι_x : buildGraph d V_x → M` of the merge `M = mergeGraphs A B` SHALL preserve structure: node injection on `keys(gNodes V_x)`, edge incidence (`edgeSource`/`edgeTarget` of every surviving edge), `edgeRelation`, `edgeWeight`, and `edgeConfidence` equal the carried view value, `gAdjFwd/gAdjBack(V_x)(v) ⊆ gAdjFwd/gAdjBack(M)(ι_x v)`, and the directed flag SHALL be `gDirected M = gDirected A || gDirected B` (canonical: directed iff either input is directed; order-independent). (Source: AVI-537 §3 LG-C1, §4; constrains `Domain.Graph.Core.mergeGraphs`, `Domain.Graph.Core.buildGraph`.) — classification: requirement-creation

#### Scenario: Edge payloads survive the leg
- **WHEN** random consistent views `A, B` are merged
- **THEN** for every edge `e ∈ gEdges A` surviving in `M`, `edgeRelation`, `edgeWeight`, and `edgeConfidence` equal the values carried by `e` (and symmetrically for `B`)

#### Scenario: Adjacency is preserved per leg and the directed flag is or-composed
- **WHEN** `M = mergeGraphs A B` is computed, including when `gDirected A = True` and `gDirected B = False` (or vice versa, in both orders)
- **THEN** `neighbors_M(ι_A v) ⊇ neighbors_A(v)` for all `v ∈ keys(gNodes A)` (and symmetrically for `B`), and `gDirected M = True` in both orders

### Requirement: Shared keys with identical labels map identically under both legs (LG-C2 — SHALL)

On the agreement region — shared node keys `k ∈ keys(extNodes A) ∩ keys(extNodes B)` carrying **identical** `Node` values, and shared edge keys carrying identical `Edge` values — the two legs SHALL map the shared key to identical entries in `M`: `Map.lookup k (gNodes M) == Map.lookup k (gNodes A) == Map.lookup k (gNodes B)` and the dual statement for edges. This SHALL hold independently of the conflict policy: both legs carry the identical value in this region, so any deterministic winner selection returns that same value — the requirement is scoped to identical payloads precisely to keep it policy-agnostic. (Source: AVI-537 §2.2; constrains `Domain.Graph.Core.mergeGraphs`, `Domain.Types.Node`, `Domain.Types.Graph.extNodes`.) — classification: requirement-creation

#### Scenario: Agreeing shared node is equal under both legs
- **WHEN** views `A, B` share a node key `k` with identical `Node` value and `M = mergeGraphs A B` is computed
- **THEN** `Map.lookup k (gNodes M)` equals both view lookups, and the same holds for a shared edge key with identical payload

#### Scenario: Agreement holds under both merge orders
- **WHEN** the agreeing views are merged as `mergeGraphs A B` and as `mergeGraphs B A`
- **THEN** `ι_A(k) = ι_B(k)` in both results on the agreement region (order-independence where values agree)

### Requirement: The merge square is a pullback on labels within the agreement region (LG-C3)

For consistent views (full overlap = agreement), the merge square SHALL be a pullback on labels: the fiber product `{ (a,b) ∈ keys(gNodes A) × keys(gNodes B) : ι_A(a) = ι_B(b) }` SHALL equal the agreement key set on identical-label keys, and the merged node set SHALL satisfy the dedup identity `Map.size (gNodes M) == |keys(gNodes A) ∪ keys(gNodes B)|` — merge introduces no identification beyond shared identical labels and keeps no duplicate copy of an agreed node or edge. (Source: AVI-537 §2.3 LG-PULLBACK, §3 LG-C3, locked by AVI-647; constrains `Domain.Graph.Core.mergeGraphs` and its left-biased `Data.Map.union`.) — classification: requirement-creation

#### Scenario: Fiber-product identity on consistent diagrams
- **WHEN** random consistent views with overlapping keys are merged
- **THEN** the fiber-product identity holds (property test) and `Map.size (gNodes M)` equals the union cardinality — no key is duplicated and none is spuriously identified

#### Scenario: Dedup identity
- **WHEN** two single-view graphs sharing `n` agreeing keys are merged
- **THEN** the merged node count is exactly `|A| + |B| − n` (each shared key counted once, INV-MERGE/CT-R8)

### Requirement: Honesty boundary — conflicts over-identify and the square is not a pullback (LG-C4)

Where two views share a key but carry **differing** labels, no value-preserving embedding into both views exists: the merge square SHALL remain a pushout (the merge exists and the shared key resolves to one entry, per `merge-confluence` M4's coequalizer with the fixed-order winner) but SHALL NOT be claimed a pullback — the fiber product contains the conflicting pair's over-identification. This non-pullback SHALL be recorded as the expected behavior (a policy artifact whose divergence is bounded by M4's disagreement set `Δ = Δ_nodes ∪ Δ_edges`), not as an invariant violation. (Source: AVI-537 §3 LG-C4, §6 AQ-LG1; constrains `Domain.Graph.Core.mergeGraphs`, consistent with `merge-confluence` M4.) — classification: requirement-creation

#### Scenario: Conflicting shared key over-identifies
- **WHEN** views `A, B` share node key `k` with different `Node` payloads
- **THEN** exactly one node `k` exists in `M` (M4 coequalizer), the square is not a pullback at `(k_A, k_B)`, and the test records this as the *expected non-pullback* boundary — divergence between the two fold orders is non-zero only on `Δ`

### Requirement: The structural projection is natural under merge (LG-C5)

The structural projection `Struct(g) = (gNodes g, gEdges g, gDirected g)` SHALL commute with merge as an isomorphism on keys: `keys(gNodes M) = ⋃_x keys(gNodes V_x)`, `keys(gEdges M) = ⋃_x keys(gEdges V_x)` (after the dangling drop), and `gDirected M = gDirected A || gDirected B` — so the global structure is the union of local structures with no spurious loss (extends `merge-confluence` M3's canonical-form guarantee to an explicit local↔global agreement). (Source: AVI-537 §3 LG-C5, §4; consolidated CT-R9; constrains `Domain.Graph.Core.mergeGraphs`.) — classification: requirement-creation

#### Scenario: Structural square commutes
- **WHEN** consistent views `A, B` are merged
- **THEN** `Set.size (keys (gNodes M))` equals the size of the union of node keys, and the node-key sets are exactly the union (naturality property)

#### Scenario: Directed flag agrees with local truth
- **WHEN** the views disagree on directedness (one directed, one undirected)
- **THEN** `gDirected M == gDirected A || gDirected B` in both merge orders (canonical, order-independent)