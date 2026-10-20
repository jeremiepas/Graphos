## Purpose

Binds the two readings of the merged context graph — the structural colimit reading `π_struct` and the modularity reading `π_mod` → `G_s` — as disjoint-field pure projections of the one merged object `M = mergeGraphs …`: single-object determinacy, combinatorial grounding of the support graph on the colimit, fresh community detection on the apex (the CT-R10 non-commutativity boundary preserved), the intra-detection monotonicity boundary, cohesion cross-reading consistency, bounded query cost, and order-independence on consistent diagrams with divergence bounded by `Δ` at conflicts. Consumes `merge-confluence` (M1–M5), `community-detection`, and `leiden-scalability` as-is; modifies neither.

## ADDED Requirements

### Requirement: Single-object determinacy and projection independence (UQI-1, UQI-2)

The merged context graph SHALL be one value `M = mergeGraphs …` carrying two readings as pure functions of `M`: the structural reading `π_struct(M) = (gNodes M, gEdges M, gDirected M)` and the modularity reading `G_s = π_mod(M) = (Map.keys (gNodes M), { {u,v} : v ∈ neighbors M u ∨ u ∈ neighbors M v })`. The two projections SHALL factor through disjoint field-sets of `Graph` (`gNodes/gEdges/gDirected` vs `gAdjFwd/gAdjBack`), so a structural query and a modularity query never interfere, and every query against `M` SHALL have exactly one answer. (Source: AVI-529 UQI-1/UQI-2, P-1; constrains `Domain.Graph.Core.mergeGraphs`, `Domain.Graph.Core.Graph`, `Domain.Graph.Query.neighbors`.) — classification: requirement-creation

#### Scenario: Projections commute with node re-indexing
- **WHEN** the merged graph `M` is re-indexed by any node permutation `ρ`
- **THEN** both `π_struct(ρ·M) = ρ·π_struct(M)` and `π_mod(ρ·M) = ρ·π_mod(M)` hold (property test)

#### Scenario: Structural and modularity reads use disjoint fields
- **WHEN** the adjacency maps `gAdjFwd/gAdjBack` are rebuilt (e.g. after `rebuildAdjacency`) without changing `gNodes/gEdges`
- **THEN** every structural read `π_struct(M)` is unchanged, and every modularity read `G_s = π_mod M` is unchanged (they read disjoint field-sets)

### Requirement: Combinatorial grounding of the support graph on the colimit (UQI-3)

The support graph `G_s = π_mod M` SHALL be grounded on the colimit object: `V(G_s) = Map.keys (gNodes M)` (so `|V(G_s)|` equals the merged node count after the dangling drop) and `m = |E_s| = (Σ_v degree M v)/2`. The degree/modularity inputs SHALL be exactly the colimit's node and edge sets. (Source: AVI-529 UQI-3, P-2; consolidated JOINT-2; constrains `Domain.Community.buildLeidenState`, `Domain.Graph.Query.neighbors/degree`.) — classification: requirement-creation

#### Scenario: Support vertex set equals colimit node set
- **WHEN** overlapping views with shared keys are merged and the dangling drop is applied
- **THEN** `Set.size (keys of G_s) == Map.size (gNodes M)` holds on the fixture

#### Scenario: Edge-count identity
- **WHEN** the support graph of `M` is built
- **THEN** `m == sum (map (degree M) (Map.keys (gNodes M))) / 2`

### Requirement: Community-query freshness on the colimit apex (UQI-4)

Any community or modularity query on `M` SHALL be `detectCommunitiesWithResolution M res` — a fresh computation on the current `M` defined on the colimit apex. It SHALL NOT reuse community ids from any source view and SHALL NOT factor through the views `V_i` (merge mandates re-detection; the operationalization of CT-R10 and `merge-confluence` CTO note 3). (Source: AVI-529 UQI-4; constrains `UseCase.Merge.mergeGraphsAndAnalyze`, `Domain.Community.detectCommunitiesWithResolution`.) — classification: requirement-creation

#### Scenario: Detection on the apex is a pure function of M
- **WHEN** `detectCommunitiesWithResolution M res` is called twice on the same `M`
- **THEN** both calls return the same partition (deterministic tie-break on equal `Q` picks the least `CommunityId`)

#### Scenario: No source-view community id survives a merge
- **WHEN** views `A, B` carry different community assignments for shared keys and are merged
- **THEN** the community map reported for `M` is recomputed from `M` and contains no community id or membership inherited from `A`'s or `B`'s maps

### Requirement: Monotonicity boundary confined to a fixed detection (UQI-5)

`Q_γ` SHALL be non-decreasing within a single detection run on a fixed graph (accepted local-moving moves strictly increase `Q_γ`). Across a merge `M → M'`, `Q_γ(opt(M'))` MAY be lower than `Q_γ(opt(M))` — cross-merge monotonicity SHALL NOT be assumed, asserted, or reported as an invariant violation; it is an observed quantity measured via the merge × cluster non-commutativity bound of `merge-confluence`. (Source: AVI-529 UQI-5, P-4; consolidated JOINT-3.) — classification: requirement-creation

#### Scenario: Within-detection monotonicity holds
- **WHEN** one detection pass accepts moves on a fixed graph
- **THEN** every accepted move strictly increases `Q_γ` (invariant assertion per pass, GT-R4/INV-2)

#### Scenario: Cross-merge decrease is allowed
- **WHEN** views `A, B` are chosen such that `Q_γ(detect(merge(A,B))) < Q_γ(detect(A))`
- **THEN** the test records the decrease as an *allowed* outcome (boundary counterexample), not an invariant violation

### Requirement: Cohesion cross-reading consistency (UQI-6)

For any community `c` of `M`, cohesion computed on `M` SHALL equal cohesion computed from the constituent views' union adjacency — the one modularity query that is consistent across representations — because `mergeGraphs` unions `gAdjFwd/gAdjBack` and preserves `gNodes`. Cohesion SHALL remain in `[0,1]` (INV-5, `max 1` denominator guard). (Source: AVI-529 UQI-6, P-3; consolidated CT-R11; constrains `Domain.Graph.Core.mergeGraphs`, `Domain.Community.cohesionScore`.) — classification: requirement-creation

#### Scenario: Cohesion agrees across representations
- **WHEN** a community `c` of `M = mergeGraphs A B` is scored
- **THEN** `cohesion_M(c) == cohesion_viewUnion(c)` on the fixture

#### Scenario: Cohesion range
- **WHEN** any community of any merged graph is scored
- **THEN** `0 ≤ cohesion(c) ≤ 1`, including communities containing isolated nodes (denominator guarded by `max 1`)

### Requirement: Query cost model and scale guard (UQI-7)

The UQI query costs SHALL respect: structural read `O(|response|)`; neighbor/degree `O(deg(v))`; modularity of a given partition `O(E_s)`; cohesion `O(E_s)`; community detection `O(maxIter·(E_s+N))` time and `O(N+C+E_s)` memory with NFData thunk discipline between iterations. The worst case of the interface SHALL be detection, and the `leiden-scalability` budgets SHALL remain the scale guard. (Source: AVI-529 UQI-7, P-6; consolidated GT-R7; constrains `Domain.Community.buildLeidenState`, `Domain.Community.localMovingPass`.) — classification: requirement-creation

#### Scenario: Detection stays within the Leiden budget
- **WHEN** `detectCommunities` runs on a sparse graph of ≥ 50k nodes with `maxIter ≤ 50`
- **THEN** wall-time scales linearly in `N + E_s` and the 100k-node PRD target (< 30 s) is met on the benchmark fixture

#### Scenario: Per-query bounds
- **WHEN** structural, neighbor/degree, or cohesion queries are issued
- **THEN** their costs are `O(|response|)`, `O(deg(v))`, and `O(E_s)` respectively (data-structure assertion, no quadratic rescan)

### Requirement: Order-independence on consistent diagrams, old-wins at conflicts (UQI-8)

For a consistent diagram (no key conflicts), `M` SHALL be order-independent (colimit uniqueness up to unique iso), so both `π_struct(M)` and `G_s = π_mod M` — and every UQI query — SHALL be order-independent. For conflicting inputs, the surviving value at each conflict SHALL be the *old* (left/first operand) merge partner's value (`Data.Map.union` left-bias), and the divergence between orders SHALL be bounded by `merge-confluence`'s disagreement set `Δ = Δ_nodes ∪ Δ_edges`. (Source: AVI-529 UQI-8, P-5; consolidated CT-R5/CT-R7; constrains `Domain.Graph.Core.mergeGraphs`, `Domain.Graph.Query.neighbors`.) — classification: requirement-creation

#### Scenario: Consistent merge is order-independent
- **WHEN** consistent views are merged under any merge permutation
- **THEN** every UQI query yields identical answers across permutations (`query(M) == query(M')`)

#### Scenario: Conflicting merge is old-wins and divergence-bounded
- **WHEN** conflicting views are merged in two orders
- **THEN** the surviving payload at each conflict equals the old (left/first) operand's value, and divergence is non-zero only on `Δ` (extends `merge-confluence` AC-2 to modularity-derived keys)