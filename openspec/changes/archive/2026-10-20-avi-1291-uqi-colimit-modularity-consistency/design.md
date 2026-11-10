# Design: avial UQI colimit–modularity consistency (AVI-1291, element J1)

Adopts register UQI-1…UQI-8 of `docs/math-requirements/AVI-529-colimit-modularity-consistency.md` into an OpenSpec capability. Notation is verbatim from `docs/math-requirements/consolidated-requirements.md` §0.

## 1. Math approach

The merged context graph is **one value**, produced once by the colimit:

```
M = colim({V_i}) ≅ mergeGraphs …  ::  Graph
```

The unified query interface (UQI) is the pair of pure projections:

```
π_struct : Graph → (Map NodeId Node, Map EdgeId Edge, Bool)
           M ↦ (gNodes M, gEdges M, gDirected M)          -- colimit / structural reading

π_mod : Graph → (V, A)
        M ↦ (Map.keys (gNodes M), { {u,v} : v ∈ neighbors M u ∨ u ∈ neighbors M v })
                                                          -- modularity reading, G_s = π_mod M
```

- `π_struct` reads the node/edge/direction **fields**; `π_mod` reads only `gAdjFwd / gAdjBack`.
- Structural queries (node record, edge record, key order-independence) resolve via `π_struct`; modularity queries (neighbors, degree, partition, `Q_γ`, cohesion) resolve via `G_s = π_mod M`.

Key formulas (one notation, glossary-backed):

- Support graph scale: `m = |E_s| = (Σ_v degree M v) / 2` (unweighted `G_s`; `edgeWeight` is not read by Leiden — consolidated §0 scope statement).
- Resolution modularity (GT-R3, Blondel–Newman): `Q_γ = (1/2m) Σ_{i,j} [A_ij − γ·k_i k_j/(2m)] δ(c_i, c_j)`.
- Cohesion (GT-R10, per-community): `cohesion(c) = (1/|c|) · Σ_{v ∈ c} |{ u ∈ c : u ∈ neighbors v }| / max(1, |neighbors v|)`; `cohesion(c) ∈ [0,1]` (INV-5).
- Detection cost: `O(maxIter·(E_s+N))` time, `O(N+C+E_s)` memory with NFData thunk discipline (GT-R7, `leiden-scalability`).

## 2. Requirements register → spec mapping

| UQI | Statement (SHALL) | Classification | Acceptance (spec scenario) | Surface |
|---|---|---|---|---|
| UQI-1 | Single-object determinacy: every query against `M` has exactly one answer | requirement-creation | property: both projections commute with node re-indexing `ρ` (P-1) | `mergeGraphs`, `neighbors` |
| UQI-2 | Projection independence: `π_struct`/`π_mod` factor through disjoint field-sets | requirement-creation | property: structural read never depends on adjacency and vice versa (P-1) | `Graph` record layout, `mergeGraphs` |
| UQI-3 | Combinatorial grounding: `V(G_s) = Map.keys (gNodes M)`, `m = (Σ_v degree M v)/2` | requirement-creation | property on overlapping-view fixtures (P-2) | `buildLeidenState`, `neighbors` |
| UQI-4 | Community-query freshness: `detectCommunities : Graph → CommunityMap` defined on the apex; never reuses view community ids; does not factor through views | requirement-creation | behavior: `detect(M)` pure; `≠ merge-detect(A,B)` (CT-R10 preservation) | `mergeGraphsAndAnalyze`, `detectCommunitiesWithResolution` |
| UQI-5 | Monotonicity boundary: `Q_γ` non-decreasing within one detection on fixed `M`; cross-merge decrease explicitly allowed | requirement-creation | counterexample recorded as allowed (P-4) | `detectCommunitiesWithResolution` |
| UQI-6 | Cohesion cross-reading: `cohesion_M(c) == cohesion_viewUnion(c)`; `cohesion ∈ [0,1]` | requirement-creation | property + range (P-3) | `mergeGraphs`, `cohesionScore` |
| UQI-7 | Cost model: structural read O(\|response\|); neighbor/degree O(deg); cohesion O(E_s); detection O(maxIter·(E_s+N)); memory O(N+C+E_s) | requirement-creation | complexity + benchmark (<30 s @ 100k, `leiden-scalability`) (P-6) | `buildLeidenState`, `localMovingPass` |
| UQI-8 | Order-independence on consistent diagrams; old-wins (left/first operand) order-dependence at conflicting keys | requirement-creation | property on both fixture classes (P-5); divergence bounded by `merge-confluence` M4's `Δ` | `mergeGraphs`, `neighbors` |

Grouping in the spec: UQI-1+2 as one requirement (determinacy is witnessed by the disjoint-projection property), UQI-3, UQI-4, UQI-5, UQI-6, UQI-7, UQI-8 as standalone requirements — eight register rows, six SHALL requirements, each carrying the mapped acceptance.

## 3. Surface grounding

- `openspec/specs/*` capabilities consumed: `merge-confluence` (M1 colimit universal property, M4 conflict coequalizer + divergence set `Δ`, M5 idempotence), `community-detection` (Leiden phases, `cohesionScore`, resolution), `leiden-scalability` (CSR adjacency, O(1)/move, one-pass gain scoring), `09-merge` (Workflow 09 merge semantics).
- `src/Graphos/**` modules constrained: `Domain/Graph/Core.hs` (`Graph:47`, `mergeGraphs:140`), `Domain/Graph/Query.hs` (`neighbors:37`, `degree:44`), `Domain/Community.hs` (`detectCommunitiesWithResolution:73`, `computeCommunityStats:92`, `cohesionScore:385`, `scoreAllCohesion:405`), `UseCase/Merge.hs` (`mergeGraphsAndAnalyze:37`).

## 4. Consistency with the consolidated layer

- **CT-R10 boundary preserved (hard requirement):** J1 must not weaken merge × cluster non-commutativity under conflicts. UQI-4 mandates fresh detection on the apex; UQI-5 confines monotonicity to a fixed object; the spec states the non-commutativity is *measured* (`merge-confluence` CTO note 3), never assumed absent. No delta requirement here claims commutativity or reuse of community ids across merge.
- **G4 reduces to G2:** the UQI is exactly the synthesized overlay (G4) of the colimit element (G2/AVI-511) and the modularity element (AVI-510); every UQI row cites, and is derivable from, CT/ GT rows of `consolidated-requirements.md` (§1 register). No new math is introduced; no consolidated row is contradicted or restated with different acceptance.
- **Old-wins vs last-write-wins:** UQI-8 states the *conflict policy in force* (old/first operand wins per `Map.union`) and asserts divergence is bounded by M4's disagreement set `Δ`. The spec does not modify M4.

## 5. Risks / boundaries (diagram honesty)

- UQI-4's "fresh computation" and UQI-8's conflicting-key behavior are the two rows whose Haskell facts (audit of `MergeResult` consumers; old-wins on modularity-derived keys) still carry the AVI-529 open items O1/O3 for dev confirmation — the feasibility gate covers them; the spec wording already matches the documented `Map.union` semantics.
- UQI-7's benchmark bound is inherited from `leiden-scalability`; no new benchmark is introduced here.

## 6. Feasibility gate

Archival of this change is gated on a `head-of-dev` confirmation comment on [AVI-1291](/AVI/issues/AVI-1291) (expected verdict: no production-code impact; property tests only).