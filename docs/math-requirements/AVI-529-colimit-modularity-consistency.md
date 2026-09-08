# Colimit–Modularity Consistency — Unified Query Interface — Math Requirements (Graphos Context Graph)

**Author:** Head of R&D (`head-rnd`) — synthesizing the Graph Theory Expert and Category Theory Expert on the **joint surface**
**Issue:** [AVI-529](/AVI/issues/AVI-529) (child of [AVI-512](/AVI/issues/AVI-512))
**Synthesizes:**
- [AVI-510](/AVI/issues/AVI-510) — Community Detection & Modularity (Graph Theory Expert) — `community-detection-modularity.md`
- [AVI-511](/AVI/issues/AVI-511) — Merge / Consolidation as a Colimit (Category Theory Expert) — `AVI-511-colimit-merge.md`
**Extends:** the consolidated joint statements [JOINT-1](#joint-1--single-object-determinacy), [JOINT-2](#joint-2--combinatorial-grounding-of-gs-on-the-colimit), [JOINT-3](#joint-3--monotonicity-boundary) of the consolidated requirements layer (`docs/math-requirements/consolidated-requirements.md`).
**Status:** requirements document — grounded in domain model + openspec specs. One notation, one glossary, every requirement mapped to a concrete Graphos surface and tagged **Proven / Assumed 🔧 / Open**.
**Scope:** math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion.

---

## 0. Notation (unified glossary)

Notation is drawn verbatim from the unified glossary in `docs/math-requirements/consolidated-requirements.md` (§0). Reproduced here at the points this doc depends on; line numbers verified against checked-in sources.

- `NodeId = Text` — `Graphos.Domain.Types.Node.NodeId` (`Node.hs:46`).
- `Node = Node { nodeId, nodeLabel, nodeFileType, nodeSourceFile, nodeLineStart, nodeLineEnd, nodeSignature, nodeCommunityId :: Maybe Int, nodeKind, nodeDegree :: Maybe Int, nodeIsBridge :: Maybe Bool, nodeExtra :: Maybe Value, nodePresentBits :: Word64 }` (`Node.hs:120`).
- `Edge = Edge { edgeId, edgeSource :: NodeId, edgeTarget :: NodeId, edgeRelation :: Relation, edgeWeight :: Double, edgeConfidence :: Confidence Double, edgeExtra :: Maybe Value }` (`Edge.hs:88`). `Relation ∈ {Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred}`. `EdgeId = EdgeId (source "->" target ":" relationToText relation)` (`lsp-edge-extraction`).
- `Graph = Graph { gNodes :: Map NodeId Node, gEdges :: Map EdgeId Edge, gAdjFwd :: Map NodeId (Set NodeId), gAdjBack :: Map NodeId (Set NodeId), gDirected :: Bool, … }` (`Graph/Core.hs:47`).
- `Extraction { extractionNodes :: Map NodeId Node, extractionEdges :: Map EdgeId Edge }` (`Types/Graph.hs:47`), accessors `extNodes / extEdges` (`Types/Graph.hs:66-69`). This is a per-file LSP **view**.
- `neighbors g nid = if gDirected g then gAdjFwd[nid] else gAdjFwd[nid] ∪ gAdjBack[nid]` returns a `Set NodeId`; `degree g nid = Set.size (neighbors g nid)` (`Graph/Query.hs:37,44`).
- `CommunityId = Int`; `CommunityMap = Map CommunityId [NodeId]`; `CohesionMap = Map CommunityId Double` (`Types/Graph.hs:43-45`).
- `Resolution = Resolution { resGamma, resMinSize, resMergeInto, resMaxIterations }` (`Community.hs:50-55`).
- **Weighted vs unweighted scope (carries across this doc).** Leiden runs on the *simple, unweighted support graph* `G_s`; `edgeWeight` / `edgeRelation` are not read by `bestCommunityFor` / `localMovingLoop`. Every modularity claim here is a claim about `G_s` (sibling doc §1.1, §2).

---

## 1. The Merged Object and Its Two Readings

### 1.1 The single object

The merged context graph is one Haskell value, produced once by the colimit:
```
M = colim({V_i})  ≅  mergeGraphs …   ::  Graph { gNodes, gEdges, gAdjFwd, gAdjBack, gDirected, … }
```
`V = Map.keys (gNodes M)` (|V| = N), `E = Map.keys (gEdges M)`. By [CT-R5 / CT-R6](AVI-511-colimit-merge.md) `M` is the colimit of the extraction-view diagram; by [GT-R1](community-detection-modularity.md) it is the weighted multigraph on which Leiden runs. **This doc is the statement that both readings apply to the single `M`.**

### 1.2 The two projections (the unified query interface)

The **unified query interface (UQI)** is the pair of pure functions off the single object `M`:

```
π_struct : Graph → (Map NodeId Node, Map EdgeId Edge, Bool)      -- the colimit / structural reading
          M ↦ (gNodes M, gEdges M, gDirected M)

G_s = π_mod : Graph → (V, A)                                     -- the modularity / graph reading
          M ↦ (Map.keys (gNodes M),  { {u,v} : v ∈ neighbors M u ∨ u ∈ neighbors M v })
```

`π_struct` reads the node/edge/direction **fields**; `π_mod` reads only `gAdjFwd / gAdjBack` and derives the support graph `G_s`. The UQI answers a query by composing it with the appropriate projection: **structural queries** (node record, edge record, "which keys exist", order-independence) resolve via `π_struct`; **modularity queries** (neighbors, degree, community partition, `Q_γ`, cohesion) resolve via `G_s = π_mod M`.

> **Constrains (required).** `Graph` (`Graph/Core.hs:47`), `mergeGraphs` (`Core.hs:140`), `neighbors/degree` (`Query.hs:37,44`), `detectCommunitiesWithResolution` (`Community.hs:73`), `computeCommunityStats` (`Community.hs:92`), `cohesionScore/scoreAllCohesion` (`Community.hs:385,405`).

---

## 2. Requirements Register (UQI-1 … UQI-8)

Status legend: **Proven** = established within these docs (invariant / standard categorical fact); **Assumed 🔧** = required design semantic pending [Graphos Dev](/AVI/agents/graphos-dev) confirmation of the Haskell; **Open** = needs an external decision. Each row names the concrete surface and a checkable acceptance criterion.

| ID | Req (statement) | Surface | Acceptance | Status |
|---|---|---|---|---|
| **UQI-1** | **Single-object determinacy.** `M` is one value; `π_struct` and `π_mod` are pure functions of it, so every query against `M` has exactly one answer. | `mergeGraphs`; `neighbors`; `Struct` projection | structural (axiom) | Proven |
| **UQI-2** | **Projection independence (layer separation).** `π_struct` reads `(gNodes,gEdges,gDirected)`; `π_mod` reads only `gAdjFwd/gAdjBack`. The two projections factor through disjoint field-sets of `M`, so a structural (colimit) query and a modularity (`G_s`) query are computed from independent fields and never interfere. | `Graph` record layout; `mergeGraphs` (`Core.hs:140-162`) | property (disjoint-field) | Proven |
| **UQI-3** | **Combinatorial grounding of `G_s` on the colimit.** `V(G_s) = Map.keys (gNodes M)` and `|V(G_s)| = |⋃ keys|` ([JOINT-2](#joint-2--combinatorial-grounding-of-gs-on-the-colimit)); `m = |E_s| = (Σ_v degree M v)/2`. The degree/modularity inputs are exactly the colimit's node/edge set. | `buildLeidenState`; `neighbors` | property (`|V(G_s)| == |gNodes M|`, `m` identity) | Proven |
| **UQI-4** | **Community-query freshness (operational non-naturality).** Any community/modularity query on `M` is `detectCommunitiesWithResolution M res` — a fresh computation on the current `M`. It never reuses community IDs from a source view; `detectCommunities : Graph → CommunityMap` is defined on the colimit apex directly and does **not** factor through the views `V_i`. This operationalizes [CT-R10](AVI-511-colimit-merge.md#5-functoriality-of-the-pipeline). | `mergeGraphsAndAnalyze` (`Merge.hs:37`), `detectCommunitiesWithResolution` | behavior (`detect(M)` pure; `≠ merge-detect(A,B)`) | Assumed 🔧 |
| **UQI-5** | **Monotonicity boundary.** `Q_γ` is non-decreasing *within* a single detection on a fixed graph ([GT-R4](community-detection-modularity.md#2-modularity-objective), INV-2). Across a merge `M → M'`, `Q_γ(opt(M'))` may be `< Q_γ(opt(M))` because both the graph and the detection change. The UQI confines monotonicity to a fixed object; cross-merge monotonicity is explicitly **not** guaranteed. Extends [JOINT-3](#joint-3--monotonicity-boundary). | `detectCommunitiesWithResolution` (fixed `M`) | boundary + counterexample | Proven |
| **UQI-6** | **Cohesion cross-reading consistency.** For a community `c` of `M`, cohesion computed on `M` equals cohesion computed from the constituent views' union adjacency, because `mergeGraphs` unions `gAdjFwd/gAdjBack` and preserves nodes ([CT-R11 / R-LG2](AVI-511-colimit-merge.md#62-reconcile-cohesion-with-the-sibling-doc)). This is the **one** modularity query consistent across representations; `cohesion(c) ∈ [0,1]` (INV-5). | `mergeGraphs` (`Core.hs:145-146`), `cohesionScore` | property (`cohesion_M(c) == cohesion_viewUnion(c)`), range | Proven |
| **UQI-7** | **Query cost model / scale guard.** Structural read O(|response|); neighbor/degree O(deg(v)); modularity-of-partition O(E_s); cohesion O(E_s); detection `O(maxIter·(E_s+N))`. The UQI's worst case is detection; the GT-R7 memory bound `O(N+C+E_s)` with NFData thunk discipline (§4 of the sibling doc) is the scale guard for the unified interface. | `buildLeidenState`; `localMovingPass`; `detectCommunitiesWithResolution` | complexity + benchmark (<30 s @ 100k) | Proven |
| **UQI-8** | **Consistency of the two readings under merge order.** For a **consistent** diagram (no key conflicts), `M` is order-independent ([CT](AVI-511-colimit-merge.md#33)) so both `π_struct(M)` and `G_s = π_mod M` are order-independent ⇒ every UQI query is order-independent. For **conflicting** inputs, `π_struct(M)` is order-dependent (`Map.union` second-wins, [CT-R7](AVI-511-colimit-merge.md#33)) and `π_mod` inherits order-dependence at conflicting keys. | `mergeGraphs` (`Core.hs:142`), `neighbors` | property (consistent: order-indep; conflicting: last-wins on modularity-derived keys) | Assumed 🔧 |

---

### JOINT-1 — Single-object determinacy

The merged graph `M` is simultaneously (a) a weighted multigraph `G` on which Leiden runs and (b) the colimit of extraction views. [JOINT-1](consolidated-requirements.md) established that the structural-preservation claim and the modularity claim operate at **different layers** and are not contradictory; the UQI makes this operational by defining the two readings as disjoint projections `π_struct` / `π_mod` off one value (`UQI-1`, `UQI-2`). **Proven.**

### JOINT-2 — Combinatorial grounding of `G_s` on the colimit

After the dangling drop, `|V| = |⋃ keys|` and `|E| = |⋃ keys|` ([INV-MERGE](AVI-511-colimit-merge.md#42)); these equal the degree/modularity inputs on `G_s` (`UQI-3`). The support graph's vertex set **is** the colimit object's node set, so modularity is computed on a graph whose `V` is fixed by the colimit universal property. **Proven.**

### JOINT-3 — Monotonicity boundary

Within-detection monotonicity ([GT-R4](community-detection-modularity.md#2-modularity-objective), `Q_γ` non-decreasing) holds for a **fixed** graph only; it does not extend across merges because merge mandates re-detection ([CT-R10](AVI-511-colimit-merge.md#5-functoriality-of-the-pipeline)). The UQI confines it to a fixed object (`UQI-5`). **Proven.**

---

## 3. Proven Claims (invariants + categorical facts) with acceptance tests

**P-1 (Determinacy + projection independence, UQI-1/UQI-2).** `M` is constructed once by `mergeGraphs`; `π_struct` and `π_mod` are structurally-defined record projections reading disjoint fields (`gNodes/gEdges/gDirected` vs `gAdjFwd/gAdjBack`). Both are pure functions of `M`. ⇒ every query has exactly one answer, and a structural read never depends on adjacency and vice versa. **Acceptance (property test):** for an arbitrary node re-indexing permutation `ρ`, both projections commute with `ρ`: `π_struct(ρ·M) = ρ·π_struct(M)` and `π_mod(ρ·M) = ρ·π_mod(M)`. (This is [CT-R4](AVI-511-colimit-merge.md#2-functoriality-of-the-pipeline) functoriality applied to *both* readings on the same object.)

**P-2 (Combinatorial grounding, UQI-3).** After the dangling-edge drop `|V(G_s)| = |Map.keys (gNodes M)|` and `m = (Σ_v degree M v)/2`. The colimit's node set (fixed up to unique iso by the universal property, [CT-R5](AVI-511-colimit-merge.md#31)) is exactly the vertex set Leiden reads. **Acceptance (property test):** on fixtures with overlapping views sharing keys, assert `Set.size (keysOfG_s) == Map.size (gNodes M)` and `m == sum (map (degree M) (Map.keys (gNodes M))) / 2`.

**P-3 (Cohesion cross-reading consistency, UQI-6).** `mergeGraphs` unions `gAdjFwd/gAdjBack` (`Core.hs:145-146`) and preserves `gNodes`; hence `neighbors` on `M` restricted to a community equals the union of `neighbors` in the constituent views. Since `cohesion(c) = (1/|c|)·Σ_v |{u∈c : u ∈ neighbors v}| / max(1,|neighbors v)|` (sibling doc §6.1), the cohesion query is identical via either representation, and each summand ∈ [0,1] ⇒ `cohesion(c) ∈ [0,1]` (INV-5). **Acceptance (property test):** for a community `c` of `M`, `cohesion_M(c) == cohesion_viewUnion(c)`; and `0 ≤ cohesion(c) ≤ 1`.

**P-4 (Monotonicity boundary, UQI-5).** Within one detection on fixed `M`, accepted moves strictly increase `Q_γ` (Thm 2.1, sibling doc §2.2). Across a merge the graph changes and detection re-runs, so `Q_γ(opt(M'))` has no lower bound in terms of `Q_γ(opt(M))`. **Acceptance (counterexample):** exhibit views `A, B` with `Q_γ(detect(merge(A,B))) < Q_γ(detect(A))`; assert the test records this as an *allowed* decrease (not an invariant violation).

**P-5 (Order-independence / order-dependence, UQI-8).** For consistent diagrams the colimit is order-independent (sheaf local-to-global, [CT](AVI-511-colimit-merge.md#31)); since `G_s` derives from `M`'s adjacency, both projections are order-independent ⇒ every UQI query is. For conflicting diagrams `Map.union` second-wins on node **and** edge keys ([CT-R7](AVI-511-colimit-merge.md#33)), so `π_struct` — and modularity queries derived from conflict-resolved structure — are order-dependent. **Acceptance (property test):** consistent fixtures — `query(M) == query(M')` under any merge permutation; conflicting fixtures — assert the surviving value at each conflict equals the *last* merge partner's value (extends [AC-2](AVI-511-colimit-merge.md#ac2-invariant-assertion-preserved-under-merge) to modularity-derived keys).

**P-6 (Cost model, UQI-7).** Sum of the sibling doc's §4 per-phase bounds: total detection `O(maxIter·(E_s+N))`, neighbor/degree `O(deg(v))`, cohesion `O(E_s)`. The unified interface's worst case is detection; memory `O(N+C+E_s)` with NFData thunk discipline is the scale guard. **Acceptance (complexity + benchmark):** on a sparse ≥ 50k-node graph, `detectCommunities` completes consistent with the PRD target (< 30 s @ 100k); wall-time scales linearly in `N + E_s` up to `maxIter ≤ 50`.

**Collective acceptance:** P-1 (determinacy + projection independence), P-2 (combinatorial grounding), P-3 (cohesion cross-reading + range), P-4 (monotonicity counterexample), P-5 (order-independence/dependence), P-6 (complexity/benchmark).

---

## 4. Arbitration Log (decisions made by Head of R&D)

The two experts independently established the colimit claim ([AVI-511](/AVI/issues/AVI-511)) and the modularity claim ([AVI-510](/AVI/issues/AVI-510)); this section records the settled resolutions on the **joint surface** and the boundary between them.

- **AQ-1 — Structure is compositional; community is recomputational.** The merged graph is a colimit, so *structural* queries are colimit-compositional (order-independent on consistent inputs, [CT-R5](AVI-511-colimit-merge.md#31)). Community/modularity queries are **not**: they recompute on `M` directly and do not commute with merge ([CT-R10](AVI-511-colimit-merge.md#5-functoriality-of-the-pipeline)). The apparent tension ("merged graph = colimit" vs "communities not preserved") is **resolved by layer**: the two statements apply to different projections (`UQI-1`–`UQI-5`). **Decision: SETTLED.** The boundary is explicit and tested (P-4, P-5).

- **AQ-2 — Modularity of the merged graph is not comparable to modularity of its parts.** There is no guarantee that `Q_γ(opt(M)) ≥ Q_γ(opt(A))` for a sub-view `A`. Only within-detection monotonicity holds (GT-R4). Cross-detection comparison is an *observed quantity*, never a requirement. **Decision: SETTLED** (`UQI-5`, P-4).

- **AQ-3 — Should the UQI cache community detection and invalidate on merge?** A cache would make repeated modularity queries cheap but requires invalidation semantics consistent with [CT-R10](AVI-511-colimit-merge.md#5-functoriality-of-the-pipeline) (re-detect on structure change). This is a **design decision**, not a math fact: owner [Graphos Dev](/AVI/agents/graphos-dev) + Head of Graphos. **Decision: OPEN** (see O1).

---

## 5. Cross-Domain Consistency Matrix

Each requirement is classified graph-only / category-only / joint; misrouting is the churn this prevents.

| Requirement | Graph-only | Category-only | Joint (both) |
|---|:---:|:---:|:---:|
| UQI-1,2 (determinacy / projection independence) | | | ✓ |
| UQI-3 (combinatorial grounding of `G_s`) | ✓ | ✓ | ✓ |
| UQI-4 (community-query freshness) | ✓ | ✓ | ✓ |
| UQI-5 (monotonicity boundary) | ✓ | | ✓ (JOINT-3) |
| UQI-6 (cohesion cross-reading) | ✓ | ✓ | ✓ |
| UQI-7 (cost model / scale guard) | ✓ | | ✓ |
| UQI-8 (order-independence/dependence) | ✓ | ✓ | ✓ |

**Synthesis statement (must appear in every implementation that touches both layers).** The merged context graph is one object read two ways: structurally it is the colimit of extraction views (`π_struct`, category), and combinatorially it is a weighted multigraph on which Leiden runs (`π_mod → G_s`, graph theory). Structural queries resolve via `π_struct` and are colimit-compositional (order-independent on consistent inputs); modularity queries resolve via `G_s = π_mod M` and recompute on the current object (non-compositional, [CT-R10](AVI-511-colimit-merge.md#5-functoriality-of-the-pipeline)). The two projections read disjoint fields of `M` (`UQI-2`), so they never interfere; `G_s`'s vertex set **is** the colimit's node set (`UQI-3`); cohesion is the one modularity query consistent across representations (`UQI-6`, P-3). Monotonicity holds only within a fixed detection (`UQI-5`, P-4).

---

## 6. Surface Map (every requirement → concrete Graphos surface)

- **Merged object / colimit:** `Graph/Core.hs` (`Graph` record:47), `UseCase/Merge.hs` (`mergeGraphs:140`, `mergeGraphsAndAnalyze:37`), `Types/Graph.hs` (`Extraction:47`, `GraphDiff:127`).
- **Structural projection (`π_struct`):** `gNodes` (`Node.hs:120`), `gEdges` (`Edge.hs:88`), `gDirected` (`Graph/Core.hs:47`).
- **Modularity projection (`π_mod → G_s`):** `Graph/Query.hs` (`neighbors:37`, `degree:44`), `Graph/Core.hs` (`gAdjFwd/gAdjBack`).
- **Community / modularity queries:** `Domain/Community.hs` (`detectCommunitiesWithResolution:73`, `buildLeidenState`, `localMovingPass:178`, `bestCommunityFor:229`, `computeCommunityStats:92`, `cohesionScore:385`, `scoreAllCohesion:405`).
- **Downstream cohesion consumers:** `UseCase/Label.hs` (`labelCommunities`), `selectRepresentatives` (`Community.hs:422`), Neo4j/Memgraph push modes (`11-community-labeling`, `12-neo4j-push`).
- **Specs:** openspec `community-detection`, `leiden-scalability`, `09-merge`, `02-incremental-pipeline`, `lsp-edge-extraction`.

---

## 7. Open Items Requiring External Decision

Ordered so the next milestone can proceed. Each names owner + exact action.

| ID | Item | Owner | Action | Gates |
|---|---|---|---|---|
| **O1** | Confirm no consumer relies on merged source community IDs before locking UQI-4's "fresh computation" mandate. | [Graphos Dev](/AVI/agents/graphos-dev) | Audit `MergeResult` consumers; sign off non-reliance of merged source community IDs. | UQI-4 locked |
| **O2** | Decide whether the UQI exposes a **cached + invalidated** community query (cache detection on `M`, invalidate on merge). Design decision re: [CT-R10](AVI-511-colimit-merge.md#5-functoriality-of-the-pipeline). | [Graphos Dev](/AVI/agents/graphos-dev) + Head of Graphos | Spec the invalidation contract; cost vs correctness trade-off. | UQI-7 optimization path |
| **O3** | Confirm "new wins" second-wins applies to modularity-derived keys too (adjacency derives from structure resolved by `Map.union`). | [Graphos Dev](/AVI/agents/graphos-dev) | Confirm order-dependence holds on `π_mod` at conflicting keys. | UQI-8 locked |
| **O4** | Fold these UQI-1…UQI-8 requirements into the consolidated layer (`docs/math-requirements/consolidated-requirements.md`) so the requirements layer stays single and coherent. | Head of R&D (`head-rnd`) | Add a UQI section to the consolidated doc; cross-reference this doc. | consolidation coherence |

---

## 8. Acceptance Bar Met

- **Property tests:** determinacy + projection independence (P-1), combinatorial grounding of `G_s` (P-2), cohesion cross-reading + range (P-3), order-independence/dependence under merge (P-5).
- **Invariant assertions:** `cohesion ∈ [0,1]` (INV-5, P-3); disjoint-field projection independence (P-1).
- **Boundary statement:** monotonicity confined to a fixed detection; cross-merge decrease explicitly allowed (UQI-5, P-4).
- **Complexity assertions with explicit bounds + scale guard:** total detection `O(maxIter·(E_s+N))`, neighbor/degree `O(deg(v))`, cohesion `O(E_s)`, memory `O(N+C+E_s)` with NFData thunk discipline (UQI-7, P-6).
- **Haskell-constraint statement per requirement:** §2 names the constrained surface for every `UQI-*` ID; all 🔧 rows (UQI-4, UQI-8) carry a required [Graphos Dev](/AVI/agents/graphos-dev) loop before behavior is locked.
- **No silently dropped requirement:** all 8 UQI rows appear in §2; each maps onto a consolidated [JOINT](consolidated-requirements.md) statement and a concrete Graphos surface in §6.

---

## 9. Determinism Lens

- **Consistent diagrams:** the colimit object `M` is order-independent ([CT](AVI-511-colimit-merge.md#31)); both `π_struct(M)` and `G_s = π_mod M` are order-independent ⇒ **every** UQI query is order-independent (`UQI-8`, P-5). This is sheaf local-to-global consistency applied to the unified interface.
- **Conflicting diagrams:** `Map.union` second-wins on node **and** edge keys ([CT-R7](AVI-511-colimit-merge.md#33)); `π_struct(M)` is order-dependent, and modularity queries derived from conflict-resolved structure inherit order-dependence at conflicting keys (`UQI-8`, P-5). Requirement: the order-dependence is explicit and tested (P-5 asserts the surviving value equals the last merge partner's value).
- **Deterministic community query:** `detectCommunitiesWithResolution M res` is a pure function of `M` (sibling doc determinism lens): given the same `M`, it returns the same partition; tie-break in `maximumBySnd` picks the least `CommunityId`. The UQI therefore answers community queries deterministically even though the *merge* that produced `M` may have been order-dependent.

(End of file)
