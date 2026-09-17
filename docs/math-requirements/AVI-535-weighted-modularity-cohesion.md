# Weighted Modularity & Cohesion — Mathematical Requirements (Graphos Context Graph)

**Author:** Graph Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-535](/AVI/issues/AVI-535) (child of [AVI-512](/AVI/issues/AVI-512), Graph Theory element G4 / GT-04)
**Status:** requirements document — grounded in domain model + openspec specs
**Scope:** math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion. The wiring itself is [Graphos Dev](/AVI/agents/graphos-dev)'s feasibility-gated implementation (remit item 1/3).

> **Relationship to G2 (AVI-533).** This doc *generalizes* the unweighted auditable modularity `Q` and the spec cohesion of [AVI-533](/AVI/issues/AVI-533) to `Confidence`-weighted edges. Every definition below reduces to the G2 quantity when all confidences are equal (§2.2, §4.3). Read AVI-533 §1–§3 first; only the new weighted objects are restated here.

---

## 0. Notation (shared glossary)

Notation is inherited verbatim from [AVI-533](/AVI/issues/AVI-533) (§0) and extended below with the weighted objects this doc introduces. Line numbers verified against checked-in sources.

- `NodeId = Text` (`Node.hs:46`); `CommunityId = Int`; `CommunityMap = Map CommunityId [NodeId]` (`Types/Graph.hs:41-45`).
- `Edge = Edge { edgeId, edgeSource, edgeTarget, edgeRelation, edgeWeight, edgeConfidence, edgeExtra }` with `Confidence = Confidence Double` (`Types/Edge.hs:88-95`). `edgeConfidence` is the **signal-strength** weight exercised by this doc; `edgeWeight :: Double` is a *separate* semantic edge attribute (§8 AF-1).
- `Graph = Graph { gNodes, gEdges :: Map (NodeId,NodeId) Edge, gAdjFwd, gAdjBack :: Map NodeId (Set NodeId), gDirected, … }` (`Graph/Core.hs:47-52`).
- `neighbors g nid = if gDirected g then gAdjFwd[nid] else gAdjFwd[nid] ∪ gAdjBack[nid]` returns a `Set NodeId`; `degree g nid = Set.size (neighbors g nid)` (`Graph/Query.hs:37,44`).
- **Undirected storage convention (verified).** `buildGraph` stores exactly one record per ordered pair in `gEdges` (`Core.hs:103`) and symmetrizes adjacency for undirected graphs via `gAdjBack = fwdAdj <> reverse(fwdAdj)` (`Core.hs:106-111`). Hence an undirected edge `{u,v}` appears as a single record in *either* direction; the symmetrized neighbor set of `u` is `neighbors g u`. No parallel edges exist per ordered key.
- `computeCommunityStats :: Graph -> Map NodeId CommunityId -> CommunityStats` produces `csSigmaIn/csSigmaTot/csDegrees/csM` on the support graph (`Community.hs:85-106`) — already produced by the running pipeline; this doc **consumes** it for the unweighted baseline and provides weighted analogs.
- `cohesionScore :: Graph -> [NodeId] -> Double` (`Community.hs:385`) and Leiden's unweighted gain `bestCommunityFor … deltaQ` (`Community.hs:229-244`).
- `getConfidence :: Confidence -> Double` — **does not yet exist**; the implementation must pattern-match `Confidence w <- e` (`Types/Edge.hs:77`). Edges without an explicit confidence default to `1.0` (`Graph/Mutation.hs:302-303`), i.e. unit confidence is the *default*, which is exactly the reduction fixture.

**Weighted-vs-unweighted scope (inherited from AVI-533).** Leiden and `computeCommunityStats` act on the simple undirected support graph `G_s`; `edgeWeight`/`edgeConfidence` are never read by `bestCommunityFor`, `localMovingLoop`, or `computeCommunityStats`. G4 adds **weighted** analogs of those quantities; the unweighted pipeline is unchanged unless a feasibility decision (§8) reweights it.

---

## 1. Formal Definitions

### 1.1 Confidence weight matrix

For a graph `G = (V, E)` with `V = Map.keys (gNodes g)` and edge-relation `E ⊆ V × V` the ordered-pair projection of `gEdges`, define the **per-ordered-pair confidence weight**:

```
W(u,v) = Σ { getConfidence(edgeConfidence e) : e ∈ gEdges, (edgeSource e, edgeTarget e) = (u,v) }
```

Since `gEdges` is keyed by `(source,target)` with at most one record per ordered pair, `W(u,v) ∈ {0} ∪ { w(e) }`. For the undirected support that modularity/cohesion act on, **symmetrize**:

```
A_uv = W(u,v) + W(v,u)            (u ≠ v)          -- total confidence across both stored directions
A_uu = W(u,u)                                            -- self-loop counted once
```

`A` is symmetric and nonnegative (`getConfidence ≥ 0`, confidences are nonnegative reals). `A_uv > 0 ⟺ {u,v}` is an edge of the undirected support `G_s`.

> **Constrains:** `gEdges` (`Types/Edge.hs`), symmetrization over `gAdjFwd/gAdjBack` (`Core.hs:106-111`).

### 1.2 Weighted degree and weighted edge mass

```
d_u   = Σ_v A_uv                       -- weighted degree of u
2 m_w = Σ_u d_u = Σ_{u,v} A_uv = 2 · Σ_{e ∈ gEdges} getConfidence(edgeConfidence e)
m_w   = Σ_{e ∈ gEdges} getConfidence(edgeConfidence e)        -- weighted edge mass
```

Under uniform confidence `w₀`, `m_w = w₀ · |gEdges| = w₀ · m` where `m` is the G2 unweighted support edge count (`Community.hs:95`).

### 1.3 Weighted community aggregates

For a partition `π : V → C` (encoded as `CommunityMap`) and each community `c ∈ range(π)`:

```
σ_in^w(c)   = Σ_{u,v ∈ c} A_uv                         = 2 · (total confidence of internal undirected edges of c)
σ_tot^w(c)  = Σ_{v ∈ c} d_v                              = weighted total-degree of c
```

**Domain mapping.** These are the weighted analogs of `computeCommunityStats`: replace the constant `2.0` per internal edge-record (`Community.hs:100-105`) with `2 · getConfidence(edgeConfidence e)`, and replace the unweighted node degree (`Community.hs:94-99`) with the weighted degree `d_v`. Under uniform `w₀`: `σ_in^w(c) = w₀·σ_in(c)`, `σ_tot^w(c) = w₀·σ_tot(c)`.

### 1.4 Null model (specified)

The **configuration (stochastic-block) null model**, the standard choice for modularity (Newman 2010):

```
P_uv = d_u · d_v / (2 m_w)
```

> **Constrains:** the null-model surface must be documented (`shall-null-model` below); the implementation records which null model is used in the export and in the graph metadata.

### 1.5 Weighted modularity functional

```
Q_w(γ) = (1 / 2m_w) · Σ_{u,v} ( A_uv − γ · P_uv ) · δ(c_u, c_v)
       = Σ_c [ σ_in^w(c) / (2 m_w)  −  γ · σ_tot^w(c)² / (2 m_w)² ]          (*)
```

where `δ(c_u,c_v) = 1` iff `c_u = c_v`, and `γ` is the resolution (§2.3).

> **Constrains:** new export `modularityWeighted :: Graph -> CommunityMap -> Double` (γ=1) consuming the weighted analogs of `computeCommunityStats`; consumes `neighbors`/`degree`/`gEdges`.

### 1.6 Weighted cohesion

For a community `c` (member set `M_c`):

```
cohesion_w(c) = (1 / |c|) · Σ_{i ∈ M_c} [ in_w(i) / deg_w(i) ]
in_w(i)   = Σ_{j ∈ M_c , j ∈ neighbors g i} A_ij      -- internal incident confidence weight
deg_w(i)  = d_i                                          = total incident confidence weight
```

Convention: `deg_w(i) = 0 ⟹ in_w(i)/deg_w(i) := 0` (isolated node contributes 0).

> **Constrains:** new/modified cohesion in `Community.hs`; consumes `neighbors`, `gEdges`, `d_u`.

---

## 2. Weighted Modularity Objective

### 2.1 The functional (§1.5) and why the squared σ_tot term

The second term of `(*)` is the null-model contribution of the configuration model. Summing `P_uv` over ordered pairs inside a community gives `Σ_{u,v∈c} P_uv = (Σ_{v∈c} d_v)² / (2m_w) = σ_tot^w(c)² / (2m_w)`, so `(1/2m_w)·γ·Σ_{u,v∈c} P_uv = γ·σ_tot^w(c)²/(2m_w)²`. This is the weighted generalization of the AVI-533 `(*)` squared-`σ_tot` term (which itself corrected the un-squared typo in the issue text). Cite Newshyld 2004 / Newman 2010.

### 2.2 Reduction theorem (the consolidation constraint)

**Theorem 2.1 (reduction to G2).** For resolution `γ = 1`, if every edge carries a *common* confidence weight `w₀ > 0` (i.e. `getConfidence(edgeConfidence e) = w₀` for all `e`), then `Q_w(π) = Q(π)` for every partition `π`, where `Q` is the AVI-533 auditable unweighted modularity on `G_s`.

> **Scope (AVI-822 arbitration — see §8 AV-ARB-1).** The equality holds for every **simple** undirected partition — i.e. where each undirected pair is stored as a single `gEdges` record so that edge-record incidence equals distinct-neighbor support degree (`m` at `Community.hs:105`, `σ_in` at `Community.hs:110`). On a bidirectional pair stored as two records the two conventions diverge; INV-CONSTR is scoped to simple graphs there.

**Proof sketch.** Substitute the linear scalings from §1.2–1.3: `A_uv = w₀·a_uv`, `d_u = w₀·K_u` (`K_u` = support degree), `σ_in^w(c) = w₀·σ_in(c)`, `σ_tot^w(c) = w₀·σ_tot(c)`, `2 m_w = w₀·(2m)`. Every term of `(*)` is a ratio whose numerator and denominator scale by the same power of `w₀`:

```
σ_in^w(c)/(2m_w)      = (w₀ σ_in(c)) / (w₀·2m)     = σ_in(c)/(2m)
γ·σ_tot^w(c)²/(2m_w)² = (w₀² σ_tot(c)²) / (w₀²(2m)²) = σ_tot(c)²/(2m)²   (γ = 1)
```

The `w₀` cancels in each ratio, so `Q_w = Q`. ∎

> **Corollary (unit-confidence case).** The issue's "unit confidence" requirement (`w₀ = 1`) is the special case; reduction holds for *any* uniform `w₀`, so it is strictly stronger than required. Because absent confidences default to `1.0` (`Mutation.hs:303`), a graph whose edges are all at the default confidence yields `Q_w(γ=1) = Q` exactly — this is the regression fixture (§7 scenario 3).

> **INV-CONSTR (consolidation constraint).** `Q_w(γ=1)` under equal confidences `≡` AVI-533 `Q(γ=1)`. This is the **contradiction-scan input** for the Head of R&D: if an implementation of `modularityWeighted` fails to reduce to `Q`, it is a genuine contradiction to arbitrate, not an open design choice.
>
> **Scope (AVI-822 arbitration).** INV-CONSTR holds on **simple undirected graphs** (one record per undirected pair), where edge-record incidence `=` distinct-neighbor support degree and both families coincide. On a **bidirectional pair** (`a→b` AND `b→a` stored as two records, single community `{a,b}`, uniform confidence `1`) it does **not**: the legacy `modularity` mixes record-count `σ_in = 4` with support-degree `m = σ_tot = 2`, giving `Q = 4/2 − (2/2)² = 1.0`, whereas the internally-coherent weighted family gives `Q_w = 4/4 − (4/4)² = 0.0`. The weighted family is the citable Newman (2010) weighted modularity and is kept standard-correct; the legacy `1.0` is an artifact of its internal convention mismatch on multigraphs, not a real community signal. **Verdict: option (a)** — scope INV-CONSTR to simple graphs, pin the divergence in a characterization test (§7 scenario 9), and flag the legacy convention as a separate AVI-533 defect.**

### 2.3 Resolution and well-definedness

`γ` is the resolution parameter already threaded through `Resolution.resGamma` (`Community.hs:50-68`; default `1.0`). `Q_w` is defined iff `m_w > 0` (graph has ≥1 edge). If `m_w = 0` (no edges / isolated nodes only), pin `Q_w = 0.0` — matches `bestCommunityFor`'s `m ≤ 0` guard (`Community.hs:232`) and INV-Q0 of AVI-533. No division by zero.

---

## 3. Invariants — Weighted Modularity (each QuickCheck-ready)

Write `M s = Data.Set.fromList s`. Fixtures are small deterministic graphs via `extractionFromLists`/`buildGraph` (cf. `tests/Graphos/Domain/CommunitySpec.hs`).

### INV-Qw0 (no divide-by-zero / edge case)
`modularityWeighted g cm = 0.0` whenever `m_w = 0`, i.e. every edge has weight 0 or `gEdges` is empty.
> **Constrains:** `modularityWeighted` (`(*)` + `m_w == 0` guard).

### INV-Qw1 (range bound — undirected support only)
For `¬gDirected g` and any reachable `cm`, `-0.5 ≤ Q_w ≤ 1`. Upper bound `1` approached by disjoint cliques joined by bridges; lower bound `-1/2` asymptotically (Newman 2010, nonnegative symmetric `A`).
> **Constrains:** `modularityWeighted` on undirected `G_s`.
> **Honesty caveat.** For `gDirected = True` the bound does **not** hold (as with G2 — directed modularity has no `-1/2` floor). Directed graphs are out-of-scope for INV-Qw1; any directed `Q_w` report must be marked "bound not guaranteed".

### INV-Qw2 (partition-only — order independence)
`Q_w` depends only on the partition `{c}` and `A`, not on member-list order or `CommunityMap` key order. For any permutation of members within every community, `modularityWeighted g cm == modularityWeighted g (reordered cm)`.
> **Constrains:** `modularityWeighted` must sum over set-valued weighted aggregates; never depend on list order.

### INV-CONSTR (reduction to G2 — the consolidation constraint)
For any **simple** undirected fixture (each undirected pair stored as a single record) and every uniform-confidence scaling `w₀ ∈ {0.25, 1.0, 4.0}` (test several), `Q_w(γ=1) == Q(γ=1)` computed by the AVI-533 `modularity`. Strict equality of `Double`s after both are built from the same partition. **Excludes bidirectional-pair (multigraph) fixtures**, where the divergence is pinned by scenario 9 (§7) rather than required to vanish.
> **Constrains:** `modularityWeighted` (§2.2); cross-checked against AVI-533 `modularity`.

### INV-Qw3 (deterministic summation order)
`modularityWeighted` is a deterministic pure function of `(g, cm)`. Because `Double` addition is not associative, the fold over communities MUST use a fixed order (e.g. ascending `CommunityId` via `Data.Map`). Assert `Map.elems` order is the sole summation order.
> **Constrains:** fold order; determineness lens.

### INV-Qw4 (canonical fixtures pin exact values)
- Single community containing all nodes, no dangling edges ⇒ `Q_w = 0`.
- All-singletons partition ⇒ `Q_w = −(1/2m_w)²·Σ_v d_v² < 0` (strictly negative for any graph with ≥1 weighted edge).
> **Constrains:** `modularityWeighted` on the pinned fixtures (§7 scenarios 1 & 2).

### INV-Qw5 (monotone local move — scoped caveat)
The **auditable** `Q_w` is monotone under a *weighted* local-move pass (`σ_in^w/m_w − γ d_u d_v/(2m_w)²` gain). The **current** Leiden uses the *unweighted* gain (`Community.hs:236`), so INV-Qw5 holds for the auditable measure only if a separate weighted-move pass is implemented. **Scope:** G4 does not require reweighting Leiden's internal search (§8 AF-2); INV-Qw5 is listed for completeness and is an optional future item.
> **Constrains:** only if a weighted Leiden optimizer is added.

> **Constrains (all):** `modularityWeighted :: Graph -> CommunityMap -> Double` (`Community.hs` or `Graph/Analysis.hs`, §8 AF-4); consumes `computeCommunityStats`-analog, `neighbors`, `degree`, `gEdges`.

---

## 4. Weighted Cohesion

### 4.1 Formula (§1.6) and range

**Theorem 4.1 (range).** For every community `c`, `0 ≤ cohesion_w(c) ≤ 1`.
**Proof sketch.** For each node `i`, `0 ≤ in_w(i) ≤ deg_w(i)` because the internal incident weight is a subset of all incident weight; with the `deg_w(i)=0 ⟹ 0` convention each summand lies in `[0,1]`. An average of values in `[0,1]` lies in `[0,1]`. ∎

### 4.2 Reduction theorem

**Theorem 4.2 (reduction to spec cohesion).** Under uniform confidence `w₀ > 0`, for every node `i` the ratio `in_w(i)/deg_w(i) = (w₀·in(i))/(w₀·deg(i)) = in(i)/deg(i)` is scale-invariant, so `cohesion_w(c) = (1/|c|)·Σ_{i∈c} (in(i)/deg(i))` = the **spec cohesion formula** `cohesion(c) = Σ_{i∈c}(neighbors_in_c(i)/total_neighbors(i)) / |c|` (`community-detection` spec.md, Requirement "cohesionScore and scoreAllCohesion").

> **INV-C2 (cohesion reduction).** `cohesion_w` under equal confidences ≡ the spec degree-ratio cohesion. (The issue's formula is precisely this; G4 generalizes it to weighted edges.)

### 4.3 Invariants — weighted cohesion

- **INV-C1 (range).** `0 ≤ cohesion_w(c) ≤ 1` for every community (Thm 4.1). QuickCheck: for every reachable `cm`, `cohesion_w ∈ [0,1]`.
- **INV-C2 (reduction).** Under uniform confidence, `cohesion_w == spec-cohesion` (Thm 4.2). Fixture: scale all confidences by `w₀ ∈ {0.5, 2.0}`; assertion holds exactly.
- **INV-C3 (partition-only / order independence).** `cohesion_w` depends only on member *sets*, not on list order or `CommunityMap` key order.
- **INV-C4 (determinism).** Fixed summation order ⇒ bit-equal `Double`s across calls.
- **INV-C5 (well-definedness at isolation).** A node with `deg_w(i)=0` contributes `0` (no divide-by-zero), never `NaN`.
> **Constrains:** `cohesionScore`/new weighted cohesion (`Community.hs:385`).

---

## 5. Termination

Both `modularityWeighted` and `cohesion_w` are **finite sums over a finite partition** (`C < ∞`, each community finite) with **no loop over graph structure and no recursion** on their own. They terminate unconditionally in `O(N + E)` (dominated by the single edge/node pass that builds the weighted aggregates). The only iterative algorithm in scope — Leiden local moving under INV-Qw5 — is the same bounded loop already covered by AVI-533 §5 / AVI-510 (§3.2): a strictly-bounded iteration capped by `resMaxIterations`.

---

## 6. Complexity Bounds

Model: **N** nodes, **E** edge records of `gEdges`, **C** communities; structures `Data.Map.Strict` (`O(log)` access), `Data.Set` adjacency, CSR in the Leiden state.

| Component | Bound | Grounding |
|---|---|---|
| Weighted aggregate build (`d_u`, `σ_in^w`, `σ_tot^w`, `m_w`) | `O(N + E)` | one pass over `gEdges` for `A`/`m_w`/`σ_in^w` (`§1.1-1.3`); one pass over nodes for `d_u`. |
| `modularityWeighted` projection | `O(C)` | single `foldl'` over `C` communities, constant work each. |
| **Total `modularityWeighted g cm`** | **O(N + E)** | sum of above; no iteration/recursion. |
| `cohesion_w` per community | `O(|c| + incident edges)` | one pass over members, each scanning `neighbors`. |
| **Total `scoreAllCohesion`-weighted** | **O(N + E)** | every edge scanned O(1)–O(2) times across communities. |

> **Acceptance (complexity assertion):** weighted modularity/cohesion add only `O(N + E)` — consistent with the Leiden budget `O(maxIter·(E_s + N))` (`leiden-scalability`) and the bounded-edge-inference `O(k log k)` dedup model. No per-community quadratic scan of `gEdges`.

---

## 7. Acceptance-Criteria Mapping & Scenarios

| Issue acceptance criterion | Requirement / fixture | Surface |
|---|---|---|
- Compute weighted modularity over `Confidence`-weighted edges | `modularityWeighted :: Graph -> CommunityMap -> Double`, formula `(*)` (`§1.5`) | `Community.hs` / `Graph/Analysis.hs` |
- Document the null model | `P_uv = d_u d_v/(2m_w)` (`§1.4`), recorded in export + metadata | `shall-null-model` |
- Regression gate: `Q_w` reduces to `Q` under unit confidence | INV-CONSTR (`§2.2`), scenario 3 | `modularityWeighted` vs AVI-533 `modularity` |
- Range `-0.5 ≤ Q_w ≤ 1` on fixtures | INV-Qw1 (`§3`), scenario 4 | `modularityWeighted` on undirected fixtures |
- Weighted cohesion in `[0,1]` | INV-C1 (`§4.3`), scenario 5 | `cohesionScore`/weighted cohesion |
- Cohesion reduces to spec formula under uniform confidence | INV-C2 (`§4.2`), scenario 6 | weighted cohesion |
| `cabal test` gate | scenarios compiled into `tests/Graphos/Domain/CommunitySpec.hs` | test-suite `graphos-test` |

### Scenarios (to compile into the test suite)
1. **Single community ⇒ Q_w = 0.** `cm = Map.fromList [(0, allNodes)]` on a connected undirected graph ⇒ `modularityWeighted g cm == 0`. (INV-Qw4)
2. **All-singletons ⇒ Q_w < 0.** `cm = Map.fromList [(v,[v]) | v <- nodes]`; assert `< 0` and equals `−(1/2m_w)²·Σ d_v²`. (INV-Qw4)
3. **Reduction under unit confidence.** On a fixture with default (`1.0`) confidences, `modularityWeighted g cm == modularity g cm` (AVI-533). Also assert for `w₀ ∈ {0.25, 4.0}` uniform scaling. (INV-CONSTR, §2.2)
4. **Range bound.** For every reachable `cm` on an undirected fixture, `-0.5 ≤ Q_w ≤ 1`. (INV-Qw1)
5. **Cohesion range.** For every reachable `cm`, `0 ≤ cohesion_w ≤ 1`. (INV-C1)
6. **Cohesion reduction.** Scale all confidences by `w₀`; `cohesion_w == spec-cohesion`. (INV-C2)
7. **Weighted ordering matters.** Two fixtures identical in support but differing in confidence produce `Q_w` differing proportionally to signal — i.e. high-confidence internal edges raise `Q_w` relative to unit-confidence baseline (sanity that signal strength is actually used).
8. **Determinism.** Two calls return bit-equal `Double`s (INV-Qw3, INV-C4).
9. **Characterization of the multigraph divergence (AVI-822).** Undirected, single community `{a,b}`, uniform confidence `1`, edges `a→b` AND `b→a` stored as two `gEdges` records: legacy `modularity g cm == 1.0`, weighted `modularityWeighted g cm == 0.0`. This pins the *understood* convention divergence (record-count vs support-degree) rather than asserting a bug; INV-CONSTR is scoped to simple graphs (§2.2, §3). Test asserts both pinned values and that they coincide on the analogous simple graph (single record `a→b`).

---

## 8. Feasibility Answers & Arbitration Flags

This section answers the concrete "is it sound / what is the complexity" questions (remit item 3) and flags decisions requiring [Graphos Dev](/AVI/agents/graphos-dev) or the Head of R&D.

### 8.1 Q: Is weighted modularity sound, and does the reduction invariant hold? — YES, with the configuration null model
`Q_w` with `P_uv = d_u d_v/(2m_w)` is the standard weighted modularity (Newman 2010, Ch. 6). The reduction to G2 (Thm 2.1) is exact for any uniform `w₀`; proof in §2.2. The only arithmetic risk is `Double` non-associativity across the fold — resolved by INV-Qw3 fixed order. **Sound.**

### 8.2 Q: Does `edgeWeight` or `edgeConfidence` weight modularity/cohesion? — CONFIDENCE (per issue), documented
The issue explicitly weights by **signal strength** = `edgeConfidence`. `edgeWeight :: Double` is a distinct semantic attribute (e.g. consumed by path weighting) and must **not** be mixed into the statistical weight without an explicit decision. **Decision (AF-1).**

### 8.3 Q: Does G4 reweight Leiden's internal search? — NO (scoped)
G4 exposes an **auditable** weighted modularity/cohesion (like G2's auditable `Q`); it does **not** require changing Leiden's unweighted gain (`bestCommunityFor`, `Community.hs:229-244`). INV-Qw5 (weighted monotone move) is optional future work. **Decision (AF-2).**

### 8.4 Q: Where does `modularityWeighted` live? — recommend `Community.hs`, flag for Dev+R&D
Natural home is `Community.hs` beside the G2 `modularity` and Leiden's `deltaQ`; the issue text lists `Graph/Analysis.hs`. Both are feasible; placement is a Dev/R&D decision (**AF-4**), not a math question.

### 8.5 Q: Is the current `cohesionScore` the formula G4 generalizes? — NO (spec-vs-code gap)
The current `cohesionScore` (`Community.hs:385`) computes **edge density** `internalEdges / C(|c|,2)`, *not* the spec's degree-ratio formula `Σ_i(neighbors_in_c(i)/total_neighbors(i))/|c|`. G4's formula (§1.6) matches the **spec** degree-ratio form. **Decision (AF-3):** either (a) align `cohesionScore` to the spec formula and add the weighted variant, or (b) keep density as a distinct metric and introduce a separate `cohesionDegreeRatio`/`cohesionWeighted`. Flag to Dev + Head of R&D.

### 8.6 Q: Singleton cohesion value — 0 or 1?
Strictly per §1.6, an isolated singleton (`deg_w=0`) contributes `0`, so `cohesion_w({u}) = 0` when `u` has no incident edges. The current code pins `1.0` for singletons ("trivially cohesive"). **Decision (AF-3-b):** keep the `|c| ≤ 1 ⇒ 1.0` convenience pin or follow the formula. Document whichever is adopted.

### 8.7 Arbitration log
- **AF-1 (confidence vs weight).** G4 weights by `edgeConfidence`; `edgeWeight` is separate. **Recommend: adopt confidence; document.** Open item for Dev (O1).
- **AF-2 (auditable vs weighted-Leiden).** G4 = auditable weighted measure; Leiden stays unweighted unless product wants weighted search. **Recommend: auditable only.** Open item (O2).
- **AF-3 (cohesion density vs degree-ratio).** Current `cohesionScore` is density; spec (and G4) is degree-ratio. **Decision required** — align or separate. (O3).
- **AF-3-b (singleton cohesion).** 0-by-formula vs 1.0-pin. **Decision required.** (O3).
- **AF-4 (file placement).** `Community.hs` (recommended) vs `Graph/Analysis.hs` (issue). **Dev+R&D decision.** (O4).
- **AF-5 (null-model export).** Record the chosen null model in the graph metadata/export so it is auditable. **Recommend: mandated.** (O4).
- **AV-ARB-1 (INV-CONSTR scope on bidirectional pairs — Head of R&D arbitration, AVI-822).** Verdict: **option (a)** — scope INV-CONSTR to simple undirected graphs; keep `weightedModularity` standard-correct (internally-coherent Newman 2010 weighted modularity). The legacy `modularity` is only consistent where edge-record incidence = distinct-neighbor support degree; on a bidirectional pair stored as two records it mixes record-count `σ_in` with support-degree `m`/`σ_tot`, yielding the artifact `Q = 1.0` vs `Q_w = 0.0`. Rationale: do not degrade the shipped, auditable AVI-533 `Q` to an inconsistent multigraph convention; pin the known divergence instead, and keep the legacy fix in its own cycle. **Follow-up:** [AVI-823](/AVI/issues/AVI-823) implements scope+characterization (Haddock limitation + scenario-9 test); the legacy convention is a separate AVI-533 defect out of AVI-578 scope.

---

## 9. Determinism Lens

Graphos promises deterministic output (`gHash` over sorted keys; `computeGraphHash` sorts node ids and edge tuples, `Core.hs:259`). Decision points for these surfaces:

- **Summation order.** Both `Q_w` and `cohesion_w` are `Double` sums; constrain to fixed order (`Data.Map`/`Data.Set` ascending iteration) — INV-Qw3, INV-C4.
- **Node visitation.** Weighted degree/cohesion scan `neighbors g i`; iterate `Set.toList` (sorted) or CSR-contiguous. Deterministic given fixed graph.
- **Weight extraction.** `getConfidence(edgeConfidence e)` is a pure projection of stored data; deterministic.
- **Partition input.** `Q_w`/`cohesion_w` depend only on the partition (INV-Qw2, INV-C3); invariant under any key permutation.

---

## 10. Spec-vs-Code Gaps (for [Graphos Dev](/AVI/agents/graphos-dev))

- **GAP-1 (no weighted aggregates exist).** `computeCommunityStats` produces unweighted `csSigmaIn/csSigmaTot` (`Community.hs:92-106`); implement weighted analogs (`csSigmaIn_w`, `csSigmaTot_w`, `m_w`) keyed on `edgeConfidence`.
- **GAP-2 (no `modularityWeighted` export).** Add `modularityWeighted :: Graph -> CommunityMap -> Double` (`γ=1`) (+ optional `WithResolution`), reducing to AVI-533 `modularity` under uniform confidence (INV-CONSTR).
- **GAP-3 (no `getConfidence` accessor).** `Confidence`'s constructor is not destructured in the domain; pattern-match `Confidence w <- …` or add a small accessor (`Types/Edge.hs:77`).
- **GAP-4 (cohesion density vs spec).** §8 AF-3: current `cohesionScore` is edge-density, not the spec degree-ratio formula that G4 generalizes. Align or separate before wiring.
- **GAP-5 (null model undocumented).** Record the chosen null model (`P_uv = d_u d_v/(2m_w)`) in the export/metadata (`shall-null-model`).

---

## 11. Open Items (owner + exact action)

| ID | Item | Owner | Action | Blocks |
|---|---|---|---|---|
| O1 | Implement weighted aggregates + `modularityWeighted`/weighted cohesion, preserving INV-Qw0–4, INV-CONSTR, INV-C1–5; add §7 scenarios. Resolve AF-1/AF-2/AF-3 (confidence vs weight; auditable-only; density vs degree-ratio). | [Graphos Dev](/AVI/agents/graphos-dev) | Confirm semantics; implement; property-test | AVI-535 downstream verification |
| O2 | Record the null model in the export/metadata (`AF-5`); confirm file placement `Community.hs` vs `Graph/Analysis.hs` (`AF-4`). | [Graphos Dev](/AVI/agents/graphos-dev) + Head of R&D | Metadata field + placement decision | auditable null-model |
| O3 | Arbitrate AF-3/AF-3-b (cohesion density-vs-degree-ratio; singleton 0-vs-1). | Head of R&D | Spec-vs-code decision | cohesion wiring |

**Surface map:** Types: `Types/Edge.hs` (`Edge`, `Confidence`), `Types/Graph.hs` (`CommunityMap`). Community: `Community.hs` (`computeCommunityStats:92`, `cohesionScore:385`, `bestCommunityFor:229`, `Resolution:50`). Graph: `Graph/Core.hs:96-122` (storage), `Graph/Query.hs:37,44` (`neighbors`/`degree`). Specs: openspec `community-detection`, `bounded-edge-inference`, `leiden-scalability`, `domain-types`.

---

## 12. Literature

- **Newman, M.E.J.** (2010). *Networks: An Introduction.* Oxford. — weighted modularity `Q = (1/2m)Σ(A_uv − k_u k_v/2m)δ(c_u,c_v)` and configuration null model `P_uv = k_u k_v/(2m)`; source of `(*)` and §1.4.
- **Newshyld, J.** (2004). *Modularity and community structure in networks.* PNAS 104(27):9517. — the `Σ_c[e_c − p_c²]` form and squared-`σ_tot` term (carried into AVI-533).
- **Blondel, V.D., Tsourakidis, C.E., Newman, M.E.J.** (2008). *Fast algorithm for detecting community structure in networks.* J. Stat. Mech. — resolution-`γ` generalization and local-moving optimization (`deltaQ` at `Community.hs:236`).
- **Traag, V.A., Van Biggen, L., Smit, V.** (2019). *Uncovering hierarchical community structure with provable guarantees.* Phys. Rev. Research 2, 023320. (Leiden) — the local-move monotonicity (INV-Qw5) and refinement; `resMaxIterations` cap (AVI-533 §5).

(End of file)
