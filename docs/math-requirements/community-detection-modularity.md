# Community Detection & Modularity — Mathematical Requirements (Graphos Context Graph)

**Author:** Graph Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-510](/AVI/issues/AVI-510) (child of [AVI-508](/AVI/issues/AVI-508))
**Status:** requirements document — grounded in domain model + openspec specs
**Scope:** math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion.

---

## 0. Notation (single glossary)

One notation throughout, sourced from the Graphos domain types (the shared glossary referenced by [AVI-508](/AVI/issues/AVI-508) is exactly these types):

- `NodeId = Text` — `Graphos.Domain.Types.Node.NodeId` (`Node.hs:46`). Derived from file+entity.
- `Node = Node { nodeId, nodeLabel, nodeFileType, nodeSourceFile, nodeLineStart, nodeLineEnd, nodeSignature, nodeCommunityId :: Maybe Int, nodeKind, nodeDegree :: Maybe Int, nodeIsBridge :: Maybe Bool, nodeExtra :: Maybe Value, nodePresentBits :: Word64 }` (`Node.hs:120`). `nodeFileType ∈ {CodeFile, DocFile, PaperFile, ImageFile, VideoFile, AudioFile, OfficeFile}`.
- `Edge = Edge { edgeId, edgeSource :: NodeId, edgeTarget :: NodeId, edgeRelation :: Relation, edgeWeight :: Double, edgeConfidence :: Confidence Double, edgeExtra :: Maybe Value }` (`Edge.hs:88`). `Relation ∈ {Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred}` (`Edge.hs:33`). `EdgeId = EdgeId Text` (`Edge.hs:21`).
- `LabeledGraph = LabeledGraph { gNodes :: Map NodeId Node, gEdges :: Map EdgeId Edge, gAdjFwd :: Map NodeId (Set NodeId), gAdjBack :: Map NodeId (Set NodeId), gDirected :: Bool, … }` (`Types/Graph.hs:83`, extended by `Graphos.Domain.Graph.Core.Graph`).
- `neighbors g nid = if gDirected g then gAdjFwd[nid] else gAdjFwd[nid] ∪ gAdjBack[nid]` — returns a `Set NodeId` (deduplicates); `degree g nid = Set.size (neighbors g nid)` (`Graph/Query.hs:37,44`).
- `CommunityId = Int`; `CommunityMap = Map CommunityId [NodeId]`; `CohesionMap = Map CommunityId Double` (`Types/Graph.hs:41-45`).
- `Resolution = Resolution { resGamma :: Double, resMinSize :: Int, resMergeInto :: MergeStrategy, resMaxIterations :: Int }`; `MergeStrategy ∈ {MergeToNeighbor, MergeToLargest}` (`Community.hs:50-55`).
- `m` = number of edges of the support graph = `(Σ_v degree v)/2`.

**Weighted vs. unweighted scope statement (critical).** The Leiden scoring in `Domain.Community` operates on the *simple, unweighted support graph*: `neighbors` returns a `Set` (multi-edges collapse to one), `degree` counts distinct neighbors (integer), and `edgeWeight` / `edgeRelation` are **not** read by `bestCommunityFor` or `localMovingLoop`. Therefore every modularity claim below is a claim about the **unweighted support multigraph-suppression** `G_s = (V, E_s)` where `E_s = { {u,v} : v ∈ neighbors(u) }`. Requirements that must hold for the *weighted* graph are explicitly marked as out-of-scope-for-Leiden (see §2 note and §6). This is a complexity-honesty and invariant-preservation constraint on `Domain.Community`, not a product decision.

---

## 1. Formal Definitions

### 1.1 Context graph as a weighted multigraph, and its support graph
The Graphos context graph is the labeled multigraph
`G = (V, E, src, tgt, rel, w)` with `V = Map.keys (gNodes g)` (|V| = N), `E = Map.keys (gEdges g)` (|E| = E_count), `src/tgt : E → V`, `rel : E → Relation`, `w : E → ℝ≥0` (`edgeWeight`). Between an ordered pair `(u,v)` there may be more than one edge (distinct `Relation`s or distinct `EdgeId`s), so `G` is a **multigraph**; edges carry a `Relation` label and a `Double` weight.

Leiden acts on the **simple support graph** `G_s = (V, A)` where `A ⊆ V×V` (or unordered pairs if `¬gDirected g`) is the set relation of `G`: `{u,v} ∈ A ⟺ v ∈ neighbors(u) ∨ u ∈ neighbors(v)`. Let `n = |V|`, `m = |E_s| = (Σ_v deg_s(v))/2`, and `deg_s(v) = Set.size (neighbors g v)`.

> **Constrains:** `Graphos.Domain.Graph.Core` (builds `gAdjFwd/gAdjBack`), `Graphos.Domain.Community.buildLeidenState` (`Community.hs:141`), `Graphos.Domain.Graph.Query.neighbors/degree` (`Query.hs:37,44`).

### 1.2 Community partition (cover by disjoint sets)
A **community assignment** is a surjection `π : V → C` onto a set of `CommunityId`s (here `C ⊆ ℤ`). The induced **partition** `P_π = { π^{-1}(c) : c ∈ range(π) }` is a cover of `V` by pairwise-disjoint non-empty sets. Equivalently encoded as `CommunityMap = Map CommunityId [NodeId]` with the map being a bijection onto a partition.

> **Invariant INV-1 (disjoint cover).** `⋃_{c} map c = V` (surjectivity onto V) **and** for distinct `c ≠ c'`, `map c ∩ map c' = ∅` (disjointness), and no `map c = ∅`. This must hold for the output of `leidenStateToCommunityMap` (`Community.hs:306`) and after every step of `mergeSmallCommunities` (`Community.hs:326`).
> **Acceptance (property test):** for the output `cm` of `detectCommunitiesWithResolution g res`, `M nub (concatMap snd cm) == M (Map.keys (gNodes g))` and all values pairwise disjoint. (Here `M s = Data.Set.fromList s`.) See §5.

### 1.3 Modularity objects carried from the domain
For a community `c`, define over `G_s`:
- `σ_in(c) = (1/2)·Σ_{v∈c} deg_s^c(v)` = half the internal-degree-sum = number of internal edges of `c` (`deg_s^c(v)` = # neighbors of `v` inside `c`).
- `σ_tot(c) = Σ_{v∈c} deg_s(v)` = total degree of `c`.
- `k_v = deg_s(v)`.
These are the exact quantities `computeCommunityStats` (`Community.hs:92`) and the Leiden state (`lsSigmaTot`, `lsDegrees`, `lsM`, `Community.hs:112`) compute. Note `σ_in`/`σ_tot` here are computed on `G_s`, **not** on the weighted `G`.

---

## 2. Modularity Objective

### 2.1 The objective functional
The modularity density optimized is the standard Blondel–Newman modularity with resolution γ, evaluated on `G_s`:

`Q_γ(π) = (1/(2m)) · Σ_{v,w ∈ same c} [ A_{vw} − γ·(k_v k_w)/(2m) ]`

where `A` is the adjacency of `G_s`. Equivalently `Q_γ = Σ_c [ in(c) − γ·(σ_tot(c)/(2m))² ]` with `in(c) = σ_in(c)/(2m)`.

### 2.2 Theorem (local moving never decreases Q within a phase)
**Theorem 2.1.** In one local-moving pass (`localMovingPass`, `Community.hs:178`), every accepted node move `i: c→d` (where `bestComm ≠ currentComm`) strictly increases `Q_γ(P_π)`: `Q_γ(π') > Q_γ(π)`.

**Proof sketch.** Moving `i` into `d` changes only the terms touching `i`. The exact modularity change is the Blondel identity:
`ΔQ = [k_{i,d} − γ·σ_tot(d)·k_i/(2m)]/(2m) − [k_{i,c} − γ·σ_tot(c)·k_i/(2m)]/(2m)`,
where `k_{i,d}` = number of edges from `i` to `d` = `countMap[d]` in `Community.hs:204` (each distinct neighbor counted once, since `neighbors` is a `Set`), and `k_{i,c}` likewise. The code's `deltaQ(target) = sigmaIn/m − γ·(sigmaTot·ki)/(2m²)` (`Community.hs:236`) equals `2·ΔQ` (algebraically identical up to the positive factor 2), so `sign(deltaQ) = sign(ΔQ)`. The move is accepted iff `bestScore > 0` (`Community.hs:244`) iff `ΔQ > 0`. Since accepted moves are the only ones applied, `Q_γ` strictly increases on each accepted move. ∎

> **Constrains:** `bestCommunityFor` (`Community.hs:229`), `localMovingLoop` (`Community.hs:188`).
> **Invariant assertion per iterative step (INV-2).** After every accepted write in `localMovingLoop`, `Q_γ(current assignment) ≥ Q_γ(assignment before this pass)`; i.e., `moved` increments never decrease `Q_γ`. Implement as a property/invariant test that recomputes `Q_γ` from the assignment before and after each pass on the deterministic fixtures and asserts non-decrease. (This is the "modularity non-decreasing" invariant the issue requires.)
> **Caveat (honesty).** INV-2 holds for the *unweighted* `G_s`. It does **not** automatically hold for weighted modularity, because `edgeWeight` is ignored by the scoring. If Graphos intends weighted modularity, this is a documented deviation requiring a separate functional; out of scope for this doc unless [AVI-508](/AVI/issues/AVI-508) reassigns it.

### 2.3 Resolution parameter γ and the auto-tune table
γ is a **monotone decreasing** penalty on intra-community degree concentration; larger γ shrinks communities, smaller γ enlarges them. Semantics: the term `γ·σ_tot(c)·k_i/(2m²)` is the "expected" edges under a null model scaled by γ. The auto-tune mapping the spec pins (`community-detection` §Requirement "Resolution data type and defaults"; reproduced in `defaultResolution` at `Community.hs:62`) is:

| graph size | γ = resGamma | resMinSize | resMaxIterations |
|---|---|---|---|
| < 1k | 1.0 | 3 | 50 |
| 1k–10k | 0.8 | 5 | 30 |
| 10k–100k | 0.5 | 10 | 20 |
| 100k+ | [0.3, 0.5] | 10–20 | 10–20 |

> **Acceptance (complexity/behavior assertion):** for a graph with exactly 50,000 nodes and no explicit `Resolution`, `detectCommunities g` applies `resGamma = 0.5, resMinSize = 10, resMaxIterations = 20` (spec scenario "Auto-tune resolution for 50k-node graph"). Constrain `Graphos.Domain.Community.detectCommunities` (`Community.hs:70`) to route by `n = Map.size (gNodes g)` through this table; user-supplied `Resolution` overrides it (spec scenario "CLI override").

---

## 3. Termination Argument

### 3.1 Stability predicate
The outer loop `leidenLoop` (`Community.hs:292`) terminates when **either** of two predicates holds (this is the exact stability predicate required):

- **SP-1 (local stability):** `moved == 0` — no node changed community in the just-finished pass (`Community.hs:298`).
- **SP-2 (fixed-point on assignment):** `lsAssignment st'' == prevAssign` — the new assignment equals the one from the previous pass (`Community.hs:302`).

Both are monotone-safe: SP-1 stops when local moves are exhausted; SP-2 stops when consecutive passes produce identical assignments (a strict-Q-increase sequence cannot repeat, so SP-2 only triggers if SP-1 would also have triggered, guarding against parity oscillation).

### 3.2 Termination proof
**Argument.** (Upper bound by counter.) The loop counter is decremented each iteration and the base case `go !st 0 _prev = st` returns immediately (`Community.hs:295`). Hence the number of full iteration cycles is ≤ `maxIter = if resMaxIterations res > 0 then resMaxIterations res else 50` (`Community.hs:287`). Termination is therefore guaranteed unconditionally by a finite cap. (Strengthening.) Within each pass, by Theorem 2.1 every accepted move strictly increases `Q_γ`, and `Q_γ` takes values in a finite set (rational numbers with bounded denominator over finitely many partitions), so the number of accepted moves in a single pass is finite. Thus each pass terminates, and the total is bounded by `maxIter`. ∎

> **Constrains:** `leidenLoop` (`Community.hs:292`), `leidenPhase` (`Community.hs:280`).
> **Invariant assertion per iterative step (INV-3).** `0 ≤ remaining ≤ maxIter` is preserved; the loop exits when `remaining == 0` or SP-1/SP-2 holds. Property test: instrument a reference Leiden and assert the observed pass count ≤ `resMaxIterations` on every deterministic fixture (spec scenario "Leiden terminates within max iterations" with `resMaxIterations = 10` → ≤ 10 cycles).

---

## 4. Complexity Bounds

Model: **N** nodes, **E** edges of `G`, `E_s` edges of the support graph `G_s`, `C` = number of communities, `D_max = max_v deg_s(v)`. All bounds match the data structures actually used (`Data.Vector.Unboxed` CSR, `Data.IntMap.Strict`, `Data.Map.Strict`).

| Phase | Bound | Grounding |
|---|---|---|
| `buildLeidenState` | `O(N + E_s)` time, `O(N + C + E_s)` memory | CSR build once (`Community.hs:141`); offsets `scanl` O(N); neighbor index map O(N). |
| One `localMovingPass` | `O(E_s + N)` time | per node: fold over neighbors O(deg) to build `countMap`; O(1) per candidate community via `IntMap.lookup`; O(deg) IntMap update on move (`Community.hs:188,204`). At most one thaw/freeze per pass = O(N) amortized copy. |
| One `refineCommunitiesOpt` | `O(E_s + N)` time | `cohesionToCommunityIdx` folds a node's neighbors O(deg); total O(E_s); batched split copies O(#split-communities). (`Community.hs:247`) |
| `leidenLoop` (full) | `O(maxIter · (E_s + N))` time | ≤ maxIter passes, each O(E_s+N). Bounded by counter (§3.2). |
| `mergeSmallCommunities` | `O(N + E_s)` time worst case | reverse index built once O(N); per small community O(1) incremental update + `bestNeighborCommunity` scans members' neighbors O(local edges). (`Community.hs:326,371`) |
| **Total `detectCommunitiesWithResolution`** | **O(maxIter·(E_s + N) + N + E_s) = O(maxIter·(E_s + N))** | sum of above. |

**Scale guard (100k+ nodes).** Memory of `LeidenState` is `O(N + C + E_s)`: five `VU.Vector` of size `N`/`E_s` (`lsAdj`, `lsOffset`, `lsDegrees`, `lsAssignment`), `lsSigmaTot :: IntMap Double` of size `C`, and `lsNeighbors`/`lsNodeIds` of size `N`. To hold 100k nodes with ~10× average degree, this is on the order of hundreds of MiB — acceptable; holding it as lazy thunks would blow up heap (each thunk ≈ 16–24 bytes + indirection). The **thunk discipline requirement**: the `NFData` instance for `LeidenState` must force all fields to normal form so `deepseq` between iterations (`Community.hs:301`) prevents thunk accumulation.

> **Acceptance (complexity assertion):** on a sparse synthetic graph with ≥ 50,000 nodes, `detectCommunities` completes in seconds, consistent with the PRD §16.1 target (< 30 s at 100k nodes) — i.e., wall-time scales linearly in `N + E_s` up to the constant `maxIter ≤ 50` (`leiden-scalability` "Large graphs cluster within target order of magnitude").
> **Acceptance (data-structure assertions, from `leiden-scalability`):** (a) `localMovingPass` updates assignments in O(1)/move with ≤ O(N) amortized copying — no full-vector copy per move (§"Constant-time node moves"); (b) refinement applies reassignments without per-node full copies (§"Batched refinement updates"); (c) `mergeSmallCommunities` builds the reverse index once and updates incrementally (§"Incremental merge-phase index"); (d) grouping in `leidenStateToCommunityMap`/refinement uses `fromListWith (++)`-avoiding O(N) insertion (§"O(N) community member grouping") — note the current code already uses `fromListWith (++)` at `Community.hs:311`, which the spec flags as a regression risk; require the O(N) variant; (e) neighbor scoring is a single pass over the neighbor vector, not per-candidate rescan (§"One-pass modularity-gain scoring"); (f) CSR adjacency via contiguous `VU.slice` (§"CSR adjacency representation"); (g) `scoreAllCohesion` avoids per-node `Set` allocation (§"Cohesion scoring without per-node neighbor allocation").

---

## 5. Partition Invariants

Every operation claiming to preserve the partition must preserve INV-1 (§1.3). Concretely:

- **INV-1 (disjoint cover).** `⋃_c map c = V`, pairwise-disjoint, no empty values. Holds after `leidenStateToCommunityMap` and after each `mergeOne` step.
- **INV-4 (merge preserves the union of members — incremental reverse-index correctness).** `mergeSmallCommunities` must satisfy: `M (concatMap snd cm_before) == M (concatMap snd cm_after)` (same multiset of memberships ⇒ same partition of V), **and** the incremental reverse index updated in `mergeOne` (`Community.hs:357`, `foldl' (\ri nid -> Map.insert nid targetCid ri) revIdx members`) reflects **all** prior merges so a small community that already received members is merged using its current membership, not its stale initial list. The code comment at `Community.hs:317-325` documents this exact fix.
  > **Constrains:** `mergeOne` (`Community.hs:342`), `buildReverseIndex` (`Community.hs:78`), `bestNeighborCommunity` (`Community.hs:371`).
  > **Acceptance (property test):** on any `cm`, `(M . concatMap snd) cm == (M . concatMap snd) (mergeSmallCommunities g res cm)`, and for each small cid the chosen target equals the target computed with a freshly rebuilt reverse index at that step (spec scenario "Merge targets match a recomputed index").
- **INV-5 (cohesion ∈ [0,1]).** Every value in the `CohesionMap` returned by `scoreAllCohesion` lies in `[0,1]`. For the settled definition in §6, each summand is a ratio of counts `≤ 1`, and the average over `|c|` nodes is in `[0,1]`; singletons/empty are pinned to `1.0`/`0.0`.
  > **Constrains:** `cohesionScore` (`Community.hs:385`), `scoreAllCohesion` (`Community.hs:405`).
  > **Acceptance (invariant assertion / property test):** for all communities `c` in any reachable `cm`, `0.0 ≤ v ≤ 1.0`. Property test over random graphs (`Graph.randomGNP`-style fixtures). See §6 for the *definition* that pins the range.
- **INV-6 (exactly-one-community-per-node).** The `lsAssignment :: VU.Vector Int` maps each node index to exactly one community id; `leidenStateToCommunityMap` groups every index exactly once (`Community.hs:311`). No node is duplicated across communities or dropped.
  > **Invariant assertion per iterative step.** Invariant INV-6 holds at every iteration of `leidenLoop`: the assignment vector is a total function `V → C`. Property test: at each pass, `n == sum (map length (Map.elems cm))` and `M (concatMap snd cm) == M (Map.keys (gNodes g))`.

---

## 6. ⚠️ Arbitration Flag — Cohesion Definition Mismatch

**Conflict (surfaced as required).** The openspec `community-detection` spec defines cohesion as the **average internal-neighbor ratio**:
`cohesion_spec(c) = (1/|c|) · Σ_{i∈c} |neighbors(i) ∩ c| / |neighbors(i)|`
(`community-detection` §Requirement "cohesionScore and scoreAllCohesion", formula on line 36). Its scenarios require: fully-inside community → `1.0`; each node with 1 internal + 9 external neighbor → `≈0.1`.

The domain implementation `cohesionScore` (`Community.hs:385`) computes **cluster density** instead:
`cohesion_code(c) = internalEdges(c) / max(1, |c|·(|c|−1)/2)`
(`Community.hs:393-402`). These are **not** equal in general. Example — a star community `K_{1,k}` (one center + k leaves, leaves mutually non-adjacent), `|c| = k+1`:
- `cohesion_spec`: center ratio `= 1` (all its k neighbors are internal); each leaf ratio `= 1/k`; average `= (1 + k·(1/k))/(k+1) = 2/(k+1)`.
- `cohesion_code`: internal edges `= k`; total possible `= C(k+1,2) = k(k+1)/2`; density `= 2/(k+1)`.
They coincide only for this particular family; e.g. a "double-bottle" community gives different values. So the two formulas diverge for non-star shapes.

**Root cause of divergence (also internal to the code).** The refinement threshold (`refineCommunitiesOpt`, `Community.hs:264`) uses `cohesionToCommunityIdx` (`Community.hs:271`), which computes exactly the *per-node* internal-neighbor ratio — i.e., the summand of `cohesion_spec`. But the *exposed* `cohesionScore` returns `cohesion_code` (density). The code's own two cohesion notions are inconsistent.

### 6.1 Proposed settled definition
Adopt **`cohesion_spec`** (average internal-neighbor ratio) as the single canonical cohesion, because (a) it is the one the openspec scenarios pin, (b) it matches the code's own refinement semantics via `cohesionToCommunityIdx`, and (c) it is defined per-node so it is well-defined for directed and undirected graphs identically to how `neighbors` is computed.

**Definition (§5 settled).** For a community `c` with member set `M_c ⊆ V`:
```
cohesion(c) = (1 / |c|) · Σ_{v ∈ c}  | { u ∈ M_c : u ∈ neighbors(g,v) , u ≠ v } |  /  max(1, |neighbors(g,v)| )
```
with `neighbors` taken per `gDirected g` (`Query.hs:37`): forward-only if directed, union of fwd+bwd if undirected. Convention: an isolated node (`neighbors(v) = ∅`) contributes ratio `1` (vacuous: all zero of its neighbors are internal), matching the singleton rule `cohesionScore … length ≤ 1 = 1.0` (`Community.hs:387`).

**Well-definedness / range.** Each summand is `count/max(1,deg) ∈ [0,1]`; the average over `|c|` nodes is in `[0,1]`. So `cohesion(c) ∈ [0,1]` and INV-5 holds by construction.

### 6.2 Property test that pins the settled definition
Implement as a property/invariant test in the community test suite:

1. **Fully-inside community → 1.0.** On a clique `K_t` (`t ≥ 2`) as a single community, `cohesion = 1.0` (spec scenario "Highly cohesive community scores near 1.0").
2. **1-in/9-out → ≈0.1.** Construct a community of 10 nodes where each node has exactly 1 internal neighbor and 9 external; assert `cohesion ≈ 0.1` (spec scenario "Loosely connected community scores near 0.0").
3. **Range bound.** For all graphs and all reachable communities, `0 ≤ cohesion(c) ≤ 1` (INV-5). Property test over random fixtures.
4. **Order-independence / determinism.** `cohesion(c) == cohesion(c')` under any permutation of the member list (the value is an average over a set, so it must not depend on `[NodeId]` order) — pins determinism required by the determinism lens.
5. **Directed vs undirected parity.** For an undirected graph, `cohesion` computed with `gDirected = True` restricted to forward edges equals the symmetric value; assert the directed computation reads only `neighbors(g,v)` per `gDirected`.

> **Constrains:** `cohesionScore` (`Community.hs:385`) — must be reimplemented to match §6.1; `scoreAllCohesion` (`Community.hs:405`) to propagate; the refinement threshold in `refineCommunitiesOpt` (`Community.hs:264`) already uses the per-node ratio, so adopting §6.1 makes the exposed cohesion **consistent** with refinement (resolves the internal inconsistency).
> **Joint-surface consistency.** `cohesionScore` feeds `CohesionMap`, which flows to `Graphos.UseCase.Label.labelCommunities` and `selectRepresentatives` (`Community.hs:422`) and to Neo4j/Memgraph push modes (`09-merge`, `12-neo4j-push`). The settled definition must keep `cohesion ∈ [0,1]` so those consumers' thresholds (`< 0.3` quality filter, representative ranking) remain well-defined. This must be looped with [Graphos Dev](/AVI/agents/graphos-dev) before finalizing any behavior-constraining requirement (issue acceptance criterion: "Explicit statement of what each requirement constrains in Haskell … loop with the Graphos Developer").

### 6.3 Escalation
This conflict and the proposed settled definition (§6.1) are **escalated jointly with the Category Theory Expert** (sibling child issue (b), [AVI-508](/AVI/issues/AVI-508)): the merged context graph is simultaneously (i) a weighted graph on which Leiden runs and (ii) the colimit of extraction views. The cohesion value used for representative/quality decisions must be consistent with cohesion defined on the colimit object — a colimit claim is still a claim about nodes and edges, and §6.1's per-node ratio must be preserved by the merge functor. The graph-only claim here (cohesion on `G_s`) is consistent with that colimit claim because merge preserves the node set and adjacency relation that `neighbors` reads.

---

## 7. Spec-vs-Code Gaps Flagged (for [Graphos Dev](/AVI/agents/graphos-dev))

These are gaps between `community-detection` and the implemented `Domain.Community`; each needs a decision (implement / document / change spec), not an assumption:

- **GAP-1 (phases).** The spec describes three phases including **aggregation** (merge into supernodes, build coarser graph, return to phase 1, loop until stable). The implementation does `leidenPhase` (local-moving + `refineCommunitiesOpt`) then `mergeSmallCommunities`; there is no explicit supernode-aggregation loop returning to phase 1 (`Community.hs:74-76`). Decide whether aggregation is deferred or whether `mergeSmallCommunities` is intended as the aggregation step (they are semantically different).
- **GAP-2 (cohesion).** Resolved by §6 (adopt `cohesion_spec`); requires code change to `cohesionScore`.
- **GAP-3 (representatives use cohesion).** The spec says representatives are selected by cohesion and quality is filtered at `< 0.3`; `selectRepresentatives` (`Community.hs:422`) ranks by **degree**, not cohesion. Decide whether to align.
- **GAP-4 (weighted modularity).** Leiden optimizes unweighted `G_s`; `edgeWeight` is ignored. If Graphos requires weighted-modularity community detection, this is a documented functional deviation (out of scope for this doc).

---

## 8. Acceptance-Criteria Mapping (issue checklist)

- **Property test per core requirement:** INV-1 disjoint cover (§5), INV-4 merge-preserves-union (§5), INV-6 exactly-one-community-per-node (§5), §6 property tests 1–5, and the auto-tune behavior assertion (§2.3). ✓
- **Invariant assertion per iterative step:** INV-2 modularity non-decreasing across passes (§2.2), INV-3 pass-counter bound (§3.2), INV-6 assignment total-function per iteration (§5). ✓
- **Complexity assertion with explicit bound + scale guard:** total `O(maxIter·(E_s+N))`, per-phase table in §4, memory `O(N+C+E_s)` scale guard with NFData thunk-discipline (§4). ✓
- **Haskell-constraint statement per requirement:** every section names the constrained surface (`Domain.Community.*`, `Domain.Graph.Core`, `Domain.Graph.Query`, `Types.Graph`) as annotated; behavior-constraining requirements (§2.3, §6) require a loop with [Graphos Dev](/AVI/agents/graphos-dev) before finalizing. ✓

---

## 9. Determinism Lens

Where Graphos promises deterministic output, the decision points that could break determinism and their constraints:
- **`bestCommunityFor` tie-breaking.** `maximumBySnd` (`Community.hs:244,212`) picks the max score; ties across candidate communities must be broken by a fixed order (e.g., smallest `CommunityId`) to be deterministic. Currently `comms :: [Int]` derives from `commOfNb :: [Int]` order, which depends on neighbor-set iteration order — constrain the implementation to sort candidate communities before `maximumBySnd`, or make `maximumBySnd` pick the least id on ties.
- **`mergeSmallCommunities` fold order.** `smallCids` is derived from `Map.toList commMap` (sorted by `CommunityId`, deterministic), and `foldlStrict'` is sequential — deterministic given a deterministic input map. ✓
- **`cohesion` (§6 settled)** is an average over a set → order-independent. ✓
- **Refinement batched split** iterates `IntMap.foldlWithKey'` (sorted keys) — deterministic. ✓

> **Constrains:** `maximumBySnd` (`Community.hs:210`), `bestCommunityFor` (`Community.hs:229`), `mergeSmallCommunities` (`Community.hs:326`), `refineCommunitiesOpt` (`Community.hs:247`).
