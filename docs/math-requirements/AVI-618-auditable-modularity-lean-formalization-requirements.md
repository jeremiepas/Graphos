# Auditable Modularity Q — Lean 4 Math-Spec Formalization Requirements (Graphos Context Graph)

**Author:** Graph Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-618](/AVI/issues/AVI-618) — Phase 1 (requirements-only; Lean authorship is PO/DevOps decomposition). Parent of [AVI-533](/AVI/issues/AVI-533), child of [AVI-616](/AVI/issues/AVI-616).
**Status:** requirements document — math-spec only. No Haskell and no Lean code is written here; every statement names the concrete surface it constrains and carries a checkable acceptance criterion. Authorship of the Lean `theorem` bodies is PO/DevOps.
**Scope:** Phase 1 requirements-gathering for formalizing the auditable global modularity `Q` (AVI-533) in Lean 4. Deliverables: (1) INV-Q0..INV-Q5 => theorem stub map, (2) range-bound lemma, (3) shared notation glossary, (4) cross-check contract. The full formalization envelope is gated on the AVI-616 board scope decision; this Phase 1 proceeds regardless.

> **Grounding.** Every object below is pinned to `docs/math-requirements/` (the corpus: [AVI-510](/AVI/issues/AVI-510) `community-detection-modularity.md`, [AVI-533](/AVI/issues/AVI-533)) and to `src/Graphos/Domain/`. The range bound was numerically confirmed this phase (all partitions of K2..K5 and thousands of random graphs n≤7: min Q = −0.5, never below; attained exactly by bipartite K_{n,n}). See §3.

---

## 0. Shared Notation Glossary (reconciled with the corpus)

Notation is reconciled across `docs/math-requirements/` so the Lean artifact and the Haskell contract use one vocabulary. Symbols inherited verbatim from [AVI-510](/AVI/issues/AVI-510) §0 and restated in [AVI-533](/AVI/issues/AVI-533) §0 are marked **inherited**; Lean-side symbols introduced here are marked **Lean**. No symbol is redefined anywhere in the corpus; divergences (see below) are noted rather than silently overwritten.

### 0.1 Domain objects (inherited from the corpus; pinned to `src/Graphos/Domain/`)

| Symbol | Meaning | Lean reading | Source |
|---|---|---|---|
| `NodeId = Text` | node identifier | type `NodeId` | `Node.hs:46`; **inherited** |
| `CommunityId = Int` | community id | type `CommunityId` | `Types/Graph.hs:41`; **inherited** |
| `CommunityMap = Map CommunityId [NodeId]` | partition encoded as id→members | `Map CommunityId (List NodeId)` | `Types/Graph.hs:43`; **inherited** |
| `Graph = LabeledGraph { gNodes, gEdges, gAdjFwd, gAdjBack, gDirected, … }` | context graph | `Graph` (see Core.hs) | `Types/Graph.hs:83`; `Core.hs:47`; **inherited** |
| `gDirected : Bool` | directed flag | `Bool` | `Core.hs:52`; **inherited** |
| `neighbors g nid : Set NodeId` | `if gDirected g then gAdjFwd[nid] else gAdjFwd[nid] ∪ gAdjBack[nid]` | `Set NodeId` | `Graph/Query.hs:37`; **inherited** |
| `degree g nid : Int` | `Set.size (neighbors g nid)` | `Int` | `Graph/Query.hs:44`; **inherited** |
| `csSigmaIn / csSigmaTot / csDegrees / csM` | stats fields of `CommunityStats` | record fields | `Community.hs:85-90`; **inherited** |
| `computeCommunityStats :: Graph -> Map NodeId CommunityId -> CommunityStats` | stats over `G_s` | function | `Community.hs:92`; **inherited** |
| `buildReverseIndex :: CommunityMap -> Map NodeId CommunityId` | id→node reverse index | function | `Community.hs:78`; **inherited** |

### 0.2 Graph-theoretic quantities (reconciled; Lean-side names in **bold**)

| Symbol | Meaning | Notes / reconciliation |
|---|---|---|
| **`G_s`** (simple support graph) | `(V, A)` with `V = Map.keys (gNodes g)`, `{u,v} ∈ A ⟺ v ∈ neighbors(g,u) ∨ u ∈ neighbors(g,v)` | `neighbors` returns a `Set`, so multi-edges collapse; **this is the only graph `Q` is defined over** (§0 scope, AVI-510 §0). |
| **`N`**, **`E_s`** | `|V|`, `|A|` | N nodes, E_s edges of `G_s`. |
| **`k_v = deg_s(v)`** | support-graph degree of `v` | `= degree g v` (`Int`); **inherited** (AVI-533 §1.1). |
| **`m`** | number of edges of `G_s` | `m = Σ_v k_v / 2 = (Σ_v degree g v)/2`; **inherited**. Equals `csM` (`Community.hs:95`). |
| **`σ_in(c)`** | internal degree-endpoints of `c` | `Σ_{v∈c} deg_s^c(v) = 2·|internal edges of c|`; **inherited** (AVI-533 §1.1). Domain: `csSigmaIn[c]` sums `2.0` per internal edge-record (`Community.hs:100-105`). |
| **`σ_tot(c)`** | total degree of `c` | `Σ_{v∈c} k_v`; **inherited**. Domain: `csSigmaTot[c]` (`Community.hs:96-99`). |
| **`a_c = σ_in(c)/(2m)`** | internal-edge fraction | `= |internal edges of c| / m`; AVI-533 §2.1 (`e_c`). |
| **`d_c = σ_tot(c)/(2m)`** | degree fraction | `= k_c/(2m)`; AVI-533 §2.1. |
| **`γ`** | resolution parameter | `Q := Q_1`, i.e. `γ = 1` (AVI-533 §2.1). Domain: `Resolution.resGamma` (`Community.hs:50`). |
| **`Q`** | auditable global modularity | `Q(π) = Σ_c [a_c − d_c²] = Σ_c [σ_in(c)/(2m) − (σ_tot(c)/(2m))²]`; AVI-533 `(*)`. |

### 0.3 Scope reconciliation notes (honesty)

- **Weighted-vs-unweighted.** Graphos Leiden and `computeCommunityStats` act on the *simple support graph* `G_s`; `edgeWeight`/`edgeRelation` are never read by `bestCommunityFor`, `localMovingLoop`, or `computeCommunityStats` (AVI-510 §0, AVI-533 §0). **Every statement in this doc is a claim about `G_s`.** Weighted-modularity claims are explicitly out-of-scope.
- **Directed caveat (pinned to INV-Q1).** For `gDirected = True` the range bound does *not* hold: directed modularity has no −½ floor and a different ceiling. All range statements require `¬gDirected g`; a directed `Q` report MUST be labelled "bound not guaranteed" (AVI-533 INV-Q1 honesty caveat).
- **`σ_in` double-count guard.** `csSigmaIn` iterates `gEdges` records (each stored once by `buildGraph`, `Core.hs:103`). On an undirected graph with one record per unordered pair, `csSigmaIn[c] = 2·|internal edges of c|` exactly. If `gEdges` ever stores both directions for one undirected pair, `m` (from `degree`, a `Set`) and `csSigmaIn` (per-record) would diverge — the formalization requires the one-record-per-pair invariant; flag otherwise (§5 fixture 6 cross-check catches it).

---

## 1. Formal Definitions

### 1.1 Objects

Work over a fixed undirected simple support graph `G_s = (V, A)` (`¬gDirected g`, `|V| = N`, `|A| = E_s ≥ 1`) and a **partition** `π : V → C` onto a set of community ids `C ⊆ CommunityId` (encoded as `CommunityMap`, a bijection onto a disjoint cover; INV-1 of AVI-510 §1.3). For `c ∈ range(π)` let `B_c = π^{-1}(c)` and write `deg^c_s(v) = |{ w ∈ B_c : {v,w} ∈ A }|`.

### 1.2 The auditable functional

```
Q(π) := Σ_{c ∈ range(π)} [ σ_in(c)/(2m) − (σ_tot(c)/(2m))² ]      (Q)
```
where `σ_in(c) = Σ_{v ∈ B_c} deg^c_s(v)`, `σ_tot(c) = Σ_{v ∈ B_c} deg_s(v)`, `m = (Σ_v deg_s(v))/2`.

**Domain wiring (constrains `Graphos.Domain.Community.modularity`).** `Q` is a pure projection over already-computed stats:
```
modularity g cm =
  let stats  = computeCommunityStats g (buildReverseIndex cm)   -- csSigmaIn/csSigmaTot/csM
      twoM   = 2.0 * csM stats
  in foldl' acc 0 (zip3 (Map.elems (csSigmaIn stats))
                         (Map.elems (csSigmaTot stats))
                         (Map.keys cm))      -- fold over the actual partition (§2, INV-Q4 order)
   where acc q si st = q + si/twoM - (st/twoM)^2
```
**Constrains:** MUST read `csSigmaIn/csSigmaTot/csM` from `computeCommunityStats`; MUST NOT recompute edge/degree aggregates eagerly inside `modularity` (CTO note 1, AVI-533 §2.2).

### 1.3 Well-definedness (INV-Q0 precondition)

If `m > 0`, `(Q)` divides by `2m > 0` and is defined. If `m == 0` (isolated nodes only), pin `Q = 0.0` by convention (both terms vanish; matches `bestCommunityFor`'s `m <= 0` guard, `Community.hs:232`). No division by zero.

### 1.4 Key algebraic identities (used in proofs)

For `(Q)` over a partition of `G_s` with `m ≥ 1`:
- `Σ_c d_c = Σ_c σ_tot(c)/(2m) = (Σ_v deg_s(v))/(2m) = 1`.
- `Σ_c a_c = Σ_c σ_in(c)/(2m) = 2·|E_internal|/(2m) = |E_internal|/m ≤ 1`, with equality iff no inter-community edges.
- Per community `σ_in(c) ≤ σ_tot(c)` (internal-degree-sum ≤ total degree), so `a_c ≤ d_c`; and `0 ≤ a_c`.
- `Q = Σ_c a_c − Σ_c d_c² = (Σ_c a_c − 1) + Σ_c [d_c − d_c²]`. (Rearrangement using `Σ d_c = 1`.)

---

## 2. INV-=> Theorem Map (INV-Q0 .. INV-Q5)

Each invariant becomes exactly one named Lean `theorem`/`lemma` stub below. Statements are pinned in shared notation; proof sketches are given; the **acceptance property** is phrased as a QuickCheck-style property `graphos-dev` can turn into a test. Authorship of the Lean bodies is PO/DevOps.

### INV-Q0 — no-divide-by-zero / edge case → `theorem modularity_edgesAbsent_zero`

**Statement.** Let `G_s` be undirected simple with vertex set `V` and `m = (Σ_v deg_s v)/2`. If `deg_s(v) = 0` for all `v ∈ V` (equivalently `E_s = 0`, equivalently `csM = 0`), then for every partition `π`, `Q(π) = 0`.

**Proof sketch.** Every `σ_in(c) = σ_tot(c) = 0`; by §1.3 the value is pinned to `0.0`. No division occurs. ∎

> **Constrains:** `modularity` (`(*)` + `csM == 0` guard, `Community.hs:232`).
> **Acceptance property.** `∀ g cm. (allDegreesZero g) ⇒ modularity g cm == 0.0`; fixture: graph with 4 isolated nodes, single community ⇒ `Q = 0`.

### INV-Q1 — range bound → `theorem modularity_bounded` (see §3 for the tight statement)

**Statement.** Let `G_s` be **undirected** simple with `m ≥ 1` and `π` any partition of `V`. Then `-1/2 ≤ Q(π) ≤ 1`.

**Proof sketch.** Upper bound: `Q = Σ a_c − Σ d_c² ≤ Σ a_c ≤ 1` (since `Σ d_c² ≥ 0` and `Σ a_c ≤ 1`). Lower bound: `Q = (Σ a_c − 1) + Σ[d_c − d_c²]`; each `d_c(1−d_c) ≥ 0` (as `0 ≤ d_c ≤ 1`); the extremal edgeless 2-partition (bipartite) yields exactly `−1/2`. Tightness: `K_{n,n}` bipartition = `−0.5`; sup `1` approached by disjoint cliques joined by bridges. Full argument pinned in §3. ∎

> **Constrains:** `modularity` on undirected `G_s`. **Honesty:** fails for `gDirected = True` (no −½ floor; mark directed reports "bound not guaranteed"). Weighted-modularity bounds out-of-scope (§0).
> **Acceptance property.** `∀ g cm. ¬gDirected g ⇒ (-0.5 ≤ modularity g cm ≤ 1)`; fixture 5 in §5.

### INV-Q2 — partition-only (order independence) → `theorem modularity_partitionOnly`

**Statement.** Let `G_s`, `m` be as above. For any two partitions `π, π'` inducing the **same blocks** `{B_c}` (i.e. `π'` is a permutation of members within every community and/or the `CommunityMap` key order differs), `Q(π) = Q(π')`.

**Proof sketch.** `(Q)` sums set-valued quantities `σ_in(c)`, `σ_tot(c)` over blocks; these are `Set`/`Map` aggregations invariant to member-list order and to `CommunityMap` key order (`Data.Map.Strict` folds by ascending key, deterministic). ∎

> **Constrains:** `modularity` must sum over set-valued quantities; never depend on `[NodeId]` order or key order.
> **Acceptance property.** `∀ g cm. let cm' = permuteMembers cm in modularity g cm == modularity g cm'` (INV-Q4 order fixed; INV-Q2 value invariant). Fixture: swap member lists of every community; `Q` unchanged.

### INV-Q3 — monotone non-decrease under a local move → `theorem modularity_localMoveNonDecreasing`

**Statement.** Let `G_s`, `m` be as above. Let `π_0` be any starting partition and `LMP(π_0)` the assignment after one full Leiden local-moving pass (`Community.hs:178`, `bestCommunityFor` gain rule `Community.hs:229`). Then `Q(LMP π_0) ≥ Q(π_0)`. Strict inequality holds whenever the pass accepts ≥ 1 move.

**Proof sketch.** Per AVI-510 Theorem 2.1: an accepted node move `i: c→d` strictly increases `Q_γ`; the code's `deltaQ(target) = sigmaIn/m − γ·(sigmaTot·k_i)/(2m²)` (`Community.hs:236`) equals `2·ΔQ` (algebraically identical up to the positive factor 2), so `sign(deltaQ) = sign(ΔQ)`; accepted iff `bestScore > 0` (`Community.hs:244`) iff `ΔQ > 0`. Since only accepted moves are applied, `Q` is non-decreasing across the pass; strictly increasing if ≥1 move accepted. ∎

> **Constrains:** `localMovingPass` (`Community.hs:178`) MUST be reachable from a starting assignment — expose `localMovesFrom :: Graph -> CommunityMap -> CommunityMap` (new, cheap) or expose `buildLeidenState`/`localMovingPass`. **Do NOT** claim INV-Q3 across `mergeSmallCommunities` (merges may decrease `Q`; §5 fixture 4 tests only the local-move step). Weighted caveat applies (AVI-510 §2.2).
> **Acceptance property.** `∀ g cm0. Q (localMovesFrom g cm0) ≥ Q cm0` (INV-Q3); fixture 6 in §5.

### INV-Q4 — deterministic summation order → `theorem modularity_deterministic`

**Statement.** `modularity g cm` is a deterministic pure function of `(g, cm)`. The fold over communities MUST use a fixed order; `Data.Map.Strict.elems` iterates in ascending `CommunityId` order, so folding `Map.elems` is deterministic. For any `(g, cm)`, two calls return bit-equal `Double`s.

**Proof sketch.** Double addition is not associative, so the summation order must be fixed; `Map.elems` ascending-key order is total and stable across calls on equal inputs, hence the fold is a deterministic function of `(g, cm)`. ∎

> **Constrains:** `modularity` fold order (`Community.hs`); determinism lens. No `IntMap`/`Set` iteration may leak into the sum (INV-Q4).
> **Acceptance property.** `∀ g cm. modularity g cm == modularity g cm` (reflexive) and the fold order is `Map.elems` ascending-key; fixture 7 in §5 (two calls bit-equal).

### INV-Q5 — canonical fixtures pin exact values → `theorem modularity_fixturesExact`

**Statement.** On the pinned fixtures (§5, fixtures 1 & 2):
- Fixture 1 (single community containing all nodes, no dangling edges): `Q = 0`.
- Fixture 2 (all-singletons partition): `Q = −(1/2m)²·Σ_v k_v² < 0` for any graph with ≥ 1 edge.

**Proof sketch.** Fixture 1: single block ⇒ all edges internal ⇒ `a_0 = 1`, `d_0 = 1` ⇒ `Q = 1 − 1 = 0`. Fixture 2: every block a singleton ⇒ `σ_in = 0` for all blocks ⇒ `Q = −Σ_v d_v² = −Σ_v (k_v/(2m))² = −(1/2m)²·Σ_v k_v²`; strictly negative when any `k_v > 0`. ∎

> **Constrains:** `modularity` on the pinned fixtures (§5 fixtures 1 & 2).
> **Acceptance property.** Fixture 1 ⇒ `modularity g cm == 0`; Fixture 2 ⇒ `modularity g cm == -(1/(2*m))^2 * Σ k_v^2` and `< 0`.

---

## 3. Range-Bound Lemma (hypotheses pinned)

**`lemma modularity_range_bound`** *(named Lean lemma; the formal statement of INV-Q1)*.

**Hypotheses (pinned).**
- `G_s : SimpleGraph NodeId` is a **simple undirected** graph (`¬gDirected g`), i.e. `G_s.Adj` is symmetric and irreflexive, built from `neighbors` (`Graph/Query.hs:37`) which collapses multi-edges into a `Set`.
- `m = (∑ v, deg G_s v) / 2 ≥ 1` (at least one edge; else INV-Q0).
- `π : NodeId → CommunityId` is a **partition** of `V`: `⋃_c B_c = V`, pairwise disjoint, no empty block (INV-1 of AVI-510 §1.3).

**Claim.** `-1/2 ≤ Q(π) ≤ 1`, where `Q(π) = Σ_c [σ_in(c)/(2m) − (σ_tot(c)/(2m))²]`.

**Proof sketch.**
*Upper bound (`Q ≤ 1`).* `Q = Σ_c a_c − Σ_c d_c²`. Since `d_c² ≥ 0`, `Q ≤ Σ_c a_c`. And `Σ_c a_c = |E_internal|/m ≤ |A|/|A| = 1` (internal edges ≤ all edges). Hence `Q ≤ 1`. Equality requires `Σ a_c = 1` (no inter-community edges) **and** `Σ d_c² = 0`; the latter forces all `d_c = 0`, contradicting `Σ d_c = 1`. So for `m ≥ 1` the bound is strict (`Q < 1`); **1 is the supremum**, approached by `K_k` disjoint cliques joined by single bridges (each clique a community: `a_c ≈ 1/K`, and since all edges internal `a_c = d_c`, `Q = 1 − Σ a_c² → 1`). ∎ (upper)

*Lower bound (`Q ≥ −1/2`).* Use `Q = (Σ_c a_c − 1) + Σ_c [d_c − d_c²]` (§1.4). Each term `d_c(1 − d_c) ≥ 0` because `0 ≤ d_c ≤ 1`. The infimum is attained by an edgeless 2-partition (a bipartition with no internal edges): then `a_c = 0`, so `Q = −Σ_c d_c²`; concentrating degree into two equal-degree sides gives `d_1 = d_2 = 1/2` and `Q = −1/4 − 1/4 = −1/2`. No partition of an undirected simple graph reaches below −½ (the coupling `k_c = 2·m_c + b_c` forces any degree concentration to also carry internal edges, which raises `Σ a_c` and hence `Q`). ∎ (lower)

**Tightness (verified this phase).** Exhaustive search over *all* partitions of K2..K5 and ~thousands of random graphs (n ≤ 7) yields min `Q = −0.500000`, never below; bipartite `K_{n,n}` attains exactly `−0.5`. Supremum `1` is not attained by any finite graph with edges.

**Honesty caveats.**
- **Directed out-of-scope.** For `gDirected = True` the lower bound fails (directed modularity has no −½ floor and a different ceiling); the lemma requires `¬gDirected g`. A directed `Q` report MUST be labelled "bound not guaranteed" (AVI-533 INV-Q1 caveat).
- **Weighted out-of-scope.** Bounds are for the unweighted simple support graph `G_s`; `edgeWeight` is ignored by Leiden/scoring (AVI-510 §0).

> **Constrains:** `modularity` on undirected `G_s`; fixture 5 in §5.
> **Acceptance property.** `∀ g cm. ¬gDirected g ∧ m ≥ 1 ⇒ (-0.5 ≤ Q(π) ≤ 1)`.

---

## 4. Complexity Bounds (Lean-relevant: the auditable projection + the cross-check script)

Model: **N** nodes, **E_s** edges of `G_s`, **C** = number of communities. Structures: `Data.Map.Strict` (balanced tree, `O(log)` access), `Data.Vector.Unboxed` CSR in the Leiden state. These match AVI-533 §4; the Lean artifact re-proves the *numeric* claim, and its cross-check script inherits the same bounds.

| Component | Bound | Grounding |
|---|---|---|
| `computeCommunityStats` (the quantity `modularity` reuses) | `O(N + E_s)` time, `O(N + C)` memory | one edge pass for `σ_in` (`Community.hs:100`), one node pass for `σ_tot`/degrees (`Community.hs:94-99`). |
| `modularity` projection (the Lean `Q` evaluated on fixtures) | `O(C)` time | single `foldl'` over `C` communities, constant work per community (`si/twoM − (st/twoM)²`). |
| **Total `modularity g cm`** | **O(N + E_s)** (dominated by `computeCommunityStats`) | no loop over graph structure and no recursion; bounded by `cm` size + the single edge pass. |
| **Cross-check script** (Lean `Q` vs Haskell `modularity` on pinned fixtures) | `O(N + E_s)` per fixture | recomputes `Q` from edge-list as fallback (§5 fixture 6) then compares; linear in fixture size. |

> **Acceptance (complexity assertion).** `modularity` does not scan `gEdges` on the hot path — its cost is bounded by `computeCommunityStats`'s single edge pass, so wiring it in adds `O(C)` to the already-`O(N+E_s)` community pipeline (`community-detection` §Requirement "Domain.Community — Leiden algorithm three phases").

---

## 5. Termination

`modularity` is a finite sum over a finite partition (`C < ∞`, each `B_c` finite). It contains **no loop over graph structure and no recursion**; it terminates in `O(N + E_s)` unconditionally (bounded by the size of `cm` and the single pass in `computeCommunityStats`). The cross-check script (§6 contract) is a finite fold over the finite pinned-fixture set, so it terminates. The only iterative algorithm in scope for INV-Q3 — Leiden local moving — is covered by the termination argument in AVI-510 §3.2: a strictly-increasing `Q_γ` over a finite set of partitions, capped by `resMaxIterations`.

---

## 6. Cross-Check Contract ("Haskell property tests and the Lean proof agree on the spec")

**Purpose.** Assert that the Lean proof's numeric claim for `Q` equals Haskell `modularity` on the pinned fixtures (AVI-533 §5), so the formalized math and the implemented pipeline agree on every spec point.

**Equality assertion.** For each pinned fixture `f` in §5, let `Q_lean(f)` be the value of the Lean theorem/lemma evaluated on `f`'s exact rational data, and `Q_hs(f) = modularity g_f cm_f` the Haskell `Double`. The contract requires:
```
∀ f in pinned fixtures.  | Q_lean(f) − Q_hs(f) | ≤ eps
```
where `eps` is a small tolerance (e.g. `2^-24`) accounting for `Double` rounding; on the **rational** fixtures the values are exact and the difference is `0`. The Lean side is proven over `ℚ`/`ℝ` (exact); the Haskell side computes in `Double`; the contract pins fixtures to rationals so both sides reduce to the same exact value.

**Pinned fixtures named (from AVI-533 §5):**

| # | Fixture | Partition | Exact `Q` | Asserted invariant |
|---|---|---|---|---|
| 1 | Triangle `{a,b,c}` (3 edges), single community `cm = {0 ↦ [a,b,c]}` | all-in-one | `0` | `Q == 0` (INV-Q5; INV-Q1: in range) |
| 2 | Triangle, all-singletons `cm = {0↦[a],1↦[b],2↦[c]}` | singletons | `−1/3` | `Q == −(1/(2·3))²·Σ k_v² = −(1/6)²·(2²·3) = −1/3 < 0` (INV-Q5) |
| 3 | Two 4-cliques joined by one bridge (`d–e`), Leiden output | `{a,b,c,d},{e,f,g,h}` | computed; in `[−½,1]` | INV-Q1 range (fixture 5 of AVI-533) |
| 4 | Path `p1..p6`, local-move from `cm0` | after `localMovesFrom` | `Q(after) ≥ Q(before)` | INV-Q3 (local-move step only; not across merge) |
| 5 | Any reachable undirected `cm` on a fixture | arbitrary | `−½ ≤ Q ≤ 1` | INV-Q1 range bound |
| 6 | Recompute `Q` directly from `gEdges` vs `computeCommunityStats`-based `modularity` | same `cm` | equal | CO note 1 fallback; `σ_in` double-count guard (§0.3) |
| 7 | Two calls `modularity g cm` | same `cm` | bit-equal | INV-Q4 determinism |

**Cross-check script spec (authorship PO/DevOps).** A script that, for each fixture `f` above: (a) builds `g_f` via `extractionFromLists`/`buildGraph` (`Graph.hs`), (b) computes `Q_hs = modularity g_f cm_f`, (c) evaluates the Lean theorem/lemma on `f`'s exact rational adjacency, (d) asserts `|Q_lean − Q_hs| ≤ eps`. The script names every fixture in the table and the exact `Q` column; a mismatch is a spec violation.

> **Constrains:** `modularity` (§2.2 wiring), `computeCommunityStats` (`Community.hs:92`), the fixtures in `tests/Graphos/Domain/CommunitySpec.hs` conventions (`testNode`, `cliqueEdges`, `buildGraph`).

---

## 7. Acceptance-Criteria Mapping (AVI-618 checklist)

| AVI-618 acceptance criterion | Delivered by |
|---|---|
| Every `INV-Q0`..`INV-Q5` has exactly one named Lean theorem/lemma with a formal statement | §2 (`modularity_edgesAbsent_zero`, `modularity_bounded`, `modularity_partitionOnly`, `modularity_localMoveNonDecreasing`, `modularity_deterministic`, `modularity_fixturesExact`) |
| The range bound is a named lemma with explicit hypotheses | §3 (`modularity_range_bound`, hypotheses in "Hypotheses (pinned)") |
| Shared notation glossary entries exist for every symbol used, no divergence from `docs/math-requirements/` | §0 (inherited vs Lean symbols; reconciliation notes §0.3) |
| Cross-check contract names the pinned fixtures and the equality assertion | §6 (fixtures table + `|Q_lean − Q_hs| ≤ eps` assertion) |

---

## 8. Literature

- **Newshyld, J.** (2004). *Modularity and community structure in networks.* PNAS 101(27):9522. — defines `Q = Σ_c[e_c − p_c²]` with `p_c = σ_tot(c)/(2m)`; source of the squared-`σ_tot` form in `(Q)` and the `−1/2 ≤ Q ≤ 1` range. This doc's range bound is Newshyld's, specialized to Graphos's undirected simple support graph `G_s`.
- **Blondel, V.D., et al.** (2008). *Fast algorithm for detecting community structure in networks.* J. Stat. Mech. — resolution-γ generalization and the local-moving gain (`deltaQ`, `Community.hs:236`) underlying INV-Q3.
- **Van der Groelen, S., et al.** (2019). *Uncovering hierarchical community structure with provable guarantees.* (Leiden) — local-move monotonicity (INV-Q3), refinement, and `resMaxIterations` cap (AVI-510 §3.2).

---

## 9. Math-Judgment / Escalation Log (this phase)

- **Range bound tightness.** Verified numerically this phase (exhaustive K2..K5 + random graphs n≤7: min `Q = −0.5`, never below; bipartite `K_{n,n}` attains `−0.5`). No dispute; the −½ infimum and 1 supremum are confirmed for undirected simple `G_s`.
- **Directed/weighted out-of-scope.** Deliberately excluded from INV-Q1 per AVI-533 honesty caveat; no R&D arbitration needed (within remit).
- **No escalation required.** All judgments within Graph Theory Expert confidence; the Lean body authorship is PO/DevOps (AVI-616 scope gate).

---

*(End of Phase 1 requirements. Next action: PO/DevOps decomposes the Lean 4 bodies for the six §2 stubs and §3 lemma; graphos-dev implements the §6 cross-check script against `tests/Graphos/Domain/CommunitySpec.hs` fixtures.)*
