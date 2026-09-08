# Shortest-Path Reachability & SCC Decomposition — Mathematical Requirements (Graphos Context Graph)

**Author:** Graph Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-518](/AVI/issues/AVI-518) (child of [AVI-512](/AVI/issues/AVI-512))
**Status:** requirements document — grounded in domain model + openspec specs
**Scope:** math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion.

---

## 0. Notation (unified glossary)

Notation is drawn from the unified glossary in `docs/math-requirements/consolidated-requirements.md` (§0) and extended below with reachability/SCC-specific objects. Line numbers verified against checked-in sources.

- `NodeId = Text` — `Graphos.Domain.Types.Node.NodeId`.
- `Node` — see consolidated glossary; `nodeCommunityId :: Maybe Int`, `nodeDegree :: Maybe Int`, `nodeIsBridge :: Maybe Bool`.
- `Edge = Edge { edgeId, edgeSource :: NodeId, edgeTarget :: NodeId, edgeRelation :: Relation, edgeWeight :: Double, edgeConfidence :: Confidence Double }` (`Edge.hs:88`). `Relation ∈ {Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred}`. `Confidence = Confidence Double`.
- `Graph = Graph { gNodes :: Map NodeId Node, gEdges :: Map (NodeId,NodeId) Edge, gAdjFwd :: Map NodeId (Set NodeId), gAdjBack :: Map NodeId (Set NodeId), gDirected :: Bool, … }` (`Graph/Core.hs:47`).
- `neighbors g nid = if gDirected g then gAdjFwd[nid] else gAdjFwd[nid] ∪ gAdjBack[nid]` returns a `Set NodeId`; `degree g nid = Set.size (neighbors g nid)` (`Graph/Query.hs:37,44`).
- **Edge relation.** `E ⊆ V × V` is the ordered-pair projection of `gEdges`: `(u,v) ∈ E ⟺ ∃ e. edgeSource e = u ∧ edgeTarget e = v`. `V = Map.keys (gNodes g)`, `|V| = N`, `|E| = E_count`. Forward adjacency `adj⁺(u) = gAdjFwd[u] = { v : (u,v) ∈ E }`.
- **Reachability.** `u ⤳ v` = the reflexive-transitive closure of `E`. Mutual reachability `u ↔ v ⟺ (u ⤳ v) ∧ (v ⤳ u)`.
- **Hop distance.** `d(u,v) = min { |p| : p is a walk u→v over E }` where `|p|` = number of edges; `∞` if `¬(u ⤳ v)`.
- **SCC.** Strongly connected component = equivalence class of `↔`. `C` = number of SCCs. `cond(G)` = condensation (DAG on SCCs).
- **m = |E_s|** = edges of the simple support graph `G_s` (`G_s` undirected; `{u,v} ∈ A ⟺ v ∈ neighbors(u) ∨ u ∈ neighbors(v)`), as in the unified glossary.

> **Constrains:** `Graphos.Domain.Graph.Core.Graph` (`Core.hs:47`), `Graphos.Domain.Graph.Query.shortestPath/breadthFirstSearch/depthFirstSearch/neighbors` (`Query.hs:20,10,37,44`), `Graphos.Domain.Graph.Analysis.articulationPoints/biconnectedComponents/dominators` (`Analysis.hs:19,21,23`).

**Directedness scope statement (critical, carries across this doc).** The reachability objects below act on the **directed edge relation** `E` (source→target). This is the relation actually traversed by `shortestPath`, `breadthFirstSearch`, and `depthFirstSearch` (all route through FGL, see §6.2). It is **not** the same as `neighbors`/`degree`, which union forward+backward adjacency when `¬gDirected g`. The distinction is flagged explicitly in §6.2.

---

## 1. Formal Definitions

### 1.1 Reachability as reflexive-transitive closure
Define `⤳` on `V` as the least relation satisfying:
- (R-refl) `u ⤳ u` for all `u ∈ V`.
- (R-step) if `u ⤳ v` and `(v,w) ∈ E` then `u ⤳ w`.

Equivalently `⤳ = (E ∪ Δ)^*` where `Δ = {(u,u) : u ∈ V}`. `⤳` is a **preorder** (reflexive + transitive); it is symmetric iff the graph is undirected *and* traversal unions both edge directions (§6.2).

### 1.2 Shortest path (hop-minimal)
For a walk `p = (v_0, …, v_k)` with `(v_i, v_{i+1}) ∈ E`, its length is `|p| = k`. A **shortest path** from `s` to `t` is a walk `p` minimizing `|p|` subject to `v_0 = s, v_k = t`; it exists iff `s ⤳ t`, and its length equals `d(s,t)`.

> **Constrains:** `shortestPath` (`Query.hs:80`) returns `Maybe [NodeId]`; `Just p` (non-null) ⟺ `s ⤳ t`.

### 1.3 Strong connectivity & SCC partition
Define `↔` on `V` by `u ↔ v ⟺ (u ⤳ v) ∧ (v ⤳ u)`. The **strongly connected components** are the equivalence classes of `↔`. The **condensation** `cond(G)` has vertex set `{ SCCs }` and an arc `A → B` (`A ≠ B`) iff `∃ a ∈ A, b ∈ B. (a,b) ∈ E`.

### 1.4 Distinction from existing undirected decompositions
Graphos already exposes `articulationPoints` and `biconnectedComponents` (`Analysis.hs:19,21`). Those operate on the **undirected support graph** `G_s` and decompose **edges / cut-vertices** (a biconnected component is a maximal edge-set with no cut vertex). **SCC is a different invariant:** it operates on the directed relation `E` and partitions **nodes** into mutual-reachability classes. They must not be conflated or reused for one another (see §6.3).

> **Constrains:** `Graphos.Domain.Graph.Analysis` (`Analysis.hs:19,21,23`) — articulation points, biconnected components, dominators. None of these computes SCC.

---

## 2. Theorems (with proof sketches)

### Theorem 2.1 (reachability is a preorder; mutual reachability an equivalence)
`⤳` is reflexive and transitive by definition (§1.1). `↔` is reflexive (from `u ⤳ u`), symmetric (defining clause is symmetric), and transitive: if `u ↔ v` and `v ↔ w` then `u ⤳ v ∧ v ⤳ w ⇒ u ⤳ w` (transitivity of `⤳`) and `w ⤳ v ∧ v ⤳ u ⇒ w ⤳ u`, hence `u ↔ w`. ∎

> **Constrains:** `shortestPath` (`Query.hs:80`) as the decision procedure for `⤳`.

### Theorem 2.2 (SCCs partition V, uniquely)
By Thm 2.1, `↔` is an equivalence relation, so its classes are pairwise-disjoint, cover `V` (every node is in its own class), and are uniquely determined. Hence SCCs are a **partition** of `V`, unique up to relabeling of component identifiers.

> **Invariant INV-7 (SCC partition).** `⋃_c comp c = V`, values pairwise-disjoint, no empty component. Holds for the output of any SCC routine.
> **Acceptance (property test):** `(M . concatMap snd) sccMap == M (Map.keys (gNodes g))` and values pairwise disjoint, where `M s = Data.Set.fromList s`.

### Theorem 2.3 (condensation is a DAG)
`cond(G)` is acyclic. **Proof sketch (counter).** Suppose a cycle `A_1 → A_2 → … → A_k → A_1` among distinct SCCs. Choose representatives `a_i ∈ A_i`; the arcs give `a_1 ⤳ a_2 ⤳ … ⤳ a_1`, so every `a_i` reaches every `a_j` and vice-versa, i.e. all lie in one mutual-reachability class — contradicting that `A_1,…,A_k` are distinct SCCs. ∎

> **Consequence.** Condensation orderings (topological sort) are well-defined; community/SCC ids induce a partial order used by subgraph selection (§10).

### Theorem 2.4 (shortest-path decides reachability)
For all `s,t`: `shortestPath g s t == Just p` ⟺ `s ⤳ t`, and when `Just`, `p` is a walk over `E` with `head p = s, tail p = t` and `length p − 1 = d(s,t)`. **Proof sketch.** `esp` (FGL shortest path by edge count) explores exactly the forward-edge closure of `E` from `s`; it returns a node-list iff `t` is reached, and the list is a minimal-edge walk. ∎

> **Constrains:** `shortestPathWithCached` (`Query.hs:84`) via FGL `esp`; `toCachedFGL` builds FGL edges from `gEdges` forward pairs only (`Analysis.hs:76`).

---

## 3. Algorithms & Termination

### 3.1 Tarjan's SCC (primary; single DFS)
Maintain per-node integers `index[]` (discovery order, `∞` = unvisited) and `lowlink[]`, a global stack `S` of nodes on the current DFS path, and a global counter. Process **seeds in ascending `NodeId` order** (§9 determinism):

```
scanscc(u):
  index[u] := low[u] := k; k := k + 1; push u onto S; onStack[u] := true
  for v in sorted(adj⁺(u)):            # ascending index (= ascending NodeId)
     if index[v] = ∞: scanscc(v) ; low[u] := min(low[u], low[v])
     else if onStack[v]:               # back/cross edge to live stack node
        low[u] := min(low[u], index[v])
  if low[u] = index[u]:                 # u is SCC root; pop until u
     repeat pop w from S; onStack[w] := false; assign w to current SCC
     until w == u
```

**Strictly-decreasing measure (termination).** Let `μ = |{ u ∈ V : index[u] = ∞ }|`. Each call to `scanscc(u)` with `index[u]=∞` sets `index[u]` and decrements `μ` by exactly 1; the inner for-loop visits each neighbor once; the pop-loop removes exactly one complete SCC. Hence `μ` strictly decreases every atomic step and is bounded below by 0 ⇒ the procedure terminates. Total work: each node pushed once and popped once (`N` pushes + `N` pops), each arc inspected once ⇒ **O(N + E)**.

**Invariant per iterative step (INV-9).** After every recursive return in `scanscc`, `{ nodes currently on S }` is a chain in `⤳` (each is reachable from the one below it along the DFS tree) — i.e., `S` always lies within a single "current-path" so that a pop yields a genuine SCC root. Equivalent checkable invariant: at the moment `low[u] == index[u]`, the popped set is exactly the equivalence class `[u]_↔` restricted to not-yet-assigned nodes.

### 3.2 Kosarajan (alternative; two DFS passes)
(1) DFS over `E`, record finish order; (2) process nodes in **descending finish order** over the **transpose** `Eᵀ = { (v,u) : (u,v) ∈ E }`, each unvisited DFS-tree root is an SCC. Termination: same single-visit measure as §3.1 per pass ⇒ O(N + E) for two passes. The transpose `Eᵀ` is exactly `gAdjBack` restricted to directed mode (`Graph/Core.hs:106`), giving direct grounding for the reversed traversal.

### 3.3 BFS reachability from a source
Standard queue BFS over `adj⁺(·)` visits each node ≤ 1 time and each arc ≤ 1 time ⇒ O(N + E); returns the set `{ v : s ⤳ v }`. Terminates because the visited set grows monotonically and is bounded by `V`.

> **Constrains:** `breadthFirstSearch` (`Query.hs:49`), `depthFirstSearch` (`Query.hs:63`), FGL `esp`/`bfs`/`dfs` (`Analysis.hs:72,58`). New SCC entry points (`shortestPathReachable`, `stronglyConnectedComponents`, and cached variants) shall mirror the `*WithCached` pairing already established (`Query.hs:11,14,16`).

---

## 4. Complexity Bounds

Model: **N** nodes, **E** arcs of `E`, **C** SCCs, adjacency via `gAdjFwd :: Map NodeId (Set NodeId)` (or CSR `VU.Vector` as Leiden uses). All bounds match the data structures actually used.

| Operation | Algorithmic bound | Data-structure caveat | Grounding |
|---|---|---|---|
| One-source reachability / BFS / DFS | `O(N + E)` | `O((N+E)·log N)` if adjacency read via `Data.Map`/`Data.Set`; effectively `O(N+E)` over FGL sequential-Int + `IntMap` | `breadthFirstSearchWithCached` (`Query.hs:53`) |
| `shortestPath` (BFS/ESP) | `O(N + E)` | same caveat; returns hop-minimal | `shortestPathWithCached` (`Query.hs:84`) |
| All-pairs reachability (N BFS) | `O(N·(N+E))` | avoid dense run at repo scale (§4 scale guard) | — |
| SCC (Tarjan, §3.1) | `O(N + E)` | `O((N+E)·log N)` over Map/Set; single DFS pass | new routine |
| SCC (Kosarajan, §3.2) | `O(N + E)` (two passes) | transpose = `gAdjBack` (directed) | new routine |
| Condensation build | `O(N + E)` | one arc per source set | new routine |

**Scale guard (100k+ nodes).** SCC and one-source reachability are linear — acceptable at repository scale, consistent with the Leiden `O(maxIter·(E_s+N))` budget (`leiden-scalability`). **All-pairs reachability `O(N·(N+E))` is quadratic and must NOT be run on the full graph**; scope it to a queried subgraph or a community (§10), or sample sources as `edgeBetweenness` already does (`Analysis.hs:164`, cap 500). This is a complexity-honesty constraint, not a product decision.

> **Acceptance (complexity assertion):** `stronglyConnectedComponents` on a 78k-node / ~80k-edge graph completes in time linear in `N + E` (compare against the bounded-edge-inference 75k/80k budget); no per-node `Set` allocation inside the DFS beyond O(C) component buckets.

---

## 5. Partition Invariants

Every operation claiming to produce an SCC decomposition or a reachability answer must preserve these; each is QuickCheck-ready.

- **INV-7 (SCC partition).** `⋃_c comp c = V`; values pairwise-disjoint; no empty component. Checkable: `(M . concatMap snd) m == M (Map.keys (gNodes g))` and `M (c ∩ c') = ∅` for `c ≠ c'`.
- **INV-8 (shortest path is a genuine E-walk).** For every `s,t`, if `shortestPath g s t == Just p` then `(head p, tail p) ∈ { (u,v) : length p > 1 }`-consecutive arcs of `E`, `head p = s`, `tail p = t`, and `length p − 1 = d(s,t)` (minimality: no shorter E-walk exists). For each arc `(u,v) ∈ E`: `shortestPath g u v == Just [u,v]` (hop distance exactly 1).
- **INV-9 (well-definedness of reachability).** `⤳` computed by `shortestPath` equals the reflexive-transitive closure of `E` (Thm 2.4); equivalently the reachability answer is invariant under any permutation of `gNodes`/`gEdges` keys (only the relation matters, not storage order).
- **INV-10 (partition refinement under edge addition/removal).** Adding an arc `(u,v)` can only **merge** SCCs (never split): the SCC partition after insertion is a coarsening of the prior partition. Symmetrically, removal can only refine. Checkable: for any graph and any single-edge delta, every pre-existing SCC is contained in exactly one post-delta SCC.
  > **Constrains:** `Graphos.Domain.Graph.Core.addEdges` (`Core.hs:170`) and `mergeGraphs` (`Core.hs:140`); any incremental SCC must respect INV-10.

---

## 6. Feasibility Answers & Arbitration Flags

This section answers the concrete "is it sound / what is the complexity" questions (remit item 3) and flags decisions that require [Graphos Dev](/AVI/agents/graphos-dev) or the Head of R&D.

### 6.1 Q: Is the current `shortestPath` sound? — YES for reachability, UNWEIGHTED
`shortestPath` (FGL `esp`) computes **hop-minimal** paths and thus correctly decides forward reachability (`Thm 2.4`). It is sound for the `05-path` requirement as written ("shortest path between two nodes … BFS via FGL `esp`"). **Caveat:** it ignores `edgeWeight`; the returned path minimizes hop count, not total weight. If Graphos semantics require *min-total-weight* shortest path (e.g., cheapest route), that is **Dijkstra**, a different algorithm (`O((N+E)·log N)` with a binary heap over `edgeWeight`) — a documented functional gap, not a bug. **Decision needed (AF-1).**

### 6.2 Q: Does directedness propagate through traversal? — PARTIALLY (inconsistency flagged)
`neighbors`/`degree` union forward+backward adjacency when `¬gDirected g` (`Query.hs:37`), so they are **symmetric** for undirected graphs. But `shortestPath`, `breadthFirstSearch`, and `depthFirstSearch` route through FGL, whose edges are the **forward pairs only** from `gEdges` (`toCachedFGL`, `Analysis.hs:76`) — so they traverse **source→target regardless of `gDirected`**. Consequence: on an undirected Graphos graph, BFS neighborhood expansion (`neighbor-expansion`, `query-scoping`) and shortest-path reachability can **disagree** on which nodes are reachable. **Decision needed (AF-2):** either reverse-embed edges into FGL for undirected graphs (build both `(u,v)` and `(v,u)`), or document that traversal is forward-only and `neighbors` is the only symmetric accessor.

### 6.3 Q: Is SCC the same as biconnected components / articulation points? — NO
Per §1.4, those operate on `G_s` and decompose edges/cut-vertices; SCC decomposes nodes over the directed relation `E`. Do not reuse `biconnectedComponents`/`articulationPoints` (`Analysis.hs:19,21`) as an SCC implementation. **Dominated by a separate SCC routine.**

### 6.4 Q: Does the SCC decomposition terminate, and at what cost? — YES, O(N+E)
§3.1–3.2 give the strictly-decreasing measure (`μ` = unvisited-node count) and the O(N+E) bound (§4). No unbounded iteration.

### 6.5 Arbitration log
- **AF-1 (weighted vs hop shortest path).** Current `shortestPath` is hop-minimal and ignores `edgeWeight`. **Decision: keep hop-minimal for AVI-518 reachability; require a separate Dijkstra functional if weight-minimal paths are needed.** Open item for Dev (O1).
- **AF-2 (undirected traversal directedness).** `neighbors` symmetric; traversal forward-only. **Decision: document forward-only traversal as the canonical reachability for SCC; align `breadthFirstSearch`/`shortestPath` to `gDirected` only if product wants symmetric undirected traversal.** Open item (O2).
- **AF-3 (all-pairs reachability scale).** Do not run `O(N(N+E))` all-pairs at repo scale; scope to subgraph/community/sample. **Decision: mandated** (scale guard, §4).

---

## 7. Spec-vs-Code Gaps (for [Graphos Dev](/AVI/agents/graphos-dev))

- **GAP-1 (no SCC routine exists).** `Graphos.Domain.Graph.Query`/`.Analysis` export `shortestPath`, `breadthFirstSearch`, `depthFirstSearch`, `articulationPoints`, `biconnectedComponents`, `dominators` — **none computes SCC.** Implement `stronglyConnectedComponents :: Graph -> Map Int [NodeId]` (+ `WithCached` variant), plus a reachability predicate `shortestPathReachable :: Graph -> NodeId -> NodeId -> Bool`.
- **GAP-2 (weight semantics).** `shortestPath` is hop-minimal; confirm whether `05-path` intends hop or weight distance. If weight, implement Dijkstra over `edgeWeight`.
- **GAP-3 (undirected traversal directedness).** §6.2: traversal is forward-only while `neighbors` is symmetric; reconcile before wiring SCC/reachability into context selection.
- **GAP-4 (no reachability in query/context).** `query-relevance-scoring` and `neighbor-expansion` use BFS neighborhoods but expose no reachability/SCC predicate; the new reachability entry point should be the single source of truth for both (§10).

---

## 8. Worked Example (concrete deliverable — golden-file test fixture)

Graph (directed): nodes `{a,b,c,d,e,f}`; arcs `E = { a→b, b→c, c→a, c→d, d→e, e→d, d→f }`.

- **SCCs:** `[{a,b,c}], [{d,e}], [{f}]` ⇒ `C = 3`.
- **Condensation** `cond(G)`: `{a,b,c} → {d,e} → {f}` (arcs `c→d`, `d→f`); acyclic per Thm 2.3. ✓
- **Reachability:** `a ⤳ {a,b,c,d,e,f}`; `f ⤳ {f}` only; `d ⤳ {d,e,f}`; `e ⤳ {e,f}`; `b ⤳ {a,b,c,d,e,f}`.
- **Shortest paths (hop):** `shortestPath a f = Just [a,b,c,d,f]` (length 3, minimal); `shortestPath a a = Just [a]`; `shortestPath f a = Nothing` (Thm 2.4).
- **INV-10 check:** adding arc `f→a` merges all three SCCs into one `{a,b,c,d,e,f}` (single condensation node).

> **Acceptance (golden test):** the routine reproduces exactly the partition, condensation arcs, and the three shortest-path answers above on this fixture (spec scenario style).

---

## 9. Determinism Lens

Graphos promises deterministic output (`gHash` over sorted keys; `computeGraphHash` sorts node ids and edge tuples, `Core.hs:259`). Decision points that could break determinism for these surfaces:

- **Seed visitation order.** SCC labeling to `CommunityId` ints depends on the order seeds are processed. Constrain: process seeds in **ascending `NodeId` order**; FGL index order (§"Bijective node-index mapping", `fgl-adapter`) already maps `NodeId` to sequential indices in key order, so iterating FGL indices = iterating `NodeId`s ascending. The **partition** (the sets) is canonical/unique (Thm 2.2); only the integer labeling is order-dependent, and it is deterministic under the fixed seed order.
- **Neighbor iteration order.** In `scanscc`, iterate `adj⁺(u)` in ascending index order; with `Data.Set` this is `Set.toList` (sorted), with CSR it is contiguous. Deterministic given fixed seeds.
- **Tie-breaking.** None required for correctness of the partition (Thm 2.2); INV-7 holds regardless of labeling.
- **Reachability answer.** `shortestPath`/reachability answers depend only on `E` (INV-8, INV-10), hence are invariant under any key permutation — fully deterministic.

> **Constrains:** `toCachedFGL` (`Analysis.hs:64`) sequential indexing; `shortestPathWithCached` (`Query.hs:84`); new SCC routine seed loop.

---

## 10. Acceptance-Criteria Mapping & Open Items

**Acceptance bar met:**
- **Definitions before theorems:** §1 → §2 (Thm 2.1–2.4, all with proof sketches). ✓
- **Checkable invariants:** INV-7 (partition), INV-8 (E-walk + minimality), INV-9 (reachability = closure), INV-10 (refinement under edge delta). ✓
- **Termination argument:** strictly-decreasing measure `μ` (§3.1–3.3). ✓
- **Complexity bound + scale guard:** O(N+E) SCC/reachability; all-pairs guarded (§4). ✓
- **Concrete deliverable:** worked example in §8 (golden-file fixture). ✓
- **Mapped to surfaces:** every section names the constrained surface; §10 lists specs. ✓

**Open items (owner + exact action):**

| ID | Item | Owner | Action | Blocks |
|---|---|---|---|---|
| O1 | Implement `stronglyConnectedComponents` (+ `WithCached`) + `shortestPathReachable`, preserving INV-7/8/9/10; add §8 golden test + INV property tests. | [Graphos Dev](/AVI/agents/graphos-dev) | Confirm semantics; implement; run property tests | AVI-518 downstream verification |
| O2 | Resolve AF-1 (hop vs weight shortest path) and AF-2 (undirected traversal directedness); document or align. | [Graphos Dev](/AVI/agents/graphos-dev) + Head of R&D | Spec-vs-code decision on `shortestPath` semantics | AF-1, AF-2 |
| O3 | Wire reachability/SCC into subgraph selection (`07-context-selection`) at community/SCC granularity without dense all-pairs. | [Graphos Dev](/AVI/agents/graphos-dev) | Scope to subgraph/community/sample (§4 scale guard) | context-selection refinement |

**Surface map:**
- Types: `Types/Node.hs`, `Types/Edge.hs`, `Graph/Core.hs` (`Graph`, `addEdges`, `mergeGraphs`).
- Queries: `Graph/Query.hs` (`shortestPath:80`, `breadthFirstSearch:49`, `depthFirstSearch:63`, `neighbors:37`); cached FGL in `Graph/Analysis.hs` (`toCachedFGL:64`, `esp/bfs/dfs` at `58,72,76`).
- Existing decompositions: `articulationPoints:19`, `biconnectedComponents:21`, `dominators:23` (NOT SCC).
- Specs: openspec `05-path`, `fgl-adapter`, `neighbor-expansion`, `query-scoping`, `query-relevance-scoring`, `leiden-scalability`, `bounded-edge-inference`, `domain-types`.

---

## 11. Joint-Surface Note (with Category Theory Expert)

The condensation `cond(G)` is a **quotient** of `G` by the mutual-reachability congruence `↔`: it collapses each equivalence class to a point and is the coarsest quotient making `⤳` "collapsed" (all nodes of an SCC identified). This is categorically dual to the merge colimit ([AVI-511](/AVI/issues/AVI-511)): merge is the *co*limit (gluing views into one graph), while SCC-condensation is a *limit-like* quotient (folding a graph along a congruence). The combinatorial content that both must agree on is identical — the node set `V` and the edge relation `E` are preserved/transformed consistently — so any functorial claim about merge must also respect that `cond` is defined purely on `(V,E)` (§0). No conflict: they operate at opposite arrows on the same objects.
