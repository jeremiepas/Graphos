# Structural-Analysis Complexity Bounds & Scale Guards — Mathematical Requirements (Graphos Context Graph)

**Author:** Graph Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-534](/AVI/issues/AVI-534) (child of [AVI-512](/AVI/issues/AVI-512), Graph Theory element G3 / GT-03)
**Status:** requirements document — grounded in domain model + openspec specs
**Scope:** math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion. The wiring itself is [Graphos Dev](/AVI/agents/graphos-dev)'s feasibility-gated implementation.

---

## 0. Notation (unified glossary)

Notation is inherited from the companion requirements doc [AVI-518](/AVI/issues/AVI-518) `docs/math-requirements/AVI-518-shortest-path-scc.md` (§0) and the consolidated glossary `docs/math-requirements/consolidated-requirements.md` (§0). Only the objects exercised by this doc are restated:

- `V` = node set of a Graphos `Graph`, `|V| = N`. `E` = edge relation, `|E| = M`. Forward adjacency `adj⁺(u) = { v : (u,v) ∈ E }`; undirected support `G_s` has `{u,v} ∈ A ⟺ v ∈ adj⁺(u) ∨ u ∈ adj⁺(v)`.
- `NodeId = Text` (`Types/Node.hs`); `Edge { edgeSource, edgeTarget, edgeRelation, edgeWeight, edgeConfidence }` (`Types/Edge.hs:88`); `Graph { gNodes :: Map NodeId Node, gEdges :: Map (NodeId,NodeId) Edge, gAdjFwd, gAdjBack, gDirected, … }` (`Graph/Core.hs:47`).
- `CachedFGL = { cfgGraph :: FGLGraph, cfgNidMap :: Vector NodeId, cfgIdxMap :: Map NodeId Int }` (`Graph/Analysis.hs:55`); `toCachedFGL` assigns **bijective sequential indices** `0..N-1` to eliminate hash collisions (`Analysis.hs:64`, cf. G1 in the AVI-512 plan).
- `esp :: Int -> Int -> FGLGraph -> [Int]` = FGL single-source/single-target shortest path by hop count (`Analysis.hs:171`), one path per call.
- **Model.** All bounds are in the `(N, M)` vocabulary of `leiden-scalability` / `bounded-edge-inference`; adjacency is read over FGL sequential indices (CSR-equivalent dense-free layout) unless stated otherwise. The bound must match whatever structure the implementation actually uses.

> **Constrains:** `Graphos.Domain.Graph.Analysis` (`Analysis.hs`), `Graphos.Domain.Graph.FGL` (`FGL.hs`), `Graphos.Domain.Graph.Core` (`Core.hs`). Specs: `leiden-scalability`, `bounded-edge-inference`, `fgl-adapter`.

---

## 1. Formal Definitions

### 1.1 Articulation point / cut vertex
For the undirected support graph `G_s`, a vertex `a ∈ V` is an **articulation point** iff `G_s − a` (node `a` and all incident edges deleted) has more connected components than `G_s`. `Art(G) = { a : a is an articulation point }`.

> **Constrains:** `articulationPoints :: Graph -> [NodeId]` (`Analysis.hs:107`) and `articulationPointsWithCached :: CachedFGL -> [NodeId]` (`Analysis.hs:111`) via FGL `ap`.

### 1.2 Biconnected component
A **biconnected component** (block) is a maximal edge-set of `G_s` with no cut vertex; equivalently a maximal subgraph admitting a cycle through every pair of its vertices (or an isolated bridge edge). `BCC(G)` partitions the edge set into blocks.

> **Constrains:** `biconnectedComponents :: Graph -> [[NodeId]]` (`Analysis.hs:119`) and `biconnectedComponentsWithCached :: CachedFGL -> [[NodeId]]` (`Analysis.hs:123`) via FGL `bcc`.

### 1.3 Dominator
For a directed graph rooted at `r`, node `d` **dominates** `u` iff every path `r → u` passes through `d`. The **immediate dominator** `idom(u)` is the unique closest such `d ≠ u`. `Dom(r)` = the immediate-dominator forest (`Analysis.hs:131`).

> **Constrains:** `dominators :: Graph -> NodeId -> Map NodeId (Maybe NodeId)` (`Analysis.hs:131`) and `dominatorsWithCached :: CachedFGL -> NodeId -> Map NodeId (Maybe NodeId)` (`Analysis.hs:135`) via FGL `dom`.

### 1.4 Edge betweenness (the quantity to compute)
For an ordered source pair `(s,t)`, let `σ(s,t)` = number of hop-minimal walks `s → t`, and `σ(s,t | e)` = those passing through edge `e`. The **edge betweenness** of `e` is
```
BC(e) = Σ_{(s,t) : s≠t} σ(s,t | e) / σ(s,t).                                    (1)
```
The **sampled edge betweenness** over a source set `S ⊆ V`, `|S| = s`, rescaled to be unbiased, is
```
ŷ_S(e) = (N / s) · Σ_{x ∈ S} δ_x(e),   δ_x(e) = Σ_{t≠x} σ(x,t | e) / σ(x,t).   (2)
```
`E[ŷ_S] = BC` when `S` is a uniform random sample of sources (each source drawn w.p. `s/N`, scaled by `N/s`). The current `Analysis.hs:157` routine computes neither `(1)` nor an unbiased estimator of it — see §6.1.

> **Constrains:** `edgeBetweenness :: Graph -> Map (NodeId,NodeId) Double` (`Analysis.hs:152`) and `edgeBetweennessWithCached :: CachedFGL -> Map (NodeId,NodeId) Double` (`Analysis.hs:157`).

---

## 2. Complexity Assertions (the G3 core)

All bounds are worst-case and stated in the `(N, M)` model with adjacency over FGL sequential indices. The bound must match the data structure actually used; if adjacency is read via `Data.Map`/`Data.Set` the bound degrades to `O((N+M)·log N)`, and the spec assertion must state which holds.

| Algorithm | Object | Assert bound | Grounding / citation | Surface |
|---|---|---|---|---|
| Articulation points | `Art(G)` | `O(N + M)` | single DFS low-link (Tarjan 1972); one pre-order pass, each arc inspected twice | `ap` (`Analysis.hs:115`) |
| Biconnected components | `BCC(G)` | `O(N + M)` | stack-based DFS; each node pushed/popped once, each arc once | `bcc` (`Analysis.hs:127`) |
| Dominators | `Dom(r)` | `O(N + M)` reducible; `O(N·M)` worst (irreducible) | Cooper–Harvey–Warford with loop-free speedup (CHW 1982); reducible code graphs ⇒ operative bound `O(N+M)` | `dom` (`Analysis.hs:141`) |
| Edge betweenness, exact | `BC` (§1.4) | `O(N·M)` | Brandes (2001): one forward BFS + one backward accumulation per source = `O(N+M)` per source × `N` sources | new routine (§6.2) |
| Edge betweenness, sampled | `ŷ_S` (§1.4) | `O(s·(N + M))`, `s = |S| ≤ 500` | same per-source cost as exact, over `s` sources; `s ≤ 500` scale guard (§4) | new routine (§6.2) |

**Theorem 2.1 (articulation/bcc linear).** Both `ap` and `bcc` perform exactly one depth-first traversal of `G_s`. Let `μ = |{ v : v unvisited }|`. Each recursive visit sets a node's discovery state and decrements `μ` by 1; each arc is traversed a constant number of times (twice for undirected `G_s`). Since `μ` strictly decreases and is bounded below by 0, the traversal terminates, and total work is `O(N + M)`. ∎

**Theorem 2.2 (dominator linear on reducible graphs).** On a reducible graph every node has a well-defined unique immediate dominator computable by processing nodes in reverse-BFS level order with the loop-free optimization (a node with a single predecessor at its own level inherits it). Each node is reduced once and each arc read once ⇒ `O(N + M)`. On irreducible graphs the base iterative reduction re-processes stragglers; the standard bound is `O(N·M)` (Cooper–Harvey–Warford 1982). Code-dependency graphs produced by Graphos are reducible (every edge traces a call/import/contains relation with well-founded nesting), so the operative bound is `O(N + M)`. ∎

**Theorem 2.3 (betweenness `O(N·M)` exact, `O(s·(N+M))` sampled).** For unweighted graphs a single-source shortest-path DAG is produced by one BFS `O(N+M)`; the Brandes backward dependency accumulation touches each arc at most twice ⇒ `O(N+M)` per source. Summing over all `N` sources gives `O(N·M)`; over a sample of `s` sources gives `O(s·(N+M))`. ∎

> **Constrains:** complexity assertions are requirements on `Analysis.hs`; they must match the data structure actually used (complexity-honesty lens).

---

## 3. Error Bound for Sampled Betweenness

### 3.1 Estimator and unbiasedness
Fix the sampling scheme: draw `S` as `s` i.i.d. uniform sources from `V`, and form `ŷ_S` by `(2)`.

**Lemma 3.1 (unbiasedness).** For any edge `e`, `E[ŷ_S(e)] = BC(e)`. *Proof.* Averaging `δ_x(e)` over a uniform source `x` gives `(1/N) Σ_x δ_x(e)`. Multiplying by the scale factor: `E[ŷ_S(e)] = (N/s)·s·(1/N)Σ_x δ_x(e) = Σ_x δ_x(e) = BC(e)`. ∎

### 3.2 Probabilistic relative-error bound
Each per-source contribution is bounded: `0 ≤ δ_x(e) ≤ N − 1` (at most `N−1` targets, each contributing a fraction in `[0,1]`). Hence each summand `(N/s)·δ_{x_i}(e)` lies in `[0, N(N−1)/s]`. By **Hoeffding** for the sum of `s` independent bounded terms, for any fixed edge `e` and any `t > 0`:
```
Pr[ |ŷ_S(e) − BC(e)| > t ] ≤ 2 · exp( − 2 t² s / (N(N−1))² ).                          (3)
```
Setting `t = ε·‖BC‖₂` and requiring the right side ≤ `δ` gives the per-edge sample guarantee
```
   s ≥ ( N(N−1) )² · ln(2/δ) / ( 2 ε² ‖BC‖₂² ).                                          (4)
```
A **union bound** over all `M` edges yields `‖ŷ_S − BC‖_∞ ≤ ε‖BC‖₂` simultaneously with probability ≥ `1 − δ` under the same `s` with `δ → δ/M`. Because `‖ŷ_S − BC‖₂ ≤ √M · ‖ŷ_S − BC‖_∞`, the aggregate statement `Pr[ ‖ŷ_S − BC‖₂ / ‖BC‖₂ ≤ ε ] ≥ 1 − δ` holds under a correspondingly smaller effective `ε` (absorbing the `√M` factor).

> **Honesty caveat (complexity-honesty lens).** Bound `(4)` is rigorous but conservative: it scales with `N⁴` because the per-coordinate range is `O(N)`. Betweenness sampling error is inherently **distribution-dependent** — no deterministic worst-case relative bound exists in general (Borgs et al., *"Distribution-Sensitive and Centroid-Based Approximation of Betweenness Centrality"*, 2014). On sparse graphs (`M = O(N)`) the effective single-source contribution norm is `O(log N)`–`O(√N)`, so the practical sample complexity is on the order of `s = O( √N · ln(1/δ) / ε² )`, matching Borgs et al.'s centroid-based result. The **operative acceptance is the empirical test** in §5, not bound `(4)`.

### 3.3 Well-definedness on the Graphos graph class
`BC` is well-defined for the graphs Graphos produces precisely because `σ(s,t) ≥ 1` whenever a hop-minimal walk exists, so every term in `(1)` is a fraction in `[0,1]` with no division by zero; pairs with `σ(s,t) = 0` (unreachable) contribute 0. For directed graphs the sum is over ordered pairs; for undirected `G_s` it is over unordered pairs and the code's `2/(N(N−1))` normalization normalizes by the number of unordered pairs.

> **Constrains:** `edgeBetweennessWithCached` normalization (`Analysis.hs:177`) and the `srcIdx < tgtIdx` pair restriction (`Analysis.hs:169`).

---

## 4. Scale Guards

Graphos scale guards (`leiden-scalability`, `bounded-edge-inference`) cap memory-bound work at repository scale. The following guards apply to the analysis surface:

- **SG-1 (sampled-source cap).** Sampled betweenness samples at most `maxSampledSources = 500` sources by default (`Analysis.hs:163`), configurable via `PipelineConfig`. The sampled estimator MUST rescale by `N / |S|` (§3.1) so the cap does not bias the mean.
- **SG-2 (forbid exact all-pairs at repo scale).** Exact edge betweenness is `O(N·M)`; it MUST NOT be invoked on the full repository graph when `N > N_exact_cap`. The cap is `N_exact_cap = 10_000` code nodes, consistent with the 10K code-node cap in `bounded-edge-inference` / checkpoint controls; above it the pipeline MUST use the sampled estimator (SG-1). This is a complexity-honesty constraint, not a product decision.
- **SG-3 (per-source memory).** The Brandes forward/backward pass per source MUST allocate `O(N + M)` and MUST NOT build a fresh structure per source beyond the shared `CachedFGL` (§"Memory optimization", `Analysis.hs:4`). Any per-source `Set`/`Map` allocation that scales with `N` must be bounded by reusing the cached adjacency, mirroring the Leiden CSR requirement (`leiden-scalability`).

**Coherence check.** SG-2 (`N_exact_cap = 10_000`) combined with SG-1 (`s ≤ 500`) bounds the worst case at repo scale to `O(500·(N+M))`, consistent with the Leiden `O(maxIter·(E_s+N))` budget and the 75k/80k-node budget in `bounded-edge-inference`.

---

## 5. Acceptance Criteria

**AC-1 (complexity assertion in spec).** The spec asserts `O(N+M)` for `ap`, `bcc` and `O(N+M)`/`O(N·M)` reducible/worst-case for `dom`, and `O(N·M)` exact / `O(s·(N+M))` sampled for edge betweenness, each naming the grounded surface above. *Check:* requirement header present, each with its citation.

**AC-2 (sampled-betweenness relative error on synthetic graph).** On a sparse synthetic graph with `N` nodes and `M = O(N)` edges, draw `s` uniform sources, compute exact `BC` and sampled `ŷ_S`; assert
```
‖ŷ_S − BC‖₂ / ‖BC‖₂ ≤ ε
```
for a configurable `ε` (default `0.1`). *Check:* property/benchmark test on e.g. `N ∈ {500, 2000}`, `M ≈ 5N`, reported ratio ≤ `ε`.

**AC-3 (scale guard enforced).** For `N > N_exact_cap = 10_000` the pipeline invokes the sampled estimator (SG-1), never exact all-pairs; for `s ≤ 500` the estimator rescales by `N/s` so the mean is unbiased. *Check:* unit test that the exact path is bypassed above the cap and the rescale factor is applied.

**AC-4 (unbiasedness regression).** With `s = N` (all sources) and the `(N/s)` factor, `ŷ_S` equals the exact `BC` (no bias, no scaling error). *Check:* property test `‖ŷ_{V} − BC‖ / ‖BC‖ ≈ 0`.

---

## 6. Feasibility Answers & Arbitration Flags

This section answers the concrete "is it sound / what is the complexity" questions (remit item 3) and flags decisions requiring [Graphos Dev](/AVI/agents/graphos-dev).

### 6.1 Q: Does the current `edgeBetweenness` compute standard edge betweenness? — NO
The current routine (`Analysis.hs:157`) iterates over sampled sources and, for each target with `srcIdx < tgtIdx`, calls `esp` **once** and increments every edge on that single path by 1 (`Analysis.hs:167-176`). Standard edge betweenness `(1)` sums `σ(s,t|e)/σ(s,t)` over **all** shortest paths per pair. The current code therefore (a) uses only one representative shortest path per pair rather than the fractional sum, and (b) never divides by `σ(s,t)`. Consequence: the quantity is a biased heuristic, sensitive to which shortest path `esp` returns (tie-breaking), and is neither `BC` nor an unbiased estimator of `BC`. The claimed relative-error bound in §3 cannot apply to it. **Decision (AF-1):** implement standard Brandes betweenness as the exact reference `BC`; only then does the sampled-error bound in §3 hold.

### 6.2 Q: What is the current complexity, and what is the target? — current is too slow
Per `(src,tgt)` pair the code makes one `esp` call at `O(N+M)` worst case; there are `s·N` such pairs (`s ≤ 500` sources, `N` targets), giving `O(s·N·(N+M))`. The target (Theorem 2.3) is `O(s·(N+M))` sampled / `O(N·M)` exact via Brandes' single-BFS-per-source. The current per-pair approach is a factor of ~`N` slower. **Decision (AF-2):** replace the per-pair `esp` loop with the Brandes forward-BFS + backward-accumulation per source.

### 6.3 Q: Do articulation/bcc/dominator bounds hold for undirected Graphos graphs? — YES, with a directedness caveat
`toCachedFGL` builds the FGL graph from `gEdges` source→target pairs only (`Analysis.hs:76`) — no reverse edges even when `gDirected g = False`. FGL `ap`/`bcc`/`dom` traverse using the directed adjacency as built. For articulation points and biconnected components the classical definition is over the **undirected** support graph `G_s`; if FGL follows directed adjacency, then on an undirected Graphos graph these would be computed on the directed version, which can differ from `Art(G_s)` / `BCC(G_s)`. **Decision (AF-3):** verify FGL `ap`/`bcc` treat edges bidirectionally, or that Graphos reverses-embeds edges for undirected graphs, before asserting `O(N+M)` correctness on undirected graphs. The cost bound `O(N+M)` holds regardless of directedness.

### 6.4 Q: Is the dominance bound really `O(N+M)`? — conditional on reducibility
Yes for reducible graphs (Theorem 2.2). Graphos code-dependency graphs are reducible in practice, but a graph containing a "diamond" with two independent paths rejoining (an irreducible 2-cycle analog) can make `dom` hit `O(N·M)`. **Decision (AF-4):** assert `O(N+M)` for the reducible class Graphos produces; document the `O(N·M)` worst-case caveat for irreducible inputs.

### 6.5 Arbitration log
- **AF-1 (semantic gap in betweenness).** Current routine ≠ standard edge betweenness. **Decision: implement Brandes exact betweenness as the reference** (feasibility item O1).
- **AF-2 (complexity gap).** Current `O(s·N·(N+M))`; target `O(s·(N+M))`. **Decision: switch to Brandes** (same item O1).
- **AF-3 (directedness of ap/bcc).** Undirected-graph correctness of FGL `ap`/`bcc`. **Decision: verify or reverse-embed edges** (item O2).
- **AF-4 (irreducible dominators).** Worst case `O(N·M)`. **Decision: document reducible-class bound** (item O2).
- **AF-5 (relative-error bound form).** No deterministic worst-case relative bound exists (distribution-dependent). **Decision: probabilistic Hoeffding bound §3.2 + empirical test AC-2** (item O3).

---

## 7. Spec-vs-Code Gaps (for [Graphos Dev](/AVI/agents/graphos-dev))

- **GAP-1 (no exact/sampled Brandes betweenness).** `edgeBetweennessWithCached` computes a biased one-pair heuristic at `O(s·N·(N+M))`; implement standard edge betweenness (exact `O(N·M)`, sampled `O(s·(N+M))`) per §2/§3.
- **GAP-2 (no error bound).** No `ε`-bound on sampled betweenness exists; add the estimator + empirical relative-error test AC-2.
- **GAP-3 (no complexity assertions in spec).** None of `ap`/`bcc`/`dom`/`edgeBetweenness` asserts a cost; add the SHALL assertions from §2.
- **GAP-4 (no scale guard for analysis).** No cap prevents exact all-pairs at repo scale; add SG-1/SG-2.
- **GAP-5 (directedness of ap/bcc).** Verify undirected-graph correctness of FGL `ap`/`bcc` (§6.3).

---

## 8. Worked Example (concrete deliverable — golden test fixture)

Graph (undirected): nodes `{a,b,c,d}`; edges `{a–b, b–c, c–d}` (a path).
- **Articulation points:** `{b, c}` (removing either disconnects the path); `a`, `d` are not cut vertices.
- **Biconnected components:** blocks `{a–b}`, `{b–c}`, `{c–d}` (each edge is a bridge).
- **Edge betweenness:** only edge `b–c` has nonzero betweenness; `BC(b–c) = 2/3` after the `2/(N(N−1))` normalization over unordered pairs (pairs `{a,d}`, `{a,c}`, `{b,d}` all route through `b–c`; here `σ=1` for each).
- **Sampled check:** sampling sources `{a, d}` rescaled by `N/s = 4/2 = 2` must reproduce `BC(b–c)` within `ε` on denser synthetic graphs (AC-2).

> **Acceptance (golden test):** the exact routine reproduces `{b,c}`, the three bridges, and `BC(b–c) = 2/3` on this fixture; the sampled routine satisfies AC-2 on a synthetic sparse graph.

---

## 9. Determinism Lens

Graphos promises deterministic output (`gHash` over sorted keys; `computeGraphHash` sorts node ids and edge tuples, `Core.hs:259`). Decision points that could break determinism for these surfaces:

- **`esp` tie-breaking.** When multiple hop-minimal paths exist between a pair, which one is returned affects the biased heuristic (§6.1). The correct Brandes computation sums over **all** shortest paths (via `σ` counts), so it is invariant to tie-breaking — fully deterministic given `(V,E)`.
- **Sampled-source selection.** The sampled estimator's value depends on which sources are drawn. Constrain: sample in a **deterministic seedable** order (e.g., fixed pseudo-random seed or ascending `NodeId`), so repeated runs produce identical `ŷ_S`. The *unbiasedness* (Lemma 3.1) holds for any sample; determinism requires a fixed sample.
- **FGL index order.** `toCachedFGL` assigns bijective sequential indices in `NodeId` key order (`Analysis.hs:64`), so iterating FGL indices = iterating `NodeId`s ascending — deterministic.
- **`ap`/`bcc`/`dom` output.** Partitions (cut vertices, blocks, dominator forest) are canonical invariants; only integer labeling is order-dependent and deterministic under the fixed index order (INV preserved).

> **Constrains:** `toCachedFGL` (`Analysis.hs:64`), Brandes accumulation order, and the sampled-source selection routine.

---

## 10. Acceptance-Criteria Mapping & Open Items

**Acceptance bar met:**
- **Definitions before theorems:** §1 → §2 (Thm 2.1–2.3, all with proof sketches). ✓
- **Complexity assertions + data-structure grounding:** §2 table + theorems + honesty caveat. ✓
- **Error bound:** §3 (unbiasedness Lemma 3.1, probabilistic Hoeffding bound `(3)`–`(4)`, honesty caveat, well-definedness 3.3). ✓
- **Scale guards:** §4 (SG-1/SG-2/SG-3) with coherence check. ✓
- **Checkable acceptance:** AC-1 (complexity assertion), AC-2 (relative error), AC-3 (scale guard), AC-4 (unbiasedness). ✓
- **Mapped to surfaces:** every section names the constrained module/spec (§0, §6, §7). ✓

**Open items (owner + exact action):**

| ID | Item | Owner | Action | Blocks |
|---|---|---|---|---|
| O1 | Implement standard edge betweenness (Brandes): exact `O(N·M)` and sampled `O(s·(N+M))`, per-source `O(N+M)` memory over `CachedFGL`; remove the biased one-pair heuristic (§6.1/AF-1, AF-2). Add unbiasedness regression AC-4. | [Graphos Dev](/AVI/agents/graphos-dev) | Confirm semantics; implement; property test | AVI-534 AC-2/AC-4 |
| O2 | Assert complexity of `ap`/`bcc`/`dom` in spec (§2); verify undirected-graph correctness of FGL `ap`/`bcc` (AF-3) and document reducible-class dominator bound (AF-4). | [Graphos Dev](/AVI/agents/graphos-dev) | Add spec assertions; verify reverse-embed or FGL semantics | AVI-534 AC-1 |
| O3 | Add sampled-betweenness relative-error test AC-2 on sparse synthetic graphs; wire `maxSampledSources`/`N_exact_cap` scale guards (SG-1/SG-2). | [Graphos Dev](/AVI/agents/graphos-dev) | Benchmark + unit test; config wiring | AVI-534 AC-2/AC-3 |

**Surface map:**
- Types: `Types/Node.hs`, `Types/Edge.hs`, `Graph/Core.hs` (`Graph`, `computeGraphHash`).
- Analysis: `Graph/Analysis.hs` (`articulationPoints:107`, `biconnectedComponents:119`, `dominators:131`, `edgeBetweenness:152`, `toCachedFGL:64`).
- FGL adapter: `Graph/FGL.hs` (`toFGL`, `nidToInt`).
- Specs: openspec `leiden-scalability`, `bounded-edge-inference`, `fgl-adapter`, `domain-types`.

---

## 11. Joint-Surface Note (with Category Theory Expert)

The dominator forest and the biconnected-component/block partition are **quotients** of `G` (and `G_s`) along combinatorial congruences: domination folds along the "dominated-by" preorder, and blocks fold along cut-vertex connectivity. This is dual to the merge colimit ([AVI-511](/AVI/issues/AVI-511)) — merge glues views into one graph (a *colimit*), while these decompositions fold a graph along a congruence (a *limit-like* quotient). The combinatorial content both must agree on is identical: the node set `V` and edge relation `E` are preserved or transformed consistently. Any functorial claim about the pipeline must therefore respect that `Art`/`BCC`/`Dom` are defined purely on `(V,E)` (§0); a colimit that introduces phantom edges would change these partitions. No conflict: they operate at opposite arrows on the same objects.
