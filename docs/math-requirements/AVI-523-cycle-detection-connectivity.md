# Cycle Detection & Connectivity Decomposition — Mathematical Requirements (Graphos Context Graph)

**Author:** Graph Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-523](/AVI/issues/AVI-523) (child of [AVI-512](/AVI/issues/AVI-512))
**Extends:** [AVI-518](/AVI/issues/AVI-518) (§0 notation, reachability/SCC/directed-cycle groundwork) and the unified glossary in `docs/math-requirements/consolidated-requirements.md`.
**Status:** requirements document — grounded in domain model + openspec specs.
**Scope:** math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion.

---

## 0. Notation (unified glossary)

Notation is drawn from [AVI-518](/AVI/issues/AVI-518) §0 and extended below with cycle/connectivity objects. Line numbers verified against checked-in sources on branch `AVI-512-add-math-in-graphos-project`.

- `NodeId = NodeId Text` — `Graphos.Domain.Types.Node.NodeId` (`Types/Node.hs:7`).
- `Node` — `Types/Node.hs:9` (12 strict fields; `nodeCommunityId :: Maybe Int`, `nodeDegree :: Maybe Int`, `nodeIsBridge :: Maybe Bool`).
- `Edge = Edge { edgeId, edgeSource :: NodeId, edgeTarget :: NodeId, edgeRelation :: Relation, edgeWeight :: Double, edgeConfidence :: Confidence Double }` (`Types/Edge.hs:18`). `Relation ∈ {Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred}`.
- **Two edge-keyed representations (critical distinction, carries across this doc).**
  - `LabeledGraph.gEdges :: Map EdgeId Edge` (`Types/Graph.hs:24`) — edges keyed by a **distinct** `EdgeId`. This representation **may contain multi-edges** (two edges over the same ordered pair with different `EdgeId`s) and self-loops. It is the extraction / merge view.
  - `Graph.gEdges :: Map (NodeId, NodeId) Edge` (`Graph/Core.hs:47`) — edges keyed by the **ordered pair**. Parallel edges over the same ordered pair **collapse** to one; self-loops `(u,u)` survive. This is the internal working graph on which every analysis algorithm runs.
- `Graph = Graph { gNodes :: Map NodeId Node, gEdges :: Map (NodeId,NodeId) Edge, gAdjFwd :: Map NodeId (Set NodeId), gAdjBack :: Map NodeId (Set NodeId), gDirected :: Bool, … }` (`Graph/Core.hs:47`). `computeGraphHash` sorts node ids + edge tuples (`Core.hs:259`) — the determinism anchor.
- `neighbors g nid = if gDirected g then gAdjFwd[nid] else gAdjFwd[nid] ∪ gAdjBack[nid]` returns a `Set NodeId`; `degree g nid = Set.size (neighbors g nid)` (`Graph/Query.hs:37,44`).
- **Directed relation.** `E ⊆ V × V` = ordered-pair projection of `gEdges`: `(u,v) ∈ E ⟺ ∃ e. edgeSource e = u ∧ edgeTarget e = v`. `V = Map.keys (gNodes g)`, `|V| = N`. Forward adjacency `adj⁺(u) = gAdjFwd[u]`.
- **Undirected support graph.** `G_s = (V, A)` with `{u,v} ∈ A ⟺ v ∈ neighbors(u) ∨ u ∈ neighbors(v)`. `m = |A|` counts undirected support edges (a self-loop `{u,u}` is one support edge incident to `u` only; see §1.6).
- **Cycle.** See §1.2. `q` = cyclomatic number (circuit rank), §1.5.
- **Connected / weakly connected components.** See §1.4. `C_c` = number of (undirected) connected components; `C_w` = number of weakly connected components (directed).
- **Self-loop count.** `ℓ = |{ u ∈ V : (u,u) ∈ E }|` (directed reading) = `|{ u : u ∈ neighbors(u) }|` (undirected reading).

> **Constrains:** `Graph` (`Core.hs:47`), `LabeledGraph` (`Types/Graph.hs:24`), `neighbors/degree` (`Query.hs:37,44`), `stronglyConnectedComponents` (`Query.hs:129`), `articulationPoints/biconnectedComponents` (`Analysis.hs:19,21`), `buildGraph/mergeGraphs/addEdges` (`Core.hs:96,140,170`), `computeCommunityStats` (`Community.hs:92`), `cohesionScore` (`Community.hs:385`).

**Directedness scope statement (carries across this doc).** Cycle/connectivity objects act on BOTH the directed relation `E` (for directed cycles, acyclicity, weak components) and the undirected support graph `G_s` (for undirected cycles, connected components). This mirrors the §0 directedness caveat of [AVI-518](/AVI/issues/AVI-518): `neighbors`/`degree` union both directions when `¬gDirected`, while `E` is always source→target. Connected-components operates on `G_s`; weakly-connected components operate on `E` read as undirected.

---

## 1. Formal Definitions

### 1.1 Graph classes Graphos produces

Graphos produces **simple directed graphs** at the internal `Graph` level (`gEdges :: Map (NodeId,NodeId) Edge`), possibly with self-loops, and **multigraphs** at the `LabeledGraph` level (distinct `EdgeId`s). All algorithmic claims below are stated on the internal `Graph` (and its `E` / `G_s`), which is what `stronglyConnectedComponents`, `articulationPoints`, and any new routine traverse. The `LabeledGraph` multigraph caveat is called out explicitly where it changes cycle semantics (§1.6, §6.3).

### 1.2 Cycle

Let `G = (V,E)` (directed) or `G_s = (V,A)` (undirected).

- **Directed cycle.** A sequence `(v_0, v_1, …, v_{k-1})` of **distinct** vertices with `(v_i, v_{i+1 mod k}) ∈ E` for all `i`. Its **length** is `k`. A **self-loop** `(u,u)` is the length-1 directed cycle (`k=1`). A **2-cycle** `(u,v),(v,u)` is `k=2`.
- **Undirected simple cycle.** A sequence `(v_0, …, v_{k-1})` of **distinct** vertices with `{v_i, v_{i+1 mod k}} ∈ A` for all `i`. For a **simple** graph (Graphos internal level) the minimal length is `k=3`; a self-loop `{u,u}` is the degenerate `k=1` undirected cycle; `k=2` is impossible in a simple graph (it would require two parallel support edges, which collapse).
- **Acyclic.** `G` (or `G_s`) is **acyclic** iff it contains no cycle. A directed acyclic graph is a **DAG**.
- **Cycle detection predicate.** `hasCycleUndirected g :: Bool` = "does `G_s` contain an undirected cycle?" `isAcyclicDirected g :: Bool` = "does `E` contain a directed cycle?"

> **Constrains:** `hasCycleUndirected`, `isAcyclicDirected` — **new predicates** (§7 GAP-1).

### 1.3 Cyclomatic number (circuit rank)

For a graph with `N` vertices, `E_count = |E|` (or `|A|`) edges, and `C_c` connected components, the **cyclomatic number** is
`q = E_count − N + C_c`.
`q` equals the dimension of the cycle space = the minimum number of edges to remove to make the graph a forest. A spanning forest has exactly `N − C_c` edges; every extra edge closes exactly one independent cycle.

### 1.4 Connectivity decomposition

- **Connected component (undirected).** A maximal `W ⊆ V` such that `∀ u,v ∈ W. u ⤳_A v` (an `A`-walk exists) and `∀ u ∈ W, w ∉ W. ¬(w ⤳_A u)` (no edge leaves `W`). The connected components partition `V`; their count is `C_c`.
- **Weakly connected component (directed).** A maximal `W ⊆ V` under mutual undirected reachability in the underlying undirected graph of `E` (ignore direction). Count `C_w`. Every strongly connected component lies within exactly one weakly connected component; distinct WCCs are disjoint, so `C_w ≤ C_s` (number of SCCs); each WCC is a union of SCCs.
- **Isolated node.** `v ∈ V` with `degree g v = 0` (`neighbors g v = ∅`). An isolated node is a connected component (and a WCC) of size 1 with no incident edges.

> **Constrains:** `connectedComponents`, `weaklyConnectedComponents`, `isolatedNodes` — **new routines** (§7 GAP-1). `degree`/`neighbors` (`Query.hs:37,44`).

### 1.5 Degenerate graphs

- **Empty graph** (`N = 0`): zero components, zero cycles, vacuously acyclic.
- **Single node, no edges** (`N = 1, E_count = 0`): one connected component (the node, isolated), one SCC, acyclic, `q = 0`.
- **Self-loop only** (`N = 1, E = {(u,u)}`): one undirected cycle (length 1) and one directed cycle (length 1); `q = 1`; **not** acyclic.

### 1.6 Self-loops and the multigraph caveat

At the internal `Graph` level a self-loop `(u,u)` is a genuine edge and is a cycle of length 1 in **both** readings (§1.2). At the `LabeledGraph` level, **parallel edges** over the same ordered pair do *not* collapse; two edges `u→v` and `v→u` yield internal arcs `(u,v)` and `(v,u)` — a directed 2-cycle — whereas two edges both `u→v` collapse to a single arc and produce **no** cycle. Hence multi-edge cycles manifest as **directed** cycles only when both directions are present; an undirected simple cycle never arises from collapsing. Cycle claims are therefore made on the internal `Graph`/`E`/`G_s`; the `LabeledGraph` multigraph caveat is a separate surface (§6.3).

---

## 2. Theorems (with proof sketches)

### Theorem 2.1 (connected components partition V)
The undirected-reachability relation `⤳_A` (reflexive-transitive closure of `A`) is an equivalence relation (reflexive, symmetric because `A` is symmetric, transitive). Its classes are pairwise-disjoint, cover `V` (every node is in its own class), and are uniquely determined. Hence connected components are a **partition** of `V`.
**Proof sketch.** Symmetry of `A` ⇒ `⤳_A` symmetric; reflexivity/transitivity by closure. Equivalence classes partition the base set. ∎

> **Invariant INV-C1 (connected-components partition).** `⋃_i W_i = V`; `W_i` pairwise-disjoint; each `|W_i| ≥ 1`. Checkable: `(M . concat) comps == M (Map.keys (gNodes g))` and `M (W_i ∩ W_j) = ∅` for `i ≠ j`, `M s = Data.Set.fromList s`.

### Theorem 2.2 (weakly connected components partition V; they coarsen SCCs)
WCCs are the connected components of the underlying undirected graph of `E`; by Thm 2.1 they partition `V`. Moreover each SCC is contained in exactly one WCC (mutual reachability in `E` implies undirected reachability), so the WCC partition is a **coarsening** of the SCC partition: every WCC is a disjoint union of SCCs, and `C_w ≤ C_s`.
**Proof sketch.** If `u ↔ v` (SCC) then `u ⤳_A v` (ignore direction) ⇒ `u,v` in one WCC. Distinct SCCs map to distinct WCCs or the same; distinct WCCs are disjoint. Hence the refinement map `SCC ↦ WCC` is surjective onto the WCCs it touches ⇒ `C_w ≤ C_s`. ∎

> **Constrains:** `stronglyConnectedComponents` (`Query.hs:129`) + new `weaklyConnectedComponents`; INV-7 of [AVI-518](/AVI/issues/AVI-518) (SCC partition) is the refinement, INV-C1 (WCC partition) the coarsening.

### Theorem 2.3 (undirected cycle detection via DFS back-edge, with self-loop exception)
Run iterative DFS over `G_s` from each unvisited seed (ascending `NodeId`). An undirected cycle exists **iff** the traversal encounters either (a) an edge to an already-visited vertex that is **not** the current node's DFS-parent (a *back-edge*), or (b) a self-loop `{u,u}`.
**Proof sketch.** (⇒) If an undirected cycle `C` exists, traverse its vertices; the first edge of `C` not on the DFS tree connects two vertices already discovered, one an ancestor ⇒ back-edge. (⇐) A back-edge `(x,y)` with `y` an ancestor of `x` closes the DFS-tree path into a cycle; a self-loop is a length-1 cycle by definition. In a simple graph a "second edge to parent" is the tree edge reversed and is **not** a cycle; only a genuine back-edge or self-loop certifies one. ∎

> **Invariant INV-C2 (cycle-detection soundness).** `hasCycleUndirected g == True ⟺ G_s contains an undirected cycle ⟺ q ≥ 1` (§2.4). Checkable against golden fixtures (§8): every fixture with a cycle returns `True`, every acyclic fixture returns `False`.

### Theorem 2.4 (cyclomatic number identity)
For any graph, `q = E_count − N + C_c` equals the dimension of the cycle space. In particular the graph has a cycle **iff** `q ≥ 1` **iff** `E_count > N − C_c`.
**Proof sketch.** A spanning forest of the graph has exactly `N − C_c` edges (each component contributes a tree on its nodes; a forest with `C_c` roots and `N` nodes has `N − C_c` edges). Adding the remaining `E_count − (N − C_c)` edges, each introduces exactly one new independent cycle (fundamental cycle basis). These cycles are independent and span the cycle space ⇒ dimension `= E_count − N + C_c`. ∎

> **Constrains:** `isAcyclicDirected`/`hasCycleUndirected` may assert `q = E_count − N + C_c` as a cross-check; counts come from `Map.size (gEdges g)`, `Map.size (gNodes g)`, and INV-C1's `C_c`.

### Theorem 2.5 (acyclicity test for directed graphs)
A directed `G` is a DAG **iff** every SCC is a singleton **and** `ℓ = 0` (no self-loop).
**Proof sketch.** (⇒) A directed cycle of length `k ≥ 2` makes its vertices mutually reachable ⇒ an SCC of size `≥ 2`; a self-loop makes `{u}` contain a cycle though it stays a singleton SCC. So acyclicity forces all SCCs singleton and `ℓ = 0`. (⇐) If all SCCs are singletons there is no cycle of length `≥ 2`; if additionally `ℓ = 0` there is no length-1 cycle ⇒ no directed cycle ⇒ DAG. ∎

> **Constrains:** `isAcyclicDirected g = (all ((==1) . length) (SCC values)) && (ℓ == 0)`. Reuses `stronglyConnectedComponents` (`Query.hs:129`) — **no new SCC computation**; only a self-loop scan `O(N)`.

### Theorem 2.6 (isolated nodes are exactly degree-0 nodes; each is its own component)
`v` is isolated ⟺ `degree g v = 0` ⟺ `v` is a connected component (and a WCC) of size 1.
**Proof sketch.** `degree g v = |neighbors g v| = 0` ⇒ no incident edge ⇒ `v` unreachable from any other node and vice-versa ⇒ `[v] = {v}` is a whole component. Conversely a size-1 component has no incident edge ⇒ degree 0. ∎

> **Invariant INV-C3 (isolated-node identity).** `isolatedNodes g == [ nid | nid <- keys (gNodes g), Set.null (neighbors g nid) ]`, and each such `nid` is a singleton in both `connectedComponents` and `weaklyConnectedComponents`.

### Theorem 2.7 (modularity well-defined on graphs with isolated nodes)
Isolated nodes contribute **zero** to modularity: they add `0` to `m = (Σ_v deg v)/2`, `0` to every community's `σ_tot` and `σ_in`. Hence deleting/adding isolated nodes leaves `Q_γ` unchanged.
**Proof sketch.** In `computeCommunityStats` (`Community.hs:94-95`) `degrees` maps **all** nodes (isolated ⇒ `0`); `m = sum degrees / 2`; isolated nodes add `0`. `σ_tot[comm(v)] += deg v = 0`; `σ_in` counts internal edges and isolated nodes have none. So the modularity sum is unaffected except that `m = 0` (pure-isolated graph) is a degenerate case requiring an `m > 0` guard. ∎

> **Constrains:** `computeCommunityStats` (`Community.hs:92`), `cohesionScore` (`Community.hs:385`, guarded by `max 1` denominator at `Community.hs:277,402`). **Acceptance:** a graph with only isolated nodes yields `m = 0`; detection must treat this as "no edges ⇒ each node its own singleton community" rather than divide by zero.

### Theorem 2.8 (self-loop is simultaneously a length-1 cycle in both readings)
A self-loop `(u,u) ∈ E` is an undirected cycle `{u,u} ∈ A` (§1.2) **and** a directed cycle `(u,u) ∈ E` (§1.2). Hence `ℓ > 0 ⟹ hasCycleUndirected g ∧ ¬isAcyclicDirected g`.
**Proof sketch.** Directly from §1.2 definitions and `gAdjFwd[u] ∋ u` when `(u,u) ∈ E` (`buildGraph`, `Core.hs:104`). ∎

> **Invariant INV-C4 (self-loop ⇒ both-cycle).** `ℓ = |{ u : u ∈ neighbors(u) }|`; `ℓ > 0 ⟹ hasCycleUndirected g == True ∧ isAcyclicDirected g == False`; `ℓ == 0` is necessary (not sufficient) for `isAcyclicDirected`.


---

## 3. Algorithms & Termination

### 3.1 Undirected cycle detection (primary; iterative DFS)
Iterative Tarjan-style DFS over `G_s` using an explicit frame stack (no per-node recursion; scales to repository size). For each seed in **ascending `NodeId`**, push a frame `(u, nextNeighborIndex, parent)`. Pop and advance:
- If `nextNeighborIndex ≥ |neighbors(u)|`: pop the frame; if it had a parent, that edge was a tree edge (no cycle from it).
- Else read neighbor `w = sorted(neighbors(u))[nextIndex]`; bump index and push frame `(u, nextIndex+1, parent)`:
  - if `w` unvisited: visit, push frame `(w, 0, Just u)` (tree edge).
  - else if `w == parent` at tree-edge depth (the edge we arrived on): skip (not a cycle in a simple graph).
  - else if `w` visited and `w ≠ parent`: **back-edge ⇒ cycle found** (`hasCycleUndirected = True`).
  - if `{u,w} = {u,u}` (i.e. `w == u`, a self-loop): **cycle found**.
Return `True` on first back-edge/self-loop; `False` after all seeds exhausted.

**Strictly-decreasing measure (termination).** Let `μ = |{ u : unvisited }|`. Each `visit(w)` marks one node ⇒ `μ` drops by exactly 1; each frame advance consumes one (node,neighbor) pair, bounded by total adjacency `Σ_v deg(v) = 2·|A|`. Hence progress is bounded ⇒ termination. Total work: each node pushed/popped once, each undirected edge inspected at most twice ⇒ **O(N + |A|)**.

### 3.2 Connected components (iterative BFS/DFS)
Standard multi-source BFS over `G_s`: process seeds ascending; each unvisited node gets the next component id; mark visited; enqueue neighbors. Terminates because the visited set grows monotonically and is bounded by `V`. Each node enqueued once, each edge scanned once ⇒ **O(N + |A|)**. Component ids assigned in ascending-seed order (determinism, §9).

### 3.3 Weakly connected components
Run §3.2 over the **undirected reading of `E`** — i.e., WCC adjacency = `gAdjFwd[u] ∪ gAdjBack[u]` **regardless of `gDirected`** (ignore direction). Same bound **O(N + E)** as §3.2. By Thm 2.2 the result coarsens the SCC partition (`Query.hs:129`).

### 3.4 Acyclicity (directed)
`isAcyclicDirected g`: (1) compute SCCs via `stronglyConnectedComponents` (`Query.hs:129`); (2) assert every component has length `1`; (3) scan `gEdges` for any `(u,u)` and assert `ℓ = 0`. Cost = SCC `O(N+E)` (§3.1 of [AVI-518](/AVI/issues/AVI-518)) + self-loop scan `O(E)` ⇒ **O(N + E)**. No new iterative algorithm — reuses the proven SCC routine (§2.5).

### 3.5 Cyclomatic number (formula)
`q = Map.size (gEdges g) − Map.size (gNodes g) + C_c`, where `C_c = length (connectedComponents g)`. Reads are `O(1)`/`O(N)`; the only iteration is §3.2. Report `q` as an integer cross-check (§2.4). **Termination:** trivial (pure arithmetic after counting).

> **Constrains:** new routines mirror the `*WithCached` pairing already established (`Query.hs:11,14,16`); all traverse the `CachedFGL`/adjacency already built by `toCachedFGL` (`Analysis.hs:64`).

---

## 4. Complexity Bounds

Model: **N** nodes, **E** arcs of `E`, **|A|** support edges, **C_c** connected components, **C_w** weak components, adjacency via `gAdjFwd/gAdjBack :: Map NodeId (Set NodeId)` (or CSR `VU.Vector` as Leiden uses). All bounds match the data structures actually used.

| Operation | Algorithmic bound | Data-structure caveat | Grounding |
|---|---|---|---|
| `hasCycleUndirected` (§3.1) | `O(N + |A|)` | `O((N+|A|)·log N)` over Map/Set; single DFS pass | new routine |
| `connectedComponents` (§3.2) | `O(N + |A|)` | same caveat; ascending-seed labeling | new routine |
| `weaklyConnectedComponents` (§3.3) | `O(N + E)` | adjacency = fwd ∪ back regardless of `gDirected` | new routine |
| `isAcyclicDirected` (§3.4) | `O(N + E)` | reuses SCC; + `O(E)` self-loop scan | `stronglyConnectedComponents` (`Query.hs:129`) |
| cyclomatic number `q` (§3.5) | `O(N + |A|)` | dominated by §3.2 counting | INV-C2 cross-check |
| self-loop scan `ℓ` | `O(E)` | single pass over `gEdges` | INV-C4 |

**Scale guard (consistent with Leiden / bounded-edge-inference).** All routines are linear in `N + E`, matching the Leiden local-moving budget (`leiden-scalability`, O(1) per move, O(N) grouping) and the bounded-edge-inference **75k-node / 80k-edge** repository-scale target (`bounded-edge-inference` §50). No per-node `Set` allocation beyond `O(C_c)` component buckets; component labeling reuses the `deepseq`/NFData discipline of `stronglyConnectedComponentsWithCached` (`Query.hs:139`). No dense all-pairs; connectivity is single-pass.

> **Acceptance (complexity assertion):** `connectedComponents` / `hasCycleUndirected` on a ~78k-node / ~80k-edge graph complete in time linear in `N + E` (compare against the bounded-edge-inference 75k/80k budget); heap profile shows `O(C_c)` component buckets, not `O(N)` per-node allocation.

---

## 5. Partition Invariants

Every operation claiming a cycle answer or a connectivity decomposition must preserve these; each is QuickCheck-ready.

- **INV-C1 (connected-components partition).** `⋃_i W_i = V`; values pairwise-disjoint; no empty component. Checkable: `(M . concat) comps == M (keys gNodes)` and `M (W_i ∩ W_j) = ∅` for `i ≠ j`.
- **INV-C2 (cycle-detection soundness + cyclomatic cross-check).** `hasCycleUndirected g == True ⟺ q ≥ 1 ⟺ E_count > N − C_c`. Independent of algorithm: assert `q = E_count − N + C_c` and `hasCycleUndirected == (q ≥ 1)` on random graphs.
- **INV-C3 (isolated-node identity + singleton components).** `isolatedNodes g == [nid | null (neighbors g nid)]`; each isolated node is a size-1 component in both `connectedComponents` and `weaklyConnectedComponents`.
- **INV-C4 (self-loop ⇒ both-cycle).** `ℓ = |{ u : u ∈ neighbors(u) }|`; `ℓ > 0 ⟹ hasCycleUndirected g == True ∧ isAcyclicDirected g == False`; `ℓ == 0` is necessary (not sufficient) for `isAcyclicDirected`.
- **INV-C5 (DAG iff singleton SCCs + no self-loop).** `isAcyclicDirected g ⟺ (all ((==1) . length) (SCC vals)) ∧ ℓ == 0`. Cross-check against `stronglyConnectedComponents` ([AVI-518](/AVI/issues/AVI-518) INV-7).
- **INV-C6 (WCC coarsens SCC).** The SCC partition refines the WCC partition: every SCC is contained in exactly one WCC. Checkable: `fmap (Scc ↦ WCC) . SCCs == WCCs ∘ (relabel)`.
- **INV-C7 (modularity isolation-invariance).** Adding/removing an isolated node leaves `m`, all `σ_tot`, and `Q_γ` unchanged (Thm 2.7); a pure-isolated graph yields `m = 0` and must not divide by zero in `cohesionScore`.
- **INV-C8 (subgraph connectivity is consistent).** For `subgraph g S` (`Query.hs:246`) and `W ⊆ S`, the component of a node `v ∈ S` inside `subgraph g S` is `comp_g(v) ∩ S` — subselection cannot merge two distinct `g`-components into one; it can only split or truncate. Checkable: `connectedComponents (subgraph g S)` partitions `S`, and each part ⊆ some `g`-component.

---

## 6. Feasibility Answers & Arbitration Flags

### 6.1 Q: Is undirected cycle detection sound and complete? — YES (Thm 2.3, INV-C2)
Iterative DFS over `G_s` detects a cycle iff one exists (back-edge or self-loop). Completeness rests on the fundamental-cycle argument (Thm 2.4); soundness on the back-edge/self-loop certificate. The only subtlety: a simple graph's tree-edge reversal to the parent is **not** a cycle — handled by tracking the DFS parent (§3.1).

### 6.2 Q: Can acyclicity be checked without a new traversal? — YES, reuse SCC (Thm 2.5)
A directed DAG test reduces to "all SCCs singleton AND no self-loop". This **reuses** `stronglyConnectedComponents` (`Query.hs:129`) plus an `O(E)` self-loop scan — no new iterative algorithm, so termination and the `O(N+E)` bound inherit from [AVI-518](/AVI/issues/AVI-518) §3.1–3.2. The self-loop clause is the **new** requirement: a singleton SCC with a self-loop is *not* acyclic (§2.5, INV-C5).

### 6.3 Q: Do multi-edges create cycles? — Only as directed 2-cycles, at the LabeledGraph level (Thm 2.1 caveat)
At the internal `Graph` level parallel edges collapse (`gEdges :: Map (NodeId,NodeId) Edge`), so an undirected simple cycle never arises from collapsing. Two opposite-direction edges `u→v`,`v→u` yield internal arcs forming a **directed** 2-cycle (detected by `isAcyclicDirected`, not `hasCycleUndirected`). Two same-direction parallel edges collapse to one arc ⇒ no cycle. Cycle claims are therefore made on the internal `Graph`; the `LabeledGraph` multigraph behavior is a separate surface and must not be conflated with undirected simple cycles. **Decision: AF-3.**

### 6.4 Q: Does the pipeline correctly handle disconnected graphs? — PARTIALLY (gaps flagged)
`shortestPath` returns `Nothing` across components ([05-path](/AVI/issues/AVI-512) "Disconnected nodes return 'no path found'"), and Leiden iterates over **all** nodes so isolated nodes remain singleton communities with `m` unaffected (Thm 2.7). **Gaps:** there is no exposed `connectedComponents`/`weaklyConnectedComponents`/`isolatedNodes` predicate (§7 GAP-1), so downstream consumers (subgraph selection, cohesion over components) cannot query connectivity; and `cohesionScore` on a community of *only* isolated nodes relies on the `max 1` guard (`Community.hs:277`) — correct but untested for the degenerate `m = 0` case. **Decision: AF-4.**

### 6.5 Arbitration log
- **AF-1 (FGL undefined behavior on self-loops).** FGL's `ap`/`bcc` (`Analysis.hs:15,19`) assume no self-loops; a self-loop in `gEdges` may produce undefined articulation/biconnectivity results. **Decision: sanitize** `gEdges` (drop self-loops) before `toCachedFGL`, or document that `articulationPoints`/`biconnectedComponents` operate on the loop-support graph. Open item for Dev (O1).
- **AF-2 (directed `articulationPoints`/`biconnectedComponents` directionality).** `toCachedFGL` builds FGL edges from `gEdges` forward pairs only (`Analysis.hs:76`); FGL `ap`/`bcc` treat edges as undirected, so these operate on the underlying undirected graph even when `gDirected = True`. **Decision: document** that articulation/biconnectivity are inherently undirected support-graph invariants; do not reuse them for directed structure. Open item (O2).
- **AF-3 (multigraph vs simple cycle scope).** §6.3: cycle claims made on internal `Graph`; `LabeledGraph` multi-edge cycles are directed-only. **Decision: mandated** scope statement.
- **AF-4 (exposed connectivity API).** §6.4: require `connectedComponents`, `weaklyConnectedComponents`, `isolatedNodes` to make disconnected-subgraph handling queryable downstream. **Decision: mandate** new routines (O3).


---

## 7. Spec-vs-Code Gaps (for [Graphos Dev](/AVI/agents/graphos-dev))

- **GAP-1 (no cycle/connectivity routines exist).** `Graphos.Domain.Graph.Query`/`.Analysis` export `shortestPath`, `breadthFirstSearch`, `depthFirstSearch`, `stronglyConnectedComponents`, `articulationPoints`, `biconnectedComponents`, `dominators`, `edgeBetweenness` — **none** computes undirected cycles, connected components, weakly connected components, isolated nodes, or directed acyclicity. Implement:
  - `hasCycleUndirected :: Graph -> Bool` (iterative DFS, §3.1).
  - `connectedComponents :: Graph -> Map Int [NodeId]` (§3.2).
  - `weaklyConnectedComponents :: Graph -> Map Int [NodeId]` (§3.3).
  - `isolatedNodes :: Graph -> [NodeId]` (§2.6, INV-C3).
  - `isAcyclicDirected :: Graph -> Bool` (§3.4, reuses SCC).
  Plus `*WithCached` variants mirroring `Query.hs:11,14,16`.
- **GAP-2 (no cyclomatic-number / DAG cross-check).** No routine asserts `q = E_count − N + C_c` or exposes acyclicity; downstream subgraph selection (`07-context-selection`) cannot cheaply detect cycles before expansion.
- **GAP-3 (FGL self-loop undefinedness).** `articulationPoints`/`biconnectedComponents` route FGL edges from `gEdges` (`Analysis.hs:76`) with no self-loop sanitization; a self-loop may yield undefined results (AF-1).
- **GAP-4 (degenerate modularity untested).** `cohesionScore` guards degree-0 via `max 1` (`Community.hs:277,402`) but no test covers a community of only-isolated nodes / `m = 0` (AF-4, INV-C7).

---

## 8. Worked Example (concrete deliverable — golden-file test fixtures)

Each fixture is a self-contained assertion set for the new routines. Node ids are lowercase single letters so ascending `NodeId` order = alphabetical; component ids are assigned in ascending-seed order.

### Fixture F1 — single directed cycle (SCC, length 3)
Nodes `{a,b,c}`; arcs `E = {(a,b),(b,c),(c,a)}`.
- `hasCycleUndirected = True` (triangle).
- `isAcyclicDirected = False` (the 3-cycle).
- `stronglyConnectedComponents = {0:[a,b,c]}` (one SCC of size 3).
- `connectedComponents = {0:[a,b,c]}`; `weaklyConnectedComponents = {0:[a,b,c]}`.
- `isolatedNodes = []`; `q = 3 − 3 + 1 = 1`.

### Fixture F2 — directed DAG (acyclic, connected)
Nodes `{a,b,c,d}`; arcs `E = {(a,b),(a,c),(b,d),(c,d)}`.
- `hasCycleUndirected = True` (undirected support has cycles: a-b-d-c-a).
- `isAcyclicDirected = True` (no directed cycle).
- `stronglyConnectedComponents = {0:[a],1:[b],2:[c],3:[d]}` (all singletons).
- `connectedComponents = {0:[a,b,c,d]}` (one undirected component); `weaklyConnectedComponents = {0:[a,b,c,d]}`.
- `isolatedNodes = []`; `q = 4 − 4 + 1 = 1` (undirected support has exactly one independent cycle).

### Fixture F3 — self-loop only (length-1 cycle in both readings)
Nodes `{u}`; arcs `E = {(u,u)}`.
- `hasCycleUndirected = True` (self-loop).
- `isAcyclicDirected = False` (self-loop is a directed cycle).
- `stronglyConnectedComponents = {0:[u]}` (singleton SCC, but **not** acyclic — INV-C5 requires the `ℓ == 0` clause).
- `connectedComponents = {0:[u]}`; `weaklyConnectedComponents = {0:[u]}`.
- `isolatedNodes = []` (degree of `u` is 1, not 0); `ℓ = 1`; `q = 1 − 1 + 1 = 1`.

### Fixture F4 — isolated node + a triangle (disconnected: two components)
Nodes `{a,b,c,z}`; arcs `E = {(a,b),(b,c),(c,a)}` and `z` with no edges.
- `hasCycleUndirected = True`; `isAcyclicDirected = False`.
- `stronglyConnectedComponents = {0:[a,b,c]}`; one singleton missing `z` ⇒ `{0:[a,b,c], 1:[z]}` if the routine labels all nodes (z is its own SCC).
- `connectedComponents = {0:[a,b,c], 1:[z]}` (two components; `C_c = 2`).
- `weaklyConnectedComponents = {0:[a,b,c], 1:[z]}`.
- `isolatedNodes = [z]`; `q = 3 − 4 + 2 = 1`.

### Fixture F5 — two disconnected triangles (disconnected subgraphs)
Nodes `{a,b,c,d,e,f}`; arcs `E = {(a,b),(b,c),(c,a),(d,e),(e,f),(f,d)}`.
- `hasCycleUndirected = True`; `isAcyclicDirected = False`.
- `stronglyConnectedComponents = {0:[a,b,c], 1:[d,e,f]}` (two SCCs).
- `connectedComponents = {0:[a,b,c], 1:[d,e,f]}` (`C_c = 2`).
- `weaklyConnectedComponents = {0:[a,b,c], 1:[d,e,f]}` (no arc joins the two triangles ⇒ `C_w = 2 = C_s`).
- `isolatedNodes = []`; `q = 6 − 6 + 2 = 2`.

### Fixture F6 — empty graph (degenerate)
Nodes `{}`; arcs `{}`.
- `hasCycleUndirected = False`; `isAcyclicDirected = True`; all decompositions empty; `isolatedNodes = []`; `q = 0`.

### Fixture F7 — weak vs strong disconnect (WCC coarsens SCC, Thm 2.2)
Nodes `{a,b,c}`; arcs `E = {(a,b),(b,c)}` (a directed path, no return arcs).
- `stronglyConnectedComponents = {0:[a],1:[b],2:[c]}` (three SCCs; the path is acyclic).
- `weaklyConnectedComponents = {0:[a,b,c]}` (one WCC; ignoring direction connects them).
- `connectedComponents = {0:[a,b,c]}`; `isolatedNodes = []`; `C_w = 1 < C_s = 3` — demonstrates INV-C6 (WCC coarsens SCC).
- `hasCycleUndirected = False` (a tree); `isAcyclicDirected = True`; `q = 3 − 3 + 1 = 1` (undirected support has exactly one cycle a-b-c-a).

> **Acceptance (golden tests):** the new routines reproduce exactly the partition counts, `isolatedNodes`, and `hasCycleUndirected`/`isAcyclicDirected` booleans above on F1–F7 (spec-scenario style); INV-C2 holds (`hasCycleUndirected == (q ≥ 1)`) and INV-C5 holds (`isAcyclicDirected == (all SCC singletons ∧ ℓ == 0)`) on every fixture; INV-C6 holds on F7.

---

## 9. Determinism Lens

Graphos promises deterministic output (`computeGraphHash` over sorted node ids + sorted edge tuples, `Core.hs:259`). Decision points that could break determinism for these surfaces:

- **Component labeling order.** Connected/weakly-connected and SCC integer ids depend on seed visitation order. Constrain: process seeds in **ascending `NodeId` order**; FGL index order (§"Bijective node-index mapping", `fgl-adapter`) already maps `NodeId` to sequential indices in key order, so iterating FGL indices = ascending `NodeId`. The **partition** (the sets) is canonical/unique (Thm 2.1); only the integer labeling is order-dependent, and it is deterministic under the fixed seed order. This mirrors the SCC determinism argument in [AVI-518](/AVI/issues/AVI-518) §9.
- **Neighbor iteration order.** In §3.1/§3.2 iterate `neighbors(u)` in ascending order; with `Data.Set` this is `Set.toList` (sorted). Deterministic given fixed seeds.
- **Self-loop / cycle certificate.** `hasCycleUndirected` returns `True` on the *first* back-edge/self-loop encountered; the boolean answer is order-independent even though the *identifying* edge is not. INV-C2 is about the boolean, so it is fully deterministic.
- **Isolated-node ordering.** `isolatedNodes` must sort its output by `NodeId` (e.g., `sort`) so the list is order-independent; membership is canonical, order is constrained.

> **Constrains:** `toCachedFGL` (`Analysis.hs:64`) sequential indexing; `computeGraphHash` (`Core.hs:259`); new routines' seed loops.

---

## 10. Acceptance-Criteria Mapping & Open Items

**Acceptance bar met:**
- **Definitions before theorems:** §1 → §2 (Thm 2.1–2.8, all with proof sketches). ✓
- **Checkable invariants:** INV-C1 (connected partition), INV-C2 (cycle soundness + cyclomatic), INV-C3 (isolated identity), INV-C4 (self-loop ⇒ both-cycle), INV-C5 (DAG test), INV-C6 (WCC coarsens SCC), INV-C7 (modularity isolation-invariance), INV-C8 (subgraph consistency). ✓
- **Termination argument:** strictly-decreasing measure `μ` (§3.1–3.5). ✓
- **Complexity bound + scale guard:** linear O(N+E) for all routines; bounded against the 75k/80k target (§4). ✓
- **Concrete deliverable:** golden-file fixtures F1–F7 in §8 (exact expected outputs). ✓
- **Mapped to surfaces:** every section names the constrained surface; §6/§10 list specs. ✓

**Open items (owner + exact action):**

| ID | Item | Owner | Action | Blocks |
|---|---|---|---|---|
| **O1** | Implement `hasCycleUndirected`, `connectedComponents`, `weaklyConnectedComponents`, `isolatedNodes`, `isAcyclicDirected` (+ `*WithCached`), preserving INV-C1–C8; add §8 golden tests + INV property tests. | [Graphos Dev](/AVI/agents/graphos-dev) | Confirm semantics; implement; run property tests | AVI-523 downstream verification |
| **O2** | Sanitize self-loops before `toCachedFGL` or document AF-1/AF-2: decide whether `articulationPoints`/`biconnectedComponents` operate on a loop-free support graph. | [Graphos Dev](/AVI/agents/graphos-dev) | Audit FGL `ap`/`bcc` on self-loop inputs; document/sanitize | AF-1, AF-2 |
| **O3** | Add a degenerate-case test for a community of only-isolated nodes / `m = 0` in `cohesionScore` (INV-C7). | [Graphos Dev](/AVI/agents/graphos-dev) | Property test: isolated-only graph ⇒ `cohesion ∈ [0,1]`, no divide-by-zero | AF-4 |
| **O4** | Fold cycle/connectivity requirements into the consolidated requirements layer (`docs/math-requirements/consolidated-requirements.md`) so the requirements layer stays single. | Head of R&D (`head-rnd`) | Add an AVI-523 section; cross-reference this doc. | consolidation coherence |

**Surface map:**
- Types: `Types/Node.hs`, `Types/Edge.hs`, `Types/Graph.hs` (`LabeledGraph`, `Extraction`).
- Core: `Graph/Core.hs` (`Graph`:47, `buildGraph`:96, `mergeGraphs`:140, `addEdges`:170, `computeGraphHash`:259).
- Queries: `Graph/Query.hs` (`neighbors`:37, `degree`:44, `stronglyConnectedComponents`:129, `subgraph`:246); cached FGL in `Graph/Analysis.hs` (`toCachedFGL`:64, `ap`/`bcc` at `15,19`).
- Community: `Community.hs` (`computeCommunityStats`:92, `cohesionScore`:385, `max 1` guards at `277,402`).
- Specs: openspec `05-path`, `neighbor-expansion`, `query-scoping`, `query-relevance-scoring`, `07-context-selection`, `leiden-scalability`, `bounded-edge-inference`, `domain-types`, `community-detection`, `fgl-adapter`.

---

## 11. Joint-Surface Note (with Category Theory Expert)

Connected components and weakly connected components are **quotients** of `G` by undirected-reachability congruences: connected-components collapses each undirected-connected block to a point; weakly-connected components collapses along the *weaker* undirected-reachability congruence (ignoring direction). These are **coarser** than the SCC-condensation quotient of [AVI-518](/AVI/issues/AVI-518) §11 (which folds along mutual *directed* reachability). The lattice of quotients, from finest to coarsest, is:
`G  →  SCC-condensation  →  WCC-condensation  →  connected-components-condensation`.
This is categorically dual to the merge colimit ([AVI-511](/AVI/issues/AVI-511)): merge *glues* views into one apex (a colimit), while these decompositions *fold* a graph along a congruence (a limit-like quotient). The combinatorial content both must agree on is identical — the node set `V` and edge relation `E` are transformed consistently — so any functorial claim about the merged graph must respect that every quotient is defined purely on `(V,E)` (§0). No conflict: merge and decomposition operate at opposite arrows on the same objects, and INV-C6 (WCC coarsens SCC) is precisely the naturality of the fold map across the quotient lattice.
