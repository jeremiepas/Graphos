# Design: SCC / condensation decomposition adoption (AVI-1291, element G5)

Adopts the mathematical register of `docs/math-requirements/AVI-518-shortest-path-scc.md` and `docs/math-requirements/AVI-523-cycle-detection-connectivity.md` into an OpenSpec capability. Notation: `docs/math-requirements/consolidated-requirements.md` §0, extended with the reachability objects of AVI-518 §0.

## 1. Math approach

**Objects.** `V = Map.keys (gNodes g)` (`|V| = N`); directed relation `E ⊆ V × V` = ordered-pair projection of `gEdges`; forward adjacency `adj⁺(u) = gAdjFwd[u]`. Reachability `u ⤳ v` = reflexive-transitive closure of `E`; mutual reachability `u ↔ v ⟺ (u ⤳ v) ∧ (v ⤳ u)`.

**SCC.** Strongly connected components are the equivalence classes of `↔` (equivalence: reflexive, symmetric by definition, transitive by transitivity of `⤳` — Thm 2.1). SCCs form a **partition** of `V`, unique up to relabeling (Thm 2.2). The **condensation** `cond(G)` has the SCCs as vertices and an arc `A → B` (`A ≠ B`) iff `∃ a ∈ A, b ∈ B. (a,b) ∈ E`.

**Condensation is a DAG (Thm 2.3).** Any condensation cycle would make all its members mutually reachable — collapsing distinct SCCs; contradiction. Hence topological order is well-defined.

**Acyclicity (Thm 2.5, INV-C5).** `G` is a DAG ⟺ every SCC is a singleton ∧ `ℓ = 0` (self-loop count).

**Algorithm (Tarjan, single DFS) and termination.** `index[]/lowlink[]`, stack `S`, seeds and neighbor iteration in **ascending `NodeId` order**; the measure `μ = |{ u : index[u] = ∞ }|` strictly decreases at every atomic step ⇒ termination; each node pushed/popped once, each arc inspected once ⇒ **O(N + E)**. (AVI-518 §3.1; Kosarajan two-pass alternative uses `gAdjBack` as the transpose.)

**Directedness scope (AVI-518 §0/§6.2, carries into scenarios).** Reachability/SCC act on the directed relation `E` (source→target), which is what `shortestPath`/BFS/DFS traverse (FGL edges from `gEdges` forward pairs). This is intentionally distinct from `neighbors`/`degree`, which union both directions when `¬gDirected`. The spec keeps traversal forward-only as the canonical SCC reachability (AF-2 decision recorded).

## 2. Invariants adopted (each becomes a scenario)

- **INV-7** SCC partition: `⋃_c comp c = V`, pairwise disjoint, no empty component.
- **INV-8** shortest path is a genuine `E`-walk, hop-minimal: `shortestPath g s t == Just p` ⟺ `s ⤳ t`, `length p − 1 = d(s,t)`; `shortestPath g u v == Just [u,v]` for `(u,v) ∈ E`.
- **INV-9** reachability = closure, invariant under key permutation.
- **INV-10** partition refinement under single-edge delta: adding an arc only merges SCCs (coarsening), removal only refines.
- **INV-C1** connected components partition `V`; **INV-C6** WCC coarsens SCC (`C_w ≤ C_s`); **INV-C3** isolated nodes = degree-0 = singleton components; **INV-C5** DAG test with self-loop clause.

## 3. Requirements register → spec mapping

| Req | Statement | Classification | Acceptance |
|---|---|---|---|
| G5-R1 | SCCs are the unique partition of `V` under `↔`; labeling deterministic (ascending seeds); INV-7 | new-feature | property (partition) + golden fixtures F1/F5 + INV-10 scenario |
| G5-R2 | `cond(G)` is a DAG; topological order exists | new-feature | acyclicity assertion on fixtures |
| G5-R3 | Reachability decided by hop-minimal `shortestPath` over `E`; DAG ⟺ singleton SCCs ∧ `ℓ = 0` (INV-8/9, INV-C5) | new-feature | golden fixture F3 + F2; forward-only traversal statement |
| G5-R4 | SCC ≠ biconnected components / articulation points; no reuse | new-feature | disambiguation scenario (§1.4/§6.3) |
| G5-R5 | Connectivity decomposition coarsens SCC (connected/WCC/isolated; INV-C1/C3/C6) | new-feature | golden fixture F7 + property tests |
| G5-R6 | Complexity `O(N + E)`; all-pairs forbidden (scale guard, GT-W4) | new-feature | complexity assertion on 78k/80k fixture |

## 4. Surface grounding

- **Capabilities**: new `scc-decomposition`; consumed unchanged: `05-path` (shortest path / no-path-found semantics — G5-R3's decision procedure), `fgl-adapter` (bijective `NodeId`→sequential index in key order: ascending-seed determinism), `07-context-selection` (future consumer; not modified), `leiden-scalability` (CSR/vocabulary consistency).
- **Modules constrained**: `src/Graphos/Domain/Graph/Core.hs` (`Graph:47`, `addEdges:170`, `mergeGraphs:140` — INV-10), `src/Graphos/Domain/Graph/Query.hs` (`shortestPath:80`, `breadthFirstSearch:49`, `depthFirstSearch:63`, `neighbors:37`, future SCC entry points), `src/Graphos/Domain/Graph/Analysis.hs` (`toCachedFGL:64`, `articulationPoints:19`, `biconnectedComponents:21` — distinct invariant, never reused for SCC).

## 5. Consistency with the consolidated layer

- **Quotient/colimit duality (AVI-518 §11, AVI-523 §11):** condensation is a quotient folding along `↔`; merge is a colimit gluing views. Both act purely on `(V, E)` at opposite arrows; no conflict with G2/AVI-511 colimit claims and no weakening of `merge-confluence`. The quotient lattice `G → SCC-cond → WCC-cond → CC-cond` is consistent with INV-C6.
- **G4 reduces to G2:** G5 introduces no merge/colimit claim; it constrains post-merge graph analysis only. INV-10 ("merge/addEdges can only coarsen the SCC partition") is derived from the `Map`-union semantics already pinned by `merge-confluence` M4/M5 — no new conflict-resolution semantics introduced.
- **GT-W4 (all-pairs forbidden)** is restated as the G5-R6 scale guard, not altered.
- Open arbitration items (AF-1 hop vs weight; AF-2 forward-only traversal; FGL self-loop sanitization AF-1 of AVI-523) are recorded here as documented decisions; changing them later is a spec change, not silent behavior.

## 6. Feasibility gate

Archival gated on `head-of-dev` confirmation of Haskell impact (expected: new pure `Domain.Graph` functions + property/golden tests; no pipeline/wiring change) via a comment on [AVI-1291](/AVI/issues/AVI-1291).