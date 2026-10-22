# scc-decomposition Specification

## Purpose

Binds the SCC/condensation register: mutual-reachability SCC is the unique partition of the node set (INV-7), the condensation of that partition is a DAG, directed reachability is decided by hop-minimal shortest paths, directed acyclicity is a pure predicate, SCC is distinguished from biconnected components, undirected/weak connectivity decompositions coarsen the SCC partition, and every decomposition runs inside the linear `O(N + E)` budget with the all-pairs scale guard. Surfaces already present in `Domain.Graph` (`stronglyConnectedComponents(WithCached)`, `shortestPathReachable`) are pinned as requirement-creation; the missing surfaces (condensation build, acyclicity predicate, connectivity decompositions + cached variants) are adopted as new-feature ahead of the separate implementation change.

## Requirements

### Requirement: SCC partition uniquely decomposes the node set (INV-7, Thm 2.2)

The system SHALL provide `stronglyConnectedComponents :: Graph -> Map Int [NodeId]` whose values are the equivalence classes of mutual directed reachability `u ↔ v ⟺ (u ⤳ v) ∧ (v ⤳ u)` on the directed relation `E ⊆ V × V` (the ordered-pair projection of `gEdges`). The classes SHALL form a partition of `V`: `⋃_c comp c = Map.keys (gNodes g)`, pairwise disjoint, none empty. Component labeling SHALL be deterministic: seed nodes processed in ascending `NodeId` order and neighbors iterated in sorted order (the `fgl-adapter` bijective key-order indexing). Adding or removing a single edge SHALL only coarsen or refine the prior partition (INV-10): every pre-existing SCC is contained in exactly one post-delta SCC. (Source: AVI-518 §1.3, Thm 2.1–2.2, §5 INV-7/INV-10.)

#### Scenario: Partition property on an arbitrary graph
- **WHEN** `stronglyConnectedComponents g` is computed on any `Graph`
- **THEN** the union of the member lists equals `Map.keys (gNodes g)` and the member lists are pairwise disjoint (property test)

#### Scenario: Golden fixture — directed triangles
- **WHEN** `g` has nodes `{a,b,c,d,e,f}` and arcs `a→b, b→c, c→a, c→d, d→e, e→d, d→f` (AVI-518 §8) or two disjoint triangles `{a,b,c}, {d,e,f}` (AVI-523 F5)
- **THEN** the SCC classes are exactly `{a,b,c}`, `{d,e}`, `{f}` (resp. `{a,b,c}`, `{d,e,f}`)

#### Scenario: Labeling is deterministic
- **WHEN** the same graph is built under two node/edge insertion orders
- **THEN** `stronglyConnectedComponents` returns the same `Map Int [NodeId]` (ascending-seed labeling; the partition itself is unique up to relabeling, Thm 2.2)

#### Scenario: Edge addition merges, never splits (INV-10)
- **WHEN** the arc `f→a` is added to the fixture above (`addEdges` on the post-dangling-drop graph)
- **THEN** every pre-existing SCC is contained in exactly one SCC of the result (the three components merge into one)

### Requirement: The condensation of the SCC partition is a DAG (Thm 2.3)

The condensation `cond(G)` — vertex set = SCC classes, arc `A → B` (`A ≠ B`) iff some `a ∈ A` and `b ∈ B` are adjacent in `E` — SHALL be acyclic, so a topological order of `cond(G)` SHALL exist and be well-defined for any `Graph`. (Source: AVI-518 §1.3, Thm 2.3.)

#### Scenario: Condensation is acyclic on the golden fixture
- **WHEN** `cond(G)` is built for the AVI-518 §8 fixture
- **THEN** the condensation is the chain `{a,b,c} → {d,e} → {f}` with no back arc (cycle-freedom assertion, Thm 2.3)

#### Scenario: Topological order exists
- **WHEN** the condensation is computed on any `Graph` with `C ≥ 1` SCCs
- **THEN** a topological ordering of the condensation vertices is produced (the SCC ids follow it under ascending-seed labeling)

### Requirement: Directed reachability decided by hop-minimal shortest paths (Thm 2.4, INV-8, INV-9)

The system SHALL provide a reachability predicate `shortestPathReachable` (or `Just _` pattern of `shortestPath`) such that `s ⤳ t` holds exactly when `shortestPath g s t == Just p` for a hop-minimal `E`-walk `p` with `head p = s`, `last p = t`, `length p − 1 = d(s,t)` (INV-8; IV: `shortestPath g u v == Just [u,v]` for every arc `(u,v) ∈ E`); reachability SHALL be invariant under any permutation of `gNodes/gEdges` keys (INV-9). Traversal SHALL act on the forward relation `E` (source→target) regardless of `gDirected`; `neighbors`/`degree` remain the symmetric accessors — the distinction is documented, not conflated (AVI-518 §6.2, AF-2). (Source: AVI-518 §2.4, §5; existing surface per AVI-530/PR #30.)

#### Scenario: Shortest path decides reachability (INV-8)
- **WHEN** the §8 fixture is queried
- **THEN** `shortestPath a f == Just [a,b,c,d,f]` (length 3), `shortestPath a a == Just [a]`, and `shortestPath f a == Nothing` (no path found, consistent with `05-path`)

#### Scenario: Key-permutation invariance (INV-9)
- **WHEN** the same relation `E` is stored under two different key orders
- **THEN** every reachability answer is identical

### Requirement: Directed acyclicity predicate (Thm 2.5, INV-C5)

The system SHALL provide `isAcyclicDirected :: Graph -> Bool` deciding DAG-hood exactly as: every SCC is a singleton **and** the self-loop count `ℓ = |{ u : (u,u) ∈ E }|` is `0` (Thm 2.5, INV-C5). (Source: AVI-518 §5; AVI-523 Thm 2.5.)

#### Scenario: A self-loop is a directed cycle (INV-C5)
- **WHEN** the fixture `F3` consists of node `{u}` and arc `(u,u)` (AVI-523 §8)
- **THEN** `stronglyConnectedComponents` yields the singleton `{u}` **and** `isAcyclicDirected` returns `False` (the `ℓ = 0` clause is required)

#### Scenario: DAG with undirected cycles
- **WHEN** fixture `F2` (`a→b, a→c, b→d, c→d`, no back arcs) is checked
- **THEN** `isAcyclicDirected == True` even though the undirected support graph has a cycle

### Requirement: Undirected cycle detection predicate (Thm 2.3, INV-C2, INV-C4)

The system SHALL provide `hasCycleUndirected :: Graph -> Bool` (and the `hasCycleUndirectedWithCached :: CachedFGL -> Bool` variant) deciding whether the undirected support graph `G_s` contains an undirected cycle: every arc is read as an undirected edge, and the support has a cycle iff some connected class holds a closed walk (Thm 2.3). A self-loop `(u,u)` is a length-1 undirected cycle (`INV-C4`); a reciprocal arc pair `u→v, v→u` collapses to a single undirected edge and so does **not** itself form a cycle (simple-undirected-graph semantics, AF-3); a lone arc between two distinct nodes is a tree edge (no cycle); any longer closed walk is a cycle. The boolean is order-independent even though the certifying edge is not (§9). The answer is consistent with the cyclomatic number: for the self-loop-free support, `hasCycleUndirected g == True ⟺ q ≥ 1 ⟺ |A| > N − C_c`, where `|A|` is the number of simple undirected support edges, `N = |V|`, and `C_c` the connected-component count (`INV-C2`; §2.4 cross-check, the self-loop being the Thm 2.3 exception). Runs in `O(N + |A|)` via union-find with union-by-rank. (Source: AVI-523 §1.2, Thm 2.3, §4, §8 fixtures F1–F7; INV-C2/INV-C4; new-feature surface of the G5 register.)

#### Scenario: Directed DAG whose support has a cycle (F2)
- **WHEN** fixture `F2` (`a→b, a→c, b→d, c→d`, a directed DAG, no back arcs) is checked
- **THEN** `isAcyclicDirected == True` **and** `hasCycleUndirected == True` — the undirected support has a cycle (a-b-d-c-a) even though no directed cycle exists

#### Scenario: Self-loop is an undirected cycle (F3, INV-C4)
- **WHEN** fixture `F3` (`{u}`, arc `(u,u)`) is checked
- **THEN** `hasCycleUndirected == True` (a length-1 undirected cycle; `INV-C4`)

#### Scenario: Reciprocal arcs do not form an undirected cycle (AF-3)
- **WHEN** two nodes `{a,b}` carry reciprocal arcs `a→b, b→a`
- **THEN** `hasCycleUndirected == False` (the pair collapses to a single undirected edge; a lone edge between two nodes is a tree edge, not a cycle)

#### Scenario: A directed tree is undirected-acyclic (F7)
- **WHEN** the directed path `a→b, b→c` (F7) is checked
- **THEN** `hasCycleUndirected == False` (a tree) even though `connectedComponents` yields a single class `{a,b,c}` (INV-C6), consistent with `q = 0`

#### Scenario: Cycle-detection soundness (INV-C2, property)
- **WHEN** any `Graph` is checked
- **THEN** `hasCycleUndirected == (hasSelfLoop ∨ q ≥ 1)` where `q = |A| − N + C_c` over the self-loop-free support (INV-C2; the self-loop is the Thm 2.3 exception)

### Requirement: SCC is not biconnected components (§1.4 disambiguation)

SCC decomposition SHALL NOT be implemented by or aliased to `articulationPoints` / `biconnectedComponents`: those operate on the undirected support graph `G_s` and decompose edges/cut-vertices, while SCC operates on the directed relation `E` and partitions **nodes** into mutual-reachability classes. The `Domain.Graph.Analysis` undirected decompositions SHALL remain untouched and undocumented as SCC synonyms. (Source: AVI-518 §1.4, §6.3; AVI-523 §6.3.)

#### Scenario: Undirected decomposition disagrees with SCC by design
- **WHEN** fixture `F2` (a directed DAG) is analyzed
- **THEN** `stronglyConnectedComponents` yields four singletons, while `biconnectedComponents`/`articulationPoints` report the undirected support structure — the two invariants are separately named and not interchangeable

### Requirement: Connectivity decomposition coarsens the SCC partition (Thm 2.2 of AVI-523, INV-C1, INV-C3, INV-C6)

The system SHALL provide `connectedComponents`, `weaklyConnectedComponents :: Graph -> Map Int [NodeId]` and `isolatedNodes :: Graph -> [NodeId]` such that: connected components (on `G_s`) and weakly connected components (on `E` read undirected) partition `V` (`⋃_i W_i = V`, pairwise disjoint, none empty — INV-C1); each SCC is contained in exactly one WCC so the WCC partition coarsens the SCC partition (`C_w ≤ C_s` — INV-C6); a node is isolated iff `degree g v = 0`, and every isolated node is a singleton component in both decompositions (INV-C3), with `isolatedNodes` output sorted by `NodeId`. (Source: AVI-523 §1.4, Thm 2.1–2.2, Thm 2.6.)

#### Scenario: WCC coarsens SCC (golden fixture F7)
- **WHEN** the directed path `a→b, b→c` (no return arcs) is decomposed
- **THEN** SCCs are `{a}, {b}, {c}` while the WCC partition is `{a,b,c}` and `C_w = 1 < C_s = 3` (INV-C6 demonstrated)

#### Scenario: Isolated node is a singleton component (INV-C3)
- **WHEN** fixture `F4` (triangle `{a,b,c}` plus edge-less node `z`) is decomposed
- **THEN** `isolatedNodes == ["z"]` (sorted), and `z` is a singleton in both `connectedComponents` and `weaklyConnectedComponents`, while `{a,b,c}` forms the other component

#### Scenario: Partition property (INV-C1)
- **WHEN** any `Graph` is decomposed
- **THEN** both component maps partition `Map.keys (gNodes g)` with no empty component (property test)

### Requirement: Linear complexity and scale guard (AVI-518 §4, GT-W4)

SCC decomposition (Tarjan single-DFS primary, Kosarajan two-pass alternative over the `gAdjBack` transpose), condensation build, one-source reachability/BFS/DFS, and the connectivity decompositions SHALL each be `O(N + E)` algorithmically (`O((N+E)·log N)` worst case over `Data.Map`/`Data.Set` adjacency), with no quadratic blow-up. Dense all-pairs reachability `O(N·(N+E))` SHALL NOT be run on the full graph — it SHALL be scoped to a queried subgraph, a community, or sampled sources (the `edgeBetweenness` cap pattern). No per-node `Set` allocation inside the traversal beyond `O(C)` component buckets. (Source: AVI-518 §3.1–3.3, §4 scale guard; AVI-523 §4; consolidated GT-W4.) (checkable today on the existing decompositions; the same bound follows the new-feature routines of this capability when they land)

#### Scenario: SCC stays linear at repository scale
- **WHEN** `stronglyConnectedComponents` runs on a ~78k-node / ~80k-edge graph (the `bounded-edge-inference` budget)
- **THEN** the runtime is consistent with the `O(N + E)` bound (complexity assertion comparing against the existing budget)

#### Scenario: All-pairs reachability is forbidden at scale
- **WHEN** an all-pairs reachability computation is requested on the full graph
- **THEN** it is rejected/sampled per the scale guard (documented scope-to-subgraph rule), never executed as `O(N·(N+E))` on the complete node set
