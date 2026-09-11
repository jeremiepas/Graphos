# fgl-adapter Specification

## Purpose
TBD - created by archiving change fix-mcp-query-perf-and-correctness. Update Purpose after archive.
## Requirements
### Requirement: Bijective node-index mapping

The FGL conversion (`toCachedFGL` and/or `toFGL`) SHALL assign each `NodeId` a distinct `Int` index in the range `0 .. N-1` where N is the number of nodes. The mapping SHALL be bijective: two distinct `NodeId`s MUST NOT share an FGL Int index. The reverse mapping (`cfgNidMap :: Map Int NodeId`) SHALL cover every index. `cachedFindIdx` SHALL be O(log N) via a `Map NodeId Int`, not O(N) association-list lookup.

#### Scenario: No node is lost to hash collision

- **WHEN** a graph contains two `NodeId`s that would collide under any hash-based `NodeId → Int` scheme (constructed by picking two strings with equal hash mod `maxBound`)
- **THEN** both nodes appear in `cfgNidMap`, both have distinct indices in `cfgIdxMap`, and `cachedFindIdx` returns `Just` for both

#### Scenario: shortestPath finds paths through collision-prone node pairs

- **WHEN** `shortestPath` (or `shortestPathWithCached`) is called with a source or target `NodeId` that would have collided under the previous `nidToInt` hash
- **AND** a path exists between them in the graph
- **THEN** the function returns `Just path` (not `Nothing`); the path contains both endpoints

#### Scenario: cachedFindIdx is O(log N)

- **WHEN** `cachedFindIdx` is called on a graph with N nodes
- **THEN** the lookup is a `Map` lookup (O(log N)), not an association-list `lookup` (O(N)); the `CachedFGL` record exposes `cfgIdxMap :: Map NodeId Int` (or equivalent O(log N) structure), not `cfgIdxList :: [(NodeId, Int)]`

### Requirement: FGL-backed algorithms preserve semantics under sequential indexing

Switching `nidToInt` (hash) to sequential `0..N-1` indices changes the internal order of FGL nodes. Algorithms that return lists (`articulationPoints`, `biconnectedComponents`, `dominators`) SHALL produce results equivalent to the pre-change implementation up to element order (i.e., comparing as `Set`s or sorted lists), so that existing tests and downstream consumers are unaffected.

#### Scenario: articulation points are unchanged as a set

- **WHEN** `articulationPoints` is called on the same graph before and after the sequential-index change
- **THEN** the returned list contains the same `NodeId`s (order may differ); `Set.fromList (old) == Set.fromList (new)`

#### Scenario: Existing test suite passes

- **WHEN** `cabal test` is run after the change
- **THEN** all existing tests pass without modification (any order-sensitive assertions on FGL-backed algorithm output are relaxed to set/sorted comparison)

### Requirement: Complexity bounds of FGL-backed structural algorithms (AVI-534 AC-1)

The FGL-backed structural algorithms in `Graphos.Domain.Graph.Analysis` SHALL run within the following worst-case bounds in the `(N, M)` model, with adjacency read over FGL sequential indices (CSR-equivalent dense-free layout; `Data.Map` lookups only at the `CachedFGL` boundary, never inside an algorithm loop) — grounded in `docs/math-requirements/AVI-534-structural-analysis-complexity.md` §2:

- `articulationPoints` (FGL `ap`, Tarjan 1972 low-link, single DFS pass): **O(N + M)** (Theorem 2.1).
- `biconnectedComponents` (FGL `bcc`, stack-based DFS, each arc once): **O(N + M)** (Theorem 2.1).
- `dominators` (FGL `dom`, Cooper–Harvey–Kennedy iterative reduction): **O(N + M)** on reducible graphs; **O(N·M)** worst-case on irreducible inputs (Theorem 2.2, CHW 1982). Graphos code-dependency graphs are reducible (every edge traces a call/import/contains relation with well-founded nesting), so the operative bound is O(N + M).
- `edgeBetweenness` (Brandes 2001): **O(N·M)** exact (one BFS + one backward dependency accumulation per source × N sources); **O(s·(N + M))** sampled over `s ≤ maxSampledSources` sources (Theorem 2.3). Per-source working memory is O(N + M) over the shared `CachedFGL` (SG-3).

#### Scenario: ap/bcc/dominators cost model is documented and asserted

- **WHEN** `cabal test` runs the AVI-573 analysis spec
- **THEN** the golden fixtures verify the semantic results (articulation points, biconnected components, dominator forest, edge betweenness) whose cost models are asserted above in this spec and in the `Analysis.hs` Haddock surface

#### Scenario: edge betweenness is Brandes, not per-pair esp

- **WHEN** `edgeBetweenness` is called on a graph where multiple hop-minimal paths exist between a pair
- **THEN** the result equals the standard edge betweenness `BC(e) = Σ σ(s,t|e)/σ(s,t)` (fractional sum over all shortest paths, deterministic, tie-break-free), verified against the AVI-534 §8 worked example (`BC(b–c) = 2/3` on the 4-node path)

### Requirement: Undirected graphs are reverse-embedded for FGL (AVI-534 AF-3)

`toCachedFGL` SHALL embed every edge `(u, v)` of an undirected Graphos graph (`gDirected g = False`) as both `(u, v)` and `(v, u)` in the FGL graph, so FGL's single-pass traversals (`ap`/`bcc` low-link DFS, `dom` BFS, Brandes BFS) see the undirected support graph `G_s` and `Art`/`BCC`/`Dom` match the classical `Art(G_s)`/`BCC(G_s)`/`Dom(G_s)`. Self-loops SHALL NOT be duplicated.

#### Scenario: articulation points on undirected graphs match the support graph

- **WHEN** `articulationPoints` is called on an undirected path graph `a–b–c–d`
- **THEN** the result is `{b, c}` (the cut vertices of `G_s`), not the directed-only traversal result

#### Scenario: edge betweenness on undirected graphs sums unordered pairs

- **WHEN** `edgeBetweenness` is called on an undirected graph
- **THEN** the sum runs over unordered pairs (`srcIdx < tgtIdx`), producing the §8 golden value `BC(b–c) = 2/3` on the 4-node path

### Requirement: buildLabelIndex and buildPathIndex use O(1)-per-insert list construction

`buildLabelIndex` and `buildPathIndex` (`Domain/Graph/Index.hs`) SHALL use O(1)-per-insert list construction. The current implementation uses `Map.fromListWith (++)` with singleton lists `[nid]` followed by `Map.map reverse`, which is O(1) per insert (`[nid] ++ existing = nid : existing` — one cons, no traversal). This keeps one-time index construction O(N × avg_tokens).

#### Scenario: Label index content is preserved

- **WHEN** `buildLabelIndex` is built with `(++)` + singleton lists `[nid]` on the same node map
- **THEN** it produces a `Map Text [NodeId]` where each term maps to the list of `NodeId`s whose label contains that term; `findMatchingNodes` returns the correct `(NodeId, score)` pairs

