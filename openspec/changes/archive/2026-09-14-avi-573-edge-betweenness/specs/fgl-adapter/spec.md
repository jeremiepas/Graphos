## ADDED Requirements

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
