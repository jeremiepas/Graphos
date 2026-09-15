# Design: avi-573-edge-betweenness

## Context

`Graphos.Domain.Graph.Analysis.edgeBetweenness` used a biased per-pair `esp` heuristic
that was neither standard edge betweenness nor a documented approximation, with an
`O(s·N·(N+M))` cost model (AVI-534 AF-2). It could not be validated against the §8
worked example (`BC(b–c) = 2/3` on the 4-node path). This change implements Brandes'
algorithm, makes undirected FGL traversal correct, and adds complexity-honesty scale
guards. Grounding doc: `docs/math-requirements/AVI-534-structural-analysis-complexity.md`.

## Key Decisions

### 1. Brandes as the single betweenness routine (AVI-534 O1, AF-1/AF-2)

Exact all-pairs runs one BFS + one backward-dependency accumulation per source × N
sources → `O(N·M)`. The sampled estimator draws `s ≤ maxSampledSources` sources and
rescales each source's contribution by `N/s`, giving `O(s·(N+M))` with no mean bias
(Lemma 3.1; `s = N` is the identity → AC-4). The biased `esp` path is removed, not
kept alongside.

### 2. Undirected reverse-embedding for FGL (AF-3)

`toCachedFGL` embeds every edge `(u, v)` of an undirected graph as both `(u, v)` and
`(v, u)` (self-loops not duplicated). FGL's single-pass `ap`/`bcc` low-link DFS, `dom`
BFS, and Brandes BFS then operate on the undirected support graph `G_s`, so
`Art`/`BCC`/`Dom` match the classical values. This is cheaper and less error-prone than
post-hoc undirected corrections and keeps one adjacency layout for every algorithm.

### 3. Unordered-pair normalization

The betweenness sum iterates unordered source/target pairs (`srcIdx < tgtIdx`), so each
pair contributes once. On the 4-node path this yields exactly `BC(b–c) = 2/3` (AC-4
golden) without the double-count a symmetric `(u,v)+(v,u)` sum would introduce.

### 4. Complexity-honesty scale guards (SG-1/SG-2/SG-3)

- **SG-1 (sampled-source cap).** At most `cfgMaxSampledSources` sources (default 500,
  `--max-sampled-sources`), drawn in deterministic seedable order (ascending sequential
  FGL index). The `N/|S|` rescale keeps the estimator unbiased under the cap.
- **SG-2 (exact all-pairs cap).** Exact `O(N·M)` is not invoked when
  `N > cfgExactBetweennessNodeCap` (default 10000, `--exact-betweenness-node-cap`); the
  sampled estimator runs instead. A complexity-honesty constraint, not a product choice.
- **SG-3 (per-source memory).** The Brandes forward/backward pass per source holds
  `O(N + M)` over the shared `CachedFGL` and builds no per-source structure beyond that.

## Non-Goals

- New CLI views or config schema beyond the two guard flags.
- The O3 probabilistic Hoeffding formalism (only the empirical AC-2 test is in scope).
- Directed-graph betweenness semantics beyond what reverse-embedding already provides.
