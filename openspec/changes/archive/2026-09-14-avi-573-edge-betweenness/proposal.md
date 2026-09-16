# Proposal: avi-573-edge-betweenness

## Why

`Graphos.Domain.Graph.Analysis.edgeBetweenness` implemented a biased per-pair
`esp` heuristic rather than standard edge betweenness. The result was neither the
classical `BC(e) = Σ σ(s,t|e)/σ(s,t)` nor a documented approximation, so it could
not be validated against the worked example in `docs/math-requirements/AVI-534-
structural-analysis-complexity.md` (§8 golden: `BC(b–c) = 2/3` on the 4-node path)
and its complexity was `O(s·N·(N+M))` (AVI-534 AF-2), worse than the target
`O(s·(N+M))`.

This change closes AVI-534 objectives **O1** and **O2** (and the AC-2/AC-3/AC-4
trailer of O3): replace the heuristic with Brandes' algorithm, make the undirected
FGL traversal correct via reverse-embedding (AF-3), assert the structural-algorithm
complexity bounds (AC-1), and add complexity-honesty scale guards (SG-1/SG-2/SG-3).

## What Changes

- **`edgeBetweenness` becomes Brandes (O1).** Exact all-pairs is `O(N·M)` (one BFS +
  one backward-dependency accumulation per source × N sources); the sampled estimator
  is `O(s·(N+M))` over `s ≤ maxSampledSources` sources with per-source `O(N+M)` memory
  over the shared `CachedFGL` (SG-3). The biased per-pair `esp` path is removed.
- **Undirected reverse-embedding (AF-3).** `toCachedFGL` embeds every edge `(u,v)` of
  an undirected graph as both `(u,v)` and `(v,u)` (self-loops not duplicated), so FGL's
  single-pass `ap`/`bcc`/`dom`/Brandes traversals see the support graph `G_s`.
- **Unordered-pair normalization.** The betweenness sum runs over unordered source/target
  pairs (`srcIdx < tgtIdx`), yielding the §8 golden value `BC(b–c) = 2/3` on the 4-node
  path without a double-count.
- **Complexity assertions (AC-1).** `articulationPoints` (`ap`), `biconnectedComponents`
  (`bcc`) and `dominators` (`dom`) are documented and asserted at `O(N+M)` / reducible
  `O(N+M)`, irreducible worst-case `O(N·M)`.
- **Scale guards (SG-1/SG-2/SG-3).** Sampled-source cap (`--max-sampled-sources`, default
  500) with `N/|S|` rescale; exact all-pairs bypassed above `cfgExactBetweennessNodeCap`
  (`--exact-betweenness-node-cap`, default 10000); per-source `O(N+M)` memory bound.

Out of scope: the AVI-534 §6.1 heuristic removal is complete, but new CLI views, config
schema changes beyond the two guards, and the O3 probabilistic Hoeffding bound formalism
(beyond the empirical AC-2 test) are not part of this change-dir record.

## Capabilities

### Modified Capabilities
- `bounded-edge-inference`: edge betweenness is standard Brandes with complexity-honesty
  scale guards (workflows: 01-full-pipeline, 02-incremental-pipeline, 07-context-selection).
- `fgl-adapter`: structural algorithms carry documented worst-case bounds; undirected
  graphs are reverse-embedded for FGL.

## Impact

- **Code:** `src/Graphos/Domain/Graph/Analysis.hs` (Brandes + scale guards),
  `src/Graphos/Domain/Graph.hs`, `src/Graphos/CLI/Parser.hs` (two flags),
  `src/Graphos/Domain/Types/Pipeline.hs`, `docs/math-requirements/AVI-534-...md`.
  Pure Domain/UseCase changes; no new dependencies.
- **Behavior:** `edgeBetweenness` returns the fractional shortest-path betweenness;
  undirected `ap`/`bcc`/`edgeBetweenness` match classical support-graph values.
- **Tests:** `tests/Graphos/Domain/Graph/StructuralSpec.hs` (AC-1..AC-4 golden fixtures).

## Specs

- `specs/bounded-edge-inference/spec.md` — new Requirement: Edge betweenness scale guards
  (SG-1/SG-2/SG-3) with scenarios AC-2/AC-3/AC-4.
- `specs/fgl-adapter/spec.md` — new Requirements: Complexity bounds of FGL-backed
  structural algorithms (AC-1) and Undirected reverse-embedding for FGL (AF-3).

## PDCA Cycle

- **Plan:** Hypothesis — the biased per-pair `esp` heuristic is not standard edge
  betweenness and its cost model is wrong. Success criteria (AVI-534 O1/O2): exact routine
  reproduces `{b,c}` + bridges + `BC(b–c) = 2/3` on the §8 fixture; sampled estimator is
  unbiased (AC-4) and within ε (AC-2); structural cost model asserted (AC-1).
- **Do:** Implement Brandes + reverse-embed + scale guards (design.md, tasks.md).
- **Check:** `cabal test` PASS — StructuralSpec golden fixtures for AC-1..AC-4.
- **Act:** Semantics change recorded in spec + Haddock; biased heuristic removed.
