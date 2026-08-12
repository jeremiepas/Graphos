<!--
  PDCA step file for task 5. Lives at tasks/05-scoreallcohesion-direct-read/plan.md.
  Scope: Replace cohesionScore per-node Set allocation with direct adjacency reads.
  No code yet. Check Criteria defined BEFORE implementation.
-->

# Task 5 — scoreAllCohesion Direct Read — PLAN

**Task slug**: `05-scoreallcohesion-direct-read`
**Attempt**: 1
**Status**: pending

## Summary

Replace `cohesionScore`'s per-node `neighbors g nid` allocation (which builds a fresh `Set` per call, ~470K allocations on solario) with direct `gAdjFwd`/`gAdjBack` reads. Eliminate the per-node `Set` allocation.

## Detail

### Scope

- **Files**: `Domain/Community.hs` — `cohesionScore`, `scoreAllCohesion`
- **Potential export changes**: `Domain.Graph.Core` and/or `Domain.Graph` may need to export `gAdjFwd`, `gAdjBack`, `gDirected` record fields (currently internal to the Graph type)
- **Refactoring**: Replace `neighbors g nid` call (which does `Map.findWithDefault Set.empty` ×2 + `Set.union`, allocating a fresh `Set`) with direct reads from `gAdjFwd` and `gAdjBack`

**Option A (preferred if inter-edge counts available):**
Derive internal-edge counts from `interEdgeCounts` that `computeCommunityAggregates` already builds: `internalEdges = totalEdgesTouchingCommunity - sum(interCommunityEdges)`. Avoids per-node neighbor iteration entirely.

**Option B (fallback):**
Direct-read `gAdjFwd`/`gAdjBack`: for each member `nid`, read `fwdNbs = Map.findWithDefault Set.empty nid adjFwd`, then `nbs = if directed then fwdNbs else Set.union fwdNbs (Map.findWithDefault Set.empty nid adjBwd)`, then filter by `memberSet`.

### Check Criteria

**Tests/gates to run:**
1. `cabal build` — must exit 0 under `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`
2. `cabal test` — must exit 0 with all existing examples passing
3. Deterministic fixture comparison: cluster the same fixture with old `neighbors`-based cohesion and new direct-read cohesion, assert identical `CohesionMap`
4. Heap profile (optional, if available): Run on large graph with `+RTS -hy`, verify reduced `Set` allocation during `scoreAllCohesion`

**Spec scenarios satisfied:**
- `leiden-scalability` — **Scenario: Cohesion scoring does not allocate per-node Sets** (WHEN scoreAllCohesion runs on 78K-node graph, THEN Set allocations are O(C) or zero, not O(sum of degrees) ~470K)
- `leiden-scalability` — **Scenario: Cohesion values are unchanged** (WHEN fixture scored with old and new approaches, THEN CohesionMap is identical)
- `leiden-scalability` — **Requirement: Cohesion scoring without per-node neighbor allocation** (MUST NOT allocate fresh Set per node via neighbors g nid, SHALL read gAdjFwd/gAdjBack directly or derive from inter-edge counts)

**PASS conditions:**
- `cabal build` exits 0
- `cabal test` exits 0 with all examples passing
- Deterministic fixture produces identical CohesionMap
- If Option A used: `computeCommunityAggregates`'s interEdgeCounts produces correct internal edge counts (verified by comparison against direct-read Option B on a small fixture)
- If Option B used: `gAdjFwd`/`gAdjBack` fields accessible from the cohesion scoring call site

**FAIL conditions:**
- `cabal build` fails — record fields not exported from Graph/Domain.Graph.Core
- `cabal test` fails — cohesion values changed (semantic regression)
- Deterministic fixture produces different CohesionMap — direct-read logic differs from neighbors-based logic
- `interEdgeCounts`-based Option A produces wrong internal-edge counts — inter-edge counting has a bug

### Affected modules

- `Domain.Community` — `cohesionScore`, `scoreAllCohesion`
- `Domain.Graph.Core` — may need to export `gAdjFwd`, `gAdjBack`, `gDirected` fields
- `Domain.Graph` — may need to export adjacency fields (depending on which module owns cohesion scoring)

### Prerequisites

- Tasks 1-4 must be complete (Pipeline.hs compiles, Leiden core optimized)
- `scoreAllCohesion` must be a pure function with clear inputs/outputs for testing
- If Option A (interEdgeCounts): `computeCommunityAggregates` must be called before `scoreAllCohesion` or its results must be available
- If Option B (direct-read): `gAdjFwd`/`gAdjBack` must be accessible from the cohesion scoring call site

### Risks

- **Option A correctness**: Deriving internal edges from interEdgeCounts assumes `computeCommunityAggregates` is correct. If Option A is used, it must be validated against Option B on a small fixture before deployment. This adds a verification step but not a code path in production.
- **Option B complexity**: Direct reads from `gAdjFwd`/`gAdjBack` require handling directed vs undirected graphs differently. For undirected graphs, both forward and backward adjacency must be unioned. This is a 5-10 line change but must match the `neighbors` function's logic exactly.
- **Export coupling**: Exposing `gAdjFwd`/`gAdjBack` from `Domain.Graph.Core` or `Domain.Graph` creates a module dependency that must be maintained. These are internal fields — any future change to the Graph representation requires updating the cohesion scoring code.
- **Performance**: For undirected graphs, Option B's `Set.union` per node still allocates intermediate sets. Option A avoids this entirely if `interEdgeCounts` is correct. Preference given to Option A if it produces correct results.

## Result

pending — awaiting Do phase.
