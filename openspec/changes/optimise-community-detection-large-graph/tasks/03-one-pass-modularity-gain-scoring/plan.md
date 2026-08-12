<!--
  PDCA step file for task 3. Lives at tasks/03-one-pass-modularity-gain-scoring/plan.md.
  Scope: Rewrite bestCommunityFor and localMovingLoop for one-pass modularity-gain scoring.
  No code yet. Check Criteria defined BEFORE implementation.
-->

# Task 3 — One-Pass Modularity-Gain Scoring — PLAN

**Task slug**: `03-one-pass-modularity-gain-scoring`
**Attempt**: 1
**Status**: pending

## Summary

Rewrite `bestCommunityFor` and `localMovingLoop` in `Domain/Community.hs` to compute edges-to-community counts in a single fold over the neighbor vector, eliminating per-candidate `VU.filter (== c)` scans. Also rewrite `cohesionToCommunityIdx` with a single count-fold.

## Detail

### Scope

- **Files**: `Domain/Community.hs` — `bestCommunityFor`, `localMovingLoop`, `cohesionToCommunityIdx`
- **Refactoring**: `localMovingLoop` builds `countMap :: IntMap Int` once per node via a single fold over `commOfNb`. `bestCommunityFor` reads `sigmaIn[c] = countMap ! c` instead of `VU.filter (== c) commOfNb`. Move accounting (`edgesToOld`, `edgesToNew`) reads from `countMap` via `findWithDefault`.
- **`cohesionToCommunityIdx`**: Single `VU.foldl'` count instead of per-community filter pass.
- **Complexity target**: O(degree) per node for scoring, not O(degree × |unique comms|). Hub nodes (degree 100-500) see 3-20× speedup.

### Check Criteria

**Tests/gates to run:**
1. `cabal build` — must exit 0 under `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`
2. `cabal test` — must exit 0 with all existing examples passing (347 examples expected)
3. Deterministic fixture comparison: cluster the same fixture with old multi-scan and new one-pass scoring, assert identical `bestComm` for each node on every pass and identical final `CommunityMap`
4. QuickCheck property (new test): For random neighbor-community vectors, one-pass `bestCommunityFor` picks the same `bestComm` as the multi-scan version

**Spec scenarios satisfied:**
- `leiden-scalability` — **Scenario: Hub-node scoring is linear in degree** (WHEN node with 200 neighbors across 15 communities is scored, THEN work is O(200) not O(200 × 15))
- `leiden-scalability` — **Scenario: Scoring picks the same best community** (WHEN fixture clustered with old and new scoring, THEN chosen bestComm for each node is identical and final CommunityMap is identical)
- `leiden-scalability` — **Scenario: sigmaTot delta is correct after a move** (WHEN node moves from A to B with 3 edges to A and 5 to B, THEN sigmaTot[A] -= ki-3, sigmaTot[B] += ki-5, reading from count map)
- `leiden-scalability` — **Requirement: One-pass modularity-gain scoring** (MUST compute edges-to-community counts in single pass, SHALL NOT re-scan per candidate community)
- `leiden-scalability` — **Scenario: Clustering results are unchanged by the optimization** (existing deterministic fixtures must pass)

**PASS conditions:**
- `cabal build` exits 0
- `cabal test` exits 0 with all examples passing
- Deterministic fixture produces identical CommunityMap (set of IDs + set of members per ID)
- QuickCheck property passes: one-pass = multi-scan on 100 random neighbor vectors

**FAIL conditions:**
- `cabal build` fails — type errors in countMap usage
- `cabal test` fails — semantics broken (wrong bestComm or wrong sigmaTot deltas)
- Deterministic fixture produces different CommunityMap — one-pass counting has a bug
- QuickCheck property fails — one-pass and multi-scan disagree on some random inputs

### Affected modules

- `Domain.Community` — `bestCommunityFor`, `localMovingLoop`, `cohesionToCommunityIdx`, `buildLeidenState` (if countMap integration touches state)
- `Domain.Graph.Core` — potentially needed for record field access in cohesion scoring

### Prerequisites

- Task 1 must be complete (Pipeline.hs compiles, baseline test suite passes)
- `localMovingLoop` must be isolated and testable
- `bestCommunityFor` must be a pure function with clear inputs/outputs for testing

### Risks

- **IntMap vs IntMap Int**: Community IDs are `Int` (not `CommunityId` wrapper), so `IntMap Int` is appropriate. Using `Map CommunityId Int` would add overhead from wrapper construction. Verify all community ID references are bare `Int`.
- **Sparse community IDs**: During refinement, new community IDs may be created. The countMap must handle arbitrary Int keys. `IntMap Int` handles sparse keys correctly.
- **`cohesionToCommunityIdx` integration**: If this function is called from multiple paths, the count-fold must be compatible with all call sites. Check all callers before applying.
- **Deterministic fixture sensitivity**: If the fixture has a hub node where the scoring order matters (e.g., equal modularity-gain ties), the one-pass version might pick a different community than the multi-scan version. This would manifest as a different CommunityMap even though both are locally optimal.

## Result

pending — awaiting Do phase.
