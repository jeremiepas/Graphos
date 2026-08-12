<!--
  PDCA step file for task 2. Lives at tasks/02-fix-quadratic-community-grouping/plan.md.
  Scope: Investigate claimed quadratic community grouping.
  No code change expected — verification task. Check Criteria defined BEFORE investigation.
-->

# Task 2 — Fix Quadratic Community Grouping — PLAN

**Task slug**: `02-fix-quadratic-community-grouping`
**Attempt**: 1
**Status**: pending

## Summary

Verify whether the claimed quadratic community grouping in `leidenStateToCommunityMap` and `refineCommunitiesOpt` actually manifests as O(N^2/C) in the current codebase. If the quadratic behavior is confirmed, replace `IntMap.fromListWith (++)` with `IntMap.fromListWith (:)`.

## Detail

### Scope

- **Files**: `Domain/Community.hs` — `leidenStateToCommunityMap` (line 292) and `refineCommunitiesOpt` (line 238)
- **Investigation**: Analyze the call sites of `IntMap.fromListWith (++)` to determine actual complexity
- **Fix (if needed)**: Replace `(++)` with `(:)` (prepend) and add optional `Map.map reverse` for stable order
- **Complexity target**: O(N) total for grouping, not O(N^2/C) for the largest community

### Check Criteria

**Tests/gates to run:**
1. `cabal build` — must exit 0 under `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`
2. `cabal test` — must exit 0 with all existing examples passing
3. Microbenchmark (if fix applied): Run on 10K-node synthetic graph with single large community (2,000+ members), verify O(N) scaling
4. Deterministic fixture comparison: cluster the same fixture with old and new grouping code, assert identical `CommunityMap` sets

**Spec scenarios satisfied:**
- `leiden-scalability` — **Scenario: Large community grouping is linear** (WHEN graph with single 2,000-member community is clustered, THEN grouping completes in time proportional to 2,000 not 2,000^2)
- `leiden-scalability` — **Scenario: Grouping output is semantically equivalent** (WHEN fixture clustered with old and new grouping, THEN set of community IDs and set of members per community are identical)
- `leiden-scalability` — **Requirement: O(N) community member grouping** (MUST build member lists with O(1) per insertion using `fromListWith (:)`, NOT `fromListWith (++)`)

**PASS conditions:**
- If no code change: investigation confirms the old pattern is already O(N) for singleton-list prepends → task documented as no-op
- If code change applied: `cabal build` exits 0, `cabal test` exits 0, deterministic fixture produces identical `CommunityMap` (set of IDs + set of members per ID match, order within community may differ)

**FAIL conditions:**
- Code change breaks `cabal test` — grouping semantics altered
- Deterministic fixture produces different community-member sets (not just different order within community) — semantic regression
- No code change but investigation incorrectly dismissed the quadratic behavior — the claim was valid

### Affected modules

- `Domain.Community` — `leidenStateToCommunityMap`, `refineCommunitiesOpt`
- `Domain.Graph.Core` — potentially exports needed for record field access (if fix changes record construction)

### Prerequisites

- Task 1 must be complete (Pipeline.hs compiles, baseline test suite passes)
- `leidenStateToCommunityMap` function must be isolated and testable independently of the full pipeline
- Access to a deterministic fixture with a known large community for scaling verification

### Risks

- **False positive**: The `(++)` prepend with singleton values `[i]` is already O(1) per insert (prepending to a list of length 0 is O(1)). The quadratic claim may apply to `fromListWith` where the accumulating value has grown from previous inserts, not singleton prepends. Need to trace the exact call pattern.
- **Member-list order change**: `fromListWith (:)` reverses the order (prepend vs append). Downstream consumers must not depend on insertion order within a community list. Verified: `representativeLabels` sorts by `nodeLabel`, `selectRepresentatives` sorts by degree. No order dependency.
- **Reversibility cost**: Adding `Map.map reverse` for stable order adds O(N) pass. Only justified if a downstream consumer benefits from the stable order.

## Result

pending — awaiting Do phase.
