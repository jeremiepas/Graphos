<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Implement computeCommunityAggregates UseCase function — PLAN

**Task slug**: `03-implement-compute-community-aggregates`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

The `computeCommunityAggregates` function already exists in `src/Graphos/UseCase/Cluster.hs:96-145`. This task is to fix the `caInterCommunityEdges` field from scalar `Int` to `![(CommunityId, Int)]` (per Decision 8 and Task 1), verify correctness, add tests, and ensure the function has no IO or Infrastructure dependencies.

## Detail

### Scope

**Existing implementation** (`Cluster.hs:96-145`):
- The function already computes member counts, cohesion, bridge counts, colors, labels, representative labels
- **Bug**: line 142 does `caInterCommunityEdges = Map.size (Map.findWithDefault Map.empty cid interEdgeCounts)` — returns the count of distinct target communities, not the per-target edge counts
- **Fix**: change to `caInterCommunityEdges = Map.toList (Map.findWithDefault Map.empty cid interEdgeCounts)` to return the `[(CommunityId, Int)]` list

**Changes:**
1. Fix `caInterCommunityEdges` on line 142 of `Cluster.hs` to return `Map.toList ...` instead of `Map.size ...`
2. Update `colorForCommunity` — it already exists in `Cluster.hs:73-74` (lifted from HTML.hs), so no palette lift needed
3. Add Hspec tests:
   - Test: a 2-community fixture produces 2 aggregates with correct counts and inter-community edge pairs
   - Test: a community with no cross-community edges has `inter_community_edges = []`
4. Add QuickCheck property:
   - `length (computeCommunityAggregates g cm cohom ap labels) == Map.size cm`
   - For each aggregate, `caInterCommunityEdges` is a list of `(target, count)` pairs where `count > 0`

### Check Criteria

**Tests/gates:**
- (a) `cabal build` with `-Werror` → exits 0
- (b) `cabal test --match "aggregates"` → all tests PASS
- (c) QuickCheck property → holds for 100 test cases
- (d) `grep -c "IO" src/Graphos/UseCase/Cluster.hs` → check that `computeCommunityAggregates` function body has no IO (grep the function scope, not the whole file)
- (e) `grep "Infrastructure" src/Graphos/UseCase/Cluster.hs` → must be 0 (no Infrastructure imports)
- (f) Verify `caInterCommunityEdges` returns a list, not a scalar

**Spec scenarios satisfied:**
- `html-lod-viewer/spec.md` — Scenario "Aggregate fields populated": community with 17 members → `member_count = 17`, `bridge_count` matches articulation points, `cohesion` from Leiden, `color` from palette, `label` from labeling or fallback
- `html-lod-viewer/spec.md` — Scenario "Inter-community edges listed": community A has 5 edges to B and 2 edges to C → `inter_community_edges` contains `[{"target": <B>, "count": 5}, {"target": <C>, "count": 2}]`
- `html-lod-viewer/spec.md` — Scenario "Community aggregates present in export": pipeline produces exactly N aggregates for N communities

**PASS conditions:**
- (a) `cabal build` exits with code 0
- (b) All Hspec tests pass
- (c) QuickCheck property holds
- (d) No IO in the function
- (e) No Infrastructure imports
- (f) `caInterCommunityEdges` is a list of `(target, count)` pairs

**FAIL boundaries:**
- (a) Compilation error → FAIL
- (b) Any Hspec test fails → FAIL
- (c) QuickCheck fails → FAIL
- (d) Function imports IO → FAIL (violates UseCase purity)
- (e) Function imports Infrastructure → FAIL (violates Clean Architecture layering)
- (f) `caInterCommunityEdges` is a scalar → FAIL (spec violation, Decision 8)

### Affected Modules

- `src/Graphos/Domain/Types/Analysis.hs` — `CommunityAggregate.caInterCommunityEdges` type change (completed in Task 1)
- `src/Graphos/UseCase/Cluster.hs` — fix `caInterCommunityEdges` computation (line 142), add tests
- `tests/UseCaseSpec.hs` or new test file — add Hspec + QuickCheck tests

### Prerequisites

- `CommunityAggregate` type with corrected `caInterCommunityEdges` field (Task 1)
- `colorForCommunity` already exists in `Cluster.hs`
- `invertCommunityMap'` helper exists in `Cluster.hs`
- `articulationPoints` or equivalent bridge-node list available from upstream

### Risks

- **`interEdgeCounts` computation is already correct**: The existing code at lines 108-120 already builds the per-target map. Only line 142 needs to change from `Map.size` to `Map.toList`. This is a low-risk fix.
- **Color palette lift**: The original task mentioned lifting the palette from `HTML.hs` to a pure location. This was already done — `communityColors` and `colorForCommunity` exist in `Cluster.hs`. No additional work needed.
- **Bridge count semantics**: `countBridge` counts articulation points that are in a community. This may not perfectly match "bridge nodes" (nodes with edges to other communities). The spec says "bridge_count" — verify this matches the intended semantics. If not, adjust to count nodes with inter-community edges instead of articulation points.
