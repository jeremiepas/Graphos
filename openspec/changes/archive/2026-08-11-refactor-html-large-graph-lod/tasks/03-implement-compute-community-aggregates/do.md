<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Implement computeCommunityAggregates UseCase function — DO

**Task slug**: `03-implement-compute-community-aggregates`
**Attempt**: 1
**Status**: pending

## Summary

Fix `caInterCommunityEdges` in `computeCommunityAggregates` from scalar count to per-target list, add Hspec tests, and add QuickCheck properties. The function already exists in `src/Graphos/UseCase/Cluster.hs:96-145` with correct inter-community edge counting logic (lines 108-120) but discards the per-target breakdown at line 142.

## Detail

### Concrete Changes

**File: `src/Graphos/UseCase/Cluster.hs`**

1. Fix line 142 — change from `Map.size` to `Map.toList`:
   ```haskell
   -- Before (line 142):
   , caInterCommunityEdges    = Map.size (Map.findWithDefault Map.empty cid interEdgeCounts)
   -- After:
   , caInterCommunityEdges    = Map.toList (Map.findWithDefault Map.empty cid interEdgeCounts)
   ```

2. This single-line change produces `[(CommunityId, Int)]` instead of `Int`, matching the corrected `CommunityAggregate` type from Task 1.

**File: `tests/UseCaseSpec.hs` (or new test file)**

1. Hspec test: inter-community edge pairs
   ```haskell
   it "produces correct inter_community_edges as list of (target, count) pairs" $
     -- Build a 2-community fixture with known cross-community edges
     let agg = computeCommunityAggregates graph commMap cohesionMap artPoints mLabels
         agg4 = head [a | a <- agg, caId a == "4"]
     in caInterCommunityEdges agg4 `shouldBe` [(8, 3), (12, 1)]
   ```

2. Hspec test: community with no cross-community edges
   ```haskell
   it "has empty inter_community_edges for isolated community" $
     -- Build a community with no edges crossing boundary
     let agg = ...
     in caInterCommunityEdges agg `shouldBe` []
   ```

3. QuickCheck property:
   ```haskell
   prop_aggregate_count :: Property
   prop_aggregate_count =
     forAll (arbitrary :: Gen (Graph, CommunityMap, CohesionMap, [NodeId], Maybe (Map CommunityId Text))) $
       \(g, cm, cohom, ap, labels) ->
         length (computeCommunityAggregates g cm cohom ap labels) === Map.size cm
   ```

### Key Decisions

- **Single-line fix**: The inter-community edge counting logic at lines 108-120 is already correct. Only line 142 needs the `Map.size` → `Map.toList` change. This is a low-risk fix.
- **No color palette lift needed**: `colorForCommunity` already exists in `Cluster.hs:73-74`. The original task concern about lifting from HTML.hs is already resolved.
- **Bridge count**: Uses articulation points (`artPoints` parameter) as the bridge count. This is the existing approach — not changed in this task.

### Dependencies

- Requires: Task 1 completed (`CommunityAggregate` type with `![(CommunityId, Int)]` field)
- Reads: `tasks/03-implement-compute-community-aggregates/plan.md`
- Unlocks: `tasks/03-implement-compute-community-aggregates/check.md`
