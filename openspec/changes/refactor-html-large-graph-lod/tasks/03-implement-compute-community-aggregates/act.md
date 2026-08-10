<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Implement computeCommunityAggregates UseCase function — ACT

**Task slug**: `03-implement-compute-community-aggregates`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Record the standardized outcome for Task 3: `computeCommunityAggregates` fix (`Map.size` → `Map.toList`) and Hspec/QuickCheck tests in `src/Graphos/UseCase/Cluster.hs`.

## Detail

### Outcome

<!-- If check passed: record the standardized outcome.
     If check failed: record FAIL — see attempt-2/ and start a new P→D→C→A cycle. -->

<!-- Standardized outcome when check passes:
     Line 142 of `Cluster.hs` was changed from `Map.size (Map.findWithDefault Map.empty cid interEdgeCounts)` to `Map.toList (Map.findWithDefault Map.empty cid interEdgeCounts)`, producing `[(CommunityId, Int)]` instead of `Int`. Hspec tests verify correct inter-community edge pairs and empty edges for isolated communities. QuickCheck property `prop_aggregate_count` holds for 100 test cases. No `IO` or `Infrastructure` imports. -->

### Dependencies

- Required: Tasks 1–2 completed, Task 3 plan and do completed
- Unlocks: Task 3 check passes → task is `[x]` in tasks.md

## Result

<!-- PASS → task is complete, mark `[x]` in tasks.md.
     FAIL → "FAIL — see attempt-2/" and start a new PDCA cycle. -->
