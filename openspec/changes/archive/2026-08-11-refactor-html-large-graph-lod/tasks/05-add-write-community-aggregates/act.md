<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 5 — Add writeCommunityAggregates to IncrementalJSON.hs — ACT

**Task slug**: `05-add-write-community-aggregates`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Record the standardized outcome for Task 5: `writeCommunityAggregates` function in `src/Graphos/Infrastructure/Export/IncrementalJSON.hs`.

## Detail

### Outcome

<!-- If check passed: record the standardized outcome.
     If check failed: record FAIL — see attempt-2/ and start a new P→D→C→A cycle. -->

<!-- Standardized outcome when check passes:
     `writeCommunityAggregates :: IncrementalWriter -> [CommunityAggregate] -> IO ()` added to `IncrementalJSON.hs`. Uses `writeKey` + `BSL.hPut` + `encode` pattern matching `writeGodNodes`. Key name is exactly `"community_aggregates"`. Called after `writeGodNodes` and before `writeAnalysisTail` in the pipeline. Round-trip test produces valid JSON with correct data. -->

### Dependencies

- Required: Task 1 completed (`CommunityAggregate` with correct `ToJSON`), Task 5 plan and do completed
- Unlocks: Task 5 check passes → task is `[x]` in tasks.md

## Result

<!-- PASS → task is complete, mark `[x]` in tasks.md.
     FAIL → "FAIL — see attempt-2/" and start a new PDCA cycle. -->
