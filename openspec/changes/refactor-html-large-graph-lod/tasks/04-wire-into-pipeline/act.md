<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Wire join + aggregates into Pipeline.hs — ACT

**Task slug**: `04-wire-into-pipeline`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Record the standardized outcome for Task 4: pipeline wiring, `epWriteCommunityAggregates` port, SQLite export, and COOP/COEP headers.

## Detail

### Outcome

<!-- If check passed: record the standardized outcome.
     If check failed: record FAIL — see attempt-2/ and start a new P→D→C→A cycle. -->

<!-- Standardized outcome when check passes:
     `joinCommunitiesToNodes` and `computeCommunityAggregates` are wired into `Pipeline.hs` after the re-cluster step. `epWriteCommunityAggregates` added to `ExportPort` class and implemented in `Wiring.hs`. SQLite export module created at `src/Graphos/Infrastructure/Export/SQLite.hs` with batched inserts. COOP/COEP headers added to `Static.hs`. Pipeline ordering: join → write-aggregates → write-nodes → write-edges. `graph.json` has non-null `community_id` on community members and `community_aggregates` key present. `graph.sqlite` has correct row counts. COOP/COEP headers present on serve. No regression in node/edge/community counts. -->

### Dependencies

- Required: Tasks 1–3 completed, Task 4 plan and do completed
- Unlocks: Task 4 check passes → task is `[x]` in tasks.md

## Result

<!-- PASS → task is complete, mark `[x]` in tasks.md.
     FAIL → "FAIL — see attempt-2/" and start a new PDCA cycle. -->
