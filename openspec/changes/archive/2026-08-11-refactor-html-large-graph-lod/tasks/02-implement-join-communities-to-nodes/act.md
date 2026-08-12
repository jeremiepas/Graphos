<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Implement joinCommunitiesToNodes UseCase function — ACT

**Task slug**: `02-implement-join-communities-to-nodes`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Record the standardized outcome for Task 2: Hspec tests and QuickCheck property for `joinCommunitiesToNodes` in `src/Graphos/UseCase/Cluster.hs`.

## Detail

### Outcome

<!-- If check passed: record the standardized outcome.
     If check failed: record FAIL — see attempt-2/ and start a new P→D→C→A cycle. -->

<!-- Standardized outcome when check passes:
     Hspec tests verify that `joinCommunitiesToNodes` correctly sets `community_id` on nodes in communities, leaves isolated nodes as `Nothing`, handles empty graphs, and assigns correct community IDs across multiple communities. QuickCheck property `prop_joinCommunities_count` holds for 100 test cases. The function has no `IO` in its type signature or body. -->

### Dependencies

- Required: Task 2 plan and do completed
- Unlocks: Task 2 check passes → task is `[x]` in tasks.md

## Result

<!-- PASS → task is complete, mark `[x]` in tasks.md.
     FAIL → "FAIL — see attempt-2/" and start a new PDCA cycle. -->
