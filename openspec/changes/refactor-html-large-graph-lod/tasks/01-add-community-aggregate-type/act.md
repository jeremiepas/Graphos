<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 1 — Add CommunityAggregate Domain type — ACT

**Task slug**: `01-add-community-aggregate-type`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Record the standardized outcome for Task 1: `CommunityAggregate` type with corrected `caInterCommunityEdges` field type and Aeson instances in `src/Graphos/Domain/Types/Analysis.hs`.

## Detail

### Outcome

<!-- If check passed: record the standardized outcome.
     If check failed: record FAIL — see attempt-2/ and start a new P→D→C→A cycle. -->

<!-- Standardized outcome when check passes:
     The `CommunityAggregate` record field `caInterCommunityEdges` was changed from `!Int` to `![(CommunityId, Int)]`. Custom `ToJSON` and `FromJSON` instances produce the `{"target":..,"count":..}` object shape. The module is under `src/Graphos/Domain/` with zero `IO` imports. -->

### Dependencies

- Required: Task 1 plan and do completed
- Unlocks: Task 1 check passes → task is `[x]` in tasks.md

## Result

<!-- PASS → task is complete, mark `[x]` in tasks.md.
     FAIL → "FAIL — see attempt-2/" and start a new PDCA cycle. -->
