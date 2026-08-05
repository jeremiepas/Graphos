<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 8 — Refactor UseCase.Export, Ingest, Label, Detect to use ports — ACT

**Task slug**: `08-refactor-usecase-others-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. The Do phase has not been executed for this task.

## Detail

### Check Outcome

| # | Criterion | Result | Notes |
|---|-----------|--------|-------|
| 1 | No Infrastructure imports in UseCase (excl. Port modules) | FAIL | 25 Infrastructure imports remain across 10 files |
| 2 | `cabal build` succeeds | PASS | Pre-refactoring baseline |
| 3 | `cabal test` passes | PASS | Pre-refactoring baseline |

### Critical Item for Implementation

Must also resolve the `AnnotatedPattern` import in `FileSystemPort`. Options:
- **Option A** (preferred): Move `AnnotatedPattern` to Domain types
- **Option B**: Define a port-local type and convert in Wiring

After resolving, must also complete `productionExportPort` in Infrastructure.Wiring (currently `error "not yet wired"`).

## Result

**NOT OK** — Task 8 requires implementation. See attempt-2/ for the next PDCA cycle.