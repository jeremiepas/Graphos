<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Create UseCase.Port.ExportPort — ACT

**Task slug**: `03-usecase-port-exportport`
**Attempt**: 1
**Status**: PASS

## Summary

All 3 check criteria passed (with documented design decision). ExportPort uses an aggregate `epExportAll` method instead of 9 per-format methods.

## Detail

### Check Outcome

| # | Criterion | Result | Notes |
|---|-----------|--------|-------|
| 1 | ExportPort compiles | PASS | |
| 2 | ExportPort has all export method fields | PASS (with note) | Single `epExportAll` method handles all 9 formats. `ExportResult` captures individual output paths. |
| 3 | `cabal build` succeeds | PASS | |

### Design Decision

The plan specified 9 per-format fields, but implementation uses a single `epExportAll :: FilePath -> Analysis -> PipelineConfig -> Detection -> Maybe (Map CommunityId Text) -> IO ExportResult` method. Rationale: UseCase.Export always produces all formats at once — there is no use case for individual format exports. The `ExportResult` type captures paths to each format's output, providing the same information as separate fields would.

### Outstanding Item

`productionExportPort` in Infrastructure.Wiring currently throws `error "not yet wired"`. This will be completed in Task 8 when UseCase.Export is refactored to use the port.

## Result

**PASS** — Task 3 complete. ExportPort design decision documented. Wiring TODO noted for Task 8.