<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 5 — Create UseCase.AppEnv and Infrastructure.Wiring — ACT

**Task slug**: `05-usecase-appenv-infrastructure-wiring`
**Attempt**: 1
**Status**: PASS

## Summary

All 4 check criteria passed. AppEnv aggregates all 6 ports, Wiring provides production implementations.

## Detail

### Check Outcome

| # | Criterion | Result | Notes |
|---|-----------|--------|-------|
| 1 | AppEnv record compiles with all 6 port fields | PASS | `extractionPort`, `exportPort`, `fileSystemPort`, `loggingPort`, `observabilityPort`, `llmPort` |
| 2 | Wiring has `productionAppEnv` type signature | PASS | Type: `LogEnv -> ObservabilityEnv -> AppEnv` (see design decision note) |
| 3 | `cabal build` succeeds | PASS | |
| 4 | `cabal test` passes | PASS | 200 examples, 0 failures |

### Design Decision

The plan specified `productionAppEnv :: GraphosConfig -> IO AppEnv`, but the implementation uses `productionAppEnv :: LogEnv -> ObservabilityEnv -> AppEnv`. Rationale: Main.hs initializes logging and observability before creating the AppEnv, so these are passed as already-initialized resources. This avoids Wiring needing to know about log file paths, OTel config, etc., and matches the actual startup sequence in Main.hs.

### Outstanding Item

`productionExportPort` throws `error "not yet wired"`. This is intentional — it will be completed in Task 8 when UseCase.Export is refactored to use the port.

## Result

**PASS** — Task 5 complete. Design decision on `productionAppEnv` type documented. ExportPort TODO noted for Task 8.