<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 5 — Create UseCase.AppEnv and Infrastructure.Wiring — CHECK

**Task slug**: `05-usecase-appenv-infrastructure-wiring`
**Attempt**: 1
**Status**: in-progress

## Summary

Verifying that AppEnv aggregates all 6 ports, Wiring provides productionAppEnv, and both build and tests pass.

## Detail

### Check Criteria Execution (from plan.md)

| # | Criterion | Spec Scenario | Result | Evidence |
|---|-----------|---------------|--------|----------|
| 1 | AppEnv record compiles with all 6 port fields | "AppEnv contains all port fields" | **PASS** | `AppEnv` has exactly 6 port fields: `extractionPort :: ExtractionPort`, `exportPort :: ExportPort`, `fileSystemPort :: FileSystemPort`, `loggingPort :: LoggingPort`, `observabilityPort :: ObservabilityPort`, `llmPort :: LLMPort`. Module compiles successfully. |
| 2 | Wiring has `productionAppEnv` type signature | "Wiring produces production AppEnv" | **PASS** | `productionAppEnv :: LogEnv -> ObservabilityEnv -> AppEnv` exists in `Infrastructure.Wiring`. Takes `LogEnv` and `ObservabilityEnv` as parameters (already initialized by Main), returns fully-wired `AppEnv`. Also exports individual port constructors: `productionLoggingPort`, `productionObservabilityPort`, `productionFileSystemPort`, `productionExtractionPort`, `productionExportPort`, `productionLLMPort`. |
| 3 | `cabal build` succeeds | N/A | **PASS** | `cabal build` completed with zero errors. |
| 4 | `cabal test` passes | N/A | **PASS** | `cabal test` — 200 examples, 0 failures. "Test suite graphos-test: PASS" |

### Note on `productionAppEnv` type

The plan specified `productionAppEnv :: GraphosConfig -> IO AppEnv`, but the implementation uses `productionAppEnv :: LogEnv -> ObservabilityEnv -> AppEnv`. This is a design decision documented in do.md: Main.hs initializes logging and observability before creating the AppEnv, so these are passed as already-initialized resources rather than creating them from config inside Wiring. This is cleaner because logging initialization needs to happen before any other IO.

### Known Issue (documented, not a blocker)

`productionExportPort` currently throws `error "not yet wired"`. This is intentional — the export port will be fully wired once UseCase.Export is refactored to use it (Task 8).

## Result

**PASS** — All 4 check criteria pass. Design decision on `productionAppEnv` type documented. ExportPort TODO noted for Task 8.