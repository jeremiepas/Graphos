<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 5 — Create UseCase.AppEnv and Infrastructure.Wiring — PLAN

**Task slug**: `05-usecase-appenv-infrastructure-wiring`
**Attempt**: 1
**Status**: pending

## Summary

Create the `UseCase.AppEnv` record (aggregates all 6 ports) and verify/complete `Infrastructure.Wiring` (production AppEnv factory). Both modules already exist (AppEnv at 25 lines, Wiring at 231 lines).

## Detail

### Scope

Two modules to verify/complete:

1. **UseCase.AppEnv** (`src/Graphos/UseCase/AppEnv.hs`, 25 lines) — Must contain an `AppEnv` record with fields for all 6 ports: `extractionPort :: ExtractionPort`, `exportPort :: ExportPort`, `fileSystemPort :: FileSystemPort`, `loggingPort :: LoggingPort`, `observabilityPort :: ObservabilityPort`, `llmPort :: LLMPort`.

2. **Infrastructure.Wiring** (`src/Graphos/Infrastructure/Wiring.hs`, 231 lines) — Must provide `productionAppEnv :: GraphosConfig -> IO AppEnv` that wires all 6 ports to their real Infrastructure implementations.

### Affected Modules

- `src/Graphos/UseCase/AppEnv.hs` — AppEnv record definition
- `src/Graphos/Infrastructure/Wiring.hs` — production wiring factory
- Both already exist and need verification/completion

### Prerequisites

- Tasks 2-4 (all port modules) must be complete so AppEnv can reference their types
- Task 1 complete ✅

### Risks

- AppEnv must import all 6 port types — circular import risk if ports import AppEnv
- Wiring must construct real implementations — if any port field signature doesn't match the actual Infrastructure function, compilation will fail
- `productionAppEnv` takes `GraphosConfig` and produces `IO AppEnv` — some port fields need IO initialization (e.g., LSP client connection setup)

## Check Criteria

| # | Criterion | Spec Scenario | PASS Condition | FAIL Condition |
|---|-----------|---------------|----------------|-----------------|
| 1 | AppEnv record compiles with all 6 port fields | "AppEnv contains all port fields" | `AppEnv` has exactly 6 port fields and `cabal build` succeeds | Missing fields or build error |
| 2 | Wiring has `productionAppEnv` type signature | "Wiring produces production AppEnv" | `productionAppEnv :: GraphosConfig -> IO AppEnv` type exists | Wrong type or missing |
| 3 | `cabal build` succeeds | N/A | Zero compilation errors | Any compilation error |
| 4 | `cabal test` passes | N/A | All tests pass (existing test suite) | Any test failure |