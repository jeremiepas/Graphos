<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 11 — Thin Main.hs via Infrastructure.Wiring — PLAN

**Task slug**: `11-thin-mainhs-via-wiring`
**Attempt**: 1
**Status**: pending

## Summary

Reduce `Main.hs` (718 lines) wiring logic to <100 lines by moving all port wiring to `Infrastructure.Wiring.productionAppEnv`. Main.hs should only parse CLI args, call wiring, and pass AppEnv to the pipeline.

## Detail

### Scope

Thin `app/Main.hs` from 718 lines to <200 lines total (with <15 imports, where wiring logic is <100 lines). The key changes:

1. Move all Infrastructure module imports from Main.hs to Wiring.hs
2. Main.hs imports only: CLI parsing modules, `UseCase.AppEnv`, `Domain.Config` types, `Infrastructure.Wiring (productionAppEnv)`
3. Main.hs flow: parse args → call `productionAppEnv` → pass `AppEnv` to `runPipeline`
4. All port construction, LSP client setup, logging initialization, etc. moves to `Infrastructure.Wiring`

### Affected Modules

- `app/Main.hs` — major thinning (718 → <200 lines)
- `src/Graphos/Infrastructure/Wiring.hs` — may need expansion (currently 231 lines)

### Prerequisites

- Tasks 6-8 complete (all UseCase modules use ports)
- Task 5 complete (AppEnv + Wiring exist)

### Risks

- Main.hs currently handles CLI parsing, config loading, LSP client initialization, signal handling, and graceful shutdown — all must be preserved
- `productionAppEnv` must handle all the initialization that was in Main.hs
- Some initialization (e.g., LSP client) may require IO that happens before AppEnv construction
- Signal handling and graceful shutdown may need special treatment

## Check Criteria

| # | Criterion | Spec Scenario | PASS Condition | FAIL Condition |
|---|-----------|---------------|----------------|-----------------|
| 1 | Main.hs < 200 lines | N/A | `wc -l app/Main.hs` shows <200 | 200+ lines |
| 2 | Main.hs imports <15 modules | N/A | `rg "^import " app/Main.hs \| wc -l` shows <15 | 15+ imports |
| 3 | `cabal build` succeeds | N/A | Zero compilation errors | Build failure |
| 4 | `cabal test` passes | N/A | All tests pass | Any test failure |
| 5 | End-to-end output preserved | N/A | `cabal run graphos -- .` produces identical output | Different output |