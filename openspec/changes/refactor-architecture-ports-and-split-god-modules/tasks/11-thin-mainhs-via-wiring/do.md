<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 11 — Thin Main.hs via Infrastructure.Wiring — DO

**Task slug**: `11-thin-mainhs-via-wiring`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. Reduce `Main.hs` (718 lines) to <200 lines by moving all port wiring to `Infrastructure.Wiring.productionAppEnv`. Main.hs should only parse CLI args, call wiring, and pass AppEnv to the pipeline.

## Detail

### What needs to be implemented

1. **Move Infrastructure imports from Main.hs to Wiring.hs**: Main currently imports ~40 Infrastructure modules. After this task, Main should only import:
   - CLI parsing (optparse-applicative)
   - `UseCase.AppEnv (AppEnv)`
   - `Infrastructure.Wiring (productionAppEnv)`
   - Domain types needed for CLI arg parsing
   - UseCase.Pipeline entry points

2. **Simplify Main.hs flow**:
   ```haskell
   main = do
     config <- parseCLI  -- CLI parsing only
     logEnv <- initLogging config
     obsEnv <- initObservability config
     appEnv <- pure $ productionAppEnv logEnv obsEnv
     result <- runPipeline appEnv config
     handleResult result
   ```

3. **Move initialization to Wiring.hs**: All port construction, LSP client setup, etc. moves from Main to `productionAppEnv`.

### Current Main.hs responsibilities (718 lines)
- CLI argument parsing
- Config file loading
- Logging initialization
- Observability (OTel) initialization
- LSP client initialization (per-language)
- Pipeline execution
- Export orchestration
- Graceful shutdown
- Signal handling

After thinning, Main.hs should handle:
- CLI argument parsing
- Calling `productionAppEnv`
- Calling `runPipeline appEnv config`
- Handling result and graceful shutdown

### Prerequisites

- Tasks 6-8 complete (all UseCase modules use ports)

### Concrete changes needed

- Refactor `app/Main.hs` — remove Infrastructure imports, add `productionAppEnv` call
- Verify `Infrastructure.Wiring.hs` — ensure `productionAppEnv` handles all initialization
- End-to-end test: `cabal run graphos -- .` must produce identical output

## Result

NOT YET IMPLEMENTED — awaiting Do phase.