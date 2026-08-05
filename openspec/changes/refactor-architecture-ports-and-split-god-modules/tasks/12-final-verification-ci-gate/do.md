<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 12 — Final verification and CI gate — DO

**Task slug**: `12-final-verification-ci-gate`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. Comprehensive verification that all architectural constraints are met: zero UseCase→Infrastructure imports, no bare IO in UseCase signatures, all modules under size limits, clean build/test.

## Detail

### What needs to be verified

Run all 5 verification gates from the design document:

1. **Gate 1 — Zero UseCase→Infrastructure imports**: `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/` must return zero (excluding Port modules which legitimately import Infrastructure types like `AnnotatedPattern`, and Wiring re-export modules)

2. **Gate 2 — Module size limits**: All modules <300 lines (excluding re-export modules like `Domain.Config.hs`, `UseCase.Extract.hs`, `UseCase.Pipeline.hs`)

3. **Gate 3 — Build passes with -Wall -Werror**: `cabal build` with dev flag must succeed

4. **Gate 4 — All tests pass**: `cabal test` must pass with zero failures (note any pre-existing flaky tests like SDKSpec)

5. **Gate 5 — No bare IO in UseCase**: `rg ":: .*IO " src/Graphos/UseCase/` must show only `MonadIO m =>` or port-constrained signatures, no bare `IO` return types

### Additional verification

- End-to-end test: `cabal run graphos -- .` produces identical output
- Verify `FileSystemPort` no longer imports `Infrastructure.FileSystem.Ignore (AnnotatedPattern)` — should be moved to Domain or port-local
- Verify `productionExportPort` is fully wired (no `error "not yet wired"`)
- Update `.opencode/context/core/standards/code-quality.md` with port pattern documentation

### Concrete steps

```bash
# Gate 1
rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/
# Expected: zero matches (excluding Port modules)

# Gate 2
find src/ -name "*.hs" -exec wc -l {} + | sort -rn | head -20
# Verify all non-re-export modules <300 lines

# Gate 3
cabal build --flag dev
# Expected: success

# Gate 4
cabal test
# Expected: all tests pass

# Gate 5
rg ":: .*IO " src/Graphos/UseCase/
# Verify no bare IO in UseCase function signatures

# E2E
cabal run graphos -- .
# Verify output matches pre-refactor output
```

## Result

NOT YET IMPLEMENTED — awaiting Do phase.