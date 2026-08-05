<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 12 — Final verification and CI gate — CHECK

**Task slug**: `12-final-verification-ci-gate`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET EXECUTED. Final verification will run after all previous tasks are complete. Recording current baseline measurements.

## Detail

### Check Criteria Execution (from plan.md)

| # | Criterion | Spec Scenario | Result | Evidence |
|---|-----------|---------------|--------|----------|
| 1 | Zero UseCase→Infrastructure imports | Design verification | **FAIL** | `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/` returns 25 matches across 10 files. Target: zero (excluding Port modules). |
| 2 | All modules <300 lines | Design verification | **FAIL** | Key violations: UseCase.Extract.hs (574 lines), UseCase.Pipeline.hs (592 lines), Main.hs (718 lines). Target: all <300 lines (excluding re-export modules). |
| 3 | Build passes with -Wall -Werror | Design verification | **PASS** | `cabal build` succeeds (dev flag enables -Wall -Wcompat -Wincomplete-uni-patterns) |
| 4 | All tests pass | Design verification | **PASS** | `cabal test` — 200 examples, 0 failures |
| 5 | No bare IO in UseCase | Design verification | **NOT YET VERIFIED** | Requires running `rg ":: .*IO " src/Graphos/UseCase/` after all refactoring is complete. |

### Baseline Measurements (current state, before Tasks 6-11)

- **UseCase Infrastructure imports**: 25 (target: 0)
- **Module sizes**: Extract.hs=574, Pipeline.hs=592, Main.hs=718 (all above 300-line limit)
- **Build status**: PASS
- **Test status**: 200/200 PASS

## Result

**NOT YET EXECUTED** — Task 12 depends on Tasks 6-11 being complete. Final verification will be re-run after all preceding tasks pass their own checks.