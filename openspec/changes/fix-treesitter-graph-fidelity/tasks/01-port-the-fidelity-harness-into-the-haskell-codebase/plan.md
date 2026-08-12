<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 1 — Port the fidelity harness into the Haskell codebase — PLAN

**Task slug**: `01-port-the-fidelity-harness-into-the-haskell-codebase`
**Attempt**: 1
**Status**: pending

## Summary

Port the fidelity harness (ImportEdgesSpec, GraphCoverageSpec, and subgraph extraction) from Python to Haskell.

## Detail

**Scope**:
- Add `tests/Graphos/Fidelity/ImportEdgesSpec.hs` and `tests/Graphos/Fidelity/GraphCoverageSpec.hs`.
- Implement `src/Graphos/UseCase/Subgraph.hs` for pure subgraph extraction.
- Add `graphos subgraph` CLI subcommand with flags: `--graph`, `--config`, `--out`, `--boundary-hops`, `--no-derive`.
- Remove Python scripts from `scripts/`.
- Update `graphos.cabal` (test-suite other-modules and exposed-modules).

**Check Criteria**:
- **Gate**: `cabal build --flag dev -Werror` must succeed for all components.
- **Scenario**: `ImportEdgesSpec` on a graph with zero `imports` edges must emit a structured Hspec failure (not an exception).
- **Scenario**: `ImportEdgesSpec` on `solario-core` graph must report recall 0.0 with 203 missing pairs and fail (baseline).
- **Scenario**: `GraphCoverageSpec` must report 86 missing files grouped by class and fail.
- **Scenario**: `graphos subgraph --graph <fixture> --config <fixture> --out <tmp>` must produce a JSON file that `graphos query --graph <out>` can load without schema errors.
- **Scenario**: All flags in `README.md` must be present in the CLI parser.
- **PASS Condition**: All criteria met and baseline numbers recorded.
- **FAIL Condition**: Uncaught exceptions in specs, schema errors in output, or missing CLI flags.

**Affected modules**:
- `src/Graphos/UseCase/Subgraph.hs`
- `tests/Graphos/Fidelity/ImportEdgesSpec.hs`
- `tests/Graphos/Fidelity/GraphCoverageSpec.hs`
- `graphos.cabal`
- `scripts/` (removal)

**Prerequisites**:
- `aeson`, `directory`, `filepath`, `temporary` (for fixtures).

**Risks**:
- Need for fixture corpora; mitigated by using `temporary` for in-test fixtures.

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
