# Plan — 1.P Port the fidelity harness into the Haskell codebase

## Scope
Land the oracle first so every later task is measurable.
Add `tests/Graphos/Fidelity/ImportEdgesSpec.hs`, `tests/Graphos/Fidelity/GraphCoverageSpec.hs`,
and `src/Graphos/UseCase/Subgraph.hs` exposed via a `graphos subgraph` CLI subcommand.
Remove the Python scripts from `scripts/`. Update `graphos.cabal`.

## Check Criteria
- [ ] All three components compile under `cabal build --flag dev` with `-Werror`.
- [ ] `ImportEdgesSpec` emits a structured Hspec failure (not an uncaught exception) on a graph
  with zero `imports` edges.
- [ ] On today's `solario-core` graph `ImportEdgesSpec` reports recall 0.0 with 203 missing pairs
  and the spec fails (baseline captured for later comparison).
- [ ] `GraphCoverageSpec` reports the 86 missing files grouped by class and the spec fails.
- [ ] `graphos subgraph --graph <fixture> --config <fixture> --out <tmp>` produces a JSON file
  loadable by `graphos query --graph <out>` without schema errors.
- [ ] Every flag shown in `README.md` exists in the CLI parser or spec module.

## Affected Modules
- `src/Graphos/UseCase/Subgraph.hs`
- `tests/Graphos/Fidelity/ImportEdgesSpec.hs`
- `tests/Graphos/Fidelity/GraphCoverageSpec.hs`
- `graphos.cabal`
- `scripts/` (removal)
- `README.md`

## Risks
- Fidelity specs need fixture corpora — mitigate by using `temporary` to create in-test fixtures.
