# Task 4 — Dispatch --json rendering for query, path, explain in app/Main.hs — PLAN

**Task slug**: `04-dispatch-json-rendering-main`
**Attempt**: 1
**Status**: pending

## Summary

Wire the `--json` flag from the newly-threaded `CommonQueryOpts` through to the existing JSON renderers in `app/Main.hs`. Ensure NO log lines leak into stdout in JSON mode. Apply `refineResponse` with `RefineConfig` built from `cqoEdges`/`cqoLabelWidth` for the `query` command so `--label-width` and `--edges` actually take effect.

## Detail

### Scope

- **Dispatcher updates** in `app/Main.hs`:
  - `QueryCmd` branch: add `if cqoJson opts then renderQueryResponseJSON response else textPath` where `response` is first passed through `refineResponse (RefineConfig {rcEdges = cqoEdges opts, rcLabelWidth = cqoLabelWidth opts})`
  - `PathCmd` branch: add `if cqoJson opts then renderPathResultJSON result else textPath`
  - `ExplainCmd` branch: add `if cqoJson opts then renderExplainResultJSON result else textPath`

- **Log hygiene** in JSON branches:
  - Existing `QueryCmd` and `PathCmd` branches contain `logInfo`/`logDebug` calls to `LogEnv`
  - The default `LogEnv` already writes to stderr — verify no `putStrLn`-based logging exists in these branches
  - If any log currently goes to stdout, move it to the `LogEnv` (stderr) when `cqoJson` is true

- **RefineConfig wiring** for `query`:
  - Currently `query` uses `defaultRefineConfig` — update to build `RefineConfig` from `cqoEdges opts` and `cqoLabelWidth opts`
  - `rcEdges` from `cqoEdges :: EdgesMode` (semantic | all)
  - `rcLabelWidth` from `cqoLabelWidth :: Int`
  - `rcBudget` from `cqoBudget :: Int` (existing budget flag) — this already exists on CommonQueryOpts
  - `rcSelfCollapse = True` (existing default)
  - `rcDeclarationDedup = True` (existing default)
  - Apply `refineResponse` before both JSON and text renderers so both modes honor the flags

- **No new renderers** — all `renderQueryResponseJSON`, `renderPathResultJSON`, `renderExplainResultJSON` already exist in `Render.hs`

### Check Criteria

**Tests to run**:
- `cabal test` — Hspec cases in `tests/Graphos/UseCase/QuerySpec.hs` for JSON/text agreement property
- `cabal build --flag dev` with `-Wall -Werror --compat -Wincomplete-uni-patterns` — clean
- Manual smoke tests against `graphos-out/graph.json`

**Spec scenarios satisfied**:
- `Scenario: Query JSON contains verdict and hash` — query-cli-contract spec
- `Scenario: Query text mode is unchanged` — query-cli-contract spec
- `Scenario: Query text and JSON agree` — query-cli-contract spec
- `Scenario: Path found renders as JSON` — query-cli-contract spec
- `Scenario: No path renders as JSON null` — query-cli-contract spec
- `Scenario: Path text mode is unchanged` — query-cli-contract spec
- `Scenario: Explain found renders as JSON` — query-cli-contract spec
- `Scenario: Explain miss renders as JSON null` — query-cli-contract spec
- `Scenario: Explain text mode is unchanged` — query-cli-contract spec

**PASS conditions**:
- `graphos query "Graph" --json | jq .verdict` returns `"strong"` (or the correct verdict for the test graph)
- `graphos query "Graph" --json` stdout parses as a single JSON document (no interleaved log lines)
- `graphos path A B --json` emits a single JSON document with `path` and `hops` fields (or `{"path": null}`)
- `graphos explain NODE --json` emits a single JSON document with `id`, `label`, `source_file`, `community` fields (or `null`)
- Text mode for all three commands produces byte-identical output to pre-change baseline (verified via diff)
- `--label-width` and `--edges` flags affect both text and JSON output for `query`
- `cabal build --flag dev` green
- `cabal test` green

**FAIL boundaries**:
- If stdout in JSON mode contains any text that is not valid JSON (log lines, error messages, etc.), the test fails — JSON output must be a single parseable document
- If text mode output differs from pre-change baseline even by whitespace, the test fails — no regression in existing behavior
- If `--label-width` or `--edges` are silently ignored (no effect on either text or JSON output), the test fails

### Affected modules

- **Modified**: `app/Main.hs` — update `QueryCmd`, `PathCmd`, `ExplainCmd` dispatch branches to check `cqoJson` and route to JSON or text renderers; wire `RefineConfig` from `CommonQueryOpts`
- **Imports from**: `Graphos.UseCase.Query.Render` (existing JSON renderers), `Graphos.UseCase.Query.Refine` (`refineResponse`, `RefineConfig`)

### Prerequisites

- Task 3 (parser changes) is complete — `QueryCmd`, `PathCmd`, `ExplainCmd` carry `CommonQueryOpts`
- `renderQueryResponseJSON`, `renderPathResultJSON`, `renderExplainResultJSON` already exist in `Render.hs`
- `refineResponse` and `RefineConfig` already exist in `Query/Refine.hs`
- `cqoJson`, `cqoEdges`, `cqoLabelWidth`, `cqoBudget` accessor functions exist on `CommonQueryOpts`

### Risks

- **Medium**: Log lines leaking into JSON stdout — must audit all log calls in the three dispatch branches. Mitigation: verify default `LogEnv` writes to stderr; add explicit check for `putStrLn` in these branches
- **Low**: `--label-width` and `--edges` for `query` are currently ignored — this is the status quo. Fixing them is a bonus within scope, not a separate task
- **Low**: JSON/text agreement — both renderers must produce semantically identical results. Mitigation: add a property test asserting agreement on verdict, hash, and node-id-set
