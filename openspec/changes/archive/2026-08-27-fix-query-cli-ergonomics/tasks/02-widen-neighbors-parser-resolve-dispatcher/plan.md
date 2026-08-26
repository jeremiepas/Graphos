# Task 2 — Widen neighbors CLI parser to <id-or-name> and resolve in dispatcher — PLAN

**Task slug**: `02-widen-neighbors-parser-resolve-dispatcher`
**Attempt**: 1
**Status**: pending

## Summary

Wire the new `resolveNodeArg` helper into the `neighbors` CLI command: widen the parser metavar from `NODE_ID` to `NODE`, update the help text, and modify the `NeighborsCmd` dispatch in `app/Main.hs` to call `resolveNodeArg`, branch on `NodeResolution`, and render appropriate output for `ResolvedSingle`, `Ambiguous`, and `NotFound` cases in both text and JSON modes.

## Detail

### Scope

- **Parser change** in `src/Graphos/CLI/Parser.hs`:
  - `neighborsOpts`: change metavar from `NODE_ID` to `NODE`
  - Update help text: `"Expand around a node (id, display name, or case-insensitive name)"`
  - `--depth` remains as-is (already part of `NeighborsOpts`)

- **Dispatcher change** in `app/Main.hs`:
  - In the `NeighborsCmd` branch (currently in the `commandHandler` or `runCommand` function):
    - Call `resolveNodeArg arg graph graphIndex`
    - On `ResolvedSingle nodeId`: call existing `neighborhoodExpansion nodeId depth graph graphIndex`, then route through existing renderers (`renderNeighborsResultJSON` if `cqoJson`, else `renderNeighborsResultText`)
    - On `Ambiguous candidates`: render candidate list in text mode (`Ambiguous` message with node id, label, source file for each) and JSON mode (`Ambiguous` JSON document with candidate array)
    - On `NotFound`: render "node not found" message in text and `null` (or error JSON) in JSON mode
  - Resolution call is pure; all IO stays in Infrastructure

- **New helpers** in `src/Graphos/UseCase/Query/Render.hs` (or inline in Main if trivial):
  - `renderAmbiguousResultJSON :: [ScoredNode] -> Text` — JSON array of candidates
  - `renderAmbiguousResultText :: [ScoredNode] -> Text` — formatted candidate list
  - `renderNotFoundResultJSON :: Maybe Text -> Text` — `null` or `{"error": "..."}`
  - `renderNotFoundResultText :: Maybe Text -> Text` — "Node not found: <arg>"

- **No IO added to UseCase** — `resolveNodeArg` is called in Main.hs (Infrastructure layer)

### Check Criteria

**Tests to run**:
- `cabal test` — Hspec cases in `tests/Graphos/CLI/ParserSpec.hs` (metavar/help) and `tests/Graphos/UseCase/QuerySpec.hs` (resolver + dispatcher behavior)
- `cabal build --flag dev` with `-Wall -Werror --compat -Wincomplete-uni-patterns` — clean

**Spec scenarios satisfied**:
- `Scenario: Direct neighbors by internal id` — neighbor-expansion spec (existing behavior must be unchanged)
- `Scenario: Display name fallback resolves a single node` — neighbor-expansion spec
- `Scenario: Case-insensitive label fallback` — neighbor-expansion spec
- `Scenario: Ambiguous name lists candidates without traversal` — neighbor-expansion spec
- `Scenario: Unknown name fails explicitly` — neighbor-expansion spec
- `Scenario: Depth bound respected` — neighbor-expansion spec (existing behavior must be unchanged)

**PASS conditions**:
- `graphos neighbors Graphos.UseCase.QuerySpec --depth 1` (against `graphos-out/graph.json`) returns the same neighborhood as `graphos neighbors mod_Graphos.UseCase.QuerySpec --depth 1`
- `graphos neighbors parse --depth 1 --json` on a graph with two `parse` nodes emits a JSON array of candidates (no BFS performed)
- `graphos neighbors no_such_thing --depth 1` prints a not-found message to stdout/stderr and exits with non-zero exit code
- `graphos neighbors <internal-id> --depth N` (existing behavior) is byte-identical to pre-change output
- Parser metavar shows `<NODE>` (not `<NODE_ID>`) in help output
- `cabal build --flag dev` green
- `cabal test` green

**FAIL boundaries**:
- If `graphos neighbors parse --depth 1` performs a BFS from one of the ambiguous candidates instead of listing them, the test fails — spec forbids fuzzy traversal on multi-match
- If the not-found case exits with code 0 instead of non-zero, the test fails — explicit failure requires non-zero exit
- If text output for internal-id neighbors changes from pre-change baseline, the test fails — no breaking changes

### Affected modules

- **Modified**: `src/Graphos/CLI/Parser.hs` — update `neighborsOpts` metavar and help text
- **Modified**: `app/Main.hs` — update `NeighborsCmd` dispatch branch to call `resolveNodeArg` and handle all three `NodeResolution` cases
- **Modified**: `src/Graphos/UseCase/Query/Render.hs` (or `Render.hs`) — add `renderAmbiguous*` and `renderNotFound*` helpers
- **New (tests)**: `tests/Graphos/CLI/ParserSpec.hs` — add cases for new metavar/help; `tests/Graphos/UseCase/QuerySpec.hs` — add cases for dispatcher branch behavior

### Prerequisites

- Task 1 (`resolveNodeArg` helper) is implemented and exported
- `neighborhoodExpansion` already exists with signature `NodeId -> Int -> Graph -> GraphIndex -> NeighborsResult`
- `renderNeighborsResultJSON` and `renderNeighborsResultText` already exist
- `cqoJson` (CommonQueryOpts) is available on `NeighborsCmd`

### Risks

- **Medium**: Adding renderer helpers in `Render.hs` — must ensure they don't break existing JSON schemas consumed by the MCP server. Mitigation: new helper functions with distinct names, no modification of existing renderers
- **Medium**: Non-zero exit code for not-found — must coordinate with the `Main.hs` error handling pattern. Mitigation: follow existing patterns for error exits in the codebase
- **Low**: Ambiguous candidate list format — text output must be readable and machine-parseable for JSON mode. Mitigation: JSON mode uses structured array; text mode uses a human-readable bullet list
