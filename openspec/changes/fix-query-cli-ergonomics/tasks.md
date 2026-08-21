## 1. Add `resolveNodeArg` helper in `Graphos.UseCase.Query`

- [x] 1.P Plan: Add a pure node-argument resolver in the UseCase layer (`src/Graphos/UseCase/Query.hs`). Scope: new `NodeResolution` sum type + `resolveNodeArg :: Text -> Graph -> GraphIndex -> NodeResolution`. Resolution order: exact id → exact label (via `giLabelIndex`) → case-insensitive label fallback. Do NOT change `neighborhoodExpansion` signature (still `NodeId -> Int -> Graph -> GraphIndex -> NeighborsResult`). Affected: `Query.hs` exports + module list. Risks: reusing `symbolLookup` must not couple expansion to fuzzy scoring.
  Check criteria (defined before code):
  - `cabal build --flag dev` is green with `-Werror`.
  - `resolveNodeArg` has a type signature and is exported from `Graphos.UseCase.Query`.
  - Unit tests in `tests/Graphos/UseCase/QuerySpec.hs` cover: exact id resolves to `ResolvedSingle`; exact label resolves to `ResolvedSingle`; case-insensitive label fallback resolves to `ResolvedSingle`; two same-label nodes in different files resolve to `Ambiguous` with 2 candidates; a miss resolves to `NotFound`.
- [x] 1.D Do: Implement `NodeResolution` + `resolveNodeArg` reusing `giLabelIndex` lookups (do not invoke `findMatchingNodes` fuzzy path). Add the 5 unit tests listed in 1.P.
- [x] 1.C Check: Run `cabal build --flag dev` and `cabal test --test-options="--match resolveNodeArg"` (or the QuerySpec subset). Record PASS/FAIL per the 3 criteria in 1.P.
- [x] 1.A Act: If all criteria PASS, standardize `resolveNodeArg` as the canonical node-argument resolver (note in module Haddoc). If FAIL, record under Attempt history and start attempt 2.

### Attempt history (1)
<!-- empty unless a retry is needed -->

## 2. Widen `neighbors` CLI parser to `<id-or-name>` and resolve in dispatcher

- [x] 2.P Plan: Wire the new resolver into the `neighbors` command. Scope: `src/Graphos/CLI/Parser.hs` (`neighborsOpts` metavar `NODE_ID` → `NODE`, help text updated) and `app/Main.hs` `NeighborsCmd` branch (call `resolveNodeArg`, branch on `ResolvedSingle`/`Ambiguous`/`NotFound`; reuse `renderNeighborsResultJSON`/`renderNeighborsResultText` for the resolved case; render an `Ambiguous` candidate list and a `NotFound` message in both text and JSON modes). Keep all IO in Infrastructure; resolution call is pure.
  Check criteria:
  - `graphos neighbors Graphos.UseCase.QuerySpec --depth 1` (against `graphos-out/graph.json`) returns the same neighborhood as `graphos neighbors mod_Graphos.UseCase.QuerySpec --depth 1` (Plan success criterion from proposal).
  - `graphos neighbors parse --depth 1 --json` on a graph with two `parse` nodes emits a JSON array of candidates (no BFS).
  - `graphos neighbors no_such_thing --depth 1` prints a not-found message and exits non-zero.
  - Existing `graphos neighbors <internal-id> --depth N` behavior is unchanged.
  - `cabal build --flag dev` green; `cabal test` green.
- [x] 2.D Do: Update `neighborsOpts` metavar/help; update `NeighborsCmd` dispatch to call `resolveNodeArg` and branch; add a `renderAmbiguousJSON`/`renderAmbiguousText` helper in `Render.hs` (or inline if trivial). Add ParserSpec + QuerySpec tests for the new path.
- [x] 2.C Check: Build, run the affected test subset, and run the 4 manual smoke commands above. Record PASS/FAIL per criterion.
- [x] 2.A Act: If PASS, standardize the dispatcher branch pattern for future node-argument commands. If FAIL, record and start attempt 2.

### Attempt history (2)
<!-- empty unless a retry is needed -->

## 3. Add `--json` + shared flags to `query`, `path`, `explain` parsers

- [x] 3.P Plan: Make `query`, `path`, `explain` accept the full `CommonQueryOpts` surface. Scope: `src/Graphos/CLI/Parser.hs` — change `QueryCmd`/`PathCmd`/`ExplainCmd` constructors to carry `CommonQueryOpts`; update `queryOpts` (keep `--dfs` as a separate flag), `pathOpts`, and the `explain` subparser to parse `--graph --budget --json --label-width --edges`. Update `renderCommandReference` to list `--json`, `--label-width`, `--edges` for all five query-family commands. Risks: constructor shape change — grep `QueryCmd\|PathCmd\|ExplainCmd` across `app/` and `tests/` first; only `app/Main.hs` should match.
  Check criteria:
  - `grep -rn "QueryCmd\|PathCmd\|ExplainCmd" app/ tests/ src/` shows only `Parser.hs` (definitions) and `Main.hs` (dispatch) matches (no stale pattern matches).
  - `cabal build --flag dev` green with `-Werror`.
  - ParserSpec: `query --json`, `path --json`, `explain --json` parse without `Invalid option`; `query --label-width 80 --edges all` parses; `renderCommandReference` output contains `--json` for all five commands.
- [x] 3.D Do: Refactor the three constructors to carry `CommonQueryOpts`; rewrite the three parsers; update `renderCommandReference`; add/extend ParserSpec cases.
- [x] 3.C Check: Run the grep, `cabal build --flag dev`, and the ParserSpec subset. Record PASS/FAIL per criterion.
- [x] 3.A Act: If PASS, the uniform flag surface is in place. If FAIL, record and start attempt 2.

### Attempt history (3)
<!-- empty unless a retry is needed -->

## 4. Dispatch `--json` rendering for `query`, `path`, `explain` in `app/Main.hs`

- [x] 4.P Plan: Route the new `--json` flag to the already-existing renderers. Scope: `app/Main.hs` — `QueryCmd`/`PathCmd`/`ExplainCmd` branches gain `if cqoJson opts then <json renderer> else <existing text path>`. Use `renderQueryResponseJSON`, `renderPathResultJSON`, `renderExplainResultJSON` from `Render.hs`. Critical: in JSON mode, ensure NO log lines go to stdout — route `logInfo`/`logDebug` to stderr (the default `LogEnv` already writes stderr; verify no `putStrLn`-based log in these branches leaks to stdout). Apply `refineResponse` with `RefineConfig` built from `cqoEdges`/`cqoLabelWidth` for `query` (text and JSON) so `--label-width`/`--edges` actually take effect (they currently use `defaultRefineConfig`).
  Check criteria:
  - `graphos query "Graph" --json | jq .verdict` returns `"strong"` (manual smoke against `graphos-out/graph.json`).
  - `graphos query "Graph" --json` stdout parses as a single JSON document (no interleaved log lines).
  - `graphos path A B --json` and `graphos explain NODE --json` each emit a single JSON document on stdout.
  - Text mode for all three commands is byte-identical to pre-change output (regression diff).
  - `cabal build --flag dev` and `cabal test` green.
- [x] 4.D Do: Edit the three dispatch branches; build `RefineConfig` from `cqoEdges`/`cqoLabelWidth` for the `query` branch; add a QuerySpec/RendererSpec test asserting `renderQueryResponseJSON` output parses as JSON and its `verdict`/`hash`/node-id-set match the text rendering for the same input.
- [x] 4.C Check: Run `cabal build --flag dev`, `cabal test`, and the 4 manual smoke commands. Record PASS/FAIL per criterion.
- [x] 4.A Act: If PASS, all query-family commands honor `--json` and the spec's "JSON output mode" requirement is satisfied. If FAIL, record and start attempt 2.

### Attempt history (4)
<!-- empty unless a retry is needed -->

## 5. Regression + documentation sweep

- [x] 5.P Plan: Final gate. Scope: run the full `cabal test`; confirm the `query-cli-contract` and `neighbor-expansion` delta spec scenarios are each covered by at least one test; update `renderCommandReference` consumers if any skill files reference the old flag surface (only regenerate skills if the project's skill-gen workflow is invoked — out of scope otherwise). Risks: silent stderr/stdout swap could break a log assertion.
  Check criteria:
  - `cabal test` is fully green (all suites, not just the query subset).
  - Every scenario in `specs/neighbor-expansion/spec.md` and `specs/query-cli-contract/spec.md` maps to at least one test case (list the mapping in the Check record).
  - `graphos query --help`, `graphos path --help`, `graphos explain --help`, `graphos neighbors --help` each print usage listing `--json` and exit 0 (the `query-cli-contract` "Help on every subcommand" scenario).
  - No new compiler warnings under `-Wall -Wcompat -Wincomplete-uni-patterns -Werror --flag dev`.
- [x] 5.D Do: Add any missing test cases found while building the scenario→test mapping; fix help text if `--help` fails; silence new warnings by root cause (not by suppression).
- [x] 5.C Check: Run `cabal test`, run the 4 `--help` commands, build with the strict flag set. Record PASS/FAIL per criterion.
- [x] 5.A Act: If all PASS, the change is ready for `/opsx-verify` and archive. If any FAIL, record under Attempt history and start attempt 2 (stop-on-failure: do not auto-fix; report and request approval for any fix).

### Attempt history (5)
<!-- empty unless a retry is needed -->