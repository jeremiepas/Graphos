<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Add --json + shared flags to query, path, explain parsers — PLAN

**Task slug**: `03-add-json-shared-flags-parsers`
**Attempt**: 1
**Status**: pending

## Summary

Thread `CommonQueryOpts` through the `queryOpts`, `pathOpts`, and `explain` subparsers so that `query`, `path`, and `explain` accept the same shared flags (`--json`, `--label-width`, `--edges`) as `symbols` and `neighbors`. Change the `QueryCmd`, `PathCmd`, and `ExplainCmd` `Command` constructors to carry `CommonQueryOpts` instead of positional tuples.

## Detail

### Scope

- **Constructor shape change** in `src/Graphos/CLI/Parser.hs`:
  - Before: `QueryCmd :: Text -> Text -> Text -> Text -> Text -> Bool -> Int` (question, mode, dfsFlag, budget, graphFlag, jsonFlag, depth)
  - After: `QueryCmd :: Text -> Text -> CommonQueryOpts` (question, mode, opts)
  - Before: `PathCmd :: Text -> Text -> Text -> Text` (from, to, budget, graphFlag)
  - After: `PathCmd :: Text -> Text -> CommonQueryOpts` (from, to, opts)
  - Before: `ExplainCmd :: Text -> Text -> Text` (node, budget, graphFlag)
  - After: `ExplainCmd :: Text -> CommonQueryOpts` (node, opts)
  - These constructors are internal to `Graphos.CLI.Parser` — grep confirms only `Parser.hs` defines them and `app/Main.hs` pattern-matches them

- **Parser rewrites**:
  - `queryOpts`: parse `QUESTION + --mode + --json + --label-width + --edges + --dfs + --budget + --graph` → `QueryCmd question mode opts`
  - `pathOpts`: parse `FROM + TO + --json + --label-width + --edges + --budget + --graph` → `PathCmd from to opts`
  - `explainCmd` (the `explain` subparser): parse `NODE + --json + --label-width + --edges + --budget + --graph` → `ExplainCmd node opts`
  - Note: `queryOpts` keeps `--dfs` as a separate flag (part of the question mode, not shared flags)

- **Command reference update** in `src/Graphos/CLI/Parser.hs`:
  - Update `renderCommandReference` to list `--json`, `--label-width`, `--edges` for `query`, `path`, and `explain` in addition to existing entries for `symbols` and `neighbors`

### Check Criteria

**Tests to run**:
- `grep -rn "QueryCmd\|PathCmd\|ExplainCmd" app/ tests/ src/` — must show only `Parser.hs` (definitions) and `Main.hs` (dispatch) matches
- `cabal build --flag dev` with `-Wall -Werror --compat -Wincomplete-uni-patterns` — clean
- `cabal test` — Hspec cases in `tests/Graphos/CLI/ParserSpec.hs` for new flag parsing

**Spec scenarios satisfied**:
- `Scenario: Query accepts shared flags` — query-cli-contract spec
- `Scenario: Reference lists json for query` — query-cli-contract spec
- `Scenario: Reference lists json for path and explain` — query-cli-contract spec

**PASS conditions**:
- `grep -rn "QueryCmd\|PathCmd\|ExplainCmd" app/ tests/ src/` shows matches only in `Parser.hs` (constructor definitions) and `Main.hs` (pattern matches on the three constructors) — no stale matches in any other file
- `cabal build --flag dev` compiles with zero warnings under `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`
- `graphos query "term" --json --help` parses without `Invalid option` error
- `graphos path A B --json --label-width 80 --edges all` parses without error
- `graphos explain NODE --json --budget 1000` parses without error
- `renderCommandReference` output includes `--json` for all five query-family commands (query, path, explain, symbols, neighbors)
- `renderCommandReference` output includes `--label-width` and `--edges` for query, path, explain (in addition to symbols and neighbors)
- Existing parsers (`symbols`, `neighbors`) are unaffected — their `CommonQueryOpts` parsing continues to work

**FAIL boundaries**:
- If `grep` reveals pattern matches on `QueryCmd`/`PathCmd`/`ExplainCmd` outside `Parser.hs` and `Main.hs`, the test fails — the constructor shape change must not leave stale matches
- If `queryOpts` no longer parses `--dfs` as a separate flag (it was existing behavior), the test fails
- If `renderCommandReference` is missing `--json` for any of the three commands, the test fails

### Affected modules

- **Modified**: `src/Graphos/CLI/Parser.hs` — rewrite `QueryCmd`, `PathCmd`, `ExplainCmd` constructors; rewrite `queryOpts`, `pathOpts`, `explain` parsers; update `renderCommandReference`
- **Modified**: `app/Main.hs` — update pattern matches on the three constructors (shape change only, no behavior change yet)
- **New (tests)**: `tests/Graphos/CLI/ParserSpec.hs` — add parse cases for `--json`, `--label-width`, `--edges` on `query`, `path`, `explain`

### Prerequisites

- `CommonQueryOpts` type already exists and is used by `symbolsOpts` and `neighborsOpts`
- `QueryCmd`, `PathCmd`, `ExplainCmd` constructors are defined only in `Parser.hs`
- `app/Main.hs` pattern-matches on these constructors (to be updated)

### Risks

- **High**: Constructor shape change is a breaking change for pattern matches — must verify all match sites via grep before editing. Mitigation: grep first, then edit both `Parser.hs` and `Main.hs` in a single commit
- **Medium**: `renderCommandReference` consumer (skill generation) must be updated — however, skill regeneration is out of scope. Mitigation: just update `renderCommandReference` in `Parser.hs`; skills regenerate on next pipeline run
- **Low**: `--dfs` flag on `queryOpts` is not part of `CommonQueryOpts` — must keep it as a separate field in `QueryCmd`. Mitigation: `QueryCmd` carries `CommonQueryOpts` plus the existing `--dfs` behavior (mode field)
