<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 5 — Regression + documentation sweep — PLAN

**Task slug**: `05-regression-documentation-sweep`
**Attempt**: 1
**Status**: pending

## Summary

Final gate: verify full test suite pass, confirm every scenario in both delta specs is covered by at least one test, validate help output on all five query-family commands, and confirm zero new warnings under strict compilation flags.

## Detail

### Scope

- **Full test suite**: Run `cabal test` (all suites, not just the query subset). Verify Hspec and QuickCheck suites all pass.

- **Scenario-to-test mapping**: Create and record a mapping from every scenario in both delta specs to at least one test case:

  **neighbor-expansion spec** (6 scenarios):
  - `Scenario: Direct neighbors by internal id` → test in `QuerySpec.hs` or `ParserSpec.hs` (existing neighbors behavior with internal id)
  - `Scenario: Display name fallback resolves a single node` → test in `QuerySpec.hs` for `resolveNodeArg` exact label match
  - `Scenario: Case-insensitive label fallback` → test in `QuerySpec.hs` for `resolveNodeArg` case-insensitive match
  - `Scenario: Ambiguous name lists candidates without traversal` → test in `QuerySpec.hs` for `resolveNodeArg` ambiguous case + test in `ParserSpec.hs` for dispatcher rendering
  - `Scenario: Unknown name fails explicitly` → test in `QuerySpec.hs` for `resolveNodeArg` not found + test in `ParserSpec.hs` for non-zero exit
  - `Scenario: Depth bound respected` → test in `QuerySpec.hs` (existing neighbors behavior)

  **query-cli-contract spec** (12 scenarios):
  - `Scenario: Query JSON contains verdict and hash` → test in `QuerySpec.hs` for JSON parse + field presence
  - `Scenario: Query text mode is unchanged` → regression test / golden output comparison
  - `Scenario: Query text and JSON agree` → property test in `QuerySpec.hs`
  - `Scenario: Query accepts shared flags` → test in `ParserSpec.hs` for `--json`, `--label-width`, `--edges`, `--budget`
  - `Scenario: Path found renders as JSON` → test in `QuerySpec.hs` or `ParserSpec.hs`
  - `Scenario: No path renders as JSON null` → test in `QuerySpec.hs` or `ParserSpec.hs`
  - `Scenario: Path text mode is unchanged` → regression test
  - `Scenario: Explain found renders as JSON` → test in `QuerySpec.hs` or `ParserSpec.hs`
  - `Scenario: Explain miss renders as JSON null` → test in `QuerySpec.hs` or `ParserSpec.hs`
  - `Scenario: Explain text mode is unchanged` → regression test
  - `Scenario: Reference lists json for query` → test in `ParserSpec.hs` for `renderCommandReference` output
  - `Scenario: Reference lists json for path and explain` → test in `ParserSpec.hs` for `renderCommandReference` output

- **Help output validation**: Run `graphos query --help`, `graphos path --help`, `graphos explain --help`, `graphos neighbors --help` and verify each:
  - Prints usage text listing `--json` (and `--label-width`, `--edges` where applicable)
  - Exits with code 0

- **Strict compilation check**: Run `cabal build --flag dev` with the project's strict flags (`-Wall -Wcompat -Wincomplete-uni-patterns -Werror`) and confirm zero warnings.

- **No new test cases needed unless gaps are found**: Only add tests for uncovered scenarios. Do not add tests for scenarios already covered by tasks 1-4.

### Check Criteria

**Tests to run**:
- `cabal test` — all suites, all cases
- `cabal build --flag dev` with `-Wall -Wcompat -Wincomplete-uni-patterns -Werror` — zero warnings
- Manual: `graphos query --help`, `graphos path --help`, `graphos explain --help`, `graphos neighbors --help` — each lists `--json` and exits 0

**Spec scenarios covered** (to be verified and recorded):
- All 6 scenarios from `neighbor-expansion/spec.md` mapped to at least one test
- All 12 scenarios from `query-cli-contract/spec.md` mapped to at least one test

**PASS conditions**:
- `cabal test` is fully green (all Hspec suites + QuickCheck property suites)
- Every scenario in both delta specs maps to at least one test case (record the mapping in the Check record)
- All four `--help` commands print usage text including `--json` and exit with code 0
- `cabal build --flag dev` produces zero warnings under `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`
- No silent stderr/stdout swap detected in test output (verify no test assertions on stderr changed)

**FAIL boundaries**:
- If any scenario lacks a test mapping, the test fails — the spec requires every scenario to be covered
- If any `--help` command does not list `--json`, the test fails — this is a `query-cli-contract` requirement
- If `cabal build --flag dev` produces new warnings (not pre-existing), the test fails — must fix by root cause, not suppression

### Affected modules

- **Modified (tests only)**: `tests/Graphos/UseCase/QuerySpec.hs` — add any missing test cases uncovered during scenario-to-test mapping
- **Modified (tests only)**: `tests/Graphos/CLI/ParserSpec.hs` — add `--help` output assertions if needed
- **No production code changes expected** unless new test gaps are discovered

### Prerequisites

- Tasks 1-4 are complete and passing
- `graphos-out/graph.json` exists for manual smoke tests (if needed)
- `renderCommandReference` has been updated in Task 3

### Risks

- **Low**: No production code changes expected — this task is verification-only
- **Medium**: Scenario-to-test mapping may reveal gaps from earlier tasks. Mitigation: record gaps here and add tests in the Do phase of this task
- **Low**: Help output for `path` and `explain` — these commands previously had no `--json` in help; now they do. The `--help` output must include the new flags
