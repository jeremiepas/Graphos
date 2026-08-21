# Task 7 — Build + cross-cutting — PLAN

**Task slug**: `07-build-cross-cutting`
**Attempt**: 1
**Status**: pending

## Summary

Final verification: legacy graph compatibility, `-Wall -Werror` clean build, all tests green (existing + new), and manual end-to-end validation against a real `graph.json`.

## Detail

### Scope

- **Legacy graph compatibility**:
  - Verify `graph.json` without `compositions` loads without crash
  - Verify `graphos research` works on a legacy graph (communities have `composition: null`)
  - No crash or missing field errors when compositions are absent
- **Build verification**:
  - `cabal build` with `-Wall -Werror` and `--flag dev` — clean, no warnings
  - `cabal test` — green (all existing tests + all new Hspec cases from Tasks 1-5)
- **Manual end-to-end verification**:
  - Run `graphos research phase work block governance --html --label solario-phases` against a real `graph.json`
  - Open the generated HTML via `file://` in a browser
  - Confirm: legend lists all 4 terms with distinct colors; nodes are color-coded by discovering term; hovering a node shows detail panel with `discovered_by`, `scores`, `best_score`; edges only connect union nodes
  - Confirm `graphos research phase --json` node set == `graphos query phase --json` node set (single-term equivalence)
  - Time end-to-end on a 10K-node graph: target < 2s (4 queries × < 500ms + induce + render)
  - Confirm `--terms-file` with a 10-line file produces a research view with all 10 terms attributed

### Check Criteria

**Tests to run**:
- `cabal build --flag dev` — exits 0, no warnings
- `cabal test` — exits 0, all tests pass
- Manual: `graphos research phase work block governance --html --label solario-phases` — produces valid HTML file
- Manual: `graphos research phase --json` — node set equals `graphos query phase --json` node set (programmatic comparison)
- Manual: `time graphos research phase work block governance --json` — wall time < 2s on 10K-node graph
- Manual: `graphos research phase --terms-file terms.txt --json` where `terms.txt` has 10 lines — output contains all 10 terms

**Spec scenarios satisfied**:
- `Scenario: legacy graph composition is null` (end-to-end verification)
- All scenarios from Tasks 1-5 (final gate before merge)

**PASS conditions**:
- `cabal build --flag dev` completes with exit code 0 and zero warnings
- `cabal test` completes with exit code 0 and 100% test pass rate
- Legacy `graph.json` (without `compositions`) produces a valid `ResearchView` with `composition: null` for all communities
- HTML renders correctly in browser with correct legend, coloring, and detail panel
- Single-term equivalence: research node set = query node set (exact match)
- Performance: end-to-end time < 2s on 10K-node graph
- `--terms-file` with 10 lines produces a view with all 10 terms in `rvTerms`

**FAIL boundaries**:
- If `cabal build` produces any warnings, the test fails (strict build policy)
- If any existing test regresses, the test fails (no acceptable regressions)
- If legacy `graph.json` crashes or produces malformed output, the test fails
- If the HTML file does not render in a browser (missing vis-network CDN or JS error), the test fails
- If single-term equivalence does not hold (node sets differ), the test fails
- If end-to-end time exceeds 2s on 10K-node graph, the test fails (performance regression)
- If `--terms-file` does not include all terms from the file, the test fails

### Affected modules

- No code changes in this task — verification only
- All modules from Tasks 1-5 are implicitly tested
- Legacy graph fixture: a minimal `graph.json` without `compositions` field (test fixture)

### Prerequisites

- All Tasks 1-6 (or 1-5 if Task 6 is deferred) must be implemented and passing
- A real `graph.json` with ~10K nodes for performance testing (e.g., Solario Core or this repo's `graph.json`)
- Browser available for manual HTML rendering verification

### Risks

- **Medium**: Performance — if the 10K-node graph test exceeds 2s, optimization may be needed (e.g., caching index lookups, parallelizing queries). This is the highest-risk check in this task.
- **Medium**: Legacy graph compatibility — if the graph loading infrastructure does not gracefully handle missing `compositions`, the research view may crash or produce incorrect output. This requires a specific test fixture.
- **Low**: Manual browser verification is subjective but bounded by the automated Hspec tests in Task 3
