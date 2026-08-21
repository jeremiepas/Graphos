# Task 5 — app/Main.hs dispatch + output — PLAN

**Task slug**: `05-app-main-dispatch-output`
**Attempt**: 1
**Status**: pending

## Summary

Wire up the `research` subcommand in `app/Main.hs`: dispatch to `buildResearchView`, write `ResearchView` JSON to stdout (or file via `--json-file`), write `renderResearchHtml` output to `graphos-out/research-<label>.html`, and handle the output path logic.

## Detail

### Scope

- **Extend**: `app/Main.hs`
  - Add case for `Research cmd` in the main dispatch
  - Load `graph.json` from the path specified by `--graph` (default `graphos-out/graph.json`)
  - Build `GraphIndex` from the loaded graph (reuse existing index loading logic)
  - Load community compositions from `graph.json` (handle absence for legacy graphs)
  - Call `buildResearchView` with terms, seeds (`--subgraph`), `RefineConfig` from `--edges`/`--budget`
  - If `--json`: emit `ResearchView` as a single JSON document on stdout (no interleaved logs)
  - If `--html` (default): write `renderResearchHtml` to `graphos-out/research-<label>.html`
  - If both `--json` and `--html`: write HTML to file and JSON to stdout
  - If `--json-file` is passed: write JSON to file instead of stdout
- **Extend**: `app/Main.hs` output path logic
  - Default output dir: `graphos-out/` (respect `--graph` parent dir if graph is elsewhere)
  - Default filename: `research-<label-or-timestamp>.html` and `.json`
  - `--output <path>` flag overrides the full path

### Check Criteria

**Tests to run**:
- `cabal test` — all Hspec cases in `test/Graphos/CLI/ParserSpec.hs` (integration tests for research dispatch)
- `cabal build --flag dev` with `-Wall -Werror` — clean, no warnings
- Manual verification: `graphos research phase work --json` against a known `graph.json` returns valid JSON
- Manual verification: `graphos research phase work --html --label test` writes `graphos-out/research-test.html`

**Spec scenarios satisfied**:
- `Scenario: research returns union of multiple queries` (end-to-end: CLI dispatch + buildResearchView + JSON output)
- `Scenario: single-term equivalence with query` (end-to-end: compare `research` node set vs `query` node set)
- `Scenario: research produces self-contained HTML` (end-to-end: HTML file exists and renders)
- `Scenario: research on terms with no matches` (end-to-end: valid empty result)
- `Scenario: terms-file appends terms` (end-to-end: parser + dispatch integration)

**PASS conditions**:
- `graphos research phase work --json` against `graphos-out/graph.json` returns valid `ResearchView` JSON on stdout
- JSON output is a single document with no interleaved log lines
- `graphos research phase work --html --label test` writes `graphos-out/research-test.html`
- The HTML file opens in a browser (file:// protocol) and renders
- Single-term equivalence: `graphos research phase --json` node set equals `graphos query phase --json` node set
- `--output <path>` overrides the full output path
- Default filename contains the label or a timestamp
- JSON to stdout + HTML to file works when both flags are set
- JSON to file works when `--json-file` is passed
- Existing commands (`query`, `serve`, etc.) are unaffected (no regression)

**FAIL boundaries**:
- If JSON output contains interleaved log lines, the test fails (violates spec: "SHELL NOT emit interleaved log lines on stdout in JSON mode")
- If the HTML file is not written to the expected path, the test fails
- If `research` crashes on a graph with no matches for any term, the test fails
- If `--output` does not override the output path, the test fails
- If the dispatched output differs from `buildResearchView` output (e.g., JSON encoding order), the test fails

### Affected modules

- **Extended**: `app/Main.hs`
- **New integration tests**: `test/Graphos/CLI/ParserSpec.hs` or `test/Integration/` (if integration tests exist)
- **Imports from**: `src/Graphos/Domain/Query/Research.hs` (ResearchView, ToJSON), `src/Graphos/UseCase/Query/Research.hs` (buildResearchView), `src/Graphos/Infrastructure/Export/HTML.hs` (renderResearchHtml), `src/Graphos/Infrastructure/FileSystem/` (graph loading, file writing)

### Prerequisites

- Task 1 (Domain types) must be implemented first
- Task 2 (UseCase: buildResearchView) must be implemented first
- Task 3 (HTML rendering: renderResearchHtml) must be implemented first
- Task 4 (CLI parser: research subcommand) must be implemented first
- Existing graph loading infrastructure (loadGraphJSON, buildGraphIndex) must be available
- Existing community composition loading must be available

### Risks

- **Medium**: File I/O in Main.hs must not interfere with JSON stdout output (logs must go to stderr, not stdout)
- **Medium**: Graph loading may need to handle the case where `compositions` is absent (legacy graph) — the `ResearchView` must still be buildable
- **Low**: The `--graph` parent directory logic for output paths is a minor edge case
