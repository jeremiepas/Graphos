<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — CLI parser: research subcommand — PLAN

**Task slug**: `04-cli-parser-research-subcommand`
**Attempt**: 1
**Status**: pending

## Summary

Add the `research` subcommand and all associated flags to `src/Graphos/CLI/Parser.hs`, including positional `<term>...`, `--subgraph`, `--terms-file`, `--label`, `--html`, `--json`, `--graph`, `--budget`, `--label-width`, `--edges`, and `--output`. Register it as a top-level command sibling to `query`, `serve`, etc.

## Detail

### Scope

- **Extend**: `src/Graphos/CLI/Parser.hs`
  - Add `ResearchOpts` record with all flags:
    - Positional: `<term>...` (one or more required)
    - `--subgraph <term>...` (zero or more)
    - `--terms-file <path>` (optional, reads newline-delimited terms)
    - `--label <text>` (optional, titles the output HTML; defaults to timestamp `research-YYYYMMDD-HHMMSS`)
    - `--html` (switch, default on)
    - `--json` (switch)
    - `--graph <path>` (optional, override graph path)
    - `--budget <n>` (optional, query budget)
    - `--label-width <n>` (optional, label width for visualization)
    - `--edges semantic|all` (optional, edge refinement mode)
    - `--output <path>` (optional, override full output path)
  - `--terms-file` reads newline-delimited terms; terms are appended to positional terms (dedup, preserve order)
  - `--label` titles the output HTML and is used in the output filename
  - Register `research` as a new top-level command via the existing `command` combinator pattern
- **Extend**: `src/Graphos/CLI/Types.hs` (if needed) — add `Research` constructor to the CLI command type
- **Hspec module**: `test/Graphos/CLI/ParserSpec.hs` (new test cases for research subcommand)

### Check Criteria

**Tests to run**:
- `cabal test` — all Hspec cases in `test/Graphos/CLI/ParserSpec.hs` (new research parser tests)
- `cabal build --flag dev` with `-Wall -Werror` — clean, no warnings

**Spec scenarios satisfied**:
- `Scenario: terms-file appends terms` (spec § "terms-file appends terms") — parser-level validation

**PASS conditions**:
- Parser accepts `research phase work block` (three positional terms)
- `--help` lists all flags for the research subcommand
- `--terms-file` with nonexistent path errors clearly (error message, not crash)
- `--subgraph` with no positional terms errors (at least one term required)
- Invalid `--edges` value errors (only `semantic` or `all` accepted)
- `--terms-file` appends terms from file to positional terms, deduplicates, preserves order
- `--output` overrides the full output path
- Parser rejects `research` with zero positional terms
- `--html` defaults to on (no flag needed)
- `--json` is a switch (off by default)
- Existing subcommands (`query`, `serve`, etc.) are unchanged (no regression in parser)

**FAIL boundaries**:
- If `research` with zero terms parses successfully (should require at least one), the test fails
- If `--edges invalid` parses without error, the test fails
- If `--terms-file` with a nonexistent file crashes instead of returning a clear error, the test fails
- If the `research` command is not registered as a top-level command, the test fails
- If existing commands' parser behavior changes (e.g., `query` flags are different), the test fails

### Affected modules

- **Extended**: `src/Graphos/CLI/Parser.hs`
- **Extended**: `src/Graphos/CLI/Types.hs` (if `Research` command constructor is needed)
- **New tests**: `test/Graphos/CLI/ParserSpec.hs` (append research parser test cases)
- **Imports from**: Existing CLI modules; no domain/usecase imports (parser is infrastructure-adjacent, pure)

### Prerequisites

- Existing `CLI.Parser` module must exist with the established subcommand registration pattern
- Existing CLI types (`CLIOptions` or similar) must exist in `CLI.Types`
- `--edges` flag must already be defined (from `query-noise-control` or existing codebase) — if not, it's a prerequisite from that change

### Risks

- **Low**: Parser additions are additive (no breaking changes); the main risk is matching the existing command registration pattern
- **Medium**: If `--terms-file` needs to read a file at parse time, it introduces IO into the parser layer — consider deferring file reading to `Main.hs` or using a pure parse + lazy read pattern
- **Low**: The `--label` default to timestamp is straightforward; the format `research-YYYYMMDD-HHMMSS` needs `Data.Time` imports
