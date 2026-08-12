<!--
  PDCA step: PLAN for Task 5 of reduce-query-context-noise.
  Scope: Wire MCP handlers to the new formatter and scored query path.
-->

# Task 5 — Wire MCP handlers to the new formatter and scored query path — PLAN

**Task slug**: `05-wire-mcp-handlers`
**Attempt**: 1
**Status**: pending

## Summary

Update `handleSelectContext` and `handleQueryGraph` in `src/Graphos/Infrastructure/Server/MCP.hs`
to use the new budget-aware formatter, scored query path, and `--edges`/`--max-hint-community-size`
args. Replace the triple `queryGraph` call with a single `queryGraphWithIndexScored` invocation.

## Detail

### Scope

- **Modified handlers**: `handleSelectContext`, `handleQueryGraph` in `src/Graphos/Infrastructure/Server/MCP.hs`.
- **Added args**: `edgeModeArg` (for `--edges`), `maxHintCommunitySizeArg` (for `--max-hint-community-size`).
- **Removed**: Triple `queryGraph` call (`MCP.hs:130-132`) → single `queryGraphWithIndexScored`.
- **Updated**: `allTools` schema descriptions to reflect new arguments.

### Check Criteria (defined BEFORE code)

| Criterion | What | Spec scenarios |
|-----------|------|----------------|
| C1 | `cabal build` clean with `-Werror` | — |
| C2 | A 3000-budget `select_context` response has `token_estimate ≤ 3000` | `budget-enforcement.over-budget-truncated` |
| C3 | `query_graph` response has `verdict`, `hash`, and `omitted` fields | `noise-control.strong-match-returns-ranked-nodes-and-verdict` |
| C4 | `queryGraphWithIndexScored` invoked exactly once per request | `noise-control.query-path-invoked-exactly-once` |
| C5 | `--edges all` restores full edge set in `select_context` | `noise-control.all-edges-mode-preserves-everything` |

**PASS conditions**:
- C1: `cabal build --flag dev` exits 0 with no warnings.
- C2: `MCPQuerySpec.hs` asserts `token_estimate ≤ 3000` for a 3000-budget `select_context` call.
- C3: Same spec asserts `verdict`, `hash`, `omitted` present in `query_graph` response.
- C4: Counting wrapper or assertion on call count confirms single invocation.
- C5: `MCPQuerySpec.hs` asserts `AMBIGUOUS` and trivia edges present when `--edges all`.

**FAIL boundaries** (not just "doesn't work"):
- FAIL if `token_estimate` exceeds the requested budget.
- FAIL if `query_graph` response is missing `verdict` or `omitted` fields.
- FAIL if `queryGraphWithIndexScored` is called more than once per request.
- FAIL if `--edges all` still filters out `AMBIGUOUS` edges.

### Affected modules

| Module | Change |
|--------|--------|
| `src/Graphos/Infrastructure/Server/MCP.hs` | Update both handlers; add args; replace triple call |

### Prerequisites

- Tasks 1–4 complete: budget-aware formatter, ranked edges/hints, god-node removal,
  scored JSON shape must all compile.
- `handleSelectContext` and `handleQueryGraph` signatures must accept new args.

### Risks

- **BREAKING `query_graph` shape** → Accepted: only known client is this opencode instance.
- **Arg threading complexity** → Mitigation: derive arg parsers from optparse-applicative
  patterns already in the codebase.
- **MCP handler tests need mock server** → Mitigation: unit-test the handler logic
  separately from the stdio transport; new `MCPQuerySpec.hs`.

### Dependency graph

- **Depends on**: Tasks 1, 2, 3, 4 (all upstream changes must compile).
- **Consumed by**: Task 6 (regression fixture tests the full MCP path end-to-end).
