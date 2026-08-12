<!--
  PDCA step: PLAN for Task 6 of reduce-query-context-noise.
  Scope: Regression fixture + PRD §16.1 latency guard.
-->

# Task 6 — Regression fixture + PRD §16.1 latency guard — PLAN

**Task slug**: `06-regression-fixture-latency-guard`
**Attempt**: 1
**Status**: pending

## Summary

Add a regression test that loads `graphos-out/graph.json`, runs the same 3000-token
`select_context` query that today returns 9213 tokens, and asserts `token_estimate ≤ 3000`
with the top-ranked node present. Also assert query latency < 500 ms (PRD §16.1).

## Detail

### Scope

- **New test module**: `tests/Graphos/Regression/ContextNoiseRegressionSpec.hs`.
- **Fixture**: Load `graphos-out/graph.json` from the project workspace.
- **Skip gracefully**: If the fixture file is absent, skip the test (do not fail the suite).
- **Metrics**: Measure `token_estimate` and wall-clock latency for the `select_context` path.

### Check Criteria (defined BEFORE code)

| Criterion | What | Spec scenarios |
|-----------|------|----------------|
| C1 | `cabal build` clean with `-Werror` | — |
| C2 | `token_estimate ≤ 3000` on the repo fixture (9213 → ≤3000) | `budget-enforcement.over-budget-truncated` |
| C3 | Top-ranked node present after truncation | `budget-enforcement.top-node-preserved` |
| C4 | Latency < 500 ms on the repo's `graph.json` | PRD §16.1 |
| C5 | `cabal test` green end-to-end | Verification Strategy §2, §3 |

**PASS conditions**:
- C1: `cabal build --flag dev` exits 0 with no warnings.
- C2: `token_estimate` measured on the truncated output for the 3000-budget query is ≤ 3000.
- C3: The highest-ranked node from the pre-truncation set appears in the rendered output.
- C4: Wall-clock time for the full `select_context` call (including traversal + formatting)
  is < 500 ms on the repo's `graph.json`.
- C5: `cabal test` exits 0 with no failures, including the new regression test.

**FAIL boundaries** (not just "doesn't work"):
- FAIL if `token_estimate` exceeds 3000 for the 3000-budget query on the repo fixture.
- FAIL if the top-ranked node is missing from the truncated output.
- FAIL if latency exceeds 500 ms (regression guard against performance regressions).
- FAIL if the regression test crashes when `graphos-out/graph.json` is absent (must skip
  gracefully using a guard, not let the exception propagate).

### Affected modules

| Module | Change |
|--------|--------|
| `tests/Graphos/Regression/ContextNoiseRegressionSpec.hs` | New file |

### Prerequisites

- Tasks 1–5 complete: the full MCP path must compile and be functional.
- `graphos-out/graph.json` exists in the workspace (or test skips gracefully).
- `countContextTokens` accessible in the test module.

### Risks

- **Fixture depends on `graphos-out/graph.json` existing** → Mitigation: check file
  presence at test startup; if absent, call `hspec`'s `it "skips - no fixture" $ skip`
  and do not fail the suite.
- **Latency variance across CI environments** → Mitigation: use a generous 500 ms guard;
  the current path has no new traversals so regression risk is low.
- **Token-reduction ratio feeds back into PRD §7.2** → Action: record measured ratio
  in the Act step if Check passes.

### Dependency graph

- **Depends on**: Tasks 1–5 (full MCP path must be functional).
- **No downstream consumers** (final integration test).
