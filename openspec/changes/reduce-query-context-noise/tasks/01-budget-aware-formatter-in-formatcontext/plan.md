<!--
  PDCA step: PLAN for Task 1 of reduce-query-context-noise.
  Scope: Add budget-aware truncation to FormatContext.
-->

# Task 1 — Budget-aware formatter in FormatContext — PLAN

**Task slug**: `01-budget-aware-formatter-in-formatcontext`
**Attempt**: 1
**Status**: pending

## Summary

Add a budget-aware variant of `formatContextForLLM` that greedily renders nodes then edges
in relevance rank order, stops when the next item would exceed the budget (computed via
`countContextTokens`), and emits a trailing `- _omitted: N nodes, M edges_` footer.

## Detail

### Scope

- **New function**: `formatContextForLLMBudgeted :: Int -> SelectedContext -> Text`
  in `src/Graphos/UseCase/FormatContext.hs`.
- **Existing function**: `formatContextForLLM` remains untouched as the unbounded path.
- **Reused**: `countContextTokens` (existing), section formatters already in FormatContext.

### Check Criteria (defined BEFORE code)

| Criterion | What | Spec scenarios |
|-----------|------|----------------|
| C1 | `cabal build` clean with `-Werror` | — |
| C2 | `token_estimate ≤ budget` for a 3000-token budget on a 9000-token untruncated context | `budget-enforcement.over-budget-truncated` |
| C3 | Highest-ranked node is present after truncation | `budget-enforcement.top-node-preserved` |
| C4 | Footer reports omitted node/edge counts | `budget-enforcement.over-budget-truncated` |
| C5 | `token_estimate` uses `countContextTokens`, not `T.length` | `budget-enforcement.token-estimate-matches-budget-units` |

**PASS conditions**:
- C1: `cabal build --flag dev` exits 0 with no warnings.
- C2–C4: Hspec assertions in `FormatContextBudgetSpec.hs` pass for a 3000-budget fixture.
- C5: Assertion that `countContextTokens rendered == token_estimate`.

**FAIL boundaries** (not just "doesn't work"):
- FAIL if `token_estimate` exceeds budget by more than 0 (hard cap is strict).
- FAIL if the top-ranked node is absent from the truncated output.
- FAIL if the footer is missing or reports incorrect omitted counts.
- FAIL if `cabal build` produces any warning under `-Wall -Werror`.

### Affected modules

| Module | Change |
|--------|--------|
| `src/Graphos/UseCase/FormatContext.hs` | Add `formatContextForLLMBudgeted` |

### Prerequisites

- Existing `countContextTokens` function available in `FormatContext`.
- Existing section formatters (`formatNodes`, `formatEdges`, etc.) usable.
- `SelectedContext` type with `scMatchScore` for relevance ranking.

### Risks

- **Greedy truncation may drop a useful second node** → Accepted; tuned in a follow-up cycle via the node-vs-edge budget split.
- **Token heuristic underestimates real tokens** → Accepted; truncate at heuristic budget so real tokens ≤ heuristic ≤ requested budget.

### Dependency graph

- **No dependencies on other tasks** (foundation-level task).
- **Consumed by**: Task 5 (MCP handler wiring).
