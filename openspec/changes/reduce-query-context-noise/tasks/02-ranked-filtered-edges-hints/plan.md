<!--
  PDCA step: PLAN for Task 2 of reduce-query-context-noise.
  Scope: Relevance-ranked, confidence-filtered edges and bounded expansion hints.
-->

# Task 2 — Relevance-ranked, confidence-filtered, bounded edges and hints — PLAN

**Task slug**: `02-ranked-filtered-edges-hints`
**Attempt**: 1
**Status**: pending

## Summary

Modify `formatKeyEdges` to sort edges by endpoint relevance (descending), honor an
`--edges semantic|all` flag (default `semantic` drops `AMBIGUOUS`-confidence and
trivia-target `contains` edges), and cap `formatExpansionHints` at top N (default 8)
by relevance, filtering mega-communities and the chat community.

## Detail

### Scope

- **Modified function**: `formatKeyEdges` → add `filterAndRankEdges` helper and
  `formatKeyEdgesFiltered` variant.
- **Modified function**: `formatExpansionHints` → add `formatExpansionHintsBudgeted` variant.
- **Exports**: `filterAndRankEdges` exported for testability.

### Check Criteria (defined BEFORE code)

| Criterion | What | Spec scenarios |
|-----------|------|----------------|
| C1 | `cabal build` clean with `-Werror` | — |
| C2 | `AMBIGUOUS` edges absent in default mode | `noise-control.trivia-edges-dropped-by-default`, `noise-control.ambiguous-edges-dropped-by-default` |
| C3 | `AMBIGUOUS` edges present with `--edges all` | `noise-control.all-edges-mode-preserves-everything` |
| C4 | Edges ordered by relevance (descending) | `noise-control.edges-are-relevance-ranked` |
| C5 | 2563-node community hidden at default max-hint-community-size=50 | `budget-enforcement.mega-community-hidden` |
| C6 | Chat community never appears in hints | `budget-enforcement.chat-community-never-suggested` |
| C7 | Empty hints section omitted entirely | `budget-enforcement.empty-hints-section-omitted` |

**PASS conditions**:
- C1: `cabal build --flag dev` exits 0 with no warnings.
- C2–C4: New `FormatContextHintsSpec.hs` assertions pass for edge filtering tests.
- C5–C7: `FormatContextHintsSpec.hs` assertions pass for expansion-hints filtering tests.

**FAIL boundaries** (not just "doesn't work"):
- FAIL if `AMBIGUOUS` edges appear in default-mode output.
- FAIL if trivia-target `contains` edges (e.g., targeting `undefined`) appear in default mode.
- FAIL if edge order is not descending by endpoint relevance score.
- FAIL if a community > 50 nodes appears in expansion hints.
- FAIL if `chatCommunityId` appears in hints under any condition.
- FAIL if hints section renders with zero entries.

### Affected modules

| Module | Change |
|--------|--------|
| `src/Graphos/UseCase/FormatContext.hs` | Add `filterAndRankEdges`, `formatKeyEdgesFiltered`, `formatExpansionHintsBudgeted` |
| `src/Graphos/Domain/Graph/Score.hs` (maybe) | Re-export or reference trivia-token list if needed |

### Prerequisites

- Task 1 complete (budget-aware formatter provides the context structure).
- Trivia-token list already defined in `Domain.Context` or `Domain.Graph.Score` — reuse, do not redefine.
- `Confidence` type with `AMBIGUOUS` constructor accessible.

### Risks

- **Trivia-token list duplicates the CLI's** → Mitigation: import from `Domain.Context`, not local definition.
- **Relevance ranking for edges uses endpoint scores** → Accepted; endpoints' rank is a good proxy, no need for a new edge-scoring function.

### Dependency graph

- **Depends on**: Task 1 (budget-aware formatter structure).
- **Consumed by**: Task 5 (MCP handler wiring).
