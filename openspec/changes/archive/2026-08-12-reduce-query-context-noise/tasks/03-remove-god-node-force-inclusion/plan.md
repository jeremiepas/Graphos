<!--
  PDCA step: PLAN for Task 3 of reduce-query-context-noise.
  Scope: Remove unconditional god-node force-inclusion in SelectContext strategies.
-->

# Task 3 — Remove unconditional god-node force-inclusion in SelectContext — PLAN

**Task slug**: `03-remove-god-node-force-inclusion`
**Attempt**: 1
**Status**: pending

## Summary

Remove the `Set.fromList (map gnId (take 5 (analysisGodNodes analysis)))` union from
`selectCommunityAware`, `selectRelevanceWeighted`, and `selectPathBased`. Populate
`scGodNodes` only from god nodes already in `nodesInBudget` (computed after selection).
Leave `selectArchitectural` unchanged.

## Detail

### Scope

- **Modified functions**: `selectCommunityAware`, `selectRelevanceWeighted`, `selectPathBased`
  in `src/Graphos/UseCase/SelectContext.hs`.
- **Kept but repurposed**: `scGodNodes` field — now populated from selected set ∩ god-node
  ids, not from `take 5 (analysisGodNodes analysis)`.
- **Unchanged**: `selectArchitectural` (purpose is hub/bridge overview).

### Check Criteria (defined BEFORE code)

| Criterion | What | Spec scenarios |
|-----------|------|----------------|
| C1 | `cabal build` clean with `-Werror` | — |
| C2 | Unrelated god node `Main` absent from Focused Parser query | `budget-enforcement.unrelated-god-node-absent` |
| C3 | Relevant god node still included when query-matched | `budget-enforcement.relevant-god-node-included` |
| C4 | `selectArchitectural` still includes god nodes | Design decision — no spec scenario |

**PASS conditions**:
- C1: `cabal build --flag dev` exits 0 with no warnings.
- C2: `SelectContextNoiseSpec.hs` asserts `Main` is absent from `scNodes` and no
  `### Hub Nodes` section is rendered for a Focused Parser query where `Main` is in an
  unrelated community.
- C3: Same spec asserts that when a query matches a node that is also a god node, it
  appears by virtue of its match score.
- C4: `selectArchitectural` still produces a non-empty `scGodNodes` for architectural queries.

**FAIL boundaries** (not just "doesn't work"):
- FAIL if `Main` (or any god node) appears in `scNodes` when it has `matchScore == 0`.
- FAIL if the `### Hub Nodes` section renders when `scGodNodes` is empty.
- FAIL if `selectArchitectural` stops including god nodes.

### Affected modules

| Module | Change |
|--------|--------|
| `src/Graphos/UseCase/SelectContext.hs` | Remove forced god-node unions in 3 strategies; repurpose `scGodNodes` |

### Prerequisites

- `analysisGodNodes` function available from the `Analysis` record.
- `scGodNodes :: Set NodeId` field exists on `SelectedContext`.
- `matchScore > 0` predicate available for relevance filtering.

### Risks

- **Removing the field entirely would lose hub annotation** → Mitigation: keep the field,
  populate from selected set intersection with god nodes.
- **`selectArchitectural` still force-includes hubs** → Accepted: that strategy's purpose
  is architectural overview.

### Dependency graph

- **No hard dependencies on other tasks** (independent of formatter changes).
- **Interacts with**: Task 1 (formatter's `### Hub Nodes` rendering — must omit section
  when `scGodNodes` is empty, already handled by Task 1/2 footer logic).
