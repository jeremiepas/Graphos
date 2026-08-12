<!--
  PDCA step: PLAN for Task 4 of reduce-query-context-noise.
  Scope: Scored JSON shape for query_graph — QueryResponse ToJSON + omitted field.
-->

# Task 4 — Scored JSON shape for query_graph (Query.hs + Score) — PLAN

**Task slug**: `04-scored-json-shape-query-graph`
**Attempt**: 1
**Status**: pending

## Summary

Ensure `QueryResponse` in `src/Graphos/Domain/Graph/Score.hs` has a `ToJSON` instance
producing `{verdict, best_score, hash, nodes, edges}` and add an `omitted` wrapper so the
MCP handler can emit `{verdict, best_score, hash, nodes, edges, omitted}`. Confirm
`queryGraphWithIndexScored` is the single source of the scored query result.

## Detail

### Scope

- **Modified type**: `QueryResponse` in `src/Graphos/Domain/Graph/Score.hs` — add an
  `omitted` record field (of type `QueryOmitted` or similar) to the type definition.
- **Modified instance**: `ToJSON QueryResponse` — add `omitted` to the JSON object.
- **No change to**: `QueryResponse` pure computation logic — `omitted` is MCP-specific and
  will be computed by the handler, not stored in the pure type.
- **Kept as single source**: `queryGraphWithIndexScored` — no new query path.

### Check Criteria (defined BEFORE code)

| Criterion | What | Spec scenarios |
|-----------|------|----------------|
| C1 | `cabal build` clean with `-Werror` | — |
| C2 | `toJSON` of a `QueryResponse` contains `verdict`, `best_score`, `hash`, `nodes`, `edges` | `noise-control.strong-match-returns-ranked-nodes-and-verdict` |
| C3 | `none`-verdict response yields empty `nodes` and `edges` | `noise-control.no-match-returns-empty-node-set-with-verdict` |
| C4 | `omitted` field present in JSON output | `noise-control.no-match-returns-empty-node-set-with-verdict` |

**PASS conditions**:
- C1: `cabal build --flag dev` exits 0 with no warnings.
- C2: `ScoreSpec.hs` asserts all five core fields exist in `toJSON` output for a `strong`
  verdict response.
- C3: Same spec asserts `nodes` and `edges` are empty arrays for a `none` verdict.
- C4: `ScoreSpec.hs` asserts `omitted` field exists with `nodes: 0, edges: 0` for `none`.

**FAIL boundaries** (not just "doesn't work"):
- FAIL if `verdict` is missing from the JSON output.
- FAIL if `best_score` is not a JSON number (should be `Number`, not `String`).
- FAIL if `none` verdict has non-empty `nodes` or `edges`.
- FAIL if `omitted` is missing from the JSON output.

### Affected modules

| Module | Change |
|--------|--------|
| `src/Graphos/Domain/Graph/Score.hs` | Add `omitted` field to `QueryResponse`; update `ToJSON` |
| `src/Graphos/UseCase/Query.hs` (maybe) | Re-export if `QueryOmitted` type is defined here |

### Prerequisites

- `QueryResponse` type with fields `verdict`, `bestScore`, `hash`, `nodes`, `edges`
  already exists.
- `ToJSON` instance already exists or needs to be created.
- `QueryOmitted` newtype (or similar) for the omitted counts.

### Risks

- **`omitted` is MCP-specific** → Mitigation: keep the field on `QueryResponse` but compute
  its value in the handler layer, not in the pure domain. The field is harmless in the
  pure layer (defaults to `{nodes: 0, edges: 0}`).
- **Breaking `query_graph` shape** → Accepted: only known client is this opencode instance.

### Dependency graph

- **No dependencies on other tasks** (domain-level change, self-contained).
- **Consumed by**: Task 5 (MCP handler wiring — `handleQueryGraph` uses the scored shape).
