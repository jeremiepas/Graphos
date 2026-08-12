<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
              A passed task reaches the same done state as in classic SDD.
  RETRY rule: if Act is NOT OK, the task does NOT pass — record the failed
              attempt under "### Attempt history (N)" (KEEP THE TRACE, never
              delete it), then start a NEW P → D → C → A attempt for the same
              task. Repeat until an attempt passes.

  Everything else matches the official spec-driven workflow.
-->

# Task 5 — Add in-place graph edge enrichment — PLAN

**Task slug**: `05-add-in-place-graph-edge-enrichment`
**Attempt**: 1
**Status**: pending

## Summary

Add `addEdges :: Graph -> [Edge] -> Graph` to `Domain.Graph.Core` and replace `buildGraphFromExtractions` in the pipeline's edge inference with `addEdges graph inferredEdges`, eliminating the 3× memory duplication window during enrichment.

## Detail

### Scope

This task modifies:
- `src/Graphos/Domain/Graph/Core.hs` — add `addEdges :: Graph -> [Edge] -> Graph` that inserts edges into existing Maps (`gEdges`, `gAdjFwd`, `gAdjBack`) without creating a new Graph
- `src/Graphos/UseCase/Pipeline.hs` — replace `buildGraphFromExtractions` call in the inference stage with `addEdges graph inferredEdges`

The `addEdges` function for each edge:
1. Insert into `gEdges` (Map EdgeId Edge)
2. Update `gAdjFwd` (Map NodeId (Set NodeId)) — add source→target adjacency
3. Update `gAdjBack` (Map NodeId (Set NodeId)) — add target→source adjacency

This is a pure Domain function (no IO), consistent with Clean Architecture rules.

### Check Criteria

**Spec scenarios satisfied:**

| Scenario ID | Spec File | Description |
|---|---|---|
| `graph-enrichment/scen:adding-inferred-edges` | `specs/graph-enrichment/spec.md` | `addEdges graph inferredEdges` adds edges to `gEdges`, `gAdjFwd`, `gAdjBack`; original Graph eligible for GC |
| `graph-enrichment/scen:no-intermediate-extraction` | `specs/graph-enrichment/spec.md` | No `Extraction` value created during enrichment; only inferred edges passed to `addEdges` |
| `graph-enrichment/scen:backward-compatibility` | `specs/graph-enrichment/spec.md` | `addEdges` on empty edge list returns identical Graph |

**Specific tests/gates:**

1. **Unit tests** for `addEdges` in `tests/Graphos/Domain/Graph/CoreSpec.hs`:
   - Empty edge list → returns identical Graph (backward compatibility)
   - Single edge → graph has 1 edge, forward/backward adjacencies updated
   - Multiple edges → all edges present, adjacencies correct
   - Duplicate edge (existing EdgeId) → edge updated, no duplicates
   - Dangling edge (source/target not in node set) → edge still inserted (graph stores edges regardless)
2. **Build gate**: `cabal test` passes with exit code 0.
3. **Integration test**: Run full pipeline — enriched graph has same node count as original, edge count = original + inferred edges.

**PASS conditions:**
- All `addEdges` unit tests pass
- `cabal test` returns exit code 0
- Full pipeline produces enriched graph with correct node count and edge count
- No `Extraction` value exists between the original Graph and the enriched Graph (verify via code inspection)

**FAIL boundaries:**
- If `addEdges` fails to update `gAdjFwd`/`gAdjBack` for any edge type, adjacency queries will return incorrect results — must identify edge types that are missed
- If the enriched graph node count differs from original, nodes are being lost or duplicated
- If `addEdges` creates a new Graph instead of returning one with updated Maps, the memory benefit is reduced but not eliminated (old Maps GC'd when unreferenced in Haskell)

### Affected Modules

- `src/Graphos/Domain/Graph/Core.hs` — new `addEdges` function
- `src/Graphos/UseCase/Pipeline.hs` — edge inference stage (replace `buildGraphFromExtractions`)
- `tests/Graphos/Domain/Graph/CoreSpec.hs` — new `addEdges` unit tests

### Prerequisites

- `Graph` type has fields `gEdges :: Map EdgeId Edge`, `gAdjFwd :: Map NodeId (Set NodeId)`, `gAdjBack :: Map NodeId (Set NodeId)`
- `inferEdges :: Graph -> [NodeId] -> [Edge]` produces edges that need to be added
- Existing tests cover pipeline correctness (node count, edge count)

### Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| `addEdges` must correctly update both forward and backward adjacencies | Incorrect graph structure | Unit tests for both directed and undirected edges |
| Dangling edges (target not in graph) may cause issues | Runtime errors in adjacency queries | Decide: reject dangling edges or allow them |
| `addEdges` on large edge lists may be slow | Performance regression | Consider `Map.union` batch instead of individual `Map.insert` if needed |

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
