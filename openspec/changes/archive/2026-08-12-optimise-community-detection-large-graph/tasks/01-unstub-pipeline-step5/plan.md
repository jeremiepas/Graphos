<!--
  PDCA step file for task 1. Lives at tasks/01-unstub-pipeline-step5/plan.md.
  Scope: Unstub Pipeline.hs Step 5 (lines 263-264, 296).
  No code yet. Check Criteria defined BEFORE implementation.
-->

# Task 1 — Unstub Pipeline Step 5 — PLAN

**Task slug**: `01-unstub-pipeline-step5`
**Attempt**: 1
**Status**: pending

## Summary

Replace the stub in `src/Graphos/UseCase/Pipeline.hs` lines 263-264 and 296 with real calls to `clusterGraphWithResolution`, `analyzeGraph`, and `computeCommunityAggregates`, producing non-null `community_id` on nodes and non-empty `community_aggregates` in output.

## Detail

### Scope

- **File**: `src/Graphos/UseCase/Pipeline.hs` only
- **Line 263-264**: Replace `(Map.empty, Map.empty)` with `(finalComm, finalCohes) = clusterGraphWithResolution enrichedGraph' res` and `Analysis Map.empty Map.empty [] [] []` with `analyzeGraph enrichedGraph' finalComm finalCohes`
- **Line 296**: Replace `let aggregates = []` with `computeCommunityAggregates joinedGraph finalComm finalCohes (GAnalysis.articulationPoints enrichedGraph') llmLabels`
- **Timing**: Add `step5Start`/`step5End` span with `graphos_cluster_step5_duration_seconds` histogram for observability
- **Forcing**: Add `evaluate`/`deepseq` at field level for `finalComm`, `finalCohes`, `length (analysisGodNodes anal)`, and aggregate count (no `NFData Analysis`/`NFData CommunityAggregate` instances available)

### Check Criteria

**Tests/gates to run:**
1. `cabal build` — must exit 0 with no warnings under `-Wall -Wcompat -Wincomplete-uni-patterns -Werror` (with `--flag dev`)
2. `cabal test` — must pass all existing examples, exit 0, zero failures
3. `cabal run graphos -- tests/fixtures/small` — must produce `graphos-out/graph.json` with:
   - `nodes[].community_id` non-null for community member nodes
   - `community_aggregates` key present and array non-empty
   - Exit code 0
4. OTLP metric `graphos_cluster_step5_duration_seconds` recorded — verify histogram entry exists in metrics output

**Spec scenarios satisfied:**
- `pipeline-clustering` — **Scenario: Step 5 produces non-empty communities** (WHEN pipeline runs on graph, THEN finalComm has communities and analysisGodNodes non-empty)
- `pipeline-clustering` — **Scenario: Step 5 does not substitute empties** (WHEN pipeline runs on non-empty graph with clustering enabled, THEN finalComm is not Map.empty and anal is not empty Analysis)
- `pipeline-clustering` — **Scenario: Written nodes have community_id** (WHEN pipeline writes graph.json, THEN every node has non-null community_id)
- `pipeline-clustering` — **Scenario: Aggregates are non-empty when clustering is enabled** (WHEN --no-cluster unset on non-empty graph, THEN community_aggregates is present and non-empty)
- `pipeline-clustering` — **Scenario: Write order is preserved** (community_aggregates after god_nodes, nodes[].community_id non-null)
- `leiden-scalability` — **Scenario: Clustering results are unchanged by the optimization** (implicit — existing deterministic fixtures must still pass via `cabal test`)

**PASS conditions:**
- `cabal build` exits 0
- `cabal test` exits 0 with all examples passing
- Small fixture `graph.json` contains `nodes` with non-null `community_id` for community-assigned nodes
- `graph.json` contains non-empty `community_aggregates` array
- `graphos_cluster_step5_duration_seconds` histogram entry exists in telemetry

**FAIL conditions:**
- `cabal build` fails — blocker, implementation has type errors
- `cabal test` fails — semantics broken by wrong function call or wrong argument order
- Small fixture produces `community_id: null` for all nodes — clusterGraphWithResolution not wired
- Small fixture produces `"community_aggregates": []` — computeCommunityAggregates not wired or empty inputs propagated
- `cabal test` skips `leiden-scalability` examples — test framework regression, not task-specific

### Affected modules

- `UseCase.Pipeline` — Step 5 unstubbing, Step 6 aggregate computation, histogram span

### Prerequisites

- Sibling change `refactor-architecture-ports-and-split-god-modules` must be merged (Pipeline.hs must compile)
- `Domain.Community.clusterGraphWithResolution` must exist and be exported
- `Domain.Graph.Analysis.analyzeGraph` must exist and be exported
- `Domain.Community.computeCommunityAggregates` must exist and be exported
- `Domain.Graph.Analysis` must export the articulation points accessor (verify exact field name — `analysisArticulationPoints` or `GAnalysis.articulationPoints`)

### Risks

- **articulation points accessor mismatch**: The exact field name on `Analysis` may be `analysisArticulationPoints` (lens-style) or `GAnalysis.articulationPoints` (record field). Will verify against `Domain.Types.Analysis` before implementation. If wrong, simple field name fix.
- **Performance regression on Step 5**: `analyzeGraph` on 78K enriched graph may be slow. This is expected — addressed by tasks 2-5 (algorithmic optimizations). Task 6 (benchmark) validates the full optimization set.
- **Pre-existing compilation errors**: Sibling uncommitted changes may block `cabal build`. Must coordinate merge order.

## Result

pending — awaiting Do phase.
