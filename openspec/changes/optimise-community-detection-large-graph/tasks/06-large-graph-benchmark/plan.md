<!--
  PDCA step file for task 6. Lives at tasks/06-large-graph-benchmark/plan.md.
  Scope: Run full pipeline on large codebase with profiling to validate optimizations.
  No code yet. Check Criteria defined BEFORE benchmark execution.
-->

# Task 6 — 78K-Node Benchmark — PLAN

**Task slug**: `06-large-graph-benchmark`
**Attempt**: 1
**Status**: pending

## Summary

Run the full pipeline on the Graphos repository (largest available fixture) with RTS profiling to validate that all optimizations from tasks 1-5 achieve the PRD §16.1 target: cluster step under 30 seconds, peak memory under 1.5GB, correct community output.

## Detail

### Scope

- **No code changes** — this is a verification/benchmark task
- **Run**: `cabal run graphos -- . -o graphos-out --update` on the Graphos repository
- **Profile**: RTS `-s` (summary) and/or `-p -h` (eventlog/heap profile) to capture:
  - `graphos_cluster_duration_seconds` / `span_cluster` duration
  - Peak memory during clustering
  - GC stats (allocations, pauses)
- **Verify**: `graphos-out/graph.json` contents:
  - All nodes have non-null `community_id`
  - `community_aggregates` length within ±5% of expected
  - `nodes`, `edges`, `communities` counts unchanged from pre-optimization

### Check Criteria

**Tests/gates to run:**
1. `cabal build` — must exit 0 under `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`
2. `cabal test` — must exit 0 with all existing examples passing (347 examples expected, zero failures)
3. `cabal run graphos -- . -o graphos-out --update` on Graphos repo — must exit 0, produce:
   - `graphos-out/graph.json` with valid JSON
   - `graphos-out/GRAPH_REPORT.md` generated
   - `graphos-out/community_graph.json` generated
4. JSON validation:
   - `graph.json` `nodes[].community_id` non-null for all nodes
   - `graph.json` `community_aggregates` non-empty array
   - Node count, edge count match pre-optimization
5. Telemetry: `graphos_cluster_step5_duration_seconds` histogram recorded

**Spec scenarios satisfied:**
- `leiden-scalability` — **Scenario: Clustering results are unchanged by the optimization** (existing deterministic fixtures pass via `cabal test`)
- `leiden-scalability` — **Scenario: Repository-scale clustering does not regress** (WHEN pipeline runs on Graphos repo with debug tracing, THEN span_cluster duration at most pre-change baseline)
- `leiden-scalability` — **Scenario: Large graphs cluster within target order of magnitude** (WHEN sparse synthetic graph of 50,000+ nodes is clustered, THEN clustering completes in seconds, consistent with PRD §16.1 target of under 30s at 100K nodes)
- `pipeline-clustering` — **Scenario: Step 5 produces non-empty communities** (WHEN pipeline runs on graph, THEN finalComm has communities and analysisGodNodes non-empty)
- `pipeline-clustering` — **Scenario: Step 5 does not substitute empties** (WHEN pipeline runs on non-empty graph, THEN finalComm is not Map.empty)
- `pipeline-clustering` — **Scenario: Aggregates reflect the community map** (WHEN pipeline produces 8,519 communities, THEN community_aggregates has ~8,519 entries)
- `pipeline-clustering` — **Scenario: Written nodes have community_id** (WHEN pipeline writes graph.json, THEN every node has non-null community_id)

**PASS conditions:**
- `cabal test` exits 0 with all examples passing (no regression from tasks 1-5)
- Pipeline run on Graphos repo exits 0
- `graph.json` has non-null `community_id` on all nodes
- `community_aggregates` non-empty array with entry count within ±5% of expected
- `span_cluster` / `graphos_cluster_step5_duration_seconds` measured and recorded
- `nodes` and `edges` counts unchanged from pre-optimization
- `GRAPH_REPORT.md` generated successfully

**FAIL conditions:**
- `cabal test` fails — any regression from tasks 1-5
- Pipeline run exits non-zero — runtime error in the optimized code
- `community_id` is null on any node — clustering not producing valid output
- `community_aggregates` is empty — aggregates computation not wired correctly
- `span_cluster` duration significantly worse than pre-optimization baseline — optimization regression

### Affected modules

- None (no code changes — verification only)
- Output files in `graphos-out/` (graph.json, GRAPH_REPORT.md, community_graph.json)

### Prerequisites

- Tasks 1-5 must be complete and passing (`cabal test` exits 0)
- Access to a large codebase for benchmarking (Graphos repo itself, or solario corpus if available)
- GHC RTS profiling support (enabled by default in GHC, no additional setup needed)

### Risks

- **Solario 78K unavailable**: The solario corpus may not be in the execution environment. The Graphos repo itself (~7,909 nodes) is the largest available fixture. The benchmark validates optimization direction and correctness but not the 78K target specifically.
- **Timing variability**: CI/different environments have different CPU performance. Absolute timing numbers are not portable. The PASS condition uses "no regression vs pre-optimization" not absolute thresholds.
- **RTS profiling overhead**: Enabling RTS profiling (`+RTS -s`) adds 2-5× overhead. For accurate measurement, run twice: once with profiling (`-s`) for GC stats, once without for actual runtime duration.

## Result

pending — awaiting Do phase.
