# Task: Fix Runtime RAM Crash (Bounded Observability)

## Goal

Fix runtime RAM crashes caused by unbounded memory growth in observability stores, extraction accumulators, and graph enrichment.

## Score: 10.12 (P0) — Fourth highest priority

## Sub-specs (6 total)

| Sub-spec | Description | Est. Effort |
|----------|-------------|-------------|
| `bounded-observability` | Bound OTLP trace buffers, IORef MetricsStore | 2 days |
| `compact-nodes` | Reduce node memory footprint (bitfields, interned strings) | 3 days |
| `extraction` | Batch extraction, bounded LSP concurrency | 2 days |
| `graph-enrichment` | In-place edge enrichment, no diff-list accumulators | 2 days |
| `pipeline` | Batch merge, no quadratic operations | 2 days |
| `streaming-extraction` | Stream file contents, don't hold in memory | 2 days |

## Acceptance Criteria

- [ ] No OOM crashes on 100k+ node graphs
- [ ] Memory usage bounded by config (max 2GB default)
- [ ] RTS profiling confirms bounded heap
- [ ] All sub-specs implemented and tested
- [ ] Memory profiling report shows improvement

## Dependencies

- atomic-graph-output-writes (P0)
- honor-graphosignore (P0)

## Blocks

- All large-graph features (6 features)
- cluster-composition
- deterministic-doc-code-edges
- detect-generated-vendored-code

## Implementation Plan

1. Profile current memory usage with `+RTS -hy -RTS`
2. Implement each sub-spec in parallel (6 engineers)
3. Integration test with large graph (100k+ nodes)
4. Add memory guardrails to pipeline
5. Update observability bounds

## Verification

- Run memory profiling on existing test corpus
- Create 100k+ node test graph
- Verify no OOM crashes
- Confirm memory bounded by config
