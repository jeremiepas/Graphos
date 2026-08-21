# Task 6 — Bound observability stores — PLAN

**Task slug**: `06-bound-observability-stores`
**Attempt**: 1
**Status**: pending

## Summary

Add capacity limits to `tracerSpans` (keep last 1000 spans), replace `msHistograms` with pre-aggregated `HistogramAgg`, and bound `dtBuffer` to 10000 events with disk flush. All changes in `Infrastructure.Observability.SDK`.

## Detail

### Scope

This task modifies only `src/Graphos/Infrastructure/Observability.SDK.hs`:
- **(a)** Replace `IORef [Span]` (tracerSpans) with a bounded buffer type that evicts oldest spans when capacity (default 1000) is reached
- **(b)** Replace `IORef (Map HistogramName [Double])` (msHistograms) with `IORef (Map HistogramName HistogramAgg)` where `HistogramAgg = {count :: !Int, sum :: !Double, min :: !Double, max :: !Double}`. Update `observeHistogram` to update aggregation in O(1).
- **(c)** Update `renderPrometheusMetrics` to render from `HistogramAgg` instead of `[Double]` — output count, sum, and bucket boundaries for each histogram
- **(d)** Add disk flush to `dtBuffer` (MVar [Text]) when it reaches capacity (10000 events) — flush to JSONL file and clear buffer before accepting new events

Note: The dead `Observability.hs` module was already removed by `cleanup-ram-fix-prework`; this task targets only `Observability.SDK`.

### Check Criteria

**Spec scenarios satisfied:**

| Scenario ID | Spec File | Description |
|---|---|---|
| `bounded-observability/scen:span-eviction` | `specs/bounded-observability/spec.md` | At capacity N, oldest span evicted, new span appended, total ≤ N |
| `bounded-observability/scen:default-span-capacity` | `specs/bounded-observability/spec.md` | Default span capacity = 1000 |
| `bounded-observability/scen:histogram-aggregation` | `specs/bounded-observability/spec.md` | `observeHistogram` updates count/sum/min/max in O(1); memory doesn't grow per observation |
| `bounded-observability/scen:prometheus-rendering` | `specs/bounded-observability/spec.md` | `renderPrometheusMetrics` outputs count, sum, bucket boundaries in valid Prometheus exposition format |
| `bounded-observability/scen:buffer-flush` | `specs/bounded-observability/spec.md` | At capacity N, all events flushed to JSONL, buffer cleared, no events lost |

**Specific tests/gates:**

1. **Unit test — span eviction**: Create an `ObservabilityEnv` with span capacity 1000. Insert 1001 spans. Verify only the last 1000 are retained (oldest was evicted).
2. **Unit test — histogram aggregation**: Call `observeHistogram` 100 times for the same metric name with known values. Verify `HistogramAgg.count == 100`, `HistogramAgg.sum == sum of values`, `HistogramAgg.min == minimum`, `HistogramAgg.max == maximum`. Verify that memory for that metric does not grow per observation (constant size).
3. **Unit test — buffer flush**: Create a `dtBuffer` with capacity 5 (for fast testing). Insert 6 events. Verify buffer has 1 event, file on disk has 5 events.
4. **Integration test — Prometheus rendering**: Generate 1000 histogram observations. Call `renderPrometheusMetrics`. Verify output contains count, sum, and is valid Prometheus format.
5. **Build gate**: `cabal test` passes with exit code 0.

**PASS conditions:**
- After inserting 10k spans, only last 1000 retained (verified by inspection or size check)
- After 100k histogram observations, memory for that metric is constant (O(1)) — `HistogramAgg` has fixed size
- After 20k debug trace events, JSONL file contains all 20k, in-memory buffer has at most 10k
- `renderPrometheusMetrics` produces valid Prometheus exposition format
- `cabal test` returns exit code 0

**FAIL boundaries:**
- If Prometheus rendering output is invalid format, `HistogramAgg` fields are not matching expected Prometheus labels
- If debug trace events are lost during flush (e.g., I/O error mid-write), the buffer should retry or log the error
- If span eviction doesn't work (still retains all spans), the `IORef` update is not using the bounded buffer correctly

### Affected Modules

- `src/Graphos/Infrastructure/Observability.SDK.hs` — span buffer, histogram aggregation, debug trace buffer, Prometheus rendering

### Prerequisites

- `Span` type exists in `Domain.Types` (or local to Observability)
- `HistogramName` is a newtype over `Text`
- `renderPrometheusMetrics` exists and is used by Prometheus endpoint
- `OtelConfig` in `Domain.Config` can carry span capacity configuration (if needed)

### Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Changing histogram type breaks Prometheus rendering | Loss of monitoring data | Fix `HistogramAgg` rendering to match expected Prometheus format |
| Disk I/O latency on buffer flush | Performance impact | Flush is synchronous but buffered (10k events); add async flush if latency is a concern |
| `--trace-size` not exposed as CLI flag | Can't configure span count at runtime | Document default of 1000 spans; add flag if users need more |

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
