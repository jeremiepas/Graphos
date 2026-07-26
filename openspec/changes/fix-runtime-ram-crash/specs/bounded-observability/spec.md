## ADDED Requirements

### Requirement: Bounded span storage

The `tracerSpans` store SHALL retain at most the last N spans (default N=1000), evicting the oldest spans when the cap is reached. N SHALL be configurable via `ObservabilityEnv`.

- **Plan**: Prevent unbounded memory growth from span accumulation during long pipeline runs.
- **Do**: Replace `IORef [Span]` with a bounded buffer that drops oldest spans when capacity is reached.
- **Check**: After inserting 10k spans, memory for span storage remains bounded to N spans.
- **Act**: If debugging requires more spans, increase N or add `--trace-size` CLI flag.

#### Scenario: Span eviction at capacity
- **WHEN** `withSpan` adds a new span and the span buffer contains N spans
- **THEN** the oldest span is evicted and the new span is appended
- **AND** the total number of stored spans remains at most N

#### Scenario: Default capacity
- **WHEN** `ObservabilityEnv` is initialized without explicit span capacity
- **THEN** the default capacity SHALL be 1000 spans

### Requirement: Pre-aggregated histograms

The `msHistograms` store SHALL store pre-aggregated histogram buckets (count, sum, min, max, quantile estimates) instead of accumulating raw `[Double]` observations. Each `observeHistogram` call updates the aggregation in O(1).

- **Plan**: Prevent unbounded `[Double]` list growth in histogram storage.
- **Do**: Replace `IORef (Map HistogramName [Double])` with `IORef (Map HistogramName HistogramAgg)` where `HistogramAgg` tracks count, sum, min, max, and configurable percentile buckets.
- **Check**: After 100k `observeHistogram` calls for the same metric name, memory for that histogram is O(1) (constant size).
- **Act**: If percentile accuracy is insufficient, increase bucket count or switch to t-digest.

#### Scenario: Histogram aggregation updates
- **WHEN** `observeHistogram metrics "x" 3.5` is called
- **THEN** the `HistogramAgg` for "x" updates its count (+1), sum (+3.5), min (min 3.5), max (max 3.5)
- **AND** memory usage for "x" does not increase per observation

#### Scenario: Prometheus rendering preserves key metrics
- **WHEN** `renderPrometheusMetrics` is called after 1000 observations
- **THEN** output includes count, sum, and bucket boundaries for each histogram
- **AND** output is valid Prometheus exposition format

### Requirement: Bounded debug trace buffer

The `dtBuffer` MVar SHALL be bounded to at most N events (default N=10000). When full, the buffer SHALL be flushed to disk (appended to the JSONL trace file) and cleared. Events SHALL NOT be silently dropped.

- **Plan**: Prevent unbounded in-memory growth of debug trace events.
- **Do**: When `dtBuffer` reaches capacity, flush all buffered events to the JSONL file and clear the buffer before accepting new events.
- **Check**: After 50k `debugTraceEvent` calls, in-memory buffer size is at most N events. JSONL file on disk contains all events.
- **Act**: If disk I/O latency is a concern, increase buffer size or add async flushing.

#### Scenario: Buffer flush at capacity
- **WHEN** `debugTraceEvent` is called and `dtBuffer` contains N events
- **THEN** all N events are written to the JSONL trace file
- **AND** the buffer is cleared
- **AND** the new event is added to the empty buffer
- **AND** no events are lost