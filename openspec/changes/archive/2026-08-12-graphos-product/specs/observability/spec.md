## ADDED Requirements

### Requirement: Infrastructure.Observability — OTLP trace export with hs-opentelemetry-sdk
Module `Graphos.Infrastructure.Observability` SHALL export: `runWithTracing :: Text -> GraphosConfig -> IO a -> IO a` (wraps IO action in span). Module `Graphos.Infrastructure.Observability.SDK` SHALL initialize the OTLP exporter on startup when `--otel` flag is set. Spans SHALL be created for each pipeline stage: `detect`, `extract`, `build`, `cluster`, `infer`, `analyze`, `export`. OTLP endpoint SHALL default to `http://localhost:4318`, overridable via `OTEL_EXPORTER_OTLP_ENDPOINT` env var or `--otel-endpoint` CLI flag. (PRD §10.1, §10.2)

#### Scenario: Traces enabled creates spans per stage
- **WHEN** user runs `graphos <path> --otel`
- **THEN** spans SHALL be created for each of the 7 pipeline stages with timing and attributes

#### Scenario: Custom OTLP endpoint
- **WHEN** user runs `graphos <path> --otel --otel-endpoint http://collector:4318`
- **THEN** spans SHALL be exported to `http://collector:4318`

#### Scenario: Telemetry kill switch
- **WHEN** `OTEL_SDK_DISABLED=true` environment variable is set
- **THEN** no spans SHALL be created or exported regardless of `--otel` flag

### Requirement: Infrastructure.Observability — IORef MetricsStore with atomic operations
Module `Graphos.Infrastructure.Observability` SHALL define `data MetricsStore = MetricsStore { msCounters :: IORef (Map Text Int), msGauges :: IORef (Map Text Double), msHistograms :: IORef (Map Text [Double]) }`. SHALL export: `newMetricsStore :: IO MetricsStore`, `incCounter :: MetricsStore -> Text -> IO ()`, `decCounter :: MetricsStore -> Text -> IO ()`, `setGauge :: MetricsStore -> Text -> Double -> IO ()`, `observeHistogram :: MetricsStore -> Text -> Double -> IO ()`. All operations SHALL use `atomicModifyIORef'` for thread safety. (PRD §10.3)

#### Scenario: Counter increment is atomic
- **WHEN** two threads call `incCounter store "files_extracted"` simultaneously
- **THEN** the counter SHALL be incremented exactly twice with no lost updates

#### Scenario: Histogram records observations
- **WHEN** `observeHistogram store "leiden_duration" 2.5` is called
- **THEN** the histogram entry SHALL contain 2.5 in its observation list

### Requirement: Infrastructure.Observability — Prometheus /metrics HTTP endpoint
When `--metrics PORT` is set, the system SHALL start an HTTP server on `PORT` serving `/metrics` in Prometheus exposition format: `# TYPE name counter`, `# TYPE name gauge`, `# TYPE name histogram` with bucket boundaries. (PRD §10.3)

#### Scenario: Prometheus endpoint returns valid format
- **WHEN** user runs `graphos <path> --otel --metrics 9090`
- **THEN** `curl http://localhost:9090/metrics` SHALL return text with `# TYPE` headers and metric values

### Requirement: Infrastructure.Logging — OTLP log bridge with trace correlation
Module `Graphos.Infrastructure.Logging` SHALL export leveled logging functions. When an OTLP span is active, log entries SHALL include a `trace_id` attribute via `setLogTraceContext`. This enables click-through from log line to trace in Grafana. Logs SHALL be shipped via OTLP to the collector when `--otel` is enabled. (PRD §10.4)

#### Scenario: Log includes trace_id during active span
- **WHEN** a log entry is created during a `runWithTracing "extract"` span
- **THEN** the log SHALL include the span's `trace_id` as an attribute

#### Scenario: Log without active span has no trace_id
- **WHEN** a log entry is created outside any span
- **THEN** the log SHALL NOT include a `trace_id` attribute

### Requirement: Debug trace JSONL output
When pipeline runs, the system SHALL write timestamped JSON events to `graphos-out/debug/*.jsonl`. Each event SHALL include: `timestamp`, `stage`, `event_type`, `details`. (PRD §10.1)

#### Scenario: Debug trace file created
- **WHEN** the pipeline runs
- **THEN** `graphos-out/debug/` SHALL contain a `.jsonl` file with one JSON object per line for stage transitions, extraction results, and errors

### Requirement: OpenTelemetry environment variable support
The system SHALL respect: `OTEL_EXPORTER_OTLP_ENDPOINT` (default `http://localhost:4318`), `OTEL_EXPORTER_OTLP_HEADERS` (auth headers), `OTEL_SERVICE_NAME` (default `graphos`), `OTEL_RESOURCE_ATTRIBUTES` (resource metadata), `OTEL_BSP_SCHEDULE_DELAY` (default 5000ms), `OTEL_SDK_DISABLED` (kill switch). CLI flags SHALL override environment variables. (PRD §10.2)

#### Scenario: OTEL_SERVICE_NAME from env var
- **WHEN** `OTEL_SERVICE_NAME=my-graphos` is set in the environment
- **THEN** traces SHALL be attributed to service "my-graphos" unless overridden by CLI