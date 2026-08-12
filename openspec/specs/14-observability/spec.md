# 14-observability Specification

## Purpose
TBD - created by archiving change graphos-product. Update Purpose after archive.
## Requirements
### Requirement: Workflow 14 — OTLP traces via hs-opentelemetry-sdk
Module `Graphos.Infrastructure.Observability` SHALL export `runWithTracing :: Text -> GraphosConfig -> IO a -> IO a`. Module `Graphos.Infrastructure.Observability.SDK` SHALL initialize OTLP exporter on startup. Spans SHALL be created for each pipeline stage (detect, extract, build, cluster, infer, analyze, export) using `inSpan`. Endpoint default: `http://localhost:4318`. CLI: `--otel` enables, `--otel-endpoint <url>` overrides. Env: `OTEL_EXPORTER_OTLP_ENDPOINT`, `OTEL_SDK_DISABLED` (kill switch). (PRD §10.1, §10.2, workflow 14)

#### Scenario: Traces enabled with --otel
- **WHEN** `graphos <path> --otel` is run
- **THEN** 7 spans SHALL be created (one per stage) with timing and attributes

#### Scenario: OTEL_SDK_DISABLED kills all telemetry
- **WHEN** `OTEL_SDK_DISABLED=true` is set
- **THEN** no spans or metrics SHALL be created regardless of `--otel`

### Requirement: Workflow 14 — IORef MetricsStore with atomic operations
`data MetricsStore` with `msCounters :: IORef (Map Text Int)`, `msGauges :: IORef (Map Text Double)`, `msHistograms :: IORef (Map Text [Double])`. Functions: `newMetricsStore :: IO MetricsStore`, `incCounter :: MetricsStore -> Text -> IO ()`, `decCounter :: MetricsStore -> Text -> IO ()`, `setGauge :: MetricsStore -> Text -> Double -> IO ()`, `observeHistogram :: MetricsStore -> Text -> Double -> IO ()`. All operations SHALL use `atomicModifyIORef'`. (PRD §10.3, workflow 14)

#### Scenario: Atomic counter increment
- **WHEN** two threads call `incCounter` simultaneously
- **THEN** counter SHALL be incremented exactly twice with no lost updates

### Requirement: Workflow 14 — Prometheus /metrics HTTP endpoint
When `--metrics PORT` is set, SHALL start HTTP server via Warp on `PORT` serving `/metrics` in Prometheus exposition format: `# TYPE name counter/gauge/histogram` with bucket boundaries. (PRD §10.3, workflow 14)

#### Scenario: Prometheus endpoint serves valid format
- **WHEN** `--metrics 9090` is set
- **THEN** `GET /metrics` SHALL return `# TYPE` headers and metric values

### Requirement: Workflow 14 — OTLP log bridge with trace correlation
Module `Graphos.Infrastructure.Logging` SHALL export leveled logging. When OTLP span is active, log entries SHALL include `trace_id` attribute via `setLogTraceContext`. Logs shipped via OTLP when `--otel` enabled. (PRD §10.4, workflow 14)

#### Scenario: Log includes trace_id during active span
- **WHEN** a log is written during `runWithTracing "extract"`
- **THEN** the log SHALL include the span's `trace_id`

### Requirement: Workflow 14 — debug trace JSONL output
System SHALL write timestamped JSON events to `graphos-out/debug/*.jsonl`. Each event: `timestamp`, `stage`, `event_type`, `details`. (PRD §10.1, workflow 14)

#### Scenario: Debug JSONL file created
- **WHEN** pipeline runs
- **THEN** `graphos-out/debug/` SHALL contain `.jsonl` with stage transition events

### Requirement: Workflow 14 — OpenTelemetry env vars
SHALL respect: `OTEL_EXPORTER_OTLP_ENDPOINT`, `OTEL_EXPORTER_OTLP_HEADERS`, `OTEL_SERVICE_NAME` (default `graphos`), `OTEL_RESOURCE_ATTRIBUTES`, `OTEL_BSP_SCHEDULE_DELAY` (default 5000ms), `OTEL_SDK_DISABLED`. CLI flags override env vars. (PRD §10.2, workflow 14)

#### Scenario: OTEL_SERVICE_NAME from env
- **WHEN** `OTEL_SERVICE_NAME=my-graphos` is set
- **THEN** traces SHALL attribute to service "my-graphos"

