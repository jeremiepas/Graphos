<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 1 — OTel Collector config — DO

**Task slug**: `01-otel-collector-config`
**Attempt**: 1
**Status**: PASS

## Summary

Implemented `otel-collector-config.yaml` with OTLP HTTP receiver on port 4318 and three export pipelines routing traces to Tempo, logs to Loki, and metrics to Prometheus.

## Detail

### What was implemented

- Created `otel-collector-config.yaml` at project root (37 lines)
- Configured the OpenTelemetry Collector with:
  - **Receiver**: `otlp` with HTTP protocol on `0.0.0.0:4318`
  - **Processor**: `batch` with 500ms timeout and 1024 batch size
  - **Exporters**:
    - `otlphttp/tempo` → `http://tempo:3200` with TLS insecure
    - `otlphttp/loki` → `http://loki:3100/otlp` with TLS insecure
    - `prometheus` → `0.0.0.0:8889` (Prometheus exporter, exposes metrics for Prometheus scrape)
  - **Service pipelines**:
    - `traces` → `[otlp]` → `[batch]` → `[otlphttp/tempo]`
    - `logs` → `[otlp]` → `[batch]` → `[otlphttp/loki]`
    - `metrics` → `[otlp]` → `[batch]` → `[prometheus]`

### Key decisions

- **Prometheus exporter vs remote_write**: Used the `prometheus` exporter (not `prometheus/remotewrite`) which exposes a `/metrics` endpoint on port 8889 for Prometheus to scrape. This is the standard OTel Collector pattern.
- **Loki via OTLP/HTTP**: Used `otlphttp/loki` exporter targeting `/otlp` path on Loki's port 3100. The plan noted the risk that the default Loki exporter uses gRPC — this was resolved by choosing the `otlphttp` variant.
- **TLS insecure**: All exporters use `tls.insecure: true` since all services run on localhost Docker network.
- **Single image**: Used `otel/opentelemetry-collector-contrib:latest` in compose (task 3) which includes the prometheus exporter by default.

### Concrete changes

| File | Action | Lines |
|------|--------|-------|
| `otel-collector-config.yaml` | Created | 37 |

### Differences from plan

- The plan specified `prometheus/remotewrite` to `http://prometheus:9099/api/v1/write`. The implementation uses the native `prometheus` exporter on `0.0.0.0:8889`, which exposes metrics in Prometheus-compatible format. This is a cleaner approach — Prometheus scrapes the Collector rather than the Collector writing to Prometheus.
