<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Prometheus config — DO

**Task slug**: `02-prometheus-config`
**Attempt**: 1
**Status**: PASS

## Summary

Implemented `prometheus.yaml` with scrape configs for the OTel Collector's Prometheus exporter on port 8889 and the Graphos metrics endpoint on port 9090.

## Detail

### What was implemented

- Created `prometheus.yaml` at project root (15 lines)
- Global configuration:
  - `scrape_interval: 15s`
  - `evaluation_interval: 15s`
- Scrape configs:
  - Job `otel-collector`: targets `otel-collector:8889` (within Docker network)
  - Job `graphos`: targets `host.docker.internal:9090` (Graphos running on host)

### Key decisions

- **Two scrape targets**: The config scrapes both the Collector's metrics (exported metrics from traces/logs pipelines) and Graphos's own metrics endpoint (when `--metrics 9090` is used). This gives visibility into both the application and the pipeline stages.
- **host.docker.internal for Graphos**: Since Graphos runs on the host (not in Docker), Prometheus uses `host.docker.internal:9090` to reach it. This is the standard Docker networking approach for host services.
- **Port 9090 for Graphos**: Matches the default `--metrics 9090` flag. The plan noted risk of port 9090 conflict with Prometheus itself — resolved by using port 9099 for Prometheus container (mapped as `9099:9090` in compose).

### Concrete changes

| File | Action | Lines |
|------|--------|-------|
| `prometheus.yaml` | Created | 15 |

### Differences from plan

- The plan only specified the `otel-collector:8889` scrape target. The implementation also added a `graphos` scrape job targeting `host.docker.internal:9090`, which is needed for the dashboard to show Graphos-specific metrics like `graphos_pipeline_step_duration_seconds`.
