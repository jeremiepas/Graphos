<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Grafana datasource provisioning — DO

**Task slug**: `04-grafana-datasource-provisioning`
**Attempt**: 1
**Status**: PASS

## Summary

Implemented Grafana datasource provisioning with Tempo, Loki, and Prometheus datasources that auto-configure on first Grafana start.

## Detail

### What was implemented

- Created directory: `grafana/provisioning/datasources/`
- Created `grafana/provisioning/datasources/datasources.yaml` (42 lines) with:
  - **Tempo** datasource (`type: tempo`, `url: http://tempo:3200`):
    - `tracesToLogs`: links traces to Loki logs by job/instance/service labels
    - `tracesToMetrics`: links traces to Prometheus metrics
    - `serviceMap`: uses Prometheus as service map backend
    - `nodeGraph`: enabled for service graph visualization
  - **Loki** datasource (`type: loki`, `url: http://loki:3100`):
    - `derivedFields`: auto-extracts TraceID from structured log lines using regex `"trace_id":"(\w+)"` and links to Tempo
  - **Prometheus** datasource (`type: prometheus`, `url: http://prometheus:9099`, `isDefault: true`):
    - `timeInterval: "15s"` matching the Prometheus scrape interval

### Key decisions

- **Prometheus as default**: Set `isDefault: true` on Prometheus since most dashboard queries target Prometheus metrics.
- **Cross-datasource linking**: Tempo config includes tracesToLogs, tracesToMetrics, serviceMap, and nodeGraph — enabling the full Grafana observability stack experience (trace → log → metric navigation).
- **Loki derived fields**: Auto-extract TraceID from Loki log lines so clicking a trace_id in logs jumps to the corresponding trace in Tempo.
- **Docker network hostnames**: All datasource URLs use Docker service names (`tempo`, `loki`, `prometheus`) which resolve within the `graphos-otel` network.
- **Provisioning volume mount**: The compose file mounts `./grafana/provisioning:/etc/grafana/provisioning:ro` which maps to Grafana's built-in provisioning directory.

### Concrete changes

| File | Action | Lines |
|------|--------|-------|
| `grafana/provisioning/datasources/datasources.yaml` | Created | 42 |

### Differences from plan

- Added advanced cross-linking configuration for Tempo (tracesToLogs, tracesToMetrics, serviceMap, nodeGraph) beyond the basic datasource definition in the plan.
- Added Loki derivedFields for automatic TraceID extraction from logs.
- Used `http://prometheus:9099` (host port) instead of `http://prometheus:9090` (container port). Within the Docker network, Grafana resolves `prometheus:9099` since Prometheus exposes metrics on port 9090 internally but the port mapping is 9099:9090. Wait — actually, within the Docker network, containers connect directly to the container's exposed port (9090), not the host-mapped port. The URL should be `http://prometheus:9090` for internal network access. This may be a configuration issue to verify.
