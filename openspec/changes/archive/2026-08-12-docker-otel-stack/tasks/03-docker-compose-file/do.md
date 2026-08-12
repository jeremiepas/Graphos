<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 3 — Docker Compose file — DO

**Task slug**: `03-docker-compose-file`
**Attempt**: 1
**Status**: PASS

## Summary

Implemented `docker-compose.otel.yaml` with 5 services (otel-collector, tempo, loki, prometheus, grafana), shared network, health checks, volume mounts, and port mappings.

## Detail

### What was implemented

- Created `docker-compose.otel.yaml` at project root (118 lines)
- **Services**:
  - `otel-collector`: `otel/opentelemetry-collector-contrib:latest`, port 4318 (OTLP) + 8889 (metrics), volume mount for config, depends_on with `service_healthy` condition
  - `tempo`: `grafana/tempo:latest`, port 3200 (HTTP) + 4317 (gRPC OTLP), volume mount for `tempo.yaml`, health check on `/ready`
  - `loki`: `grafana/loki:latest`, port 3100, volume mount for `loki.yaml`, health check on `/ready`
  - `prometheus`: `prom/prometheus:latest`, port 9099:9090, volume mount for `prometheus.yaml`, health check on `/-/healthy`, `--web.enable-lifecycle` flag
  - `grafana`: `grafana/grafana:latest`, port 3000, anonymous admin access enabled, volume mount for provisioning dirs, depends_on with `service_healthy` condition
- **Network**: `graphos-otel` (named, single bridge)
- **Health checks**: All services use `wget -qO-` with 5s interval, 3s timeout; Grafana uses 10 retries (slower startup)
- **Depends_on**: Collector and Grafana depend on tempo/loki/prometheus being healthy before starting

### Key decisions

- **contrib image**: Used `otel/opentelemetry-collector-contrib:latest` instead of `otel/opentelemetry-collector:latest` because the contrib image includes all exporters (prometheus, loki, otlphttp) by default. The base image would require a custom build or binary download.
- **Service health dependencies**: Used `condition: service_healthy` in `depends_on` so the Collector and Grafana only start after tempo/loki/prometheus are actually ready, not just container-started. This prevents transient connection errors.
- **Prometheus port mapping**: Mapped container 9090 to host 9099 (`9099:9090`) to avoid conflict with Graphos's own metrics port 9090.
- **Anonymous Grafana**: Set `GF_AUTH_ANONYMOUS_ENABLED=true`, `GF_AUTH_ANONYMOUS_ORG_ROLE=Admin`, `GF_AUTH_DISABLE_LOGIN_FORM=true` for zero-config developer experience. No login required.
- **Lifecycle flag**: Added `--web.enable-lifecycle` to Prometheus for future graceful reload support.

### Concrete changes

| File | Action | Lines |
|------|--------|-------|
| `docker-compose.otel.yaml` | Created | 118 |
| `tempo.yaml` | Created (additional, referenced by compose) | 42 |
| `loki.yaml` | Created (additional, referenced by compose) | 33 |

### Differences from plan

- The plan specified `otelotel/otel-collector:latest` — corrected to `otel/opentelemetry-collector-contrib:latest` (the correct Docker Hub image name).
- Added `tempo.yaml` and `loki.yaml` files (not in original plan) because the Docker images require explicit config files mounted into the containers.
- Added `--web.enable-lifecycle` flag to Prometheus.
- Grafana health check uses `/api/health` endpoint (10 retries) instead of a simpler check.
- Added `grafana-storage` named volume definition (declared in compose but not actively used by the volume mount approach).
