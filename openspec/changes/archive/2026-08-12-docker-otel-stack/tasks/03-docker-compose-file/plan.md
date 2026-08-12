<!--
  PDCA-PER-TASK workflow.
-->

# Task 3 — Docker Compose file — PLAN

**Task slug**: `03-docker-compose-file`
**Attempt**: 1
**Status**: pending

## Summary

Create `docker-compose.otel.yaml` with 5 services: otel-collector, tempo, loki, prometheus, grafana — with volume mounts, health checks, network, and port mappings.

## Detail

### Scope

- Create a single new file: `docker-compose.otel.yaml` in the project root
- Define 5 services:
  - **otel-collector** (image: `otelotel/otel-collector:latest`, ports: `4318:4318`, volumes: `otel-collector-config.yaml:/etc/otelcol-config.yaml`, depends_on: [tempo, loki, prometheus])
  - **tempo** (image: `grafana/tempo:latest`, ports: `3200:3200`, volumes: tempo-data)
  - **loki** (image: `grafana/loki:latest`, ports: `3100:3100`, volumes: loki-data)
  - **prometheus** (image: `prom/prometheus:latest`, ports: `9099:9099`, volumes: `prometheus.yaml:/etc/prometheus/prometheus.yml`, prometheus-data)
  - **grafana** (image: `grafana/grafana:latest`, ports: `3000:3000`, volumes: grafana-data + grafana provisioning dirs)
- Define a shared Docker network
- Add health checks for each service

### Check Criteria

**What will be tested:**
1. File exists at `docker-compose.otel.yaml`
2. `docker compose -f docker-compose.otel.yaml config` validates without errors
3. All 5 services start: `docker compose -f docker-compose.otel.yaml up -d` succeeds
4. All services report "healthy" within 30 seconds (health check passes)

**Spec scenarios satisfied:**
- `SC-all-services-healthy` (spec.md, Scenario: All services start healthy) — WHEN `scripts/otel-up.sh` executed, THEN all 5 containers report "healthy" within 30s
- `SC-otel-collector-accepts-otlp` (spec.md, Scenario: OTel Collector accepts OTLP on 4318) — requires port 4318 mapped and Collector running

**PASS conditions:**
1. `docker compose config` exits 0
2. All 5 containers start (exit code 0 from `docker compose up -d`)
3. All 5 health checks pass within 30s (`docker compose ps` shows "healthy" for all)

**FAIL conditions:**
1. YAML validation fails (bad compose syntax)
2. Any container fails to start (crash, missing image)
3. Port conflict — container fails to bind port
4. Health check fails after 30s (service not ready)

### Affected modules

- New file only: `docker-compose.otel.yaml`
- No Haskell code changes
- Depends on files from tasks 1 and 2 (otel-collector-config.yaml, prometheus.yaml)

### Prerequisites

- Docker + Docker Compose installed and running
- Tasks 1 and 2 completed (config files must exist)
- No other service using ports 3000, 3200, 3100, 9099, or 4318

### Risks

- Port 3200 (Tempo) may conflict with existing `solario-tempo` container (known from task 3 check in tasks.md)
- Port 9099 avoids Graphos's :9090 but verify no other service uses it
- Grafana port 3000 is commonly used by other services
- Large images (Grafana ~200MB, Tempo ~80MB) may cause slow startup on slower connections

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next. -->
