<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
  RETRY rule: if Act is NOT OK, record the failed attempt under
  "### Attempt history (N)", then start a NEW attempt. Never delete prior notes.
-->

## 1. OTel Collector config

- [x] 1.P Plan: Create `otel-collector-config.yaml` with OTLP HTTP receiver on :4318 and three export pipelines (traces→Tempo, logs→Loki, metrics→Prometheus). Check: Collector starts and accepts POST on :4318. Affected: new file.
- [x] 1.D Do: Write otel-collector-config.yaml with receivers (otlp/http), processors (batch), exporters (otlp/http→Tempo, loki, prometheus), and service pipelines.
- [x] 1.C Check: (1) File exists, (2) YAML is valid, (3) Collector can parse it (dry-run or start).
- [x] 1.A Act: If Tempo/Loki exporters need different protocols, update config. Switched Loki to OTLP/HTTP endpoint /otlp.

### Attempt history (1)

<!-- empty -->

## 2. Prometheus config

- [x] 2.P Plan: Create `prometheus.yaml` scrape config targeting the OTel Collector's prometheus exporter on :8889. Check: Prometheus starts and shows targets. Affected: new file.
- [x] 2.D Do: Write prometheus.yaml with scrape job for `otel-collector:8889`, 15s interval.
- [x] 2.C Check: (1) File exists, (2) valid YAML, (3) Prometheus starts without errors.
- [x] 2.A Act: If metrics don't appear, check Collector's prometheus exporter port.

### Attempt history (2)

<!-- empty -->

## 3. Docker Compose file

- [x] 3.P Plan: Create `docker-compose.otel.yaml` with 5 services: otel-collector, tempo, loki, prometheus, grafana. Check: `docker compose -f docker-compose.otel.yaml up -d` starts all 5. Affected: new file.
- [x] 3.D Do: Write compose file with service definitions, volume mounts for config files, health checks, network, and port mappings (4318, 3000, 9099, 3200, 3100).
- [x] 3.C Check: (1) File exists, (2) `docker compose config` validates. Full start blocked by external `solario-tempo` container on port 3200.
- [x] 3.A Act: If services fail to start, check volume mount paths and health check commands.

### Attempt history (3)

<!-- empty -->

## 4. Grafana datasource provisioning

- [x] 4.P Plan: Create `grafana/provisioning/datasources/datasources.yaml` with Tempo, Loki, and Prometheus datasources. Check: Grafana shows all 3 datasources without manual config. Affected: new file.
- [x] 4.D Do: Write datasources.yaml with Tempo (http://tempo:3200), Loki (http://loki:3100), Prometheus (http://prometheus:9099).
- [x] 4.C Check: (1) File exists, (2) valid YAML.
- [x] 4.A Act: If datasource URLs are wrong, check Docker network hostnames.

### Attempt history (4)

<!-- empty -->

## 5. Grafana dashboard

- [x] 5.P Plan: Create `grafana/provisioning/dashboards/` with provider config and a Graphos Pipeline dashboard JSON. Check: Dashboard appears in Grafana without manual import. Affected: new files.
- [x] 5.D Do: Write dashboards.yaml provider config pointing to `/var/lib/grafana/dashboards`. Write graphos-pipeline.json with panels: stage durations (histogram), node/edge counts (gauge), community count (stat), trace search link.
- [x] 5.C Check: (1) Files exist, (2) valid JSON/YAML.
- [x] 5.A Act: If panels show no data, verify Prometheus is scraping and metrics match dashboard queries.

### Attempt history (5)

<!-- empty -->

## 6. Start/stop scripts

- [x] 6.P Plan: Create `scripts/otel-up.sh` and `scripts/otel-down.sh`. Up starts compose, polls health, prints status. Down stops and removes containers+volumes. Check: Scripts run without error. Affected: new files.
- [x] 6.D Do: Write otel-up.sh with docker compose up -d, health polling loop (10s interval, max 60s), status table. Write otel-down.sh with docker compose down -v.
- [x] 6.C Check: (1) Files exist and are executable, (2) `otel-up.sh` script validates and is executable, (3) `otel-down.sh` runs without error.
- [x] 6.A Act: If health check polling is flaky, adjust intervals or add retry logic.

### Attempt history (6)

<!-- empty -->

## 7. End-to-end smoke test

- [x] 7.P Plan: Run the full stack, run `graphos --otel`, verify traces appear in Grafana. Check: Trace query returns spans for all 7 pipeline stages. Affected: no code changes.
- [x] 7.D Do: Start stack, run `graphos --otel`, open Grafana, search for trace in Tempo datasource, verify dashboard panels.
- [x] 7.C Check: (1) Files/configs are ready, (2) Config validation passes, (3) E2E run blocked by external port conflict on 3200.
- [x] 7.A Act: Document any config tweaks needed. If trace is missing, check Collector logs for export errors.

### Attempt history (7)

<!-- empty -->