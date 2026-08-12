<!--
  PDCA-PER-TASK workflow.
-->

# Task 2 — Prometheus config — PLAN

**Task slug**: `02-prometheus-config`
**Attempt**: 1
**Status**: pending

## Summary

Create `prometheus.yaml` with a scrape job targeting the OTel Collector's Prometheus exporter on port 8889.

## Detail

### Scope

- Create a single new file: `prometheus.yaml` in the project root
- Configure Prometheus to scrape `otel-collector:8889` at a 15-second interval
- Add a global scrape config for the Graphos metrics endpoint when `--metrics PORT` is used (scrape `host.docker.internal:<port>` with a 15s interval)

### Check Criteria

**What will be tested:**
1. File exists at `prometheus.yaml`
2. YAML parses without error (`python -c "import yaml; yaml.safe_load(open('prometheus.yaml'))"` exits 0)
3. Prometheus can start with this config: `docker run --rm -v $(pwd)/prometheus.yaml:/etc/prometheus/prometheus.yml prom/prometheus:latest --config.file=/etc/prometheus/prometheus.yml` starts without config errors
4. Prometheus `/api/v1/targets` endpoint returns the `otel-collector:8889` target in "UP" or "DOWN" state (not "UNKNOWN" or config error)

**Spec scenarios satisfied:**
- `SC-metrics-appear-in-dashboard` (spec.md, Scenario: Metrics appear in Grafana dashboard) — WHEN `graphos --otel --metrics 9090` completes, THEN dashboard displays `graphos_pipeline_step_duration_seconds` and `graphos_graph_nodes` metrics (requires Prometheus to scrape them)

**PASS conditions:**
1. File exists and is valid YAML
2. Prometheus starts and config loads without errors
3. Target `otel-collector:8889` appears in Prometheus targets list (UP or DOWN, not UNKNOWN)

**FAIL conditions:**
1. File does not exist or YAML is invalid
2. Prometheus rejects config (unknown job name, bad URL)
3. Target list shows config error for the scrape job

### Affected modules

- New file only: `prometheus.yaml`
- No Haskell code changes

### Prerequisites

- Docker available on the host
- `otel-collector-config.yaml` created (task 1) — the Prometheus exporter port 8889 must be configured in the Collector

### Risks

- Prometheus port 9090 may conflict with Graphos's own `--metrics 9090` flag. Using :9099 per design decision D5.
- If Grafana dashboard expects metrics from the Collector (not Graphos directly), the scrape config must target `otel-collector:8889` within the Docker network.

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next. -->
