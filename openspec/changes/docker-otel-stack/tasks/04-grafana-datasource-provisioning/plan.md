<!--
  PDCA-PER-TASK workflow.
-->

# Task 4 — Grafana datasource provisioning — PLAN

**Task slug**: `04-grafana-datasource-provisioning`
**Attempt**: 1
**Status**: pending

## Summary

Create Grafana provisioning configuration with datasources for Tempo, Loki, and Prometheus so they appear automatically on first Grafana start.

## Detail

### Scope

- Create directory structure: `grafana/provisioning/datasources/`
- Create `grafana/provisioning/datasources/datasources.yaml` with:
  - **Tempo** datasource: `http://tempo:3200` (type: tempo)
  - **Loki** datasource: `http://loki:3100` (type: loki)
  - **Prometheus** datasource: `http://prometheus:9099` (type: prometheus)
- Set `isDefault: true` for Prometheus datasource
- Use Grafana provisioning format with `editors` config

### Check Criteria

**What will be tested:**
1. File exists at `grafana/provisioning/datasources/datasources.yaml`
2. YAML parses without error
3. After stack start (task 3), Grafana API returns all 3 datasources: `curl -s http://localhost:3000/api/datasources | jq '.[].name'` includes "Tempo", "Loki", "Prometheus"

**Spec scenarios satisfied:**
- `SC-datasources-auto-provisioned` (spec.md, Scenario: Datasources auto-provisioned) — WHEN Grafana starts, THEN "Tempo", "Loki", and "Prometheus" datasources appear without manual config

**PASS conditions:**
1. File exists and is valid YAML
2. All 3 datasources appear in Grafana datasource list after stack start
3. Each datasource is reachable (Grafana shows "Saved & tested" or connectivity check passes)

**FAIL conditions:**
1. File does not exist or YAML is invalid
2. Any datasource missing from Grafana list
3. Datasource URLs resolve to wrong containers (network isolation issue)

### Affected modules

- New directory + file: `grafana/provisioning/datasources/datasources.yaml`
- No Haskell code changes
- Depends on task 3 (docker-compose.otel.yaml with grafana volume mount for provisioning dirs)

### Prerequisites

- Docker available on the host
- Task 3 completed (docker-compose.otel.yaml with grafana volume mount for `grafana/provisioning/`)

### Risks

- Docker network hostname resolution — Grafana must resolve "tempo", "loki", "prometheus" within the compose network
- Grafana provisioning requires specific directory structure (`provisioning/datasources/` must be mounted into `/etc/grafana/provisioning/datasources/` in the container)

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next. -->
