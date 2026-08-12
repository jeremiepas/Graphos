<!--
  PDCA-PER-TASK workflow.
-->

# Task 5 — Grafana dashboard — PLAN

**Task slug**: `05-grafana-dashboard`
**Attempt**: 1
**Status**: pending

## Summary

Create Grafana dashboard JSON for "Graphos Pipeline" and a dashboard provider config so the dashboard auto-loads on first Grafana start.

## Detail

### Scope

- Create directory structure: `grafana/provisioning/dashboards/`
- Create `grafana/provisioning/dashboards/dashboards.yaml` — dashboard provider pointing to `/var/lib/grafana/dashboards`
- Create `grafana/provisioning/dashboards/graphos-pipeline.json` with panels:
  - **Stage durations** — histogram panel using `graphos_pipeline_step_duration_seconds` metric
  - **Node count** — gauge panel using `graphos_graph_nodes` metric
  - **Edge count** — gauge panel using `graphos_graph_edges` metric
  - **Community count** — stat panel using `graphos_communities` metric
  - **Trace search link** — annotation or row link to Grafana Explore → Tempo datasource

### Check Criteria

**What will be tested:**
1. Files exist:
   - `grafana/provisioning/dashboards/dashboards.yaml`
   - `grafana/provisioning/dashboards/graphos-pipeline.json`
2. YAML parses without error
3. JSON parses without error (`python -c "import json; json.load(open('grafana/provisioning/dashboards/graphos-pipeline.json'))"`)
4. After stack start, dashboard "Graphos Pipeline" appears in Grafana: `curl -s http://localhost:3000/search?type=dashboards&q=graphos | jq '.[].title'` includes "Graphos Pipeline"

**Spec scenarios satisfied:**
- `SC-pipeline-dashboard-auto-loaded` (spec.md, Scenario: Pipeline dashboard auto-loaded) — WHEN Grafana starts, THEN "Graphos Pipeline" dashboard appears in dashboards browser
- `SC-metrics-appear-in-dashboard` (spec.md, Scenario: Metrics appear in Grafana dashboard) — WHEN `graphos --otel --metrics 9090` completes, THEN dashboard displays `graphos_pipeline_step_duration_seconds` and `graphos_graph_nodes`

**PASS conditions:**
1. All files exist and parse correctly
2. Dashboard "Graphos Pipeline" appears in Grafana after stack start
3. Dashboard panels render (no error states in Grafana UI)

**FAIL conditions:**
1. Files do not exist or are invalid YAML/JSON
2. Dashboard does not appear in Grafana after start (provisioning mount issue)
3. Panels show error (wrong metric names, datasource not configured)

### Affected modules

- New directory + files:
  - `grafana/provisioning/dashboards/dashboards.yaml`
  - `grafana/provisioning/dashboards/graphos-pipeline.json`
- No Haskell code changes
- Depends on task 3 (volume mount for dashboards dir)

### Prerequisites

- Docker available on the host
- Task 3 completed (docker-compose.otel.yaml with grafana volume mount)
- Prometheus datasource configured (task 4) — panels need valid datasource reference

### Risks

- Dashboard JSON schema version — Grafana 10+ uses a specific dashboard schema; must match the Grafana image version
- Metric names in panels must match actual Graphos metrics emitted by `--metrics` flag
- If Graphos metrics endpoint is at `--metrics 9090` on the host, Prometheus must scrape from the host (not a container). The dashboard panels query Prometheus, not Grafana directly.

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next. -->
