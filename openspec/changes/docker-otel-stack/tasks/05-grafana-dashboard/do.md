<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 5 — Grafana dashboard — DO

**Task slug**: `05-grafana-dashboard`
**Attempt**: 1
**Status**: PASS

## Summary

Implemented the "Graphos Pipeline" Grafana dashboard with 4 panels (stage durations, node/edge counts, community count, trace search) and a dashboard provider config for automatic loading.

## Detail

### What was implemented

- Created directory: `grafana/provisioning/dashboards/`
- Created `grafana/provisioning/dashboards/dashboards.yaml` (12 lines):
  - Provider named "Graphos Pipeline", type `file`, options pointing to `/var/lib/grafana/dashboards`
- Created `grafana/provisioning/dashboards/graphos-pipeline.json` (167 lines) with:
  - **Panel 1 — Stage Durations (p95)**: Timeseries histogram using `histogram_quantile(0.95, sum(rate(graphos_pipeline_step_duration_seconds_bucket[$__rate_interval])) by (le, step))` — shows p95 duration per pipeline stage
  - **Panel 2 — Node / Edge Counts**: Stat panel with two queries: `graphos_graph_nodes` and `graphos_graph_edges`
  - **Panel 3 — Community Count**: Stat panel with query `graphos_community_count`
  - **Panel 4 — Trace Search**: Traces panel using TraceQL query `{resource.service.name="graphos"}` from the Tempo datasource

### Key decisions

- **Dashboard variables**: Used Grafana template variables (`${prometheus}`, `${tempo}`) for datasource selection instead of hardcoded datasource UIDs. This makes the dashboard portable across environments where datasource UIDs may differ.
- **Schema version 38**: Matches Grafana 10+ dashboard schema.
- **30s refresh**: Dashboard auto-refreshes every 30 seconds to show recent pipeline runs.
- **Metric names**: Used actual Graphos metric names from the codebase: `graphos_pipeline_step_duration_seconds_bucket`, `graphos_graph_nodes`, `graphos_graph_edges`, `graphos_community_count`.
- **TraceQL query**: Used `{resource.service.name="graphos"}` to filter traces to only Graphos-originated spans, which is the standard OTel resource attribute pattern.

### Concrete changes

| File | Action | Lines |
|------|--------|-------|
| `grafana/provisioning/dashboards/dashboards.yaml` | Created | 12 |
| `grafana/provisioning/dashboards/graphos-pipeline.json` | Created | 167 |

### Differences from plan

- The plan mentioned a histogram panel for stage durations; the implementation uses a timeseries panel with `histogram_quantile(0.95, ...)` which is the standard Prometheus pattern for histogram quantiles (raw histogram buckets are summed and p95 computed).
- The plan mentioned a "Trace search link" as an annotation or row link; the implementation uses a full Traces panel (not a simple link) which provides interactive trace search directly in the dashboard.
