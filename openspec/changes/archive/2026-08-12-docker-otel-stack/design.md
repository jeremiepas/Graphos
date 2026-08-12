## Context

Graphos already emits OTLP telemetry (traces, logs, metrics) via `hs-opentelemetry-sdk` when `--otel` is enabled. The `fix-mvar-shutdown-crash` change added per-component timeouts so the pipeline exits cleanly even when no collector is running. This change provides the missing collector and visualisation layer.

## Goals / Non-Goals

**Goals:**
- One-command Docker stack that receives all OTLP output from Graphos
- Zero-config Grafana with pre-loaded datasources and dashboard
- Start/stop scripts for developer convenience
- All 5 services healthy within 30s on modern hardware

**Non-Goals:**
- Production deployment config (this is a dev tool)
- Custom Grafana alerting rules
- Persistent storage beyond Docker volumes (data lost on `otel-down.sh`)
- CI integration (future iteration)

## Decisions

### D1: OTel Collector as central router

**Decision**: Use an OpenTelemetry Collector as the single OTLP receiver. It routes traces → Tempo, logs → Loki, metrics → Prometheus.

**Alternatives considered:**
- A) Each backend receives directly — Tempo can receive OTLP but Loki and Prometheus cannot
- B) **OTel Collector as router** — single entry point, protocol translation, future-proof for adding exporters
- C) Use Grafana Agent instead — heavier, more config, overkill for local dev

**Rationale**: The Collector is the standard pattern. It keeps port 4318 as the single endpoint (matching `--otel` default). Adding new backends later is a config change, not a code change.

### D2: Tempo for traces (not Jaeger)

**Decision**: Use Grafana Tempo as the trace backend.

**Alternatives considered:**
- A) Jaeger — more mature but separate UI, doesn't integrate with Grafana dashboards
- B) **Tempo** — native Grafana integration, TraceQL, no separate UI needed
- C) Zipkin — less feature-rich, no Grafana integration

**Rationale**: Tempo gives native Grafana integration — same UI for traces, logs, and metrics. TraceQL is powerful for querying. One less service to manage vs Jaeger + Grafana.

### D3: Loki for logs (not Elastic)

**Decision**: Use Grafana Loki for log storage.

**Alternatives considered:**
- A) Elasticsearch/ELK — powerful but heavy, separate stack
- B) **Loki** — lightweight, label-based, native Grafana integration
- C) stdout only — no persistence, no querying

**Rationale**: Loki is lightweight and integrates with Grafana. The OTLP log bridge in `hs-opentelemetry-sdk` already ships logs to an OTLP endpoint — the Collector routes them to Loki via the `loki` exporter.

### D4: Prometheus for metrics (not InfluxDB)

**Decision**: Use Prometheus with remote_write from the OTel Collector.

**Alternatives considered:**
- A) InfluxDB — more flexible but heavier, separate UI
- B) **Prometheus** — standard, lightweight, native Grafana support, matches existing `renderPrometheusMetrics` format
- C) OTLP metrics only — Tempo doesn't store metrics; need a metrics backend

**Rationale**: Graphos already exposes Prometheus-format metrics. Prometheus scrapes the Collector's prometheus exporter, and Grafana queries Prometheus. Consistent with the existing metrics format.

### D5: Fixed ports for developer ergonomics

**Decision**: Use fixed, well-known ports: Grafana :3000, OTel Collector :4318, Prometheus :9099, Tempo :3200, Loki :3100.

**Rationale**: Hardcoded ports simplify the dev experience. No environment variable configuration needed for local dev. :9099 for Prometheus avoids collision with Graphos's own :9090 metrics port.

## Architecture

```
Graphos --otel
  │
  ├─ OTLP traces ──► :4318 ─┐
  ├─ OTLP logs ────► :4318 ─┤ OTel Collector
  └─ OTLP metrics ─► :4318 ─┘
                              ├─► Tempo (:3200)     ← traces
                              ├─► Loki (:3100)       ← logs
                              └─► Prometheus (:9099) ← metrics
                                       │
                              Grafana (:3000)
                                ├─ Tempo datasource  (Explore → TraceQL)
                                ├─ Loki datasource   (Explore → LogQL)
                                └─ Prometheus datasource (Dashboard → PromQL)
```

## File Layout

```
docker-compose.otel.yaml          # Compose file
grafana/
  provisioning/
    datasources/
      datasources.yaml           # Tempo, Loki, Prometheus datasources
    dashboards/
      dashboards.yaml            # Dashboard provider config
      graphos-pipeline.json      # Pre-built pipeline dashboard
scripts/
  otel-up.sh                     # Start stack, wait for healthy
  otel-down.sh                   # Stop and remove
otel-collector-config.yaml       # Collector pipelines: receive OTLP → export to backends
prometheus.yaml                  # Scrape config for Collector's prometheus exporter
```

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| Docker not installed | `otel-up.sh` checks for docker + docker compose |
| Port conflicts | Document port usage; use non-standard :9099 for Prometheus |
| Large trace volumes fill Tempo | Use Tempo's default local storage with size limits |
| Grafana dashboard needs updates after pipeline changes | Dashboard is version-controlled; update as part of pipeline changes |
| Prometheus scrape misses short-lived metrics | Collector exposes metrics as prometheus remote_write, not scrape |

## Verification Strategy

1. `scripts/otel-up.sh` — all 5 services healthy within 30s
2. `curl localhost:4318/v1/traces -X POST -d '{}'` — returns 200
3. `graphos --otel` — pipeline completes, exit 0
4. Open `http://localhost:3000` — datasources listed, dashboard visible
5. Grafana Explore → Tempo — search for graphos trace, see 7 pipeline stage spans
6. `scripts/otel-down.sh` — all containers removed

## Iteration & Rollback

- If Tempo is too resource-heavy, switch to Jaeger all-in-one
- If Loki ingest fails, check OTel Collector `loki` exporter config
- If metrics don't appear, switch Prometheus from scrape to remote_write
- If stack is useful in CI, add `docker-compose.otel.ci.yaml` with ephemeral volumes