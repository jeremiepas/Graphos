## Why

Graphos emits OTLP traces, metrics, and logs via `hs-opentelemetry-sdk` but there is no local observability stack to receive them. Running `graphos --otel` pushes to `localhost:4318` where nothing listens, causing the MVar shutdown crash we just fixed with timeouts. Developers need a one-command Docker stack (Grafana + Tempo + Loki + Prometheus + OTel Collector) to visualise pipeline performance, debug slow stages, and validate that telemetry is wired correctly.

## What Changes

- Add a `docker-compose.otel.yaml` that brings up Grafana, Tempo, Loki, Prometheus, and an OpenTelemetry Collector (OTLP receiver → Tempo/Loki/Prometheus backends)
- Add a `grafana/` directory with provisioning dashboards (datasources + a Graphos pipeline dashboard)
- Add a `scripts/otel-up.sh` and `scripts/otel-down.sh` for one-command start/stop
- Add a `README` section documenting how to use the stack with `--otel`

## Capabilities

### New Capabilities
- `otel-docker-stack`: Docker Compose observability stack with Grafana, Tempo, Loki, Prometheus, and OTel Collector; provisioning config and pipeline dashboard; start/stop scripts

### Modified Capabilities

## Impact

- New files: `docker-compose.otel.yaml`, `grafana/provisioning/`, `grafana/dashboards/`, `scripts/otel-up.sh`, `scripts/otel-down.sh`
- No Haskell code changes — the stack consumes existing OTLP output from `--otel` flag
- Developer dependency: Docker + Docker Compose
- Default ports: Grafana :3000, OTel Collector :4318, Prometheus :9099, Tempo :3200, Loki :3100

## PDCA Cycle

- **Plan**: One-command `scripts/otel-up.sh` starts the full stack. `graphos --otel` traces appear in Grafana within 60s. Success = pipeline traces in Tempo, logs in Loki, metrics in Prometheus, all visible in a single Grafana dashboard.
- **Do**: Create docker-compose, Grafana provisioning, dashboard JSON, start/stop scripts.
- **Check**: `scripts/otel-up.sh && sleep 5 && cabal run graphos -- . --otel && open http://localhost:3000` shows traces, logs, and metrics in Grafana.
- **Act**: If stack is useful, add to CI for observability regression testing. Document in project README.