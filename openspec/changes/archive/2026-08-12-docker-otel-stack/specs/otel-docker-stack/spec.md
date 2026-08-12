## ADDED Requirements

### Requirement: Docker Compose OTel stack
The project SHALL provide a `docker-compose.otel.yaml` file that starts Grafana, Tempo, Loki, Prometheus, and an OpenTelemetry Collector with a single command. The OTel Collector SHALL listen on port 4318 (HTTP) for OTLP trace and log ingest. The stack SHALL be startable via `scripts/otel-up.sh` and stoppable via `scripts/otel-down.sh`.

- Plan: Eliminate the "nothing listening on :4318" problem by providing a ready-to-use observability stack.
- Do: Write docker-compose with 5 services, network config, volume mounts, and health checks.
- Check: `scripts/otel-up.sh` starts all 5 containers; all report healthy within 30s.
- Act: If startup is slow, tune health check intervals or add depends_on condition checks.

#### Scenario: All services start healthy
- **WHEN** `scripts/otel-up.sh` is executed
- **THEN** all 5 containers (grafana, tempo, loki, prometheus, otel-collector) SHALL report status "healthy" within 30 seconds

#### Scenario: OTel Collector accepts OTLP on 4318
- **WHEN** an OTLP HTTP POST is sent to `http://localhost:4318/v1/traces`
- **THEN** the OTel Collector SHALL return HTTP 200

### Requirement: Grafana provisioning
Grafana SHALL be provisioned with Tempo (traces), Loki (logs), and Prometheus (metrics) as datasources. A pre-configured "Graphos Pipeline" dashboard SHALL be loaded automatically on first start. No manual datasource or dashboard configuration SHALL be required.

- Plan: Zero-config experience — open Grafana and see data immediately.
- Do: Add grafana/provisioning/datasources/ and grafana/provisioning/dashboards/ YAML files; add dashboard JSON.
- Check: After stack start, `http://localhost:3000` shows all 3 datasources and the pipeline dashboard without any manual setup.

#### Scenario: Datasources auto-provisioned
- **WHEN** Grafana starts as part of the OTel stack
- **THEN** the datasources "Tempo", "Loki", and "Prometheus" SHALL appear in Grafana's datasource list without manual configuration

#### Scenario: Pipeline dashboard auto-loaded
- **WHEN** Grafana starts as part of the OTel stack
- **THEN** a dashboard named "Graphos Pipeline" SHALL appear in the dashboards browser without manual import

### Requirement: Pipeline traces visible in Grafana
When Graphos runs with `--otel`, pipeline stage traces SHALL be visible in Grafana Tempo via the "Graphos Pipeline" dashboard. Each pipeline stage (detect, extract, build, cluster, infer, analyze, export) SHALL appear as a named span.

- Plan: Validate end-to-end: Graphos → OTel Collector → Tempo → Grafana.
- Do: Rely on existing hs-opentelemetry-sdk instrumentation; Tempo queries via TraceQL.
- Check: After `graphos --otel`, searching for the trace ID in Grafana shows all 7 pipeline stage spans.

#### Scenario: Pipeline traces appear in Grafana
- **WHEN** `graphos --otel` completes a pipeline run
- **THEN** the trace ID SHALL be queryable in Grafana's Explore view (Tempo datasource) and SHALL show spans for detect, extract, build, cluster, infer, analyze, and export

### Requirement: Pipeline metrics visible in Grafana
When Graphos runs with `--otel --metrics PORT`, Prometheus SHALL scrape the Graphos metrics endpoint and the "Graphos Pipeline" dashboard SHALL display stage durations, node/edge counts, and community counts.

- Plan: Validate: Graphos metrics endpoint → Prometheus scrape → Grafana dashboard.
- Do: Add Prometheus scrape config for the Graphos host; add Prometheus datasource; add dashboard panels.
- Check: Dashboard shows `graphos_pipeline_step_duration_seconds` and `graphos_graph_nodes` metrics after a run.

#### Scenario: Metrics appear in Grafana dashboard
- **WHEN** `graphos --otel --metrics 9090` completes a pipeline run
- **THEN** the "Graphos Pipeline" Grafana dashboard SHALL display at least one non-zero value for `graphos_pipeline_step_duration_seconds` and `graphos_graph_nodes`

### Requirement: Start and stop scripts
The project SHALL provide `scripts/otel-up.sh` and `scripts/otel-down.sh`. `otel-up.sh` SHALL run `docker compose -f docker-compose.otel.yaml up -d` and wait for all services to be healthy. `otel-down.sh` SHALL stop and remove all containers and volumes.

- Plan: One-command UX for developers — no Docker expertise needed.
- Do: Write shell scripts with error checking, health polling, and status output.
- Check: `scripts/otel-up.sh && scripts/otel-down.sh` completes without error.

#### Scenario: otel-up starts and waits for healthy
- **WHEN** `scripts/otel-up.sh` is executed
- **THEN** the script SHALL print status for each service and exit 0 only when all services report healthy

#### Scenario: otel-down cleans up
- **WHEN** `scripts/otel-down.sh` is executed after `scripts/otel-up.sh`
- **THEN** all OTel stack containers and named volumes SHALL be removed