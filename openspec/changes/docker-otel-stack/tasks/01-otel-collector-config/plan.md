<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
  RETRY rule: if Act is NOT OK, record the failed attempt under
  "### Attempt history (N)", then start a NEW attempt. Never delete prior notes.
-->

# Task 1 — OTel Collector config — PLAN

**Task slug**: `01-otel-collector-config`
**Attempt**: 1
**Status**: pending

## Summary

Create `otel-collector-config.yaml` with an OTLP HTTP receiver on port 4318 and three export pipelines routing traces to Tempo, logs to Loki, and metrics to Prometheus.

## Detail

### Scope

- Create a single new file: `otel-collector-config.yaml` in the project root
- Configure the OpenTelemetry Collector (image: `otelotel/otel-collector:latest`) with:
  - **Receiver**: `otlp` on endpoint `:4318` (HTTP)
  - **Processors**: `batch` for traces and metrics
  - **Exporters**:
    - `otlp/tempo` → `http://tempo:3200` for traces
    - `loki` → `http://loki:3100/loki/api/v1/otlp` for logs
    - `prometheus/remotewrite` → `http://prometheus:9099/api/v1/write` for metrics
  - **Service pipelines**:
    - `traces` → receiver → batch → tempo
    - `logs` → receiver → batch → loki
    - `metrics` → receiver → batch → prometheus

### Check Criteria

**What will be tested:**
1. File exists at `otel-collector-config.yaml`
2. YAML parses without error (`python -c "import yaml; yaml.safe_load(open('otel-collector-config.yaml'))"` exits 0)
3. Collector can parse the config: `docker run --rm -v $(pwd)/otel-collector-config.yaml:/etc/otelcol-config.yaml otelotel/otel-collector:latest --config /etc/otelcol-config.yaml` runs without config parse errors (exit code may be non-zero due to missing backends, but must not be a config error)
4. OTLP HTTP POST to `http://localhost:4318/v1/traces` with a dummy payload returns HTTP 200

**Spec scenarios satisfied:**
- `SC-otel-collector-accepts-otlp` (spec.md, Scenario: OTel Collector accepts OTLP on 4318) — WHEN OTLP POST to `:4318/v1/traces`, THEN returns 200

**PASS conditions:**
1. File exists and is valid YAML
2. Collector config loads without parse errors
3. Collector accepts POST `200` on port 4318

**FAIL conditions:**
1. File does not exist or YAML is invalid → exit code non-zero from parser
2. Collector rejects config (e.g., unknown exporter, bad port)
3. POST to `:4318` returns non-2xx status

### Affected modules

- New file only: `otel-collector-config.yaml`
- No Haskell code changes

### Prerequisites

- Docker available on the host
- Docker Compose plugin installed

### Risks

- Port 4318 may be in use by another process (e.g., another OTLP collector)
- Loki exporter protocol — default Loki exporter uses gRPC, not OTLP/HTTP; may need to switch to `http` protocol or use `/otlp` endpoint path. If so, this will be an Act adjustment.

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next. -->
