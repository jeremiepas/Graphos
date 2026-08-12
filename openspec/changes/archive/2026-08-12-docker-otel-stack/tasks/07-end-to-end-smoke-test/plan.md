<!--
  PDCA-PER-TASK workflow.
-->

# Task 7 — End-to-end smoke test — PLAN

**Task slug**: `07-end-to-end-smoke-test`
**Attempt**: 1
**Status**: pending

## Summary

Validate end-to-end: start the stack, run Graphos with `--otel`, verify traces appear in Grafana Tempo with all 7 pipeline stage spans.

## Detail

### Scope

- No code changes — this is a validation exercise
- Run the full stack via `scripts/otel-up.sh`
- Execute `cabal run graphos -- <path> --otel` (or a simple test run)
- Query Grafana Tempo via Explore → TraceQL for spans from the run
- Verify spans for all 7 pipeline stages: detect, extract, build, cluster, infer, analyze, export

### Check Criteria

**What will be tested:**
1. Stack is running: `scripts/otel-up.sh` completes, all services healthy
2. Graphos runs with OTLP enabled: `cabal run graphos -- <path> --otel` exits 0
3. OTLP endpoint accepts data: `curl -s -o /dev/null -w '%{http_code}' -X POST http://localhost:4318/v1/traces -H 'Content-Type: application/json' -d '{"resourceSpans":[]}'` returns 200
4. Tempo query returns spans: Use Grafana API or `docker exec` into Grafana to query Tempo datasource for recent traces

**Spec scenarios satisfied:**
- `SC-pipeline-traces-in-grafana` (spec.md, Scenario: Pipeline traces appear in Grafana) — WHEN `graphos --otel` completes, THEN trace ID is queryable in Grafana Explore (Tempo datasource) with spans for detect, extract, build, cluster, infer, analyze, export

**PASS conditions:**
1. All 5 services healthy (from task 3/6)
2. Graphos `--otel` run completes with exit code 0
3. At least one trace appears in Tempo with spans matching all 7 pipeline stages
4. No OTLP errors in Collector logs (check `docker logs otel-collector`)

**FAIL conditions:**
1. Services fail to start
2. Graphos `--otel` exits non-zero
3. Collector returns non-200 for OTLP POST
4. No traces appear in Tempo within 60s of Graphos completion
5. Collector logs show export errors (e.g., `error sending span: connection refused` to Tempo endpoint)
6. Spans present but missing pipeline stage names (instrumentation issue — but this is outside scope of this change)

### Affected modules

- No code changes
- Consumes outputs from tasks 1-6

### Prerequisites

- Docker + Docker Compose running
- Tasks 1-6 completed and passing
- GHC/Cabal installed for running Graphos
- A valid Graphos input path for the pipeline run

### Risks

- Trace ingestion lag — Tempo may take several seconds to index a trace; polling required
- Port 3200 conflict from external `solario-tempo` container (known from earlier task checks) — must resolve before e2e test
- If Graphos runs with a minimal input, it may not exercise all 7 pipeline stages (depends on input path)
- Collector batch processor delays — spans may be buffered and not immediately visible in Tempo

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next. -->
