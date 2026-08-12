<!--
  PDCA-PER-TASK workflow.
-->

# Task 6 — Start/stop scripts — PLAN

**Task slug**: `06-start-stop-scripts`
**Attempt**: 1
**Status**: pending

## Summary

Create `scripts/otel-up.sh` and `scripts/otel-down.sh` for one-command stack management with health polling and status output.

## Detail

### Scope

- Create directory: `scripts/`
- Create `scripts/otel-up.sh` with:
  - Prerequisite checks: `docker --version`, `docker compose version`
  - Run `docker compose -f docker-compose.otel.yaml up -d`
  - Poll health checks: loop with 10s interval, max 60s wait, check `docker compose -f docker-compose.otel.yaml ps` for "healthy" status
  - Print status table showing each service name and status
  - Exit 0 only when all services are healthy
- Create `scripts/otel-down.sh` with:
  - Run `docker compose -f docker-compose.otel.yaml down -v`
  - Print confirmation of removed containers and volumes

### Check Criteria

**What will be tested:**
1. Files exist at `scripts/otel-up.sh` and `scripts/otel-down.sh`
2. Files are executable (`test -x scripts/otel-up.sh && test -x scripts/otel-down.sh`)
3. `bash -n scripts/otel-up.sh` exits 0 (syntax valid)
4. `bash -n scripts/otel-down.sh` exits 0 (syntax valid)
5. `scripts/otel-down.sh` runs without error on a stopped stack
6. `scripts/otel-up.sh` completes within 60s and all services healthy (full integration test)

**Spec scenarios satisfied:**
- `SC-otel-up-healthy` (spec.md, Scenario: otel-up starts and waits for healthy) — WHEN `scripts/otel-up.sh` executed, THEN exits 0 only when all services report healthy
- `SC-otel-down-cleanup` (spec.md, Scenario: otel-down cleans up) — WHEN `scripts/otel-down.sh` executed after `scripts/otel-up.sh`, THEN all containers and volumes removed

**PASS conditions:**
1. Both files exist and are executable
2. Both files pass bash syntax check
3. `otel-down.sh` cleans up (no containers/volumes remain)
4. `otel-up.sh` starts stack and exits 0 with all services healthy

**FAIL conditions:**
1. Files missing or not executable
2. Bash syntax errors
3. `otel-up.sh` does not wait for health checks
4. `otel-up.sh` exits non-zero when services are healthy
5. `otel-down.sh` leaves containers or volumes behind

### Affected modules

- New files:
  - `scripts/otel-up.sh`
  - `scripts/otel-down.sh`
- No Haskell code changes
- Depends on task 3 (docker-compose.otel.yaml must exist)

### Prerequisites

- Docker + Docker Compose installed and running
- Task 3 completed (docker-compose.otel.yaml exists)

### Risks

- Health check polling timeout may need adjustment on slower hardware (60s is tight for 5 large containers)
- If `docker compose` command fails (e.g., Docker daemon not running), scripts should fail fast with a clear error message
- Race condition: `otel-up.sh` might check health before Grafana datasource provisioning finishes (~5s after first start)

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next. -->
