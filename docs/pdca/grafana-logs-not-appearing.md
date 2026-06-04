# PDCA: Logs Not Appearing in Grafana When CLI Runs

**Date**: 2026-05-22
**Status**: Plan → Do → Check → Act

---

## Plan — Root Cause Analysis

### Problem
When running the Graphos CLI with `--otel --debug`, logs do not appear in Grafana (Loki data source).

### Root Causes Identified

#### 1. **Pipeline creates a SEPARATE LogEnv (CRITICAL)**
- **File**: `src/Graphos/UseCase/Pipeline.hs`, lines 74 and 310
- `env <- defaultLogEnv logLevel` creates a **new** `LogEnv` that has **no OTLP log shipping enabled**
- `initObservability` creates its own `logEnv` and enables OTLP log shipping on it (line 564)
- The pipeline then uses `env` (the bare one) for all `logInfo`, `logDebug`, `logTrace` calls
- Those calls go to `shipLogToOtlp`, which checks `leOtlpConfig` → finds `Nothing` → does nothing
- **Result**: All pipeline logs are console-only; none reach the OTLP Collector → Loki

#### 2. **Log shipping requires `--debug` flag AND `--otel` (by design but confusing)**
- `initObservability` line 562: `when (logLevel >= LevelTrace) $ enableOtlpLogShipping ...`
- `LevelTrace` requires `--debug` flag
- Running `--otel` without `--debug` means NO log shipping at all
- This is overly restrictive — users running `--otel` likely want logs in Grafana at any level

#### 3. **OTLP Collector endpoint mismatch**
- `defaultOtelConfig` has `otelLogsEndpoint = "http://localhost:4318/v1/logs"`
- Docker compose maps OTel Collector to port **14319** on the host: `"14319:4318"`
- So the CLI must use `http://localhost:14319/v1/logs` when running on the host
- The `--otel-endpoint` flag exists, but the default is **wrong** for the Docker Compose setup
- Comment in `docker-compose.yml` says `http://localhost:14318` for "OTLP HTTP — via Tempo" and `14319` for "direct to Collector"

#### 4. **`httpPostLog` silently swallows errors**
- Line 201: `catch (\(_ :: SomeException) -> pure (error "ignored"))` 
- If the OTLP Collector is unreachable, the error is completely silenced
- No way to debug "why are logs not appearing?" from the CLI output

#### 5. **`flushOtlpLogs` race condition with `tryTakeMVar`**
- `flushOtlpLogs` uses `tryTakeMVar` which empties the MVar
- If a concurrent `shipLogToOtlp` is also modifying the buffer, there's a potential for lost logs
- The `modifyMVar_` in `shipLogToOtlp` replaces the buffer content, but `flushOtlpLogs` racing could miss entries

### Data Flow Diagram (Current — Broken)

```
CLI (graphos . --otel --debug)
  │
  ├─ initObservability → logEnv (OTLP enabled) ← stored in ObservabilityEnv.otelLogEnv
  │                                        │
  │                                        └── enableOtlpLogShipping(logEnv, ...) ✓
  │
  ├─ Pipeline.runPipeline
  │     │
  │     ├─ env <- defaultLogEnv logLevel    ← NEW LogEnv, OTLP DISABLED ✗
  │     │
  │     └─ logInfo env "..."                ← uses bare `env`, NO OTLP shipping ✗
  │        └─ shipLogToOtlp env ...         ← leOtlpConfig = Nothing → NOOP ✗
  │
  └─ Result: No logs reach OTLP Collector → No logs in Loki → No logs in Grafana
```

### Data Flow Diagram (Fixed)

```
CLI (graphos . --otel [--debug])
  │
  ├─ initObservability → obsEnv.otelLogEnv ← OTLP enabled for ALL log levels when --otel
  │
  ├─ Pipeline.runPipeline
  │     │
  │     └─ logInfo (otelLogEnv obsEnv) "..." ← uses ObservabilityEnv's logEnv ✓
  │        └─ shipLogToOtlp ...              ← leOtlpConfig = Just ... → ships to OTLP ✓
  │
  └─ Result: Logs reach OTLP Collector → Loki → Grafana ✓
```

---

## Do — Changes

### Change 1: Pipeline uses `otelLogEnv` from `ObservabilityEnv` instead of creating a new one
- **File**: `src/Graphos/UseCase/Pipeline.hs`
- Remove `env <- defaultLogEnv logLevel` (lines 74, 310)
- Use `otelLogEnv obsEnv` for all log calls

### Change 2: Enable OTLP log shipping when `--otel` is active (not just `--debug`)
- **File**: `src/Graphos/Infrastructure/Observability.hs`
- Change `when (logLevel >= LevelTrace)` to always enable when `otelEnabled`
- When `--otel` is active, all levels of logs should ship (INFO and above at minimum)

### Change 3: Add log level filtering in OTLP shipping, not in init
- **File**: `src/Graphos/Infrastructure/Logging.hs`
- `shipLogToOtlp` should ship all logs that pass the visibility threshold
- The `logLevel` on `LogEnv` already controls which logs are emitted
- No need for a second gate in `initObservability`

### Change 4: Fix default OTLP endpoint for Docker Compose
- **File**: `src/Graphos/Infrastructure/Observability.hs`
- Change default endpoint comment/docs to note port 14319 for Docker Compose
- Or better: update `docker-compose.yml` comments

### Change 5: Improve error visibility in OTLP log shipping
- **File**: `src/Graphos/Infrastructure/Logging.hs`
- Log a warning when HTTP POST to OTLP Collector fails instead of silently ignoring

---

## Check — Verification

- [ ] Build succeeds: `cabal build`
- [ ] CLI runs with `--otel` alone (without `--debug`) and ships INFO+ logs
- [ ] CLI runs with `--otel --debug` and ships ALL logs
- [ ] Docker Compose is up, Grafana shows logs from a CLI run
- [ ] Pipeline uses the correct LogEnv (not a stale one)

---

## Act — Follow-up

- Consider making the minimum shipped level configurable (e.g., `--otel-log-level`)
- Consider adding a health-check on startup that pings the OTLP Collector endpoint
- Document the port mapping (14319 = host port for OTel Collector) in README