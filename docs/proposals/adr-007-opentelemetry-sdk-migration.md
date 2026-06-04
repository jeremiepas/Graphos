# ADR-007: Migrate from Custom OTLP Implementation to hs-opentelemetry-sdk

**Date**: 2026-05-22
**Status**: Under Review
**Owner**: Graphos Contributors

---

## Context

Graphos currently implements OpenTelemetry tracing, metrics, and log shipping using **~918 lines of hand-rolled Haskell** (`Observability.hs` + `Logging.hs`). This custom implementation has caused real production bugs:

1. **Logs never reached Grafana** — Pipeline created a separate `LogEnv` without OTLP shipping enabled (fixed in ADR-006). The bug existed because the custom code had no type-level guarantee that the `LogEnv` flowing through the pipeline was connected to OTLP.

2. **OTLP JSON format is hand-concatenated strings** — No schema validation, no protobuf support, fragile to spec changes.

3. **Errors silently swallowed** — `httpPostLog` caught all exceptions and discarded them, making debugging impossible.

4. **No W3C trace context propagation** — Custom `IORef (Maybe Text)` for trace IDs doesn't support traceparent/tracestate headers, breaking distributed trace correlation.

5. **No spec compliance** — Missing: sampling, baggage, span links, span events in OTLP format, proper metric temporality, log severity mapping is approximate.

### Current Custom Implementation

| File | Lines | Purpose |
|------|-------|---------|
| `Infrastructure/Observability.hs` | 611 | Tracer, Span, MetricsStore, OtelConfig, OTLP JSON encoding, Prometheus HTTP server, background exporter |
| `Infrastructure/Logging.hs` | 307 | LogEnv, OtlpLogConfig, log levels, OTLP log shipping, buffer/flush |

**Total: ~918 lines of custom OTLP code**

### hs-opentelemetry-sdk Overview

[`hs-opentelemetry-sdk`](https://hackage.haskell.org/package/hs-opentelemetry-sdk) is the official Haskell OpenTelemetry SDK providing:

- **TracerProvider** with batch processor and OTLP HTTP exporter
- **Environment variable configuration** (`OTEL_EXPORTER_OTLP_ENDPOINT`, `OTEL_SERVICE_NAME`, etc.)
- **W3C trace context propagation** (traceparent/tracestate headers)
- **Proper protobuf/JSON OTLP encoding** (spec-compliant)
- **Log bridge** (via `hs-opentelemetry-logging`)
- **Built-in shutdown** with flush guarantees

Key packages:

| Package | Purpose |
|---------|---------|
| `hs-opentelemetry-sdk` | Core SDK: TracerProvider, sampling, processors |
| `hs-opentelemetry-exporter-otlp` | OTLP HTTP exporter (traces + metrics) |
| `hs-opentelemetry-logging` | Log bridge: emits Haskell log records as OTLP log records |
| `hs-opentelemetry-propagator-w3c` | W3C trace context propagation |

---

## Decision

**Migrate from the custom OTLP implementation to `hs-opentelemetry-sdk`**, replacing `Observability.hs` and `Logging.hs` with the official SDK while preserving the Domain/UseCase/Infrastructure layer boundary.

---

## Rationale

| Factor | Custom (Current) | hs-opentelemetry-sdk |
|--------|-------------------|----------------------|
| **Spec compliance** | Partial, hand-rolled JSON | Full OTLP spec, protobuf+JSON |
| **Trace context** | Custom `IORef (Maybe Text)` | W3C traceparent/tracestate |
| **Error handling** | Silently swallowed | Proper error reporting |
| **Config** | CLI flags only | CLI flags + env vars (`OTEL_*`) |
| **Maintainability** | 918 lines we maintain | Upstream maintained |
| **Bug surface** | High (already had LogEnv bug) | Low — battle-tested library |
| **Log shipping** | Custom MVar buffer + flush | SDK built-in log bridge |
| **Metrics** | Custom IORef maps + Prometheus renderer | SDK MeterProvider + OTLP metrics |

The custom implementation was reasonable as a bootstrap, but has proved fragile. The `LogEnv` bug demonstrated that the lack of type-level guarantees in the custom approach leads to subtle runtime failures.

---

## Migration Plan

### Phase 1: Add SDK dependency, keep current API

**Goal**: Introduce `hs-opentelemetry-sdk` alongside the custom implementation, no behavior change.

1. Add `hs-opentelemetry-sdk`, `hs-opentelemetry-exporter-otlp`, `hs-opentelemetry-logging` to `graphos.cabal` build-depends
2. Create `Graphos.Infrastructure.Observability.SDK` — new module wrapping the official SDK
3. Keep `Observability.hs` and `Logging.hs` untouched
4. `SDK` module provides the same `ObservabilityEnv` type and `initObservability` / `shutdownObservability` API

**File changes**:
- `graphos.cabal` — add dependencies
- `src/Graphos/Infrastructure/Observability/SDK.hs` — new SDK wrapper (~150 lines)

### Phase 2: Switch pipeline to SDK

**Goal**: Replace the custom implementation with the SDK wrapper.

1. Update `Pipeline.hs` and `Main.hs` to use `Observability.SDK` instead of `Observability`
2. Remove custom tracer (`Tracer`, `Span`, `SpanKind`, `SpanStatus`, `withSpan`, etc.)
3. Remove custom metrics (`MetricsStore`, `incCounter`, `setGauge`, `observeHistogram`, `renderPrometheusMetrics`)
4. Remove custom log shipping (`OtlpLogConfig`, `shipLogToOtlp`, `flushOtlpLogBuffer`, `httpPostLog`)
5. Remove custom OTLP JSON encoding (`encodeOTLPTraces`, `encodeOTLPMetrics`, `encodeSpan`, etc.)
6. Remove custom HTTP POST (`httpPost`)
7. Keep `Logging` module for the log level + console output, but remove OTLP shipping from it
8. Keep `DebugTraceEnv` (structured JSONL tracing to disk) — this is Graphos-specific, not OTel

**File changes**:
- `src/Graphos/UseCase/Pipeline.hs` — use SDK `ObservabilityEnv`
- `app/Main.hs` — use SDK `ObservabilityEnv`
- `src/Graphos/Infrastructure/Observability.hs` — gut: remove Tracer, MetricsStore, OtelConfig, OTLP encoding, HTTP, Prometheus server; keep only `DebugTraceEnv`
- `src/Graphos/Infrastructure/Logging.hs` — remove OTLP shipping, keep console logging + log levels

**Estimated net reduction**: ~750 lines removed, ~150 lines added (SDK wrapper) = **~600 lines saved**

### Phase 3: Clean up and harden

1. Remove `http-conduit` / `http-client` from `graphos.cabal` if no longer used elsewhere
2. Remove `wai` / `warp` if Prometheus server is replaced by OTLP metrics push
3. Update `docker-compose.yml` environment variables (`OTEL_SERVICE_NAME`, `OTEL_EXPORTER_OTLP_ENDPOINT`)
4. Add `OTEL_*` env var documentation to README
5. Update Grafana dashboards if metric/label names change
6. Full test coverage for SDK initialization and shutdown

---

## Impact on Architecture

### Clean Architecture Preservation

The migration **preserves the layer boundary**. The SDK wrapper stays in `Infrastructure/`:

```
Domain/         — No change (pure, no IO)
UseCase/        — No change (uses ObservabilityEnv interface)
Infrastructure/
  ├── Observability/
  │   └── SDK.hs      — NEW: wraps hs-opentelemetry-sdk
  │   └── Legacy.hs   — TEMPORARY: old custom impl (during Phase 1)
  ├── Logging.hs      — Simplified: console-only, no OTLP
  └── DebugTrace.hs   — Extracted from Observability.hs
```

### API Surface (unchanged)

```haskell
-- What Pipeline.hs and Main.hs see (interface preserved)
data ObservabilityEnv = ObservabilityEnv
  { otelTracer     :: Tracer              -- now from hs-opentelemetry-sdk
  , otelMetrics    :: MetricsStore        -- now from SDK MeterProvider
  , otelDebugTrace :: DebugTraceEnv       -- still custom (Graphos-specific)
  , otelLogEnv     :: LogEnv              -- still custom (console + log bridge)
  }

initObservability :: LogLevel -> OtelConfig -> Maybe Int -> FilePath -> IO ObservabilityEnv
shutdownObservability :: ObservabilityEnv -> IO ()
```

The `OtelConfig` type is simplified — most fields become `OTEL_*` env vars:

```haskell
-- Before (custom)
data OtelConfig = OtelConfig
  { otelTracesEndpoint  :: String    -- replaced by OTEL_EXPORTER_OTLP_ENDPOINT env var
  , otelMetricsEndpoint :: String    -- replaced by OTEL_EXPORTER_OTLP_ENDPOINT env var
  , otelLogsEndpoint    :: String    -- replaced by OTEL_EXPORTER_OTLP_ENDPOINT env var
  , otelServiceName     :: String    -- replaced by OTEL_SERVICE_NAME env var
  , otelServiceVersion  :: String    -- replaced by OTEL_RESOURCE_ATTRIBUTES env var
  , otelExportInterval  :: Int       -- replaced by OTEL_BSP_SCHEDULE_DELAY env var
  , otelEnabled         :: Bool      -- replaced by OTEL_SDK_DISABLED env var
  }

-- After (SDK)
data OtelConfig = OtelConfig
  { otelEnabled         :: Bool      -- CLI --otel flag
  , otelEndpoint        :: String    -- CLI --otel-endpoint flag (optional, env var fallback)
  , otelServiceName     :: String    -- CLI flag or OTEL_SERVICE_NAME
  }
```

### CLI Flags (preserved)

```bash
# Before (custom) — requires --debug for logs
graphos . --otel --debug

# After (SDK) — logs enabled with --otel alone
graphos . --otel

# Environment variable configuration also available
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:14319 graphos . --otel
OTEL_SERVICE_NAME=graphos graphos . --otel
```

---

## Alternatives Considered

| Alternative | Pros | Cons | Why Rejected? |
|-------------|------|------|---------------|
| Keep custom implementation | No dependency; full control | 918 LOC to maintain; already buggy; no spec compliance | The LogEnv bug demonstrated it's fragile |
| Partial migration (SDK for traces, custom for metrics/logs) | Incremental; less risk | Mixed codebases; still custom metrics/log encoding | Added complexity without full benefit |
| Use `hs-opentelemetry-api` only (no SDK) | Lighter dependency | Still need custom processor/exporter | Doesn't solve the underlying problem |
| Use different language's OTel SDK (via FFI or sidecar) | Mature ecosystems (Go, Python) | FFI overhead; sidecar ops complexity; anti-Haskell | Defeats purpose of Haskell codebase |

---

## Risks

| Risk | Severity | Mitigation |
|------|----------|------------|
| `hs-opentelemetry-sdk` may lack GHC 9.10 compatibility | Medium | Check Hackage matrix; pin version; test early in Phase 1 |
| SDK API may differ from our custom interface | Low | Wrapper module isolates pipeline from SDK internals |
| Metric/label names may change (affecting Grafana dashboards) | Low | Phase 3 includes dashboard update; naming follows OTel conventions |
| Log bridge (`hs-opentelemetry-logging`) may be immature | Medium | If immature, keep custom console logging + SDK log bridge side by side |
| Debug trace (`DebugTraceEnv`) is Graphos-specific, not in SDK | None | Keep it — it writes local JSONL, unrelated to OTel protocol |

---

## Success Criteria

- [ ] `graphos . --otel` produces logs in Grafana (Loki) at INFO level without `--debug`
- [ ] `graphos . --otel` produces traces in Grafana (Tempo)
- [ ] `graphos . --otel` produces metrics in Grafana (Prometheus)
- [ ] `OTEL_EXPORTER_OTLP_ENDPOINT` env var works as alternative to `--otel-endpoint`
- [ ] `OTEL_SDK_DISABLED=true` disables all telemetry without code changes
- [ ] W3C trace context propagation works (traceparent header)
- [ ] Clean shutdown flushes all pending telemetry
- [ ] No custom OTLP JSON encoding remains in codebase
- [ ] `Observability.hs` reduced to < 100 lines (debug trace only)
- [ ] `Logging.hs` reduced to < 100 lines (console logging only)
- [ ] All existing tests pass
- [ ] Build succeeds with GHC 9.10

---

## Related

- `src/Graphos/Infrastructure/Observability.hs` — Custom OTLP implementation (to be replaced)
- `src/Graphos/Infrastructure/Logging.hs` — Custom log shipping (to be simplified)
- `docs/pdca/grafana-logs-not-appearing.md` — PDCA analysis of the LogEnv bug
- `docker-compose.yml` — OTel Collector, Loki, Tempo, Grafana setup
- `monitoring/otel-collector/otel-collector-config.yaml` — Collector pipeline config
- [`hs-opentelemetry-sdk` on Hackage](https://hackage.haskell.org/package/hs-opentelemetry-sdk)
- [`hs-opentelemetry` on GitHub](https://github.com/iand675/hs-opentelemetry)