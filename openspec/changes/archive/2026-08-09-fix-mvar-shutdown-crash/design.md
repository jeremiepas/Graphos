## Context

The `fix-pipeline-e2e` change fixed the metrics server thread (now uses `async`/`cancel`) and LSP disconnect (now has 5s timeout), but the pipeline still crashes with "thread blocked indefinitely in an MVar operation" during `shutdownObservability`. The crash occurs after all data is saved. The root cause is `hs-opentelemetry-sdk`'s internal batch processor threads that use MVars for synchronization — when `shutdownTracerProvider` tries to flush spans to an unavailable OTLP collector, those threads block indefinitely on MVar operations.

## Goals / Non-Goals

**Goals:**
- Pipeline exits with code 0 after successful run, even when OTLP collector is unavailable
- No "thread blocked indefinitely in an MVar operation" error in logs
- Configurable shutdown timeout for OTel cleanup
- Graceful degradation: data saved = success, even if cleanup times out

**Non-Goals:**
- Fixing hs-opentelemetry-sdk internals (upstream library)
- Eliminating all background threads (some are necessary for OTel functionality)
- Adding OTLP collector health checks before pipeline start

## Decisions

### D1: Timeout wrapper at pipeline call site

**Decision**: Wrap `shutdownObservability` in `System.Timeout.timeout` at the two pipeline call sites in `UseCase.Pipeline`. If timeout fires, log warning and treat as success (pipeline data already saved).

**Alternatives considered:**
- A) Fix inside `shutdownObservability` only — doesn't protect against future MVar blocks from new infrastructure
- B) **Timeout at call site** — defense in depth; protects against any component blocking in shutdown
- C) Use GHC's `-threaded` RTS option `+RTS -xp` for pre-emption — unreliable, doesn't solve MVar blocking

**Rationale**: Option B is the most robust — even if a new infrastructure component blocks in the future, the pipeline won't hang. The call site is the natural place because the pipeline already catches `SomeException` and knows whether data was saved.

### D2: Timeout inside `shutdownObservability` for each component

**Decision**: Inside `shutdownObservability`, wrap each sub-cleanup (metrics server cancel, OTLP SDK flush, debug trace flush) in its own timeout with catch. Log warnings for any component that times out, continue with next cleanup.

**Alternatives considered:**
- A) Single timeout for entire shutdown — one slow component prevents others from cleaning up
- B) **Per-component timeout** — each component gets independent 5s; one hanging doesn't block others
- C) No timeout, rely on call site — risky, SDK could hang forever

**Rationale**: Per-component timeout ensures graceful degradation. If `shutdownTracerProvider` hangs, the debug trace flush still runs. Log messages make debugging easier.

### D3: Exit code handling in Main.hs

**Decision**: When the pipeline's `runPipeline` returns `Right result` (success), but shutdown cleanup fails, exit with code 0. Only exit with failure if the pipeline itself failed.

**Alternatives considered:**
- A) Always exit non-zero on any error — misleading; data is valid
- B) **Exit 0 if data saved, non-zero only if pipeline failed** — matches user expectation
- C) New exit code 2 for "success with cleanup warnings" — over-engineering

**Rationale**: Users care about whether their graph was produced. A cleanup timeout is a warning, not a failure.

### D4: `--otel-shutdown-timeout` CLI flag

**Decision**: Add `--otel-shutdown-timeout` flag (default: 10s) to control how long the pipeline waits for OTel cleanup.

**Alternatives considered:**
- A) No flag, hardcoded 10s — inflexible for slow networks
- B) **Configurable flag** — lets users tune for their environment
- C) Read from `OTEL_SHUTDOWN_TIMEOUT` env var — less discoverable

**Rationale**: A CLI flag is discoverable (`--help`) and explicit. Default 10s is generous for most setups.

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| Timeout too short for legitimate slow OTLP flush | Default 10s; configurable via `--otel-shutdown-timeout` |
| Data loss if spans not flushed | Acceptable: spans are diagnostic, not user data; pipeline output already saved |
| New background threads in future could block | Call-site timeout is defense in depth |
| `cancel` on async thread may not immediately stop Warp server | Warp uses `runSettings` which blocks; `cancel` throws `AsyncException` to the thread |

## Verification Strategy (Check)

1. `cabal build` — zero warnings
2. `cabal test` — all tests pass
3. `cabal run graphos -- .` — exits with code 0, no "thread blocked" error
4. `cabal run graphos -- . --otel` — exits with code 0 even without OTLP collector running
5. `cabal run graphos -- . --otel-shutdown-timeout 2` — exits quickly with short timeout
6. Verify graph.json is valid after run

## Iteration & Rollback (Act)

- If timeout causes data loss: increase default or remove call-site timeout
- If `cancel` on Warp causes resource leaks: switch to `runSettingsSocket` with explicit socket close
- If hs-opentelemetry-sdk fixes MVar handling in future version: remove per-component timeouts
- Document shutdown behavior and OTLP collector dependency in README