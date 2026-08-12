# Task 4 — Fix MVar deadlock in pipeline shutdown — PLAN

**Task slug**: `04-fix-mvar-deadlock`
**Attempt**: 1
**Status**: pending

## Summary

Fix the MVar deadlock in pipeline shutdown by replacing bare `forkIO`-based metrics server with `async`-based management. The metrics server thread will be tracked as `Maybe (Async ())` in `ObservabilityEnv` and cancelled via `cancel` in `shutdownObservability` before flushing, preventing indefinite blocking on cleanup.

## Detail

### Scope

- **File to modify**: `Infrastructure.Observability` — replace `forkIO` with `async` from `Control.Concurrent.Async`
- **New field**: `otelServerThread :: Maybe (Async ())` in `ObservabilityEnv`
- **Shutdown change**: Call `cancel otelServerThread` in `shutdownObservability` to terminate metrics server thread before flush
- **Fallback**: Apply same pattern to `Observability.SDK` (used by Pipeline) if separate

### Check Criteria

**What tests/gates will be run:**
- `cabal build` — zero warnings (with `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`)
- `cabal test` — all tests pass (90/90)
- `cabal run graphos -- .` — pipeline completes without "thread blocked indefinitely in an MVar operation" error

**What spec scenarios this task must satisfy:**
- `pipeline-shutdown` spec — **"Pipeline completes without MVar deadlock"**: WHEN `cabal run graphos -- .` runs on a repository, THEN the pipeline SHALL exit cleanly with exit code 0, no "thread blocked indefinitely" error.
- `pipeline-shutdown` spec — **"Observability thread timeout on shutdown"**: WHEN the Prometheus metrics server thread does not respond within 5 seconds, THEN the pipeline SHALL log `[WARN] Metrics server shutdown timed out` and continue with remaining cleanup.

**What the exact PASS conditions are:**
1. `cabal build` exits with code 0, zero warnings
2. `cabal test` exits with code 0, all 90 tests pass
3. `cabal run graphos -- .` exits with code 0, no "thread blocked indefinitely" in stderr
4. `ObservabilityEnv` contains `otelServerThread :: Maybe (Async ())`
5. `shutdownObservability` calls `cancel` on the async thread before flush
6. If async thread doesn't respond within 5s, logs `[WARN] Metrics server shutdown timed out`

**What would constitute a FAIL:**
- `cabal run graphos -- .` still crashes with MVar deadlock during shutdown
- `cabal test` fails due to missing `async` imports or type changes in `ObservabilityEnv`
- Metrics server thread not cancelled — still blocks on shutdown
- Graceful shutdown degraded — data not saved before process exit

### Affected Modules

| Module | Layer |
|--------|-------|
| `Infrastructure.Observability` | Infrastructure — `ObservabilityEnv`, `shutdownObservability`, metrics server setup |
| `Infrastructure.Observability.SDK` | Infrastructure — may need same pattern (used by Pipeline) |
| `UseCase.Pipeline` | UseCase — no changes needed; depends on Observability cleanup |

### Prerequisites

- `async` package available in cabal dependencies
- `ObservabilityEnv` type and `shutdownObservability` function identified
- Understanding of current `forkIO` placement in metrics server startup

### Risks

| Risk | Mitigation |
|------|------------|
| `async` cancel may not interrupt blocked thread | Wrap server loop in `withAsync` + `cancel` pattern; 5s timeout as final safety |
| STM `TVar` alternative too large a refactor | Stick to `async`/`cancel` pattern — minimal change per design decision D3 |
| Metrics server thread must finish flush before exit | Cancel after flush attempt; flush uses MVar which should respond if not blocked |
| Both `Observability.hs` and `Observability.SDK` may need fix | Audit both files; apply pattern to whichever one owns the metrics thread |

## Result

Pending — first cycle.
