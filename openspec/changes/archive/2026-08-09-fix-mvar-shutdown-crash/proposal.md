## Why

The pipeline completes all 7 stages and saves graph.json, but crashes during shutdown with "thread blocked indefinitely in an MVar operation". The error is non-blocking for data (graph.json is saved before the crash) but causes a non-zero exit code and confusing error message.

Root cause analysis reveals the crash originates from `hs-opentelemetry-sdk`'s internal batch span processor, which spawns background threads managed by MVars. During `shutdownTracerProvider`, these threads must flush pending spans and terminate, but they can block indefinitely if:
1. The OTLP collector at `localhost:4318` is not running — the HTTP POST to flush spans hangs until connection timeout
2. The `cancel` of the metrics server thread via `async` causes a race with the OTLP exporter's MVar synchronization
3. The `disconnectLSP` timeout (5s) may not be enough if the LSP server is slow to respond

The `fix-pipeline-e2e` change already replaced `forkIO` with `async`/`cancel` for the metrics server and added timeouts for LSP disconnect, but the underlying hs-opentelemetry-sdk thread management was not addressed.

## What Changes

- Wrap `shutdownObservability` in a `System.Timeout.timeout` (10-second limit) at the pipeline call site, so the process exits cleanly even if the OTLP SDK hangs during cleanup
- Add explicit graceful degradation: if the shutdown timeout fires, log a warning and exit with success code (0) since all data was already saved
- Wrap the `shutdownTracerProvider` call inside `shutdownObservability` in its own timeout (5s) with exception catching, so one component's hang doesn't prevent others from cleaning up
- Add a `--otel-timeout` CLI flag for configuring the shutdown timeout (default: 10s)

## Capabilities

### Modified Capabilities
- `observability`: Shutdown must complete within configurable timeout; graceful degradation when OTLP collector unavailable

## Impact

- `UseCase.Pipeline`: Two call sites where `shutdownObservability` is called — wrap in timeout
- `Infrastructure.Observability.SDK`: `shutdownObservability` — wrap `shutdownTracerProvider` in timeout + catch
- `app/Main.hs`: Exit code handling — treat shutdown timeout as non-fatal when pipeline data was saved
- `Infrastructure.Observability`: Same timeout wrapping for the non-SDK path

## PDCA Cycle

- **Plan**: Pipeline exits with code 0 after successful run, even when OTLP collector is unavailable. `--otel-timeout` flag controls shutdown grace period. Measured by: `cabal run graphos -- .` exits 0, no "thread blocked" error, graph.json valid.
- **Do**: Add timeout wrappers at pipeline call site and inside shutdownObservability. Add `--otel-timeout` flag. Handle exit codes.
- **Check**: `cabal run graphos -- .` exits with code 0. No "thread blocked" error. graph.json valid with ≥5 communities. `cabal test` green. `cabal build` zero warnings.
- **Act**: Document shutdown behavior. Record OTLP collector dependency for clean shutdown. Feed findings into next iteration if other MVar issues surface.