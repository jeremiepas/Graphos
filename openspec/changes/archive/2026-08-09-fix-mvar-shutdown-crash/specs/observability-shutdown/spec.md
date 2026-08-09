## ADDED Requirements

### Requirement: Observability shutdown timeout
`shutdownObservability` SHALL complete within a per-component timeout of 5 seconds per sub-cleanup (metrics server cancel, OTLP SDK flush, debug trace flush). Each sub-cleanup MUST be wrapped independently in `System.Timeout.timeout` with `Control.Exception.catch` so that one component hanging does not prevent other components from cleaning up. If any sub-cleanup times out or throws an exception, the function SHALL log a warning and continue to the next sub-cleanup.

- Plan: Prevent MVar block propagation from one OTel component to all others during shutdown.
- Do: Wrap each sub-cleanup in `timeout 5000000` with `catch (\e -> hPutStrLn stderr ... >> pure ())`.
- Check: Scenarios below verify timeout behavior and continued cleanup after a hanging component.
- Act: If legitimate OTLP flushes exceed 5s in production, increase default or make per-component timeout configurable.

#### Scenario: Metrics server cancel times out
- **WHEN** the metrics server `cancel` does not complete within 5 seconds
- **THEN** `shutdownObservability` SHALL log a warning containing "metrics server" and "timeout" and proceed to OTLP SDK flush

#### Scenario: OTLP SDK flush times out
- **WHEN** `shutdownTracerProvider` does not return within 5 seconds (e.g., OTLP collector at localhost:4318 unavailable)
- **THEN** `shutdownObservability` SHALL log a warning containing "OTLP" and "timeout" and proceed to debug trace flush

#### Scenario: All sub-cleanups succeed
- **WHEN** all sub-cleanups (metrics server cancel, OTLP SDK flush, debug trace flush) complete within their 5-second timeouts
- **THEN** `shutdownObservability` SHALL return `()` without logging any timeout warnings

### Requirement: Pipeline call-site shutdown timeout
The pipeline SHALL wrap the call to `shutdownObservability` in `System.Timeout.timeout` with a configurable duration (default: 10 seconds). If the overall shutdown exceeds this duration, the pipeline SHALL log a warning and treat the shutdown as completed (not an error). The pipeline exit code SHALL be 0 when the pipeline stages completed successfully, regardless of whether the shutdown timed out.

- Plan: Defense in depth — even if a new infrastructure component blocks in shutdown, the pipeline will not hang indefinitely.
- Do: Add `timeout (otelShutdownTimeout * 1000000) (shutdownObservability env)` at both pipeline call sites in `UseCase.Pipeline`. On `Nothing`, log warning.
- Check: Scenarios verify exit code 0 after successful pipeline run with timeout, and non-zero exit when pipeline itself fails.
- Act: If timeout default proves too short, increase or make configurable via environment variable in addition to CLI flag.

#### Scenario: Successful pipeline with shutdown timeout
- **WHEN** all pipeline stages complete successfully and `shutdownObservability` exceeds the configured timeout
- **THEN** the process SHALL exit with code 0 and the graph output file SHALL be valid

#### Scenario: Failed pipeline
- **WHEN** a pipeline stage fails (returns `Left` error)
- **THEN** the process SHALL exit with a non-zero exit code regardless of whether shutdown succeeds

#### Scenario: No MVar error on shutdown
- **WHEN** the pipeline runs with `--otel` flag and the OTLP collector is not running at localhost:4318
- **THEN** the process SHALL NOT produce "thread blocked indefinitely in an MVar operation" error and SHALL exit with code 0

### Requirement: CLI flag for OTel shutdown timeout
The CLI SHALL accept a `--otel-shutdown-timeout` flag with an integer argument (seconds, default: 10). The flag SHALL be available under the existing observability option group. The value SHALL be passed to `runPipeline` and used as the overall timeout for `shutdownObservability` at the pipeline call site.

- Plan: Make shutdown grace period discoverable and configurable for different network environments.
- Do: Add `--otel-shutdown-timeout INT` to the optparse-applicative parser, pass through `PipelineConfig` or equivalent.
- Check: Scenario verifies flag acceptance and override of default timeout.
- Act: If users need sub-second precision, extend flag to accept fractional seconds.

#### Scenario: Custom timeout via CLI flag
- **WHEN** the user runs `graphos --otel-shutdown-timeout 2 <path>`
- **THEN** the pipeline SHALL use 2 seconds as the overall `shutdownObservability` timeout instead of the default 10 seconds

#### Scenario: Default timeout without flag
- **WHEN** the user runs `graphos <path>` without `--otel-shutdown-timeout`
- **THEN** the pipeline SHALL use 10 seconds as the default `shutdownObservability` timeout