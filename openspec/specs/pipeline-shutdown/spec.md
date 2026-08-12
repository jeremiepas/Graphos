# pipeline-shutdown Specification

## Purpose
TBD - created by archiving change fix-pipeline-e2e. Update Purpose after archive.
## Requirements
### Requirement: Pipeline shutdown — no MVar deadlock

Module `Graphos.UseCase.Pipeline` SHALL complete the pipeline without blocking indefinitely. All MVar operations in the shutdown path (observability thread cleanup, LSP server disconnect, metrics server shutdown) SHALL be wrapped in `System.Timeout.timeout` with a 5-second limit. If a timeout occurs, the pipeline SHALL log a warning and continue with cleanup of remaining resources. (PRD §3.2, workflow 01 stage 7)

#### Scenario: Pipeline completes without MVar deadlock
- **WHEN** `cabal run graphos -- .` runs on a repository
- **THEN** the pipeline SHALL exit cleanly with exit code 0, no "thread blocked indefinitely" error

#### Scenario: Observability thread timeout on shutdown
- **WHEN** the Prometheus metrics server thread does not respond within 5 seconds
- **THEN** the pipeline SHALL log `[WARN] Metrics server shutdown timed out` and continue with remaining cleanup

