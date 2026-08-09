# Observability Consolidation Capability

## Purpose

Maintain exactly one observability implementation to eliminate dead/duplicate code, reduce maintenance burden, and prevent parallel implementations from drifting.
## Requirements
### Requirement: Single observability implementation

The project SHALL contain exactly one observability implementation, `Graphos.Infrastructure.Observability.SDK`. The dead parallel module `Graphos.Infrastructure.Observability` SHALL be removed from the source tree and from `graphos.cabal`.

#### Scenario: Dead module removed

- **WHEN** the source tree and `graphos.cabal` are inspected
- **THEN** `src/Graphos/Infrastructure/Observability.hs` does not exist and `Graphos.Infrastructure.Observability` is not listed in exposed-modules

#### Scenario: Build and tests unaffected

- **WHEN** `cabal build` and `cabal test` run after removal
- **THEN** both succeed with no missing-module errors, confirming the module was unreferenced

#### Scenario: Observability behavior preserved

- **WHEN** the pipeline runs with observability enabled
- **THEN** spans, histograms, and debug traces behave identically to before the removal (SDK.hs untouched by this change)

### Requirement: Span durations measure forced work

Pipeline span timing (PRD §10 observability, §16.1 performance targets) MUST force evaluation of the measured computation to normal form between the span's start and end timestamps. Pure stage results (build, cluster) SHALL be forced with `deepseq`/`evaluate` inside the timed window so recorded durations reflect actual computation, not thunk creation.

#### Scenario: Build and cluster spans report real durations

- **WHEN** the pipeline runs on a repository-scale input (thousands of nodes) with debug tracing enabled
- **THEN** the emitted `span_build` and `span_cluster` events report durations of at least one millisecond, not nanoseconds

#### Scenario: Work is not attributed to neighboring spans

- **WHEN** the build stage constructs the graph
- **THEN** the graph is in normal form before `span_build` ends, so subsequent spans do not absorb build work

### Requirement: Debug trace directory created only on demand

The debug-trace environment (PRD §10.4) MUST NOT create any directory or file when tracing is disabled. When tracing is enabled, the trace directory SHALL be created lazily at flush time, and only when at least one event was buffered. A `traces/` directory SHALL exist if and only if a trace JSONL file was written into it.

#### Scenario: Disabled tracing leaves no folder

- **WHEN** the pipeline runs with debug tracing disabled
- **THEN** no `traces/` directory is created in the output or working directory

#### Scenario: Enabled but eventless run leaves no folder

- **WHEN** debug tracing is enabled but no events are emitted before shutdown
- **THEN** no trace directory or file is created

#### Scenario: Enabled run with events writes folder and file together

- **WHEN** debug tracing is enabled and at least one span event is emitted
- **THEN** the trace directory exists and contains exactly the flushed JSONL file, verifiable by `cabal test`

