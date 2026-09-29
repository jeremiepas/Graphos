## Purpose

Memory-budget-guard: guarantee every pipeline run operates inside an explicit
memory budget derived from the machine (or set by the user), fails gracefully
when the budget is exhausted, and refuses to start work that cannot fit — so
no run ever ends in a kernel OOM kill or an opaque RTS abort. (workflow 01;
PRD §16.1 scale targets)

## ADDED Requirements

### Requirement: Derived default heap budget

The system SHALL derive a default heap budget from the machine's available
memory when `--max-heap` is not given on the CLI and no heap size is set in
graphos.yaml: budget = MemAvailable − safety reserve, clamped to
[512 MB, 32 GB]. The system SHALL log the derived value and the input values
at INFO on stderr before the pipeline starts, and SHALL run under that RTS
heap limit. When MemAvailable cannot be read (non-Linux), the system SHALL
warn and run uncapped (current behavior).

#### Scenario: Default budget derived and applied
- **WHEN** `graphos ./src` runs on a machine with 20 GB available and no `--max-heap` flag
- **THEN** the process re-executes with an RTS `-M` limit equal to the derived default
- **AND** a `[memory]` INFO line states the budget value and that it was derived from available memory

#### Scenario: Explicit max-heap wins
- **WHEN** `--max-heap 2G` is passed explicitly
- **THEN** the derived default is not computed and 2G is used as the RTS limit

#### Scenario: Unavailable memory info degrades gracefully
- **WHEN** MemAvailable cannot be read (e.g. non-Linux platform)
- **THEN** a WARN is logged ("no memory budget applied") and the run proceeds without an RTS limit

### Requirement: Pre-flight memory guard

Before the pipeline starts, the system SHALL read the machine's available
memory and compare it to the active heap budget. When available memory is
less than the budget, the system SHALL log a WARNING naming the shortfall.
When `--fail-on-low-memory` is set, the system SHALL instead exit before any
stage runs with an error that names the budget, available memory, and the
flag that requested strictness. The embedding pass additionally SHALL log a
projected in-memory footprint (node count × vector size) at INFO before its
first batch, and SHALL refuse to start when available memory cannot hold the
projection.

#### Scenario: Low memory warns by default
- **WHEN** the pipeline starts with a 8G budget on a machine with 2 GB available
- **THEN** a WARNING naming both values is logged and the pipeline continues

#### Scenario: Strict mode aborts before stages
- **WHEN** `--fail-on-low-memory` is set and available memory is below the budget
- **THEN** the process exits non-zero before Detect runs, with an error naming budget, available, and the flag

#### Scenario: Embedding projection logged
- **WHEN** the embedding pass starts on a graph of 100,000 nodes with 1024-dim vectors
- **THEN** an INFO line states the projected assignment footprint before the first batch is sent

### Requirement: Graceful heap-exhaustion failure

When the RTS heap limit is reached during a stage, the system SHALL surface a
failure that names the stage in progress, the configured budget, and the
resume command; it SHALL NOT lose the last completed stage's checkpoint. The
exit SHALL be controlled (non-zero status, single error message) even though
the RTS raises an async HeapOverflow exception.

#### Scenario: Heap overflow mid-stage preserves checkpoint
- **WHEN** the pipeline exceeds its `-M` budget during clustering and the Build checkpoint exists
- **THEN** the checkpoint file is intact and a subsequent run resumes from it
- **AND** the error message names the failed stage and the budget value

#### Scenario: Error names budget and remedy
- **WHEN** a heap-exhaustion failure occurs
- **THEN** the message includes the budget size and suggests `--max-heap` with a larger value or reducing scope