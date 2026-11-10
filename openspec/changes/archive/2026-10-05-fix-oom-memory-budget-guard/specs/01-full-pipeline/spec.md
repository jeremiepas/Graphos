## ADDED Requirements

### Requirement: Workflow 01 — memory budget at startup

Before Stage 1 Detect, the pipeline SHALL establish an active memory budget:
an explicit `--max-heap` when given, otherwise a machine-derived default
(available memory minus a safety reserve), otherwise uncapped with a warning.
The active budget SHALL be logged at INFO and SHALL hold for all seven stages
of the run. (Workflow 01; PRD §16.1 scale targets)

#### Scenario: Budget established before Detect
- **WHEN** `graphos ./src` starts with no explicit `--max-heap`
- **THEN** a `[memory]` INFO line with the derived budget is logged before the Detect stage's first output
- **AND** the process runs under the corresponding RTS heap limit

#### Scenario: Uncapped fallback warns
- **WHEN** available memory cannot be read and no `--max-heap` is given
- **THEN** a WARN states the run is uncapped and no RTS limit is applied