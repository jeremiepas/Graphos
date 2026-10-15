## MODIFIED Requirements

### Requirement: Workflow 14 — debug trace JSONL output
System SHALL write timestamped JSON events to `<output-dir>/debug/*.jsonl` when debug tracing is enabled. Each event: `timestamp`, `stage`, `event_type`, `details`. Debug tracing SHALL be enabled only when the effective log level is debug or trace and `--no-observability` is not set; `--no-observability` SHALL disable the debug trace environment so that no trace directory or file is created. `observability.debugTraceDir` SHALL be resolved against the output directory when relative, and `--debug-trace DIR` overrides it; a trace file SHALL never be written to the working directory unless that is the directory named by the operator. (PRD §10.1, workflow 14)

#### Scenario: Debug JSONL file created
- **WHEN** pipeline runs with `--debug`
- **THEN** `<output-dir>/debug/` SHALL contain `.jsonl` with stage transition events

#### Scenario: --no-observability writes no trace
- **WHEN** `graphos . --no-observability` runs with a global config declaring `debugTraceDir: "./traces"` and `debug: true`
- **THEN** no `traces/` directory exists in the working directory or the output directory after the run, verifiable by `cabal test` on the observability initialisation

#### Scenario: Relative trace directory lands under the output directory
- **WHEN** `graphos . -o build/out --debug` runs with `debugTraceDir: "traces"`
- **THEN** trace files are written under `build/out/traces/`
