## ADDED Requirements

### Requirement: Pipeline Timeout CLI Flag

The `graphos` CLI SHALL accept a `--timeout SECONDS` flag that sets a wall-clock timeout for the entire pipeline execution. When the timeout expires, the pipeline SHALL terminate with a non-zero exit code and a descriptive error message indicating the timeout was reached.

#### Scenario: Timeout expires during extraction
- **WHEN** `graphos . --timeout 30` is run and extraction takes longer than 30 seconds
- **THEN** the process exits with code 1 and prints a message like "Pipeline timed out after 30s"

#### Scenario: No timeout flag provided
- **WHEN** `graphos .` is run without `--timeout`
- **THEN** the pipeline runs without a time limit (same as current behavior)

#### Scenario: Timeout longer than pipeline duration
- **WHEN** `graphos . --timeout 600` is run and the pipeline completes in 120 seconds
- **THEN** the pipeline completes normally and the timeout has no effect

### Requirement: Timeout Preserves Checkpoint

When a timeout expires, any checkpoint data written before the timeout SHALL be preserved so that a subsequent run with `--update` can resume from the last completed step.

#### Scenario: Timeout during extraction with existing checkpoint
- **WHEN** `graphos . --timeout 60` is run, extraction starts, and timeout expires mid-extraction
- **THEN** the checkpoint file at `graphos-out/graph.checkpoint.json` (if written) remains intact for resume

#### Scenario: Timeout before any checkpoint
- **WHEN** `graphos . --timeout 5` is run and timeout expires before Step 2 (extraction) completes
- **THEN** no partial checkpoint data is written; the next run starts fresh