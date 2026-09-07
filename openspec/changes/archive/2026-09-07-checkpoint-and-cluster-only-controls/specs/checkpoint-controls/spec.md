## ADDED Requirements

### Requirement: Explicit fresh build

The system SHALL provide `--fresh` / `--no-checkpoint` that forces full
re-extraction and ignores any existing checkpoint.

#### Scenario: fresh ignores checkpoint
- **WHEN** `graphos ./src --fresh` runs and a checkpoint exists
- **THEN** extraction runs from scratch and the checkpoint is not reused

### Requirement: Checkpoint decision logging

The system SHALL log at INFO whether a run resumed from a checkpoint or performed
a full extraction.

#### Scenario: resume logged
- **WHEN** a run reuses an existing checkpoint
- **THEN** an INFO log states it is resuming and includes the checkpoint path

#### Scenario: full extraction logged
- **WHEN** no checkpoint is used (absent or `--fresh`)
- **THEN** an INFO log states a full extraction is being performed

### Requirement: Genuine cluster-only

When `--cluster-only` is set, the system SHALL load nodes and edges from the
checkpoint and re-run only clustering and its dependent stages, skipping
extraction and edge inference.

#### Scenario: cluster-only skips extraction
- **WHEN** `graphos ./src --cluster-only` runs with a valid checkpoint
- **THEN** the Extract and Infer stages do not execute
- **AND** clustering runs on the checkpoint's nodes and edges

#### Scenario: cluster-only without checkpoint errors
- **WHEN** `--cluster-only` is set and no usable checkpoint exists
- **THEN** the tool exits with a clear error explaining a checkpoint is required
