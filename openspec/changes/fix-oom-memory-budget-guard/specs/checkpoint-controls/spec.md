## MODIFIED Requirements

### Requirement: Checkpoint decision logging

The system SHALL log at INFO whether a run resumed from a checkpoint or
performed a full extraction. When a run fails from heap exhaustion, the log
SHALL record the failed stage and state that the checkpoint was preserved for
resume.

#### Scenario: resume logged
- **WHEN** a run reuses an existing checkpoint
- **THEN** an INFO log states it is resuming and includes the checkpoint path

#### Scenario: full extraction logged
- **WHEN** no checkpoint is used (absent or `--fresh`)
- **THEN** an INFO log states a full extraction is being performed

#### Scenario: heap-exhaustion logged for resume
- **WHEN** a run dies from heap exhaustion in the Cluster stage with a Build checkpoint on disk
- **THEN** an ERROR log names Cluster, the budget, and states the checkpoint is preserved
- **AND** a subsequent run resumes from that checkpoint