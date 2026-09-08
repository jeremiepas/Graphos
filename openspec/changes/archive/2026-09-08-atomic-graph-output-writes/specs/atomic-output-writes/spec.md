## ADDED Requirements

### Requirement: Atomic artifact writes

The system SHALL write each primary output artifact to a temporary file in the
target directory and atomically rename it into place, so a reader never observes
a partially written artifact.

#### Scenario: interrupted write leaves prior file intact
- **WHEN** a run is interrupted while writing `graph.json`
- **THEN** the previous `graph.json` remains valid and complete
- **AND** no partially written `graph.json` is present at the final path

#### Scenario: successful write replaces atomically
- **WHEN** `graph.json` is written successfully
- **THEN** the final file is the complete new content with no intermediate truncation observable

### Requirement: Staged rebuild swap

The system SHALL perform a full rebuild in a staging location and swap it into the
output directory only after all artifacts are written successfully.

#### Scenario: failed rebuild preserves existing output
- **WHEN** a full rebuild fails partway through
- **THEN** the existing `graphos-out/` contents are unchanged

### Requirement: Startup graph validation

The system SHALL validate an existing `graph.json` before use and fail with a
clear, actionable message if it is corrupt.

#### Scenario: corrupt graph reported clearly
- **WHEN** `graph.json` is truncated or invalid JSON
- **THEN** the tool reports the corruption and the path
- **AND** suggests recovering from the checkpoint or rebuilding

#### Scenario: valid graph loads normally
- **WHEN** `graph.json` is well-formed
- **THEN** it loads without warnings
