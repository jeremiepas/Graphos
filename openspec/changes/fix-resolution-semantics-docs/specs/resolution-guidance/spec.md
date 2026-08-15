## ADDED Requirements

### Requirement: Accurate resolution documentation

The system's `--resolution` help text and README SHALL describe the parameter's
effect consistently with its implemented behavior, verified empirically.

#### Scenario: help text matches behavior
- **WHEN** a user reads `graphos --help` for `--resolution`
- **THEN** the described direction of effect matches what the implementation produces
- **AND** no contradictory range guidance (e.g. "try 0.3–0.5") is given if it worsens outcomes

### Requirement: Dense-subgraph caveat

The documentation SHALL state that no resolution value can split a
densely-connected subgraph and SHALL point to the community-size cap as the
remedy.

#### Scenario: caveat present
- **WHEN** a user reads the resolution documentation
- **THEN** it notes that resolution cannot break up a dense blob and references the size cap

### Requirement: Effective resolution echo

The system SHALL log the effective resolution value at INFO before clustering.

#### Scenario: resolution logged
- **WHEN** clustering begins
- **THEN** an INFO log states the effective resolution value in use

### Requirement: Documentation-behavior consistency check

The system SHALL be corrected so documentation and behavior agree when the
implemented resolution mapping contradicts intended Leiden semantics.

#### Scenario: mapping corrected when defective
- **WHEN** investigation shows the resolution-to-Leiden mapping is inverted relative to intent
- **THEN** the mapping is fixed and the documentation reflects the corrected behavior
