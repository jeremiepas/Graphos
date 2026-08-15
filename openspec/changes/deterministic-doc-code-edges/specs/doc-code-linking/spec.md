## ADDED Requirements

### Requirement: Co-location doc-code edges

The system SHALL create a `documents` edge from a documentation file to code
files located in the same directory or a descendant directory.

#### Scenario: README links to sibling code
- **WHEN** `libraries/jwt-verifier/README.md` and `libraries/jwt-verifier/src/lib.rs` exist
- **THEN** a `documents` edge connects the README to nodes of `src/lib.rs`

#### Scenario: unrelated doc not linked by co-location
- **WHEN** a README exists in a different top-level directory from a code file
- **THEN** no co-location `documents` edge is created between them

### Requirement: Symbol-mention doc-code edges

The system SHALL create a `documents` edge from a documentation node to the code
node that defines an identifier when that identifier appears in the doc text.

#### Scenario: doc mentions a defined symbol
- **WHEN** a doc contains the token `set_remote_execution` and a code node defines `set_remote_execution`
- **THEN** a `documents` edge connects the doc node to that defining node

#### Scenario: common word not treated as symbol
- **WHEN** a doc contains a common word that is not a defined identifier
- **THEN** no symbol-mention edge is created

### Requirement: Edge confidence tagging

The system SHALL tag co-location and symbol-mention edges with the `documents`
relation and high confidence, distinct from similarity-based `inferred` edges.

#### Scenario: documents edges survive semantic filter
- **WHEN** a query is run with `edges = semantic`
- **THEN** `documents` edges are retained
- **AND** low-confidence `inferred` doc↔code edges may be dropped
