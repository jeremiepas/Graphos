## ADDED Requirements

### Requirement: JGF graph envelope

The system SHALL serialize the graph as a JSON Graph Format document
(`application/vnd.jgf+json`): a top-level `graph` object with `directed: true`,
`type: "graphos.code-knowledge-graph"`, `nodes`, `edges`, and `metadata`.

#### Scenario: emitted file is a JGF document
- **WHEN** a full run writes the graph output
- **THEN** the file has a top-level `graph` object containing `nodes`, `edges`, and `metadata`
- **AND** `graph.directed` is `true` and `graph.type` identifies the graphos graph

### Requirement: Lossless field mapping

The system SHALL preserve every current node and edge field: node `id` and
`label` remain top-level with the other node fields under node `metadata`; edge
`source`, `target`, and `relation` remain top-level with `weight`, `confidence`,
and `extra` under edge `metadata`; graph-level `communities`, `cohesion`,
`god_nodes`, and `community_labels` live under `graph.metadata.graphos`.

#### Scenario: round-trip preserves all fields
- **WHEN** a graph is written as JGF and read back
- **THEN** every node and edge field is equal to the original
- **AND** communities, cohesion, god nodes, and community labels are recovered unchanged

#### Scenario: node metadata carries graphos fields
- **WHEN** a node with a `source_file`, `kind`, and `community` is serialized
- **THEN** those values appear under that node's `metadata`

### Requirement: Backward-compatible loading

The system SHALL load both the JGF envelope and the legacy top-level
`nodes`/`edges` schema, selecting by the presence of a top-level `graph` object.

#### Scenario: legacy graph.json still loads
- **WHEN** a file using the legacy top-level `nodes`/`edges` schema is loaded
- **THEN** it parses into the same in-memory graph as before

#### Scenario: JGF file loads
- **WHEN** a file with a top-level `graph` object is loaded
- **THEN** it is parsed as JGF into the in-memory graph

### Requirement: Schema versioning

The system SHALL record a `schemaVersion` under `graph.metadata.graphos` and
SHALL reject a document whose major version is unknown with a clear error.

#### Scenario: unknown major version rejected
- **WHEN** a JGF document declares a major `schemaVersion` the reader does not support
- **THEN** loading fails with an error identifying the unsupported version
