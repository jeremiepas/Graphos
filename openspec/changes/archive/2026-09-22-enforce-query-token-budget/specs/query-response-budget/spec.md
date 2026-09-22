## ADDED Requirements

### Requirement: Serialized-size budget enforcement

The system SHALL bound the serialized size of a query response to the requested
`budget`, dropping the lowest-ranked results first and reporting how many were
omitted.

#### Scenario: response fits within budget
- **WHEN** a query is issued with `budget = 2000`
- **THEN** the serialized response does not exceed the budget's byte allowance
- **AND** the response includes `omitted` counts for nodes and edges dropped

#### Scenario: high-value results retained
- **WHEN** results must be dropped to fit the budget
- **THEN** the highest-scoring nodes and their connecting edges are retained

### Requirement: Compact node serialization

The system SHALL serialize each node in list responses with `id`, `source_file`,
`score`, `kind`, and a `label` truncated to `maxLabelChars` (default 120), and
SHALL NOT emit full source text in list responses.

#### Scenario: label truncated
- **WHEN** a node's source text is 4000 characters and `maxLabelChars = 120`
- **THEN** the emitted `label` is at most 120 characters plus an ellipsis marker

### Requirement: Short stable node identifiers

The system SHALL identify nodes with a short stable identifier derived from file
and location (or a content hash) rather than embedding full source text in the ID.

#### Scenario: id is short and stable
- **WHEN** a node is emitted
- **THEN** its `id` is a bounded-length token (e.g. `path#line` or hash)
- **AND** re-running extraction on unchanged input yields the same `id`

#### Scenario: preview available separately
- **WHEN** a caller needs the snippet
- **THEN** a separate `preview` field carries the (truncated) source text

### Requirement: Query size controls

The system SHALL expose `--max-nodes` and `--max-label-chars` on the CLI and as
MCP tool parameters.

#### Scenario: max-nodes caps count
- **WHEN** `--max-nodes 20` is set and 500 nodes match
- **THEN** at most 20 nodes are returned, ranked by score, with `omitted` reporting the remainder
