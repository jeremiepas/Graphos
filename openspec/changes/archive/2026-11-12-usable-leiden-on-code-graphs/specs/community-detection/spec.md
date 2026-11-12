## ADDED Requirements

### Requirement: Partition on the undirected projection

Community detection and cohesion scoring SHALL treat every edge as undirected,
whatever the `--directed` setting.

#### Scenario: directed graph is not collapsed
- **WHEN** a directed graph whose symbols are sinks of `contains` edges is clustered
- **THEN** the partition equals the partition of the same graph built undirected

### Requirement: Bounded documents co-location

The pipeline co-location pass SHALL link one node per doc file to one node per
code file of the doc's directory subtree, and SHALL emit no co-location edge
for a doc whose subtree holds more than 25 code files.

#### Scenario: heading nodes do not multiply edges
- **WHEN** a Markdown file with two headings sits beside two code files
- **THEN** exactly two `documents` co-location edges are produced

#### Scenario: no doc nodes, no documents edges
- **WHEN** the graph has no doc nodes
- **THEN** no `documents` edge is inferred at any edge density

### Requirement: Sparse density excludes co-location

Under `--edge-density Sparse` the system SHALL NOT infer co-location
`documents` edges.

#### Scenario: sparse build
- **WHEN** a build with a doc file under `src/` runs with `--edge-density Sparse`
- **THEN** no co-location `documents` edge is present
