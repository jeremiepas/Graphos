## ADDED Requirements

### Requirement: Mutated graphs persist under the same versioned schema

When mutations are explicitly persisted to `graph.json` (capability
`cypher-mutation`), the writer SHALL emit the same versioned schema as the
pipeline (top-level `schema_version` unchanged, `nodes`/`edges` plus the existing
top-level sections), copying the previous file to a timestamped backup first.
Mutation-only changes SHALL ride existing fields: extra node labels in each
node's `extra` object (`extra_labels`), non-model properties in node/edge
`extra`, and model fields (label/kind, source_file, weight, confidence, ...)
written in place. Derived sections (`communities`, `cohesion`, `god_nodes`,
`community_aggregates`) SHALL be carried over from the loaded file, with node
degrees and adjacency recomputed.

#### Scenario: persisted mutation keeps schema version

- **WHEN** a mutated graph is written back to `graph.json`
- **THEN** the document contains the same top-level `schema_version` it was loaded with and loads successfully via the standard reader

#### Scenario: extra labels and properties round-trip

- **WHEN** a node with an added extra label and a `SET` non-model property is persisted and reloaded
- **THEN** the extra label and the property are visible to Cypher queries after the reload

#### Scenario: backup precedes write

- **WHEN** persistence overwrites an existing `graph.json`
- **THEN** a `graph.json.bak-<timestamp>` copy of the pre-mutation file exists before the write completes