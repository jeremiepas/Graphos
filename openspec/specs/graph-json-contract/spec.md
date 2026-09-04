# graph-json-contract Specification

## Purpose

Define `graph.json` as a versioned, tolerantly-read interchange format so that graphs produced
by a different Graphos version, by a partially completed run, or by an external tool can be
loaded by the query family. Today the loader is strict and unversioned: a single unknown enum
value or missing top-level key aborts the whole decode (`UseCase/Load.hs:42–71`, `:89–97`;
`Domain/Types/Edge.hs:48–52`; `Domain/Types/Node.hs:53–62`, `:123–136`), and
`community_aggregates` is written (`Infrastructure/Export/IncrementalJSON.hs:104–107`) but never
read back.

## Requirements

### Requirement: graph.json declares a schema version

Every `graph.json` written by Graphos SHALL contain a top-level `schema_version` string. The
loader SHALL accept a file without `schema_version` as the pre-versioning baseline, and SHALL
refuse a major version it does not implement with a single actionable error naming the file, the
found version and the supported range.

#### Scenario: Written graphs carry a version

- **WHEN** the pipeline exports `graph.json`
- **THEN** the document contains a top-level `schema_version`

#### Scenario: Legacy graphs still load

- **WHEN** a `graph.json` produced before this change (no `schema_version`) is loaded
- **THEN** the load succeeds and is treated as the baseline version

#### Scenario: Unsupported major version fails clearly

- **WHEN** a `graph.json` declares a major version greater than the one implemented
- **THEN** the load fails with one error naming the path, the found version and the supported
  range, and does not emit a decoder-internal message

### Requirement: Unknown enum values degrade instead of aborting

The loader SHALL map an unknown `relation` to `inferred` and an unknown `file_type` to `code`,
each recorded as a counted warning, instead of failing the decode. A `--strict-graph` flag SHALL
restore fail-fast behaviour for producers that want to validate their output.

#### Scenario: Unknown relation is degraded and counted

- **WHEN** a graph contains an edge with `"relation": "re_exports"`
- **THEN** the load succeeds, the edge is present with relation `inferred`, and the run reports
  one degraded-relation warning

#### Scenario: Unknown file type is degraded and counted

- **WHEN** a node declares `"file_type": "other"`
- **THEN** the load succeeds, the node's file type is `code`, and the run reports one
  degraded-file-type warning

#### Scenario: Strict mode still fails

- **WHEN** the same graph is loaded with `--strict-graph`
- **THEN** the load fails, naming the offending value and the node or edge id

### Requirement: Optional node fields and top-level sections

`source_file` SHALL be optional on a node (absent or `null` is accepted and rendered as an empty
location). The top-level `communities`, `cohesion` and `god_nodes` sections SHALL be optional and
default to empty when absent, so an un-clustered or partially written graph still loads for
query purposes.

#### Scenario: Node without a source file loads

- **WHEN** a graph contains a node with `"source_file": null` (for example a synthetic external
  package node)
- **THEN** the load succeeds and the node is queryable

#### Scenario: Un-clustered graph loads

- **WHEN** a graph produced with clustering disabled has no `communities`, `cohesion` or
  `god_nodes` keys
- **THEN** the load succeeds with empty community data and `graphos query` returns node results

#### Scenario: Node-level defects are skipped, not fatal

- **WHEN** a graph contains 100 nodes of which 2 are malformed beyond degradation
- **THEN** the load succeeds with 98 nodes and reports 2 skipped nodes

### Requirement: Reader and writer key sets are symmetric

Every top-level key written by the exporter SHALL be read back by the loader, and every key the
loader requires SHALL be written unconditionally by the exporter. `community_aggregates` SHALL
round-trip.

#### Scenario: Round-trip preserves all top-level sections

- **WHEN** a clustered graph is exported and immediately re-loaded
- **THEN** nodes, edges, communities, cohesion, god nodes, community labels, compositions and
  community aggregates are all present after the round-trip, with the same counts as written

#### Scenario: Externally produced graph is queryable

- **WHEN** a graph produced by `scripts/subgraph_from_patterns.py` from an existing
  `graph.json` is passed via `--graph`
- **THEN** `graphos query`, `graphos explain` and `graphos neighbors` operate on it without
  schema errors

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
