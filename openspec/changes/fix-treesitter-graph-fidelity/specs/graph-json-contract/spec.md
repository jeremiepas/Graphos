# graph-json-contract Capability — Delta

## Purpose

Define `graph.json` as a versioned, tolerantly-read interchange format so that graphs produced
by a different Graphos version, by a partially completed run, or by an external tool can be
loaded by the query family. Today the loader is strict and unversioned: a single unknown enum
value or missing top-level key aborts the whole decode (`UseCase/Load.hs:42–71`, `:89–97`;
`Domain/Types/Edge.hs:48–52`; `Domain/Types/Node.hs:53–62`, `:123–136`), and
`community_aggregates` is written (`Infrastructure/Export/IncrementalJSON.hs:104–107`) but never
read back.

## ADDED Requirements

### Requirement: graph.json declares a schema version

Every `graph.json` written by Graphos SHALL contain a top-level `schema_version` string. The
loader SHALL accept a file without `schema_version` as the pre-versioning baseline, and SHALL
refuse a major version it does not implement with a single actionable error naming the file, the
found version and the supported range.

#### PDCA

- **Plan**: There is no version field anywhere in the writer or reader today, so no migration or
  compatibility decision can be expressed.
- **Do**: Emit `schema_version` in the incremental writer; parse it optionally in the loader.
- **Check**: Scenarios below verify version emission, legacy loading, and clear failure on an
  unsupported major version.
- **Act**: On the first breaking schema change, bump the major version and add a migration note
  rather than silently changing field semantics.

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

#### PDCA

- **Plan**: `Edge.hs:48–52` and `Node.hs:53–62` currently `fail` inside a whole-document
  `eitherDecode`, so one unknown token discards a 130 MB graph.
- **Do**: Degrade with accounting in the default path; keep a strict opt-in.
- **Check**: Scenarios below verify default degradation, counted warnings, and strict-mode
  failure.
- **Act**: If a degraded value appears frequently for a known producer, promote it to a real
  enum member in a follow-up rather than leaving it degraded.

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

#### PDCA

- **Plan**: External/package nodes and partial/crashed runs currently produce unloadable graphs
  because `source_file` and top-level sections are strictly required.
- **Do**: Make `source_file` optional and give `communities`/`cohesion`/`god_nodes` empty
  defaults; skip malformed individual nodes/edges.
- **Check**: Scenarios below verify optional fields, optional top-level sections, and per-item
  recovery.
- **Act**: If skipped nodes/edges become common for Graphos-produced graphs, tighten the writer
  rather than masking a producer bug.

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

#### PDCA

- **Plan**: `community_aggregates` is written at `IncrementalJSON.hs:104–107` and never parsed
  (`Load.hs:89–97`); `community_labels` and `compositions` are written only when present, while
  `communities`/`cohesion`/`god_nodes` are required by the reader — a crashed run therefore
  produces an unloadable file.
- **Do**: Read `community_aggregates`; make required sections optional; keep a single list of
  top-level keys shared by writer and reader.
- **Check**: Scenarios below verify round-trip symmetry and loadability of externally produced
  graphs.
- **Act**: Add any future top-level section to the shared list, so the symmetry test fails when
  a writer-only key is introduced.

#### Scenario: Round-trip preserves all top-level sections

- **WHEN** a clustered graph is exported and immediately re-loaded
- **THEN** nodes, edges, communities, cohesion, god nodes, community labels, compositions and
  community aggregates are all present after the round-trip, with the same counts as written

#### Scenario: Externally produced graph is queryable

- **WHEN** a graph produced by `scripts/subgraph_from_patterns.py` from an existing
  `graph.json` is passed via `--graph`
- **THEN** `graphos query`, `graphos explain` and `graphos neighbors` operate on it without
  schema errors
