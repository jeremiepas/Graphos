# explorer-queries

New `around` and `cluster` subcommands + `--filetype` / `--kind` / `--mixed-only` /
`--code-only` / `--doc-only` filter flags on the query family, returning the existing
`QueryResponse` (for filtered queries) and new `AroundResponse` / `ClusterResponse` JSON
types for the new subcommands.

## ADDED Requirements

### Requirement: graphos around subcommand

The system SHALL provide `graphos around <node> [--depth N] [--json] [--budget N]
[--label-width N] [--edges semantic|all]` which returns a single `AroundResponse` JSON
document (when `--json` is set) containing: the node itself, its in-edges (dependencies) with
the neighbor node, its out-edges (dependents) with the neighbor node, its community (id,
label, composition) if assigned, and the articulation points (bridges) within its community.
The command SHALL honor the uniform flag surface (`--graph`, `--budget`, `--json`,
`--label-width`, `--edges`) and SHALL NOT emit interleaved log lines on stdout in JSON mode.

#### Scenario: around returns structural bundle
- **WHEN** `graphos around mod_Auth --json` is run on a graph where `mod_Auth` has 3 in-edges,
  2 out-edges, and is in community 483
- **THEN** stdout is a single JSON document with `node`, `in_edges` (length 3), `out_edges`
  (length 2), `community.id = 483`, `community.composition` present, `bridges` (list), and
  `depth = 1`

#### Scenario: around on unknown node
- **WHEN** `graphos around nonexistent --json` is run
- **THEN** the command exits non-zero with a clear error message on stderr and no stdout JSON

#### Scenario: around resolves display names
- **WHEN** `graphos around "Auth" --json` is run and `Auth` is the label of node `mod_Auth`
- **THEN** the command resolves the label to `mod_Auth` (via the same `resolveNodeArg` helper
  as `neighbors`) and returns the same result as `graphos around mod_Auth --json`

### Requirement: graphos cluster subcommand

The system SHALL provide `graphos cluster <id> [--json] [--budget N] [--label-width N]
[--edges semantic|all]` which returns a single `ClusterResponse` JSON document containing:
the community id, its label, its `CommunityComposition`, its members grouped by `nodeKind`,
the articulation points in the community, and the `References` edges crossing `CodeFile`↔
`DocFile` inside the community.

#### Scenario: cluster returns full detail
- **WHEN** `graphos cluster 483 --json` is run on a graph where community 483 has 12 code + 4
  doc members and 3 cross-type edges
- **THEN** stdout is a single JSON document with `id = 483`, `label`, `composition`
  (`code: 12, doc: 4, mixed_ratio: 0.33, code_doc_edges: 3`), `members_by_kind` grouping all
  16 members by their `nodeKind`, `bridges`, and `cross_type_edges` (length 3)

#### Scenario: cluster on unknown community
- **WHEN** `graphos cluster 9999 --json` is run and community 9999 does not exist
- **THEN** the command exits non-zero with a clear error message on stderr

### Requirement: Explorer filter flags on query family

The `query`, `symbols`, and `neighbors` subcommands SHALL accept new filter flags:
`--filetype <code|doc|paper|image|video|audio|office>`, `--kind <text>`, `--mixed-only`,
`--code-only` (shorthand for `--filetype code`), `--doc-only` (shorthand for `--filetype doc`).
The flags SHALL act as post-filters on the result set, narrowing which nodes are returned
without changing the query algorithm. `--mixed-only` SHALL drop nodes whose community has
`ccMixedRatio == 0`.

#### Scenario: filetype filter narrows query results
- **WHEN** `graphos query "auth" --filetype doc --json` is run on a mixed graph
- **THEN** the returned nodes are all `DocFile` nodes matching the query; `CodeFile` matches
  are dropped from the result set

#### Scenario: mixed-only filter drops pure communities
- **WHEN** `graphos query "auth" --mixed-only --json` is run and community 483 has
  `ccMixedRatio = 0.33` while community 12 has `ccMixedRatio = 0`
- **THEN** nodes in community 12 are dropped from the result set; nodes in community 483 are
  retained

#### Scenario: kind filter narrows by nodeKind
- **WHEN** `graphos query "auth" --kind function --json` is run
- **THEN** only nodes whose `nodeKind == Just "function"` are returned

### Requirement: Filter flags on around and cluster

The `around` and `cluster` subcommands SHALL accept `--filetype`, `--kind`, `--code-only`,
`--doc-only` as filters on the returned members / edges. `--mixed-only` is not applicable to
`cluster` (the cluster is already chosen) but SHALL be accepted without error for uniformity
and SHALL be a no-op.

#### Scenario: around with filetype filter
- **WHEN** `graphos around mod_Auth --filetype doc --json` is run
- **THEN** the `in_edges` and `out_edges` lists contain only edges whose neighbor is a
  `DocFile` node; code neighbors are dropped

### Requirement: Filter flags honor uniform acceptance

The new filter flags SHALL be accepted by every query-family subcommand (`query`, `path`,
`explain`, `symbols`, `neighbors`, `around`, `cluster`) without "invalid option" errors,
consistent with the `query-cli-contract` uniform-flag-acceptance requirement. Subcommands
where a flag is not applicable SHALL accept it without error (no-op) rather than rejecting it.

#### Scenario: kind flag on path command
- **WHEN** `graphos path A B --kind function --json` is run
- **THEN** the command runs normally; `--kind` is accepted (and may filter the returned path
  nodes, or be a no-op on `path` — TBD during implementation, but never an error)

### Requirement: HTTP port endpoints for around and cluster

Once the `add-query-api-port-and-view` HTTP port lands, the system SHALL expose
`GET /api/around?node=<id>&depth=<n>` and `GET /api/cluster?id=<n>` returning the same
`AroundResponse` / `ClusterResponse` JSON as the CLI `--json` path, with the same filter
query parameters (`filetype`, `kind`, `mixed-only`).

#### Scenario: HTTP around matches CLI
- **WHEN** `curl '/api/around?node=mod_Auth&depth=1'` is called
- **THEN** the response JSON is byte-for-byte equal to `graphos around mod_Auth --depth 1 --json`
  for the same `graph.json`