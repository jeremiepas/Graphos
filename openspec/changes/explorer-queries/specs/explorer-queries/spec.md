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
  (length 2), `community.id = 483`, `community.composition` present (or `null` if
  compositions absent), `bridges` (list), and `depth = 1`

#### Scenario: around on unknown node
- **WHEN** `graphos around nonexistent --json` is run
- **THEN** the command exits non-zero with a clear error message on stderr and no stdout JSON

#### Scenario: around resolves display names
- **WHEN** `graphos around "Auth" --json` is run and `Auth` is the label of node `mod_Auth`
- **THEN** the command resolves the label to `mod_Auth` (via `resolveNodeArg`) and returns
  the same result as `graphos around mod_Auth --json`

#### Scenario: around with depth
- **WHEN** `graphos around mod_Auth --depth 2 --json` is run
- **THEN** the `in_edges` and `out_edges` include nodes up to 2 hops away (BFS expansion)

### Requirement: graphos cluster subcommand

The system SHALL provide `graphos cluster <id> [--json] [--budget N] [--label-width N]
[--edges semantic|all]` which returns a single `ClusterResponse` JSON document containing:
the community id, its label (if available), its `CommunityComposition` (if available), its
members grouped by `nodeKind`, the articulation points in the community, and the `References`
edges crossing `CodeFile`↔doc-like inside the community.

#### Scenario: cluster returns full detail
- **WHEN** `graphos cluster 483 --json` is run on a graph where community 483 has 12 code + 4
  doc members and 3 cross-type edges
- **THEN** stdout is a single JSON document with `id = 483`, `label`, `composition`
  (`code: 12, doc: 4, mixed_ratio: 0.33, code_doc_edges: 3` or `null` if absent),
  `members_by_kind` grouping all 16 members by their `nodeKind`, `bridges`, and
  `cross_type_edges` (length 3)

#### Scenario: cluster on unknown community
- **WHEN** `graphos cluster 9999 --json` is run and community 9999 does not exist
- **THEN** the command exits non-zero with a clear error message on stderr

#### Scenario: cluster without compositions
- **WHEN** `graphos cluster 483 --json` is run on a legacy graph without `compositions`
- **THEN** `composition` is `null` in the JSON; `members_by_kind`, `bridges`, and
  `cross_type_edges` are still populated from the graph

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

#### Scenario: mixed-only no-op on legacy graph
- **WHEN** `graphos query "auth" --mixed-only --json` is run on a legacy graph without
  `compositions`
- **THEN** the command logs a warning "--mixed-only ignored: no community compositions
  available" and returns all nodes (no-op filter)

#### Scenario: kind filter narrows by nodeKind
- **WHEN** `graphos query "auth" --kind function --json` is run
- **THEN** only nodes whose `nodeKind == Just "function"` are returned

#### Scenario: code-only shorthand
- **WHEN** `graphos query "auth" --code-only --json` is run
- **THEN** the result is identical to `graphos query "auth" --filetype code --json`

### Requirement: Filter flags on around and cluster

The `around` and `cluster` subcommands SHALL accept `--filetype`, `--kind`, `--code-only`,
`--doc-only` as filters on the returned members / edges. `--mixed-only` SHALL be accepted
without error on `cluster` (no-op, since the cluster is already chosen) and on `around`
(filters edges/members by community mixedness).

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
- **THEN** the command executes normally; `--kind` is accepted (filters path nodes or is a
  no-op, never an error)

#### Scenario: mixed-only on cluster is no-op
- **WHEN** `graphos cluster 483 --mixed-only --json` is run
- **THEN** the command executes normally; `--mixed-only` is accepted and is a no-op (the
  cluster is already chosen, not filtered)

#### Scenario: help lists explorer flags
- **WHEN** `--help` is passed to any of `query`, `path`, `explain`, `symbols`, `neighbors`,
  `around`, `cluster`
- **THEN** usage text lists `--filetype`, `--kind`, `--mixed-only`, `--code-only`, `--doc-only`
  alongside the common flags

### Requirement: HTTP port endpoints for around and cluster

Once the `add-query-api-port-and-view` HTTP port lands, the system SHALL expose
`GET /api/around?node=<id>&depth=<n>&filetype=...&kind=...&mixed-only=1` and
`GET /api/cluster?id=<n>&filetype=...&kind=...` returning the same `AroundResponse` /
`ClusterResponse` JSON as the CLI `--json` path, with the same filter query parameters.

#### Scenario: HTTP around matches CLI
- **WHEN** `curl '/api/around?node=mod_Auth&depth=1'` is called
- **THEN** the response JSON is byte-for-byte equal to `graphos around mod_Auth --depth 1 --json`
  for the same `graph.json`