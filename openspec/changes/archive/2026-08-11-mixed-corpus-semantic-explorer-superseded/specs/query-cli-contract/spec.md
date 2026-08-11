# query-cli-contract

Delta — new filter flags and subcommands join the uniform contract.

## MODIFIED Requirements

### Requirement: Explorer filter flags uniformly accepted

The `query`, `path`, `explain`, `symbols`, `neighbors`, `around`, and `cluster` subcommands
SHALL accept `--filetype <code|doc|paper|image|video|audio|office>`, `--kind <text>`,
`--mixed-only`, `--code-only`, and `--doc-only` without "invalid option" errors. The flags
SHALL act as post-filters on the result set where applicable and SHALL be accepted as no-ops
on subcommands where they are not semantically applicable (e.g. `--mixed-only` on `path`).
This extends the uniform-flag-acceptance requirement to the explorer filter family.

#### Scenario: filetype accepted by path
- **WHEN** `graphos path A B --filetype doc --json` is run
- **THEN** the command executes normally; `--filetype` is accepted (filters path nodes or is a
  no-op, never an error)

#### Scenario: help lists explorer flags
- **WHEN** `--help` is passed to any of `query`, `path`, `explain`, `symbols`, `neighbors`,
  `around`, `cluster`
- **THEN** usage text lists `--filetype`, `--kind`, `--mixed-only`, `--code-only`, `--doc-only`
  alongside the common flags

### Requirement: around and cluster JSON output mode

With `--json`, the `around` and `cluster` subcommands SHALL emit a single JSON document on
stdout containing the `AroundResponse` / `ClusterResponse` fields respectively, with no
interleaved log lines. The text rendering (without `--json`) SHALL render a human-readable
summary of the same fields.

#### Scenario: around JSON is a single document
- **WHEN** `graphos around mod_Auth --json` is run
- **THEN** stdout parses as a single JSON object with `node`, `in_edges`, `out_edges`,
  `community`, `bridges`, `depth` fields

#### Scenario: cluster JSON is a single document
- **WHEN** `graphos cluster 483 --json` is run
- **THEN** stdout parses as a single JSON object with `id`, `label`, `composition`,
  `members_by_kind`, `bridges`, `cross_type_edges` fields

#### Scenario: Text and JSON agree on around
- **WHEN** `graphos around mod_Auth` is run with and without `--json`
- **THEN** both renderings report the same node, the same in-edge / out-edge counts, the same
  community id, and the same bridge set