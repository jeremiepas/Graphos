# query-cli-contract

Uniform, machine-readable CLI contract across the query family (`query`, `path`,
`explain`, `symbols`, `neighbors`) (PRD §13.1 commands, §13.2 flags). Eliminates the
inconsistent flag surface that forced agents to discover the contract by triggering
errors.

## Purpose

Ensure every query-family command shares a consistent flag surface, JSON output contract, and budget behavior so that CLI and HTTP clients produce identical results.
## Requirements
### Requirement: Uniform flag acceptance
Every query-family subcommand SHALL accept `--graph <path>`, `--budget <n>`, `--json`,
`--label-width <n>`, and `--edges semantic|all`, and MUST support `--help` without
error. No query-family subcommand may reject a flag that another accepts.

#### Scenario: Budget accepted by explain
- **WHEN** `graphos explain <node> --budget 5000` is run
- **THEN** the command executes normally instead of failing with an invalid-option error

#### Scenario: Help on every subcommand
- **WHEN** `--help` is passed to any of `query`, `path`, `explain`, `symbols`, `neighbors`
- **THEN** usage text listing the common flags is printed and the exit code is zero

### Requirement: JSON output mode
With `--json`, every query-family subcommand SHALL emit a single JSON document on stdout containing the same information as the text rendering — including verdict, best score, result-set hash, scored nodes (id, label, score, source file, community), edges, and suggestions where applicable — with stable field names and no interleaved log lines. The same JSON contract SHALL be exposed over HTTP by `graphos serve` at `/api/query`, `/api/path`, `/api/explain`, `/api/symbols`, and `/api/neighbors` (see spec `query-http-port`), so that `curl '/api/query?q=X'` against a given `graph.json` and `graphos query "X" --json` against the same `graph.json` produce field-equivalent JSON (same verdict, hash, and node-id set). No HTTP `/api/*` response SHALL interleave log lines or non-JSON text.

#### Scenario: Query JSON contains verdict and hash
- **WHEN** `graphos query "term" --json` is run
- **THEN** stdout parses as JSON with fields for verdict, best score, result-set hash, nodes, edges, and suggestions

#### Scenario: Text and JSON agree
- **WHEN** the same query is run with and without `--json`
- **THEN** both renderings report the same verdict, hash, and node id set

#### Scenario: HTTP and CLI JSON agree
- **WHEN** `GET /api/query?q=auth` is called against a `graph.json` and `graphos query "auth" --json` is run against the same `graph.json`
- **THEN** both JSON documents have the same `verdict`, the same `hash`, and the same set of `nodes[*].id`

#### Scenario: HTTP neighbors and CLI JSON agree
- **WHEN** `GET /api/neighbors?id=<id>&depth=2&json=true` is called and `graphos neighbors <id> --depth 2 --json` is run against the same graph
- **THEN** both JSON documents have the same `center_node`, the same set of `nodes[*].id`, and the same `max_depth`

### Requirement: Budget honored uniformly
The `--budget` value SHALL bound the token estimate of the rendered output on every
query-family subcommand, truncating at the tail per the query-legibility ordering rules.

#### Scenario: Neighbors respects budget
- **WHEN** `graphos neighbors <id> --depth 3 --budget 500` would produce more output than the budget allows
- **THEN** output stops within the budget estimate and a footer reports the omitted counts

### Requirement: Query command JSON and shared flags
`graphos query <question>` SHALL accept `--json`, `--label-width <n>`, and
`--edges semantic|all` in addition to its existing `--dfs`, `--budget`, and `--graph`
flags. With `--json`, it SHALL emit a single JSON document on stdout containing the
existing `QueryResponse` fields (verdict, best score, result-set hash, scored nodes,
edges, suggestions) and no interleaved log lines. Without `--json`, the existing text
rendering SHALL be unchanged.

#### Scenario: Query JSON contains verdict and hash
- **WHEN** `graphos query "term" --json` is run
- **THEN** stdout parses as a single JSON document with fields for verdict, best score, result-set hash, nodes, edges, and suggestions, and no log lines are interleaved on stdout

#### Scenario: Query text mode is unchanged
- **WHEN** `graphos query "term"` is run without `--json`
- **THEN** the existing text rendering (verdict header, scored nodes, edges, suggestions) is byte-identical to the pre-change output

#### Scenario: Query text and JSON agree
- **WHEN** the same query is run with and without `--json` against the same graph
- **THEN** both renderings report the same verdict, best score, result-set hash, and ordered node id set

#### Scenario: Query accepts shared flags
- **WHEN** `graphos query "term" --json --label-width 80 --edges all --budget 1000` is run
- **THEN** the command executes normally instead of failing with an invalid-option error, and label elision / edge mode are applied to the rendered output

### Requirement: Path command JSON output
`graphos path <from> <to>` SHALL accept `--json` and, when set, SHALL emit a single JSON
document on stdout via the existing `renderPathResultJSON` renderer: `{"path": [<nodeId>...],
"hops": <n>}` for a found path and `{"path": null}` when no path exists, with no interleaved
log lines on stdout.

#### Scenario: Path found renders as JSON
- **WHEN** `graphos path A B --json` is run and a shortest path exists
- **THEN** stdout is a single JSON object with `path` set to the ordered node id list and `hops` set to `length path - 1`

#### Scenario: No path renders as JSON null
- **WHEN** `graphos path A B --json` is run and no path connects the two nodes
- **THEN** stdout is the single JSON object `{"path": null}`

#### Scenario: Path text mode is unchanged
- **WHEN** `graphos path A B` is run without `--json`
- **THEN** the existing text rendering (hop count and per-node `--rel-->` lines) is unchanged

### Requirement: Explain command JSON output
`graphos explain <node>` SHALL accept `--json` and, when set, SHALL emit a single JSON
document on stdout via the existing `renderExplainResultJSON` renderer: the node's id,
label, source file, and community, or JSON `null` when the node is not found. No
interleaved log lines on stdout.

#### Scenario: Explain found renders as JSON
- **WHEN** `graphos explain <node> --json` is run and the node exists
- **THEN** stdout is a single JSON object with `id`, `label`, `source_file`, and `community` fields

#### Scenario: Explain miss renders as JSON null
- **WHEN** `graphos explain <node> --json` is run and the node does not exist
- **THEN** stdout is the single JSON value `null`

#### Scenario: Explain text mode is unchanged
- **WHEN** `graphos explain <node>` is run without `--json`
- **THEN** the existing text rendering (NODE/ID/Source/Location/Type/Degree/Community/CONNECTIONS block) is unchanged

### Requirement: Command reference reflects the uniform flag surface
`renderCommandReference` in `Graphos.CLI.Parser` SHALL list `--json`, `--label-width`, and
`--edges` for every query-family subcommand (`query`, `path`, `explain`, `symbols`,
`neighbors`) so generated skill and help output matches the actual flag surface.

#### Scenario: Reference lists json for query
- **WHEN** `renderCommandReference` output is inspected
- **THEN** the `graphos query` entry lists `--json`, `--label-width`, and `--edges` alongside `--dfs`, `--budget`, and `--graph`

#### Scenario: Reference lists json for path and explain
- **WHEN** `renderCommandReference` output is inspected
- **THEN** both the `graphos path` and `graphos explain` entries list `--json` alongside `--graph`

