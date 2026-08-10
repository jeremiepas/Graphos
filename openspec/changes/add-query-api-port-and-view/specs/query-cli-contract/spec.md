# query-cli-contract

Uniform, machine-readable CLI contract across the query family (`query`, `path`, `explain`, `symbols`, `neighbors`) (PRD §13.1 commands, §13.2 flags). Eliminates the inconsistent flag surface that forced agents to discover the contract by triggering errors. This delta extends the contract to the HTTP API port so CLI `--json` and HTTP `/api/*` are a single contract.

## MODIFIED Requirements

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