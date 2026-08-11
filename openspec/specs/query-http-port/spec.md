# query-http-port

HTTP API port for the query family, exposing the same JSON contract as `graphos ... --json` (PRD §13.1 commands, §13.2 `--json`/`--budget`, spec `query-cli-contract`) over Warp so the HTML navigator and other HTTP clients can call the real scored query path against a pre-loaded `graph.json`.

## Purpose

Expose the full query family as a zero-IO HTTP port so the HTML navigator and external clients can query the graph without forking or duplicating logic.

## Requirements

### Requirement: Query HTTP endpoint returns the CLI JSON contract
The `graphos serve` HTTP server SHALL expose `GET /api/query?q=<question>&mode=bfs|dfs&budget=<n>` that loads `graph.json` once at startup, runs `queryGraphWithIndexScored`, refines with `defaultRefineConfig`, and returns a single JSON document that is byte-for-byte field-equivalent to `graphos query "<question>" --dfs --budget <n> --json` for the same `graph.json`: the same `verdict`, `bestScore`, `hash`, `nodes` (id/label/score/source_file/community), `edges`, and `suggestions` (PRD §13.1 `graphos query`, §13.2 `--json`, §16.1 query < 500ms). No log lines or other text SHALL be interleaved in the response body.

#### Scenario: HTTP query equals CLI JSON for strong match
- **WHEN** the server is started against a `graph.json` and `GET /api/query?q=auth&mode=bfs&budget=2000` is called while `graphos query "auth" --budget 2000 --json` produces verdict `strong`
- **THEN** the HTTP response body parses as JSON with the same `verdict`, `bestScore`, `hash`, and the same set of `nodes[*].id` as the CLI `--json` output

#### Scenario: NoMatch returns suggestions and empty nodes
- **WHEN** `GET /api/query?q=zzzznonexistent` is called against a graph where the CLI reports verdict `none`
- **THEN** the response JSON has `verdict` equal to `"none"`, an empty `nodes` array, an empty `edges` array, and a non-empty `suggestions` array matching the CLI `--json` output

#### Scenario: DFS mode honored
- **WHEN** `GET /api/query?q=auth&mode=dfs` is called
- **THEN** the returned `nodes` set equals the set produced by `graphos query "auth" --dfs --json` against the same graph

#### Scenario: Budget defaults applied
- **WHEN** `GET /api/query?q=auth` is called with no `budget` query parameter
- **THEN** the server uses the default budget `2000` (matching `graphos query` default, PRD §13.2)

### Requirement: Query-family HTTP endpoints
The `graphos serve` HTTP server SHALL expose the full query family over HTTP with the same JSON contract as the corresponding CLI `--json` output (PRD §13.1 `path`/`explain`/`symbols`/`neighbors`, spec `query-cli-contract`): `GET /api/path?from=<a>&to=<b>` (returns `{"path": [...nodeIds], "hops": <n>}` or `{"path": null}`), `GET /api/explain?node=<id>` (returns the node detail JSON), `GET /api/symbols?name=<n>&json=true` (returns the `SymbolResult` JSON), and `GET /api/neighbors?id=<id>&depth=<n>&json=true` (returns the `NeighborsResult` JSON). Each endpoint SHALL share the single in-memory graph + index loaded at startup.

#### Scenario: Path endpoint matches CLI
- **WHEN** `GET /api/path?from=AuthModule&to=Database` is called
- **THEN** the response JSON `path` array equals the node-id sequence printed by `graphos path AuthModule Database` against the same graph, and `hops` equals `length(path) - 1`

#### Scenario: Path endpoint reports no path
- **WHEN** `GET /api/path?from=X&to=Y` is called for two nodes with no connecting path
- **THEN** the response JSON is `{"path": null}` with HTTP status 200

#### Scenario: Symbols endpoint returns SymbolResult JSON
- **WHEN** `GET /api/symbols?name=RequestHandler&json=true` is called
- **THEN** the response body parses as JSON with `found`, `not_found`, and `suggestions` fields matching `graphos symbols RequestHandler --json`

#### Scenario: Neighbors endpoint returns NeighborsResult JSON
- **WHEN** `GET /api/neighbors?id=<id>&depth=2&json=true` is called
- **THEN** the response body parses as JSON with `center_node`, `nodes`, `edges`, and `max_depth` fields matching `graphos neighbors <id> --depth 2 --json`

### Requirement: In-memory graph loaded once at startup
The `graphos serve` HTTP server SHALL load `graph.json` and build the `GraphIndex` exactly once at startup, hold them in memory for the lifetime of the server, and serve all `/api/*` requests from that single loaded state without per-request file reads (PRD §16.1 query < 500ms). If the configured `graph.json` cannot be loaded, the server SHALL exit with a non-zero status and a clear error message before binding the port.

#### Scenario: Repeated requests do not reload the file
- **WHEN** two consecutive `GET /api/query?q=auth` requests are made to a running server
- **THEN** both responses return the same `hash` for the same query, and the `graph.json` file is not read a second time after startup

#### Scenario: Missing graph.json fails fast
- **WHEN** `graphos serve --graph nonexistent/graph.json --port 8090` is started
- **THEN** the process exits with a non-zero exit code and prints an error mentioning the missing file, and no port is bound

### Requirement: Serve CLI flags control the API port
The `graphos serve` command SHALL accept `--graph <path>` (default `graphos-out/graph.json`) to select the graph the API serves, `--api-only` to serve only `/api/*` (no static files), and `--no-api` to serve only static files with no `/api/*` (reproducing the pre-change behavior) (PRD §13.1 `graphos serve`, §13.2 flags). The default behavior (no flag) SHALL serve both static files and the `/api/*` endpoints on the same port.

#### Scenario: Default serves both static and API
- **WHEN** `graphos serve --dir graphos-out --port 8080` is run with `graphos-out/graph.json` present
- **THEN** `GET /graph.html` returns the HTML and `GET /api/query?q=auth` returns the query JSON

#### Scenario: No-api disables API
- **WHEN** `graphos serve --dir graphos-out --port 8080 --no-api` is run
- **THEN** `GET /api/query?q=auth` returns HTTP 404 and `GET /graph.html` still returns the HTML

#### Scenario: Api-only disables static files
- **WHEN** `graphos serve --api-only --graph graphos-out/graph.json --port 8081` is run
- **THEN** `GET /api/query?q=auth` returns the query JSON and `GET /graph.html` returns HTTP 404

### Requirement: CORS and content type for API responses
Every `/api/*` response SHALL have `Content-Type: application/json; charset=utf-8` and `Access-Control-Allow-Origin: *` so the self-contained `graph.html` (served from `file://` or another origin) can call the API. `OPTIONS` requests to `/api/*` SHALL return HTTP 200 with the CORS header and an empty body. Non-`GET` requests (except `OPTIONS`) to `/api/*` SHALL return HTTP 405.

#### Scenario: CORS header present
- **WHEN** `GET /api/query?q=auth` is called with header `Origin: http://localhost:3000`
- **THEN** the response includes `Access-Control-Allow-Origin: *` and `Content-Type: application/json; charset=utf-8`

#### Scenario: OPTIONS preflight succeeds
- **WHEN** `OPTIONS /api/query` is called
- **THEN** the response has HTTP status 200, an empty body, and the `Access-Control-Allow-Origin: *` header

#### Scenario: POST rejected
- **WHEN** `POST /api/query` is called
- **THEN** the response has HTTP status 405

### Requirement: Query endpoint latency target
The `/api/query` endpoint SHALL respond within 500ms for a graph loaded in memory (PRD §16.1 query < 500ms) on a pre-built graph, excluding the one-time startup load. The endpoint SHALL NOT perform any file IO in the request hot path.

#### Scenario: Repeated query under latency target
- **WHEN** 10 consecutive `GET /api/query?q=auth` requests are made to a running server with the graph already loaded
- **THEN** each request completes in under 500ms measured server-side (no reload of graph.json)