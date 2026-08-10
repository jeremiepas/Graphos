## Why

The HTML navigator's search box (`graphos serve`) only performs a client-side substring filter over `allNodes` — it never runs the real scored query path (inverted-index term matching, BFS/DFS subgraph expansion, verdict, did-you-mean suggestions, result-set hash) that `graphos query` (PRD §13.1, workflow 04-query) exposes. Users navigating the graph therefore get weaker results inside the browser than on the CLI, and there is no HTTP port exposing the `QueryResponse` JSON contract (PRD §13.2 `--json`, spec `query-cli-contract`) for other clients. We close that gap now because the scored query path and `renderQueryResponseJSON` already exist and only need a thin HTTP adapter — doing it now keeps the navigator and CLI from diverging further.

## What Changes

- **Add a query HTTP port** to `graphos serve`: a new `GET /api/query?q=<question>&mode=bfs|dfs&budget=<n>` endpoint that loads `graph.json` once, runs `queryGraphWithIndexScored`, and returns the exact same `QueryResponse` JSON that `graphos query --json` emits (verdict, bestScore, hash, scored nodes, edges, suggestions). PRD §13.1 (`graphos serve`), §13.2 (`--json`, `--budget`), §16.1 (query < 500ms).
- **Add a path/explain/symbols/neighbors HTTP port** (same family) so the navigator and other clients can call the full query family over HTTP with the identical JSON contract (spec `query-cli-contract`): `GET /api/path?from=&to=`, `GET /api/explain?node=`, `GET /api/symbols?name=`, `GET /api/neighbors?id=&depth=`.
- **Upgrade the navigator search view** in `graph.html`: replace the client-side substring `showSearchResults` with a `fetch('/api/query?q=...')` call that renders the `QueryResponse` — verdict badge, best score, did-you-mean suggestions, scored nodes (ranked, score-desc), and the matched subgraph edges as a graph view inside the navigator (focus + highlight matched nodes/edges on the existing vis-network canvas). PRD §13.1 (`graphos serve --dir`).
- **New `serve` flags**: `--graph <path>` (graph.json to serve against, default `graphos-out/graph.json`), `--api-only` (disable static file serving, expose only `/api/*`), and `--no-api` (current static-only behavior, for back-comat). PRD §13.2.
- The `serve` command loads the graph + index **once** at startup and keeps it in memory for the lifetime of the server (no per-request reload), satisfying PRD §16.1 query latency target.

## Capabilities

### New Capabilities
- `query-http-port`: HTTP API port (`/api/query`, `/api/path`, `/api/explain`, `/api/symbols`, `/api/neighbors`) exposing the query-family JSON contract over Warp, sharing the `QueryResponse`/`SymbolResult`/`NeighborsResult` types with the CLI `--json` path.
- `navigator-query-view`: In-`graph.html` graph view that calls the query HTTP port and renders the scored QueryResponse (verdict, suggestions, scored nodes, matched subgraph edges) on the existing vis-network canvas, replacing the substring-only search.

### Modified Capabilities
- `query-cli-contract`: The uniform JSON contract (verdict, bestScore, hash, nodes, edges, suggestions) is now delivered over HTTP `/api/*` in addition to CLI `--json`, with the same field names and no interleaved log lines.

## Impact

- **Code**: New `Infrastructure.Server.QueryAPI` (Warp `Application` for `/api/*`, in-memory `LabeledGraph`+`GraphIndex`); `Infrastructure.Server.Static` upgraded to compose static + API apps (or a new combined `Server.Serve`); `app/Main.hs` `Serve` branch extended with `--graph`, `--api-only`, `--no-api`; `CLI.Parser` `serveOpts` gains those flags; `Infrastructure.Export.HTML` `showSearchResults` rewritten to `fetch('/api/query')` and render the `QueryResponse` graph view.
- **APIs**: New public HTTP endpoints under `/api/*` (additive, no breaking change to CLI). `graphos serve` gains `--graph`, `--api-only`, `--no-api` (additive; default behavior changes from static-only to static+api — marked as a behavior change, not BREAKING since output is additive).
- **Dependencies**: No new libraries — reuses `warp`, `wai`, `aeson`, and existing `UseCase.Query` / `UseCase.Load` pure functions.
- **Tests**: New Hspec tests for `/api/query` JSON contract (verdict/hash/nodes/edges parity with `renderQueryResponseJSON`), `/api/path`, `/api/explain`, `/api/symbols`, `/api/neighbors`, and in-memory graph reuse across requests. HTML search behavior verified via the existing self-contained HTML generation test pattern.
- **Build**: New module + extended `serve` flags; `-Wall -Werror` clean.

## PDCA Cycle

- **Plan**: Hypothesis — exposing the real scored query path over HTTP and rendering its `QueryResponse` in the navigator makes in-browser search as accurate as `graphos query --json`. Success measured by PRD §16.1 (query response < 500ms over HTTP on a pre-built graph) and by byte-for-byte parity between `curl /api/query?q=X` and `graphos query "X" --json` for the same `graph.json` (same verdict, hash, node-id set).
- **Do**: Implement `Infrastructure.Server.QueryAPI` (in-memory graph + pure query reuse), compose it into the `serve` Warp app, extend `serveOpts` with `--graph`/`--api-only`/`--no-api`, and rewrite `graph.html` search to `fetch('/api/query')` and render the subgraph view.
- **Check**: (1) `cabal test` passes with new Hspec cases asserting `/api/query` JSON equals `renderQueryResponseJSON` output for the same query; (2) HTTP latency < 500ms on a pre-loaded graph (PRD §16.1); (3) manual: `graphos serve` → search in navigator shows verdict + scored nodes + matched edges; (4) `--no-api` reproduces the old static-only behavior; (5) `-Wall -Werror` clean.
- **Act**: Standardize the "CLI `--json` == HTTP `/api/*` JSON" invariant in `query-cli-contract` spec so future query-family commands ship both surfaces together. Feed lessons (e.g. in-memory graph lifecycle, CORS for local file:// fallback) into the next cycle for a potential `--watch`-aware query API that hot-reloads `graph.json`.