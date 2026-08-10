## Context

`graphos serve` (PRD §13.1) currently serves only static files via `Infrastructure.Server.Static.startStaticServer` — a Warp `Application` that maps `GET /` to `graph.html` and resolves other paths under `--dir`. The `graph.html` search box (`showSearchResults` in `Infrastructure.Export.HTML`) filters `allNodes` client-side by substring on `label`/`source_file`; it never invokes the real scored query path (`UseCase.Query.queryGraphWithIndexScored` + `UseCase.Query.Refine.refineResponse`) that the CLI `graphos query --json` uses. The CLI already produces a stable `QueryResponse` JSON via `renderQueryResponseJSON` (spec `query-cli-contract`), and `UseCase.Load.loadGraphFromFile` returns a `LoadResult { lrGraph, lrIndex, lrCommunities, lrCohesion }`. The opportunity is to load the graph once in the server and reuse the existing pure query functions over HTTP, then render the response in the navigator — no new domain logic required.

Architecture layers involved (Clean Architecture, PRD §4.1):
- **Domain**: unchanged — `Domain.Graph.Score.QueryResponse`, `Domain.Graph.Index.GraphIndex`, `Domain.Types`.
- **UseCase**: unchanged — `UseCase.Load.loadGraphFromFile`, `UseCase.Query.*`, `UseCase.Query.Refine.refineResponse`. These are pure and already IO-free in their hot path.
- **Infrastructure**: new `Infrastructure.Server.QueryAPI` (Warp `Application` over `/api/*`) + extended `Infrastructure.Server.Static` (compose static + API) + extended `Infrastructure.Export.HTML` (navigator JS) + extended `app/Main.hs` (`Serve` branch) + extended `Graphos.CLI.Parser.serveOpts`.

## Goals / Non-Goals

**Goals:**
- Expose the full query-family JSON contract (`/api/query`, `/api/path`, `/api/explain`, `/api/symbols`, `/api/neighbors`) over HTTP with field-equivalence to CLI `--json`.
- Load `graph.json` once at startup; serve all `/api/*` from in-memory `Graph` + `GraphIndex` (PRD §16.1 query < 500ms).
- Render the `QueryResponse` in the `graph.html` navigator as a graph view (verdict, scored nodes, matched subgraph edges, suggestions) on the existing vis-network canvas.
- Keep `graph.html` self-contained and usable offline (`file://`) via fallback to the existing substring search.
- Preserve the old static-only behavior behind `--no-api`; keep default additive (static + API).

**Non-Goals:**
- No new query algorithms or scoring changes (reuse `queryGraphWithIndexScored` + `refineResponse` verbatim).
- No hot-reload of `graph.json` during a server run (the `--watch`-aware API is a future cycle).
- No authentication, rate limiting, or TLS on `/api/*` (local dev tool; CORS `*` is intentional).
- No new external dependencies — reuse `warp`, `wai`, `aeson`.
- No persistence of query results to `graphos-out/memory/` from the HTTP path (that stays an explicit CLI/MCP action).

## Decisions

### Decision 1: New `Infrastructure.Server.QueryAPI` module, not extend `Static`
**Choice**: Add a new `Infrastructure.Server.QueryAPI` exposing `queryApp :: LoadResult -> Application` (pure over the loaded `LoadResult`) and `apiApp :: LoadResult -> Application` that routes `/api/*`. Compose with `Static.staticApp` at the `Infrastructure.Server` boundary via a small `chooseApp`/`Wai.Application` dispatcher in `Static` (or a new `Infrastructure.Server.Serve` combinator).

**Rationale**: `Static.hs` is a focused file-server; mixing JSON routing and in-memory graph state there would bloat it and couple two concerns. A separate module keeps each `Application` testable in isolation and respects the "explicit exports on every module" convention.

**Alternatives considered**:
- *Extend `Static.hs` with `/api/*` branches* — rejected: couples static serving with stateful API, harder to test, harder to support `--no-api`/`--api-only`.
- *Use `servant`* — rejected: new heavy dependency, not warranted for 5 GET endpoints; `wai` routing by `pathInfo` is already used in `Static.hs`.
- *Run two Warp servers (static + API) on two ports* — rejected: forces two ports/CORS for the HTML page; single port keeps `graph.html` fetches same-origin.

### Decision 2: Hold `LoadResult` in an `MVar`/`IORef` loaded once at startup
**Choice**: `startQueryServer :: FilePath -> ... -> IO ()` calls `loadGraphFromFile` once before `runSettings`, stores the `LoadResult` in an `IORef LoadResult` (or a record `ServerState`), and `queryApp state` closes over it. The request handler reads the `IORef` (never writes) — no per-request file IO.

**Rationale**: `LoadResult` is immutable after load; an `IORef` is the lightest mutable container and matches the "IO pushed to edge" rule (PRD §4.1, §15.2). `MVar` would imply possible blocking semantics we do not need (read-only access).

**Alternatives considered**:
- *Reload per request* — rejected: violates PRD §16.1 latency; also rereads `graph.json` each time.
- *`TVar`/STM* — rejected: no concurrent writers; STM overhead is unnecessary for read-only state.
- *Pass `LoadResult` as a pure arg via a closure without any ref* — equivalent for read-only; `IORef` is chosen only to keep the startup/serve lifecycle explicit and to leave room for a future hot-reload cycle.

### Decision 3: Reuse `renderQueryResponseJSON` / `renderSymbolResultJSON` / `renderNeighborsResultJSON` for HTTP bodies
**Choice**: The `/api/*` handlers call the **same** `UseCase.Query` + `UseCase.Query.Refine` + `UseCase.Query.Render` functions the CLI uses, then return the rendered `Text` as the response body (with `Content-Type: application/json; charset=utf-8`). For `/api/path` and `/api/explain` (which have no existing JSON renderer), add minimal Aeson `ToJSON` encoders in `UseCase.Query.Render` (e.g. `renderPathResultJSON`, `renderExplainResultJSON`) so CLI and HTTP share one renderer.

**Rationale**: Guarantees byte-for-byte field equivalence between `curl /api/query?q=X` and `graphos query "X" --json` (spec `query-cli-contract` MODIFIED). One renderer = one contract.

**Alternatives considered**:
- *Encode `QueryResponse` directly with Aeson in the HTTP layer* — rejected: risks divergence from `renderQueryResponseJSON`; CLI already uses `show . toJSON`, so reuse the same function.
- *Generate a new OpenAPI/typed schema* — out of scope; no new dependency.

### Decision 4: Compose apps by `pathInfo` prefix dispatch
**Choice**: A top-level `Application` checks `pathInfo req`: if it starts with `"api"` → route to `apiApp`; otherwise route to `staticApp`. `--no-api` omits the `apiApp` branch (404 on `/api/*`); `--api-only` omits the `staticApp` branch (404 on non-`/api/*`). `OPTIONS` and `POST` handling live in `apiApp`.

**Rationale**: Simplest composition with no extra deps; mirrors the existing `pathInfo` style in `Static.hs`. Keeps CORS handling in one place (`apiApp`).

**Alternatives considered**:
- *`wai-middleware`* — rejected: another dependency for trivial routing.
- *Two separate `Application`s mounted on different prefixes via a reverse proxy* — rejected: not a single-process local dev tool.

### Decision 5: Navigator fetch with graceful fallback
**Choice**: `showSearchResults(query)` first attempts `fetch('/api/query?q=...&mode=bfs')`; on network failure (or non-200) it falls back to the existing client-side substring filter. A small `apiAvailable` flag is set on first success and cleared on first failure to avoid repeated failing fetches. The verdict/scored-nodes/edges rendering reuses the existing `vis.DataSet` (`overviewNodesDataset`/`overviewEdgesDataset` or `drilldown*`) to highlight the matched subgraph: matched node ids get a highlight color, non-matched are dimmed, matched edges are emphasized; "Reset" restores prior colors.

**Rationale**: Keeps `graph.html` self-contained (works from `file://`, PRD §16.3 reliability). No new JS dependency; reuses vis-network datasets already in the page.

**Alternatives considered**:
- *Require the server (no fallback)* — rejected: breaks the self-contained HTML guarantee and `file://` usability.
- *Embed the whole graph + index in JS and run the query client-side* — rejected: duplicates the Haskell query logic in JS, risks divergence, inflates HTML size.

### Decision 6: `serveOpts` flags are additive
**Choice**: Extend `Graphos.CLI.Parser.serveOpts` with `--graph <path>` (default `graphos-out/graph.json`), `--api-only` (switch), `--no-api` (switch). `Serve` command gains a `graphPath` and two booleans. The `Command` constructor `Serve` is extended (arity change internal to the CLI; not a public API break).

**Rationale**: Additive flags; default (no flag) becomes static+API which is strictly more capable than before, so existing users see no regression. `--no-api` is the explicit back-comat path.

**Alternatives considered**:
- *New `graphos serve-api` subcommand* — rejected: fragments the surface; `serve` is the natural home (PRD §13.1).

## Risks / Trade-offs

| Risk | Mitigation |
| --- | --- |
| Default behavior change (static-only → static+API) surprises users with port-bound `/api/*` | Provide `--no-api` for exact old behavior; document in `serveOpts` help and `renderCommandReference`; `--api-only` for pure API users. |
| In-memory `LoadResult` goes stale if `graph.json` changes (no `--watch`) | Document as a non-goal; tell users to restart `serve` or use `--watch` (future cycle). Print startup log with the loaded file's mtime/hash. |
| Large graphs hold memory for the server lifetime | Acceptable for a local dev tool; document memory ≈ graph size; recommend `--api-only` + smaller graph for constrained envs. |
| `file://` fallback keeps two code paths (substring + API) | Keep substring fallback minimal and clearly the degraded path; prefer API when available. The fallback never claims a verdict. |
| `OPTIONS`/CORS surface broadens attack area for a local tool | Bind to `0.0.0.0` only by explicit flag; default `localhost`-usable; CORS `*` is acceptable for local dev (matches existing `Static.hs`). |
| New JSON renderers for `/api/path` and `/api/explain` diverge from CLI text form | Add `renderPathResultJSON`/`renderExplainResultJSON` in `UseCase.Query.Render` and unit-test them alongside the existing renderers; keep CLI `path`/`explain` text rendering unchanged but expose `--json` consistently in a later cycle (out of scope here beyond the HTTP port). |

## Verification Strategy (Check)

- **Build**: `cabal build` clean with `-Wall -Wcompat -Wincomplete-uni-patterns -Werror` (PRD §15.2).
- **Unit/property tests (`cabal test`)**:
  - `Server.QueryAPISpec`: for a fixture `graph.json`, assert `GET /api/query?q=auth` response body equals `renderQueryResponseJSON (refineResponse defaultRefineConfig (gNodes g) (queryGraphWithIndexScored g idx "auth" "bfs" 2000))` (byte-for-byte) — proves CLI/HTTP parity.
  - `Server.QueryAPISpec`: `/api/path`, `/api/explain`, `/api/symbols`, `/api/neighbors` parity with their `render*JSON` counterparts.
  - `Server.QueryAPISpec`: `OPTIONS /api/query` → 200 + CORS; `POST /api/query` → 405; missing graph → server startup exits non-zero.
  - `Server.QueryAPISpec`: two consecutive requests return the same `hash` and do not touch the file (use a file-access assertion or a one-shot `IORef` load counter).
  - `CLI.ParserSpec`: `serve --no-api`, `serve --api-only`, `serve --graph X` parse correctly; `renderCommandReference` lists the new flags.
- **Latency check**: a microbenchmark asserting 10 consecutive `/api/query` requests each complete < 500ms on the in-memory graph (PRD §16.1).
- **HTML/JS**: extend the existing self-contained HTML generation test to assert the generated `graph.html` contains the `/api/query` fetch call and the fallback branch, and that `showSearchResults` renders the verdict header.
- **Manual**: `graphos .` → `graphos serve` → open `graph.html`, type a query, observe verdict + scored nodes + highlighted subgraph; `curl /api/query?q=...` matches `graphos query --json`.

## Iteration & Rollback (Act)

- If `cabal test` fails on parity (HTTP ≠ CLI JSON): treat as a contract bug, fix the shared renderer, do not patch only one side. Record under "Attempt history" in the failing task; re-run PDCA for that task.
- If latency exceeds 500ms: profile the hot path (likely index lookup or Aeson encode); ensure no per-request file IO; consider caching the encoded JSON for the most recent query (future cycle).
- If the `file://` fallback regresses: keep the substring path behind an explicit `apiAvailable=false` flag and unit-test both branches.
- Rollback: revert the `serve` default to static-only by flipping the default of `--no-api` to true (one-line change) while keeping the API code merged but dormant, if the additive behavior causes issues in the field.
- Standardize: after Check passes, update `docs/workflows/04-query.md` (or a new `17-serve-api.md`) to document the HTTP contract; add a CI check that the `query-cli-contract` parity test runs for both CLI `--json` and HTTP `/api/*`. Feed `--watch`-aware hot-reload into the next PDCA cycle.

## Open Questions

- Should `/api/explain` and `/api/path` also gain a CLI `--json` flag in this change, or only the HTTP port? (Lean: HTTP port only here; CLI `--json` parity for `path`/`explain` is a small follow-up cycle to avoid scope creep.)
- Default bind host: keep `0.0.0.0` (current `Static.hs`) or switch to `127.0.0.1` for safer local-only default? (Lean: keep `0.0.0.0` to match current behavior; revisit in a security cycle.)
- Should the API expose `/api/communities` and `/api/analysis` (god nodes, bridges)? (Out of scope; future cycle.)