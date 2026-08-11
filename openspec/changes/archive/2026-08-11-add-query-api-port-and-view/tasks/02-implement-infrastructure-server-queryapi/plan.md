<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Implement Infrastructure.Server.QueryAPI — PLAN

**Task slug**: `02-implement-infrastructure-server-queryapi`
**Attempt**: 1
**Status**: pending

## Summary

Create the HTTP API surface for the query family (`/api/query`, `/api/path`, `/api/explain`, `/api/symbols`, `/api/neighbors`) as a new Warp `Application` that closes over a pre-loaded `LoadResult`. Reuse existing pure query functions and JSON renderers from `UseCase.Query` and `UseCase.Query.Render`. Ensure CORS, content-type, OPTIONS/405 handling, and a per-request no-file-IO guarantee.

## Detail

### Scope of this task

- Add `src/Graphos/Infrastructure/Server/QueryAPI.hs`.
- Expose `apiApp :: LoadResult -> Application` and `startQueryServer :: Int -> LoadResult -> IO ()`.
- Implement routing on `pathInfo` for the five `/api/*` endpoints.
- Map query parameters to the existing pure functions:
  - `/api/query?q=&mode=&budget=` → `queryGraphWithIndexScored` + `refineResponse defaultRefineConfig`
  - `/api/path?from=&to=` → `pathQueryWithIndex`
  - `/api/explain?node=` → `explainNodeWithIndex`
  - `/api/symbols?name=` → `symbolLookup`
  - `/api/neighbors?id=&depth=` → `neighborhoodExpansion`
- Render all response bodies with the existing shared JSON renderers (`renderQueryResponseJSON`, `renderSymbolResultJSON`, `renderNeighborsResultJSON`, `renderPathResultJSON`, `renderExplainResultJSON`).
- Add CORS (`Access-Control-Allow-Origin: *`) and `Content-Type: application/json; charset=utf-8` to every response.
- `OPTIONS /api/*` → 200 empty body with CORS.
- Non-GET (except OPTIONS) `/api/*` → 405.
- Unknown `/api/*` → 404.
- Add the module to `graphos.cabal` exposed-modules.
- Add an Hspec module `tests/Graphos/Infrastructure/Server/QueryAPISpec.hs`.

### Check Criteria (defined before code exists)

These criteria must be met before marking Task 2 complete. `check.md` will execute them verbatim.

1. **Module compiles with explicit exports**
   - Command: `cabal build --flag dev`
   - PASS: build succeeds with no warnings treated as errors; `apiApp` and `startQueryServer` are exported from `Graphos.Infrastructure.Server.QueryAPI`.
   - FAIL: missing export, compile error, or `-Werror` warning.

2. **`/api/query` parity with CLI JSON**
   - In `QueryAPISpec`, create a fixture graph (`Graph` + `GraphIndex`), call `apiApp` with `GET /api/query?q=auth&mode=bfs&budget=2000`, and compare the response body to `renderQueryResponseJSON (refineResponse defaultRefineConfig (gNodes g) (queryGraphWithIndexScored g idx "auth" "bfs" 2000))`.
   - PASS: exact `Text` equality (same verdict, hash, nodes).
   - FAIL: any divergence in body bytes.

3. **`/api/path` parity with renderer**
   - Call `GET /api/path?from=A&to=B` against `apiApp` and compare the body to `renderPathResultJSON (pathQueryWithIndex g idx "A" "B")`.
   - PASS: exact `Text` equality; `Nothing` path yields `{"path":null}`; `Just` path yields `{"path":[...],"hops":n}`.
   - FAIL: wrong JSON shape or missing `hops`.

4. **CORS and method handling**
   - `OPTIONS /api/query` → HTTP 200, empty body, header `Access-Control-Allow-Origin: *`.
   - `POST /api/query` → HTTP 405.
   - `GET /api/foo` → HTTP 404.
   - PASS: all three assertions hold in `QueryAPISpec`.
   - FAIL: any status or header mismatch.

5. **No per-request file IO**
   - The handler must close over `LoadResult` (or an `IORef LoadResult`) loaded before the server runs. No `readFile`, `loadGraphFromFile`, or `doesFileExist` inside the request handler.
   - PASS: review of `QueryAPI.hs` shows zero file-system IO inside `apiApp` / handlers.
   - FAIL: any file-system call reachable from request handling.

### Affected modules / files

- New: `src/Graphos/Infrastructure/Server/QueryAPI.hs`
- New: `tests/Graphos/Infrastructure/Server/QueryAPISpec.hs`
- Modify: `graphos.cabal` (add `Graphos.Infrastructure.Server.QueryAPI` to `exposed-modules`; add `Graphos.Infrastructure.Server.QueryAPISpec` to `other-modules` in test suite if not auto-discovered)

### Prerequisites

- Task 1 complete: `renderPathResultJSON` and `renderExplainResultJSON` exist in `UseCase.Query.Render` and are exported.
- Pure query functions (`queryGraphWithIndexScored`, `pathQueryWithIndex`, `explainNodeWithIndex`, `symbolLookup`, `neighborhoodExpansion`) and `LoadResult` are available from `UseCase.Query` / `UseCase.Load`.
- `warp` and `wai` are already library dependencies.
- A small fixture `Graph` + `GraphIndex` builder exists in the test suite (reuse the same fixture as Task 5 / `UseCase.QuerySpec`).

### Risks

| Risk | Mitigation |
| --- | --- |
| `wai-extra` not in dependencies for testing | Test via `Network.Wai.Test` (`defaultRequest` + `requestMethod`/`pathInfo`/`queryString`) or direct `Application` invocation; both are available through `wai`. |
| Budget query parameter ignored (hard-coded 2000) | Read `budget` from query string with default 2000; add a test case. |
| `mode` default mismatch | Default `mode` to `"bfs"` to match CLI. |
| Missing `Access-Control-Allow-Origin` on error responses | Build a single `corsResponse`/`jsonResponse` helper used by every branch (200, 404, 405, OPTIONS). |
| `startQueryServer` signature differs from design | Document final signature in `check.md`; if it diverges, update `design.md` in Task 3. |

## Result

Pending — plan complete. Next step is `do.md`: implement the module and its test.
