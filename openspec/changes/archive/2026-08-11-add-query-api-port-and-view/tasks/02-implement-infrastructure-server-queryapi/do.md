<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Implement Infrastructure.Server.QueryAPI — DO

**Task slug**: `02-implement-infrastructure-server-queryapi`
**Attempt**: 1
**Status**: in-progress → completed

## Summary

Implemented the HTTP query API surface as `Graphos.Infrastructure.Server.QueryAPI`, fixed the `budget` query parameter to be honored, and added `Graphos.Infrastructure.Server.QueryAPISpec` proving parity with the CLI JSON renderers, CORS/method handling, and zero per-request file IO.

## Detail

### What was implemented

1. **`src/Graphos/Infrastructure/Server/QueryAPI.hs`**
   - `apiApp :: LoadResult -> Application` routes `/api/query`, `/api/path`, `/api/explain`, `/api/symbols`, `/api/neighbors`.
   - All handlers close over the pre-loaded `LoadResult` (no file IO in request path).
   - `OPTIONS` returns 200 with CORS; non-GET returns 405; unknown `/api/*` returns 404.
   - Every response sets `Content-Type: application/json; charset=utf-8` and `Access-Control-Allow-Origin: *`.
   - Reuses existing pure query functions and JSON renderers from `UseCase.Query` / `UseCase.Query.Render`.
   - `startQueryServer :: Int -> LoadResult -> IO ()` helper for running a standalone API server.
   - **Fix**: the `/api/query` handler now reads the `budget` query parameter (default 2000) and passes it to `queryGraphWithIndexScored`, instead of hard-coding 2000.

2. **`tests/Graphos/Infrastructure/Server/QueryAPISpec.hs`**
   - Builds an in-memory `LoadResult` from a small fixture graph (no `graph.json` file involved).
   - Asserts byte-for-byte JSON parity for `/api/query`, `/api/path`, `/api/explain`, `/api/symbols`, and `/api/neighbors` against the corresponding `render*JSON` functions.
   - Asserts `OPTIONS /api/query` → 200 + `Access-Control-Allow-Origin: *`.
   - Asserts `POST /api/query` → 405.
   - Asserts `GET /api/unknown` → 404.
   - Asserts response headers: `Content-Type: application/json; charset=utf-8` and CORS.
   - Asserts the handler works with an in-memory `LoadResult` (no file reads).

3. **Supporting changes outside Task 2 scope but required to make the tests compile/pass**
   - `src/Graphos/CLI/Parser.hs`: derived `Show` and `Eq` for `Command` so the new `CLI.ParserSpec` parser assertions can use `shouldBe`.
   - `src/Graphos/CLI/Parser.hs`: updated `renderCommandReference` to document `graphos serve` as "Serve HTML + query API" with `--dir`, `--graph`, `--port`, `--api-only`, `--no-api`.
   - `tests/Graphos/CLI/ParserSpec.hs`: added `serveOpts` parser tests for default, `--graph`, `--api-only`, `--no-api`, and combined flags.
   - `tests/fixtures/scaffold/graphos-global-skill.md` and `graphos-query-global-skill.md`: updated the embedded command reference to match the new `renderCommandReference` output.
   - `graphos.cabal`: added `Graphos.Infrastructure.Server.QueryAPISpec` to test `other-modules` and added `wai`, `http-types`, `optparse-applicative` to test build-depends.

### Key decisions

- Kept the module separate from `Static.hs` as decided in `design.md` (Decision 1).
- Did not add `wai-extra` as a new dependency; the spec invokes the `Application` directly via `Network.Wai` and `Network.Wai.Internal` to capture responses.
- The `budget` fix is minimal and scoped to this task; it was the only divergence from the task plan found in the pre-existing code.

### Concrete changed files

- `src/Graphos/Infrastructure/Server/QueryAPI.hs`
- `src/Graphos/CLI/Parser.hs`
- `tests/Graphos/Infrastructure/Server/QueryAPISpec.hs` (new)
- `tests/Graphos/CLI/ParserSpec.hs`
- `tests/fixtures/scaffold/graphos-global-skill.md`
- `tests/fixtures/scaffold/graphos-query-global-skill.md`
- `graphos.cabal`

## Result

Implementation complete. The module compiles with `-Wall -Werror`, and all new tests pass. Ready for `check.md`.
