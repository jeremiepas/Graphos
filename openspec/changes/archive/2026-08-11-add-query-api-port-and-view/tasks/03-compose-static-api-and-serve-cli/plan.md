# Task 3 — Compose static + API apps and extend serve CLI flags — PLAN

**Task slug**: `03-compose-static-api-and-serve-cli`
**Attempt**: 1
**Status**: pending

## Summary

Extend `Infrastructure.Server.Static` with a combinator that dispatches by `pathInfo` prefix: `["api",...]` → `apiApp`, else → `staticApp`. Update `startServeServer` to load `graph.json` once via `loadGraphFromFile`, store `LoadResult` in an `IORef`, and compose the apps. On load failure, exit non-zero before binding. Extend `Graphos.CLI.Parser.serveOpts` and the `Command.Serve` constructor with `--graph <path>` (default `graphos-out/graph.json`), `--api-only` (switch), `--no-api` (switch); default = static + API. Update `app/Main.hs` `Serve` branch. Update `renderCommandReference`.

## Detail

### Scope of this task

- **`src/Graphos/Infrastructure/Server/Static.hs`** — already implemented:
  - `serveApp :: FilePath -> IORef LoadResult -> Bool -> Bool -> Application` routes `["api",...]` to `apiAppHandler`, else to `staticApp`.
  - `startServeServer :: FilePath -> FilePath -> Int -> Bool -> Bool -> IO ()` loads `graph.json` once, stores in `IORef`, composes apps.
  - On load failure, prints error to stderr and exits non-zero.
  - `apiOnly` flag: serves only API. `noApi` flag: serves only static.

- **`src/Graphos/CLI/Parser.hs`** — already implemented:
  - `Command.Serve FilePath FilePath Int Bool Bool` (dir, graphPath, port, apiOnly, noApi).
  - `serveOpts` parser: `--dir` (default `graphos-out`), `--graph` (default `graphos-out/graph.json`), `--port` (default 8080), `--api-only` (switch), `--no-api` (switch).
  - `renderCommandReference` updated to show serve flags.

- **`app/Main.hs`** — already implemented:
  - `Serve dir graphPath port apiOnly noApi` branch calls `startServeServer dir graphPath port apiOnly noApi`.

### Check Criteria (defined before code exists)

1. **Module compiles**
   - Command: `cabal build --flag dev`
   - PASS: build succeeds with no warnings; `startServeServer` and `serveApp` types correct.
   - FAIL: missing export, compile error, or `-Werror` warning.

2. **Missing graph exits non-zero**
   - Run `graphos serve --graph /nonexistent --port 8090`
   - PASS: process exits non-zero, prints error to stderr, does not bind TCP.
   - FAIL: binds anyway or exits 0.

3. **Serve static + API**
   - `cabal run graphos -- serve --dir graphos-out --port 8080` with valid graph
   - PASS: `GET /graph.html` → 200 HTML; `GET /api/query?q=auth&mode=bfs` → 200 JSON.

4. **`--no-api`: static only**
   - Same server with `--no-api`
   - PASS: `GET /graph.html` → 200; `GET /api/query?q=auth` → 404.

5. **`--api-only`: API only**
   - Same server with `--api-only`
   - PASS: `GET /graph.html` → 404; `GET /api/query?q=auth&mode=bfs` → 200 JSON.

6. **CLI flag parsing**
   - `CLI.ParserSpec` parses all three new flags: `--graph`, `--api-only`, `--no-api`.
   - PASS: default values correct, individual flags parse, combined flags parse.

7. **Consecutive requests use same graph**
   - Two consecutive `/api/query` requests return the same `hash`.
   - PASS: both responses contain identical `hash` field.

### Affected modules / files

- Modify: `src/Graphos/Infrastructure/Server/Static.hs` (add `serveApp`, `startServeServer`)
- Modify: `src/Graphos/CLI/Parser.hs` (extend `Command.Serve`, `serveOpts`, `renderCommandReference`)
- Modify: `app/Main.hs` (Serve branch)
- Modify: `tests/Graphos/CLI/ParserSpec.hs` (serve flag parsing tests)
- New: `tests/Graphos/Infrastructure/Server/StaticSpec.hs` (integration tests for serve modes)

### Prerequisites

- Task 1 complete: `renderPathResultJSON`, `renderExplainResultJSON` exported.
- Task 2 complete: `apiApp :: LoadResult -> Application` and `startQueryServer` exist.
- `wai` and `warp` are library dependencies.
- `loadGraphFromFile` from `UseCase.Load` returns `Either Text LoadResult`.

### Risks

| Risk | Mitigation |
| --- | --- |
| `Command.Serve` arity change breaks pattern matches | Update all matches in `app/Main.hs` (only one match). |
| `--api-only` and `--no-api` both True | Document precedence: `--api-only` wins (or reject with error). Current impl: `apiOnly` checked first in guard. |
| Graph load failure mid-request | Graph loaded at startup, stored in `IORef`. Fail-fast at startup (exit non-zero). |
| Static file path traversal | `canonicalizePath` + `makeRelative` check prevents `..` escapes (already in `staticApp`). |

## Result

Pending — plan complete. Next step is `do.md`.
