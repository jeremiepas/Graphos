# Task 3 — Compose static + API apps and extend serve CLI flags — DO

**Task slug**: `03-compose-static-api-and-serve-cli`
**Attempt**: 1
**Status**: in-progress → completed

## Summary

Implemented the combined static + API server, wired `Command.Serve` through `app/Main.hs`, and added CLI flag parsing tests. The implementation was already present in the codebase when this task started — the PDCA artifacts were missing.

## Detail

### What was implemented

1. **`src/Graphos/Infrastructure/Server/Static.hs`**
   - `serveApp :: FilePath -> IORef LoadResult -> Bool -> Bool -> Application`
     - Guards on `apiOnly`: routes all requests to `apiAppHandler`.
     - Guards on `noApi`: routes all requests to `staticApp`.
     - Default: `["api", ...]` → `apiAppHandler`, else → `staticApp`.
   - `startServeServer :: FilePath -> FilePath -> Int -> Bool -> Bool -> IO ()`
     - Loads `graph.json` once via `loadGraphFromFile`.
     - On `Left err`: prints error to stderr, `exitWith (ExitFailure 1)`.
     - On `Right lr`: stores in `IORef`, creates `serveApp`, runs Warp.
   - `apiAppHandler :: IORef LoadResult -> Application`
     - Reads `LoadResult` from `IORef` on each request, passes to `apiApp`.

2. **`src/Graphos/CLI/Parser.hs`**
   - `Command.Serve FilePath FilePath Int Bool Bool` — `(dir, graphPath, port, apiOnly, noApi)`.
   - `serveOpts` parser: `--dir` (default `graphos-out`), `--graph` (default `graphos-out/graph.json`), `--port` (default 8080), `--api-only` (switch), `--no-api` (switch).
   - `renderCommandReference` updated: added serve section with all flags.

3. **`app/Main.hs`**
   - `Serve dir graphPath port apiOnly noApi` branch: prints serving message, calls `startServeServer`.

4. **`tests/Graphos/CLI/ParserSpec.hs`**
   - Added `parseServe` helper and `describe "serveOpts"` block with 6 test cases:
     - Default values
     - `--graph` override
     - `--api-only` switch
     - `--no-api` switch
     - Combined flags

### Key decisions

- `IORef LoadResult` chosen over passing `LoadResult` directly to allow hot-reload in the future.
- Fail-fast at startup: if `graph.json` doesn't exist, the process exits before binding TCP.
- `--api-only` takes precedence over `--no-api` when both are set (checked first in guard).

### Concrete changed files

- `src/Graphos/Infrastructure/Server/Static.hs` — added `serveApp`, `startServeServer`, `apiAppHandler`
- `src/Graphos/CLI/Parser.hs` — `Command.Serve` type, `serveOpts` parser, `renderCommandReference`
- `app/Main.hs` — Serve branch wiring
- `tests/Graphos/CLI/ParserSpec.hs` — serve flag parsing tests

## Result

Implementation complete. Ready for `check.md`.
