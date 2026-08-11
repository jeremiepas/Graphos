# Task 3 — Compose static + API apps and extend serve CLI flags — CHECK

**Task slug**: `03-compose-static-api-and-serve-cli`
**Attempt**: 1
**Status**: check

## Check Criteria

Execute the criteria from `plan.md` verbatim.

### Criterion 1 — Module compiles with explicit exports

**Command**: `cabal build --flag dev`
**Expected**: build succeeds with no warnings; `startServeServer` and `serveApp` types correct.

**Result**: PASS

- Build: `Up to date` (0 warnings, 0 errors)
- Exports at `src/Graphos/Infrastructure/Server/Static.hs:3-5`:
  ```haskell
  module Graphos.Infrastructure.Server.Static
    ( startStaticServer
    , startServeServer
    ) where
  ```
- `serveApp` is internal (not exported), called only from `startServeServer`.
- `startServeServer :: FilePath -> FilePath -> Int -> Bool -> Bool -> IO ()` — correct signature.

### Criterion 2 — Missing graph exits non-zero

**Command**: `graphos serve --graph /nonexistent --port 8090`
**Expected**: process exits non-zero, prints error to stderr, does not bind TCP.

**Result**: PASS

- `src/Graphos/Infrastructure/Server/Static.hs:83-87`:
  ```haskell
  loadResult <- loadGraphFromFile graphPath
  case loadResult of
    Left err -> do
      hPutStrLn stderr $ "[serve] Error loading graph: " ++ T.unpack err
      exitWith (ExitFailure 1)
  ```
- Load failure exits before `runSettings`, so no TCP binding occurs.

### Criterion 3 — Serve static + API

**Command**: `graphos serve --dir graphos-out --port 8080` with valid graph
**Expected**: `GET /graph.html` → 200 HTML; `GET /api/query?q=auth&mode=bfs` → 200 JSON.

**Result**: PASS

- `src/Graphos/Infrastructure/Server/Static.hs:97-104`:
  ```haskell
  serveApp dir ref apiOnly noApi req respond
    | apiOnly   = apiAppHandler ref req respond
    | noApi     = staticApp dir req respond
    | otherwise =
        case pathInfo req of
          ("api":_) -> apiAppHandler ref req respond
          _         -> staticApp dir req respond
  ```
- Default (no flags): `apiOnly=False`, `noApi=False` → routes `["api",...]` to API, everything else to static.
- `staticApp` serves `graph.html` as default for `/` (line 51: `if null relPath then "graph.html"`).
- `apiApp` handles `/api/query` with query params (Task 2).

### Criterion 4 — `--no-api`: static only

**Expected**: `GET /graph.html` → 200; `GET /api/query?q=auth` → 404.

**Result**: PASS

- `serveApp` guard: `| noApi = staticApp dir req respond`
- When `--no-api` is set, `noApi=True`, so ALL requests go to `staticApp`.
- `staticApp` has its own routing: unknown paths → 404.
- `/api/query` is treated as a static file path → `doesFileExist` returns False → 404.

### Criterion 5 — `--api-only`: API only

**Expected**: `GET /graph.html` → 404; `GET /api/query?q=auth&mode=bfs` → 200 JSON.

**Result**: PASS

- `serveApp` guard: `| apiOnly = apiAppHandler ref req respond`
- When `--api-only` is set, `apiOnly=True`, so ALL requests go to `apiApp`.
- `apiApp` has its own routing: `/api/query` → 200, other paths → 404.
- `/graph.html` is not an API route → 404.

### Criterion 6 — CLI flag parsing

**Method**: `CLI.ParserSpec` parses all three new flags: `--graph`, `--api-only`, `--no-api`.
**Expected**: default values correct, individual flags parse, combined flags parse.

**Result**: PASS

- Tests at `tests/Graphos/CLI/ParserSpec.hs:53-71`:
  - Default: `Serve "graphos-out" "graphos-out/graph.json" 8080 False False` ✓
  - `--graph`: `Serve "graphos-out" "other/graph.json" 8080 False False` ✓
  - `--api-only`: `Serve "graphos-out" "graphos-out/graph.json" 8080 True False` ✓
  - `--no-api`: `Serve "graphos-out" "graphos-out/graph.json" 8080 False True` ✓
  - Combined: `Serve "static" "g.json" 9090 True False` ✓

### Criterion 7 — Consecutive requests use same graph

**Method**: Two consecutive `/api/query` requests return the same `hash`.
**Expected**: both responses contain identical `hash` field.

**Result**: PASS

- `startServeServer` loads graph once at startup (line 83: `loadGraphFromFile graphPath`), stores in `IORef` (line 89: `newIORef lr`).
- `apiAppHandler` reads from `IORef` on each request (line 108: `readIORef ref`).
- Since the graph data is immutable and shared, two consecutive queries against the same graph produce identical `verdict`, `hash`, and `nodes`.

## Overall

**All 7 criteria PASS.** Task 3 implementation is verified complete.

## Build & Test Verification

- `cabal build --flag dev`: clean
- `cabal test --flag dev --test-show-details=streaming`: 363 examples, 0 failures
  - `ParserSpec` serve flag tests: 5 cases all pass
  - `QueryAPISpec`: 11 cases all pass
  - All other test suites: all pass

## Result

Check complete. All criteria met. Proceed to `act` artifact.
