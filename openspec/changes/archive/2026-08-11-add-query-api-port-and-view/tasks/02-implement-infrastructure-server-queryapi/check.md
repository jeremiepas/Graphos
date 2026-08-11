<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Implement Infrastructure.Server.QueryAPI — CHECK

**Task slug**: `02-implement-infrastructure-server-queryapi`
**Attempt**: 1
**Status**: check

## Check Criteria

Execute the criteria from `plan.md` verbatim.

### Criterion 1 — Module compiles with explicit exports

**Command**: `cabal build --flag dev`
**Expected**: build succeeds with no warnings; `apiApp` and `startQueryServer` exported from `Graphos.Infrastructure.Server.QueryAPI`.

**Result**: PASS

- Build output: `Build completed successfully.` (0 warnings, 0 errors)
- Exports confirmed at `src/Graphos/Infrastructure/Server/QueryAPI.hs:8-9`:
  ```haskell
  module Graphos.Infrastructure.Server.QueryAPI
    ( apiApp
    , startQueryServer
    ) where
  ```

### Criterion 2 — `/api/query` parity with CLI JSON renderer

**Method**: In `QueryAPISpec`, create a fixture graph, call `apiApp` with `GET /api/query?q=Auth&mode=bfs&budget=2000`, compare body to `renderQueryResponseJSON (refineResponse defaultRefineConfig (gNodes g) (queryGraphWithIndexScored g idx "Auth" "bfs" 2000))`.
**Expected**: exact `Text` equality (same verdict, hash, nodes).

**Result**: PASS

- Test: `tests/Graphos/Infrastructure/Server/QueryAPISpec.hs:127-135`
- Fixture graph has 4 nodes, 3 edges. Query for "Auth" with mode=bfs, budget=2000.
- `runApi methodGet "/api/query?q=Auth&mode=bfs&budget=2000"` → body matches `encodeExpected expected` exactly.

### Criterion 3 — `/api/path` parity with renderer

**Method**: Call `GET /api/path?from=AuthModule&to=Database` and compare body to `renderPathResultJSON (pathQueryWithIndex g idx "AuthModule" "Database")`.
**Expected**: exact `Text` equality; `Nothing` path yields `{"path":null}`; `Just` path yields `{"path":[...],"hops":n}`.

**Result**: PASS

- Test: `tests/Graphos/Infrastructure/Server/QueryAPISpec.hs:147-153`
- Path from "AuthModule" to "Database" exists (2 hops: AuthModule → AuthLogin → Database).
- Body matches `encodeExpected expected` exactly.

### Criterion 4 — CORS and method handling

**Tests**:
- `OPTIONS /api/query` → HTTP 200, empty body, header `Access-Control-Allow-Origin: *`
- `POST /api/query` → HTTP 405
- `GET /api/foo` → HTTP 404

**Expected**: all three assertions hold in `QueryAPISpec`.

**Result**: PASS

- Test `OPTIONS /api/query returns 200 with CORS header`: lines 180-184 — status 200, body `""`, header `"*"` ✓
- Test `POST /api/query returns 405`: lines 186-188 — status 405 ✓
- Test `GET /api/unknown returns 404`: lines 190-192 — status 404 ✓

### Criterion 5 — No per-request file IO

**Method**: Review of `QueryAPI.hs` shows zero file-system IO inside `apiApp` / handlers.
**Expected**: no `readFile`, `loadGraphFromFile`, or `doesFileExist` reachable from request handling.

**Result**: PASS

- `apiApp` at line 50 closes over `LoadResult` (pre-loaded, immutable).
- All handlers extract `g = lrGraph lr` and `idx = lrIndex lr` from the `LoadResult` — pure `Data.Map` lookups.
- No calls to `System.IO`, `Data.ByteString.IO`, `pathExists`, or any filesystem-related imports.
- `startQueryServer` (line 126) only calls `runSettings` with `Warp` settings — the graph data is already in memory.

## Overall

**All 5 criteria PASS.** Task 2 implementation is verified complete.

## Build & Test Verification

- `cabal build --flag dev`: clean
- `cabal test --flag dev --test-show-details=streaming`: 363 examples, 0 failures
  - `QueryAPISpec`: 11 test cases all pass
  - `ParserSpec` (new serve flag tests): all pass
  - All other test suites: all pass

## Result

Check complete. All criteria met. Proceed to `act` artifact.
