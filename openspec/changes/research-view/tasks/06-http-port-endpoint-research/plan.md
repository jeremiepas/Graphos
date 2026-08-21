# Task 6 — HTTP port endpoint for research (deferred) — PLAN

**Task slug**: `06-http-port-endpoint-research`
**Attempt**: 1
**Status**: pending

## Summary

Add `GET /api/research?terms=a,b,c&subgraph=d,e&edges=semantic` to `src/Graphos/Infrastructure/Server/QueryAPI.hs` — returns the same `ResearchView` JSON as CLI `--json`, with `terms` parsed as a comma-separated list. **Dependency**: waits for `query-http-port` to merge.

## Detail

### Scope

- **Extend**: `src/Graphos/Infrastructure/Server/QueryAPI.hs` (deferred until `query-http-port` lands)
  - Add route handler for `GET /api/research`
  - Parse query parameters: `terms` (comma-separated, required), `subgraph` (comma-separated, optional), `edges` (`semantic` or `all`, optional)
  - Call `buildResearchView` with parsed terms and seeds
  - Return `ResearchView` as JSON with `Content-Type: application/json`
  - Return 400 if `terms` is empty or missing
- **Hspec module**: `test/Graphos/Infrastructure/Server/QueryAPISpec.hs` (new test cases for research endpoint)

### Check Criteria

**Tests to run**:
- `cabal test` — all Hspec cases in `test/Graphos/Infrastructure/Server/QueryAPISpec.hs` (new research endpoint tests)
- `cabal build --flag dev` with `-Wall -Werror` — clean, no warnings

**Spec scenarios satisfied**:
- `Scenario: HTTP research matches CLI` (spec § "HTTP research matches CLI")
- `Scenario: empty terms returns 400` (spec § "empty terms returns 400")

**PASS conditions**:
- `GET /api/research?terms=phase,work&edges=semantic` returns valid `ResearchView` JSON
- Response is byte-for-byte equal to `graphos research phase work --edges semantic --json` for the same `graph.json`
- `terms` parameter is parsed as a comma-separated list
- `subgraph` parameter is parsed as a comma-separated list of seed terms
- Empty `terms` (e.g., `?terms=`) returns HTTP 400 with a clear error message
- Missing `terms` returns HTTP 400 with a clear error message
- Edge cases: `terms` with a single term works correctly

**FAIL boundaries**:
- If the HTTP response is not byte-for-byte equal to the CLI output for the same inputs, the test fails
- If `terms=` does not return 400, the test fails
- If the endpoint crashes on malformed query parameters, the test fails
- If the endpoint imports from the CLI module (circular or architecture violation), the test fails

### Affected modules

- **Extended**: `src/Graphos/Infrastructure/Server/QueryAPI.hs` (deferred)
- **New tests**: `test/Graphos/Infrastructure/Server/QueryAPISpec.hs` (append research endpoint test cases)
- **Imports from**: `src/Graphos/Domain/Query/Research.hs` (ResearchView, ToJSON), `src/Graphos/UseCase/Query/Research.hs` (buildResearchView), existing HTTP server infrastructure (Warp/Scotty/Servant, TBD based on query-http-port)

### Prerequisites

- Task 1 (Domain types) must be implemented
- Task 2 (UseCase: buildResearchView) must be implemented
- The `query-http-port` change must be merged (provides the HTTP server infrastructure and the `GET /api/query` endpoint pattern to follow)
- Existing `QueryAPI.hs` must exist with established endpoint patterns

### Risks

- **High**: This task is BLOCKED by `query-http-port` — cannot implement until that change is merged. The plan is a placeholder that will be executed after the dependency.
- **Medium**: Byte-for-byte parity with CLI `--json` requires that the HTTP endpoint uses the exact same `ResearchView` → JSON encoding path (no re-encoding)
- **Low**: Query parameter parsing is straightforward with the existing HTTP framework
