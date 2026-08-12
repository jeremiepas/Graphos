<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Wire join + aggregates into Pipeline.hs — PLAN

**Task slug**: `04-wire-into-pipeline`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Wire `joinCommunitiesToNodes` and `computeCommunityAggregates` into the export pipeline (`Pipeline.hs`), add the `epWriteCommunityAggregates` port method to `ExportPort`, implement it in `Wiring.hs`, and wire the SQLite export. This connects the pure UseCase functions to the actual export flow.

## Detail

### Scope

**Pipeline changes** (`src/Graphos/UseCase/Pipeline.hs`):
1. After the re-cluster step (post-Leiden), call `joinCommunitiesToNodes enrichedGraph' finalComm` to produce `enrichedGraph''` with `community_id` set on all nodes
2. Compute `computeCommunityAggregates enrichedGraph'' finalComm finalCohes artPoints mLabels` after analysis
3. Write aggregates via the new `epWriteCommunityAggregates` port method
4. Use `enrichedGraph''` (with joined community IDs) for all downstream export (`exportAll`, Neo4j, Obsidian)

**ExportPort changes** (`src/Graphos/UseCase/Port/ExportPort.hs`):
- Add `epWriteCommunityAggregates :: IncrementalWriter -> [CommunityAggregate] -> IO ()` to the `ExportPort` class

**Wiring changes** (`src/Graphos/Infrastructure/Export/Wiring.hs`):
- Implement `epWriteCommunityAggregates` using the `writeCommunityAggregates` function from `IncrementalJSON.hs` (Task 5)

**SQLite export** (new module, per Design Decision 4):
- Create `src/Graphos/Infrastructure/Export/SQLite.hs` — streams nodes, edges, and community-edge-pairs into a `graph.sqlite` file via `direct-sqlite`
- Tables: `nodes`, `edges`, `community_edge_pairs` with appropriate indexes
- Batched inserts in transactions to keep peak memory flat
- Denormalize `src_community` and `tgt_community` onto `edges` for single-table drill-down queries

**Static server changes** (`src/Graphos/Server/Static.hs`):
- Add COOP/COEP cross-origin isolation headers: `Cross-Origin-Opener-Policy: same-origin` and `Cross-Origin-Embedder-Policy: require-corp`
- This enables OPFS for `@sqlite.org/sqlite-wasm` in the browser

### Check Criteria

**Tests/gates:**
- (a) `cabal build` with `-Werror` → exits 0
- (b) `cabal test` (full suite) → all tests PASS
- (c) Run on small fixture: `cabal run graphos -- tests/fixtures/small` → produces `graph.json` with non-null `community_id` on community members and `community_aggregates` key
- (d) Run on small fixture → produces `graph.sqlite` with correct row counts
- (e) Verify node/edge/community counts unchanged vs. baseline on the same fixture
- (f) `curl -I http://localhost:8080/graph.html` → shows COOP/COEP headers

**Spec scenarios satisfied:**
- `html-lod-viewer/spec.md` — Scenario "Community aggregates present in export": pipeline produces exactly N aggregates for N communities
- `html-lod-viewer/spec.md` — Scenario "Serve delivers the LOD HTML": `graphos serve --dir graphos-out --port 8080` serves the viewer
- `node-schema/spec.md` — Scenario "Community ID populated after Leiden": `community_id` is non-null for community members
- `node-schema/spec.md` — Scenario "Every community member has a non-null community_id": all nodes in communities have correct `community_id`

**PASS conditions:**
- (a) `cabal build` exits with code 0
- (b) Full test suite passes
- (c) `graph.json` has non-null `community_id` on community members and `community_aggregates` key present and populated
- (d) `graph.sqlite` exists with correct schema and row counts
- (e) Counts match baseline (no regression)
- (f) COOP/COEP headers present

**FAIL boundaries:**
- (a) Compilation error → FAIL
- (b) Any test fails → FAIL
- (c) `community_id` is null for community members → FAIL (spec violation)
- (d) `community_aggregates` missing from `graph.json` → FAIL (spec violation)
- (e) Node/edge/community counts differ from baseline → FAIL (regression)
- (f) COOP/COEP headers missing → FAIL (OPFS won't work)

### Affected Modules

- `src/Graphos/UseCase/Pipeline.hs` — add join + aggregate calls, wire ports
- `src/Graphos/UseCase/Port/ExportPort.hs` — add `epWriteCommunityAggregates` to class
- `src/Graphos/Infrastructure/Export/Wiring.hs` — implement the new port method
- `src/Graphos/Infrastructure/Export/SQLite.hs` — NEW: SQLite export module
- `src/Graphos/Server/Static.hs` — add COOP/COEP headers
- `.cabal` — add `direct-sqlite` dependency if not already present

### Prerequisites

- `joinCommunitiesToNodes` implemented and tested (Task 2)
- `computeCommunityAggregates` implemented and tested (Task 3)
- `writeCommunityAggregates` in `IncrementalJSON.hs` (Task 5 — may run in parallel)
- `CommunityAggregate` Domain type with correct field types (Task 1)

### Risks

- **`unsafeCoerce` pattern in Wiring.hs**: The incremental writer uses `unsafeCoerce` to share state across port implementations. Extending this with new port methods must be done carefully to avoid memory leaks or incorrect state sharing.
- **SQLite export ordering**: Must be called after JSON export (since it uses the same `Graph` input) but the streaming write order matters — aggregates must be written before nodes in the JSON.
- **`direct-sqlite` dependency**: Verify it's in the dependency tree. If not, adding it is a small `.cabal` change — low risk.
- **COOP/COEP headers**: These headers may break consumers embedding the HTML in an iframe. This is an accepted tradeoff (see Design Decision 7).
