<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 5 — Add writeCommunityAggregates to IncrementalJSON.hs — PLAN

**Task slug**: `05-add-write-community-aggregates`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Add `writeCommunityAggregates` to `src/Graphos/Infrastructure/Export/IncrementalJSON.hs` — a function that streams the `community_aggregates` JSON array to the incremental writer. Mirrors the existing `writeGodNodes` pattern. Export it for use by `Wiring.hs`.

## Detail

### Scope

**Implementation** (`src/Graphos/Infrastructure/Export/IncrementalJSON.hs`):
```haskell
writeCommunityAggregates :: IncrementalWriter -> [CommunityAggregate] -> IO ()
writeCommunityAggregates iw aggregates = do
  writeKey iw "community_aggregates"
  BSL.hPut (iwHandle iw) (encode aggregates)
```

This mirrors `writeGodNodes` (uses `writeKey` + `BSL.hPut` + `encode`). The function must be called after `writeGodNodes` and before `writeAnalysisTail` to maintain correct JSON structure.

### Check Criteria

**Tests/gates:**
- (a) `cabal build` with `-Werror` → exits 0
- (b) Temp-file round-trip test: create an `IncrementalWriter` on a temp file, call `writeCommunityAggregates` with a 2-element list, call `closeWriter`, then parse the file → verify it contains `"community_aggregates": [...]` with valid JSON
- (c) `grep "community_aggregates" src/Graphos/Infrastructure/Export/IncrementalJSON.hs` → must be present

**Spec scenarios satisfied:**
- `html-lod-viewer/spec.md` — Scenario "Community aggregates present in export": the JSON export includes `community_aggregates` with the correct number of entries
- `html-lod-viewer/spec.md` — Scenario "Streaming write preserves low memory": the incremental writer streams aggregates without building the full JSON AST

**PASS conditions:**
- (a) `cabal build` exits with code 0
- (b) Round-trip test passes: the temp file contains valid JSON with the correct `community_aggregates` key and data
- (c) The key name is exactly `"community_aggregates"`

**FAIL boundaries:**
- (a) Compilation error → FAIL
- (b) Round-trip test fails: file doesn't contain valid JSON → FAIL
- (c) Key name is not `"community_aggregates"` → FAIL

### Affected Modules

- `src/Graphos/Infrastructure/Export/IncrementalJSON.hs` — add `writeCommunityAggregates` function and export it
- `tests/IncrementalJSONSpec.hs` — add round-trip test (optional but recommended)

### Prerequisites

- `CommunityAggregate` Domain type with Aeson `ToJSON` instance (Task 1)
- `IncrementalWriter` type and `writeKey` function already exist in `IncrementalJSON.hs`

### Risks

- **Ordering**: Must be called after `writeGodNodes` and before `writeAnalysisTail` in the pipeline. The JSON structure requires this ordering. Document the dependency clearly.
- **Test helper pattern**: The round-trip test (open temp file, write, close, parse) is a useful pattern for other incremental writer tests. Consider making it a reusable test helper.
- **Parallel with Task 4**: This task is independent of the Pipeline wiring (Task 4). It can be done in parallel — Task 4 just imports the function that Task 5 produces.
