<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 1 — Add CommunityAggregate Domain type — PLAN

**Task slug**: `01-add-community-aggregate-type`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

Add the `CommunityAggregate` Domain type to `src/Graphos/Domain/Types/Analysis.hs` with the corrected `caInterCommunityEdges` field type (`![(CommunityId, Int)]` per Decision 8) and Aeson instances with snake_case field naming. This is the canonical aggregate shape for HTML overview, SQLite export, and downstream consumers.

## Detail

### Scope

The `CommunityAggregate` type already exists in `src/Graphos/Domain/Types/Analysis.hs:85-117` but has `caInterCommunityEdges :: !Int` (scalar count) instead of the list-of-pairs shape required by the spec and design. This task fixes that field type and ensures all instances are correct.

**Changes:**
1. Change `caInterCommunityEdges :: !Int` to `caInterCommunityEdges :: ![(CommunityId, Int)]` in the `CommunityAggregate` record (line 93 of `Analysis.hs`).
2. Update `ToJSON` instance: the field serializes as a JSON array of objects `[{"target": <cid>, "count": <n>}, ...]` — Aeson's default `[(Int, Int)]` encoding produces `[<cid>, <n>]` tuples which don't match the spec's `{"target":..,"count":..}` shape. A custom `toJSON` or `withObject` mapping is needed.
3. Update `FromJSON` instance: parse the `{"target":..,"count":..}` array format back into `[(CommunityId, Int)]`.
4. Export from `Graphos.Domain.Types` (already done via the module export list).

### Check Criteria

**Tests/gates:**
- (a) `cabal build` with `-Werror` → must compile without errors
- (b) Hspec property: serialize a `CommunityAggregate` with `caInterCommunityEdges = [(4, 5), (8, 2)]` → verify the JSON object contains `"inter_community_edges"` as an array of objects with `"target"` and `"count"` keys
- (c) Hspec parse: parse the expected JSON shape → verify round-trip equality
- (d) `ls src/Graphos/Domain/Types/Analysis.hs` → confirm module path under `Domain/`
- (e) `grep -c "IO" src/Graphos/Domain/Types/Analysis.hs` → must be 0

**Spec scenarios satisfied:**
- `html-lod-viewer/spec.md` — Scenario "Inter-community edges listed": community A has 5 edges to B and 2 edges to C → `inter_community_edges` contains entries for B (count 5) and C (count 2) as `{"target": <cid>, "count": <n>}` objects
- `node-schema/spec.md` — no direct impact (this is an aggregate type, not a node field)

**PASS conditions:**
- (a) `cabal build` exits with code 0
- (b) JSON serialization test passes: the `inter_community_edges` field is an array of `{"target": <Int>, "count": <Int>}` objects
- (c) JSON deserialization test passes: round-trip equality
- (d) Module path confirmed under `src/Graphos/Domain/`
- (e) Zero `IO` imports in the module

**FAIL boundaries:**
- (a) Compilation error or warning with `-Werror` → FAIL
- (b) `inter_community_edges` serializes as a flat array `[4, 5, 8, 2]` instead of `[{"target":4,"count":5},{"target":8,"count":2}]` → FAIL (spec violation)
- (c) Type is not under `src/Graphos/Domain/` → FAIL
- (d) Module imports `IO`-related types → FAIL

### Affected Modules

- `src/Graphos/Domain/Types/Analysis.hs` — modify `CommunityAggregate` record field and Aeson instances

### Prerequisites

- `CommunityId` and `NodeId` types are available from `Graphos.Domain.Types.Graph` (already imported)
- `Data.Aeson` is already imported

### Risks

- **Aeson default encoding**: `[(Int, Int)]` serializes as a flat array of pairs, not as objects with named keys. Must use a custom `toJSON`/`parseJSON` or a wrapper newtype to get the `{"target":..,"count":..}` shape. This is a known Aeson limitation — not a blocker but requires careful handling.
- **Downstream consumers**: `Cluster.hs:142` currently does `Map.size (Map.findWithDefault Map.empty cid interEdgeCounts)` which returns an `Int`. After the type change, this expression no longer type-checks — the caller (`computeCommunityAggregates`) must be updated to return the raw map. This is handled in Task 3's Do step.
