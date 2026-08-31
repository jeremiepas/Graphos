---
description: "Task 2 — Persist compositions to graph.json"
---
---
description: "Plan: add gCompositions to Graph, wire into pipeline, implement loadGraphFromFile, and tests"
---

# Task 2 — Persist compositions to graph.json — PLAN

**Task slug**: `02-persist-compositions-to-graph-json`
**Attempt**: 1
**Status**: completed

## Summary

Add `gCompositions :: Maybe (Map CommunityId CommunityComposition)` to the `Graph` record, update serialization, wire `computeCompositions` into the pipeline post-Leiden, implement loading from graph.json, and add Hspec tests for round-trips, legacy compatibility, and pipeline integration.

## Detail

### Scope

- **Graph record update** in `src/Graphos/Domain/Graph/Core.hs`:
  Add `gCompositions :: Maybe (Map CommunityId CommunityComposition)` with `Nothing` default
- **ToJSON / FromJSON** in same module or `Infrastructure/Export/JSON.hs`:
  - `ToJSON`: omit `compositions` key when `Nothing`; write map when `Just`
  - `FromJSON`: read `compositions` key (default `Nothing` if absent)
- **Pipeline wiring** in `src/Graphos/UseCase/Pipeline.hs`:
  After Leiden produces `CommunityMap`, call `computeCompositions g commMap` and attach via `gCompositions = Just ...`
- **Load wiring** in `src/Graphos/UseCase/Load.hs`:
  Parse `compositions` from graph.json; `Nothing` on legacy graphs

### Spec Scenarios (from specs/cluster-composition/spec.md)

| Spec ID | Scenario | Task Coverage |
|---|---|---|
| SC-2.1 | Composition counts match membership (12+4+0, 3 cross refs) → persisted JSON | Full pipeline: compute → attach → serialize → verify key |
| SC-2.2 | Cross-type edge count excludes non-References | End-to-end: cross-edges = 3 not 8 in persisted output |
| SC-2.3 | Dominant kind ignores Nothing | `dominant_kind` field in JSON |
| SC-5.1 | Legacy graph loads without compositions | `FromJSON` default, `gCompositions = Nothing`, queries succeed |

### Check Criteria (defined BEFORE code)

**Tests to run:**
```bash
cabal test
# Focus on Graph/Core tests and Load tests:
cabal test --test-options="-tag graph-roundtrip"   # if tags used
cabal test --test-options="-tag load"              # if tags used
```

**Spec scenario gates:**

| ID | Test name pattern | PASS condition | FAIL condition |
|---|---|---|---|
| SC-2.1 | `graph.*roundtrip.*with.*compositions` | Serialized JSON contains `"compositions"` key; parsed back yields original map | JSON missing key or parsed map differs |
| SC-5.1 | `legacy.*graph.*loads.*without.*compositions` | `FromJSON` on JSON without `"compositions"` key → `gCompositions = Nothing`; `gCompositions g` returns `Nothing` | Parse error or `gCompositions /= Nothing` |
| Integration | `pipeline.*produces.*compositions` | After running pipeline, `gCompositions g /= Nothing` and map has entries | `gCompositions g == Nothing` after pipeline run |

**Additive field gate:**
- Adding `gCompositions` to `Graph` record must not break existing pattern matches (use `Maybe` with sensible default or update all callers)
- PASS: `cabal build` succeeds with no orphan warnings on `Graph` instances
- FAIL: `cabal build` fails due to unmatched patterns or type errors

**Pipeline wiring gate:**
- After `computeCompositions`, every community in `CommunityMap` must have a corresponding entry in the compositions map
- PASS: `Map.size compositionsMap == Map.size commMap`
- FAIL: Mismatch in sizes (missing communities)

**Exact FAIL boundaries:**
- If `gCompositions` is added as `Map CommunityId CommunityComposition` (not `Maybe`), existing code that constructs `Graph` without this field breaks → FAIL (design calls for `Maybe`)
- If `ToJSON` writes `compositions: {}` for `Nothing`, it pollutes legacy consumers → FAIL (should omit key)
- If pipeline doesn't call `computeCompositions`, `gCompositions` stays `Nothing` and no compositions appear in output → FAIL (missing wiring)

### Affected Modules

- `src/Graphos/Domain/Graph/Core.hs` — add `gCompositions` field, update `ToJSON`/`FromJSON`
- `src/Graphos/UseCase/Pipeline.hs` — wire `computeCompositions` post-Leiden
- `src/Graphos/UseCase/Load.hs` — parse `compositions` from JSON
- `src/Graphos/Infrastructure/Export/JSON.hs` — may need updates for Graph serialization

### Prerequisites

- Task 1 must be complete: `CommunityComposition` record + `computeCompositions` function must exist
- Existing `Graph` type supports record field extension
- Existing pipeline passes `Graph` through Leiden → community map

### Risks

- **Risk**: `Graph` record extension requires updating every place that constructs a `Graph`. Use smart constructor or update syntax (`{ gCompositions = existing.gCompositions }`) to minimize breakage.
- **Risk**: Pipeline wiring must happen at the right point — after Leiden (which creates `CommunityMap`) but before export.
- **Medium risk**: Load.hs must handle missing `compositions` key gracefully. Aeson's `.?` operator provides this.
- **Low risk**: Adding a `Maybe` field is backward-compatible by design.

## Result

All subtasks completed. `gCompositions :: Maybe (Map CommunityId CommunityComposition)` added to `Graph` record. Pipeline wires `computeCompositions` post-Leiden. Load parses `compositions` from `graph.json` with graceful fallback. Build passes with `-Wall -Werror`. Tests pass (633 examples, 0 failures).
