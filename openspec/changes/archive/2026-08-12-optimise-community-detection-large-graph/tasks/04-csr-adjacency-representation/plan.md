<!--
  PDCA step file for task 4. Lives at tasks/04-csr-adjacency-representation/plan.md.
  Scope: Replace lsNeighbors vector-of-vectors with CSR representation.
  No code yet. Check Criteria defined BEFORE implementation.
-->

# Task 4 — CSR Adjacency Representation — PLAN

**Task slug**: `04-csr-adjacency-representation`
**Attempt**: 1
**Status**: pending

## Summary

Replace `lsNeighbors :: V.Vector (VU.Vector Int)` in `LeidenState` with CSR format: `lsAdj :: VU.Vector Int` (contiguous neighbors) + `lsOffset :: VU.Vector Int` (start index per node, length N+1). Update `buildLeidenState`, `localMovingLoop`, and `cohesionToCommunityIdx`.

## Detail

### Scope

- **Files**: `Domain/Community.hs` — `LeidenState` data type, `buildLeidenState`, `localMovingLoop`, `cohesionToCommunityIdx`, `NFData` instance
- **Data type change**: Remove `lsNeighbors`, add `lsAdj` and `lsOffset`
- **`buildLeidenState`**: After collecting per-node neighbor lists, concatenate into `lsAdj` and accumulate offsets into `lsOffset`
- **`localMovingLoop`**: Replace `lsNeighbors V.! i` with `VU.slice (lsOffset V.! i) len lsAdj` where `len = (lsOffset V.! (i+1)) - (lsOffset V.! i)`
- **`cohesionToCommunityIdx`**: Same CSR slice pattern
- **`NFData` instance**: Add `lsAdj` and `lsOffset` fields, remove `lsNeighbors`
- **Backward compatibility**: Keep `lsNeighbors` populated during transition (optional, for safe rollback)
- **Complexity target**: CSR build O(N + E), one-shot. Read via O(1) slice. Cache-friendly vs vector-of-vectors' 78K heap objects.

### Check Criteria

**Tests/gates to run:**
1. `cabal build` — must exit 0 under `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`
2. `cabal test` — must exit 0 with all existing examples passing (347 examples expected)
3. Deterministic fixture comparison: cluster the same fixture with vector-of-vectors and CSR representations, assert identical `CommunityMap`
4. `rnf` applied to constructed `LeidenState` — verifies full NFData forcing (no thunks)

**Spec scenarios satisfied:**
- `leiden-scalability` — **Scenario: Neighbor access is contiguous** (WHEN local-moving pass reads neighbors for node i, THEN neighbors occupy contiguous slice of adjacency vector via VU.slice)
- `leiden-scalability` — **Scenario: CSR build preserves neighbor sets** (WHEN graph loaded into LeidenState, THEN set of indices in CSR slice equals set in legacy lsNeighbors for every node, dangling-edge self-loop fallbacks preserved)
- `leiden-scalability` — **Scenario: Clustering output is unchanged by the representation swap** (WHEN deterministic fixture clustered with both representations, THEN final CommunityMap is identical)
- `leiden-scalability` — **Scenario: Deep evaluation completes over the whole state** (WHEN rnf applied to LeidenState, THEN all fields evaluated to normal form)
- `leiden-scalability` — **Requirement: CSR adjacency representation** (MUST store adjacency in CSR form, SHALL NOT use vector-of-vectors)
- `leiden-scalability` — **Requirement: Fully forcing LeidenState NFData** (MUST force all fields to normal form)

**PASS conditions:**
- `cabal build` exits 0
- `cabal test` exits 0 with all examples passing
- Deterministic fixture produces identical CommunityMap (set of IDs + set of members per ID)
- CSR slice for every node i contains exactly the same indices as legacy `lsNeighbors V.! i`
- `rnf leidenState` evaluates all fields without error

**FAIL conditions:**
- `cabal build` fails — type errors in CSR field access
- `cabal test` fails — semantics broken (wrong neighbor slices, wrong bestComm)
- Deterministic fixture produces different CommunityMap — CSR slicing has off-by-one or offset bug
- CSR slice set != legacy neighbor set for any node — CSR build dropped or duplicated neighbors
- `rnf` fails to force a field — thunk accumulation risk

### Affected modules

- `Domain.Community` — `LeidenState` data type, `buildLeidenState`, `localMovingLoop`, `cohesionToCommunityIdx`, `NFData` instance
- `Domain.Graph.Core` — potentially needed for record field access (if `scoreAllCohesion` reads adjacency from Graph)

### Prerequisites

- Task 1 must be complete (Pipeline.hs compiles, baseline test suite passes)
- `buildLeidenState` must be the only constructor path for `LeidenState` (verify no other code constructs it)
- `localMovingLoop` and `cohesionToCommunityIdx` are the only callers of `lsNeighbors`
- Dangling-edge self-loop fallback logic must be preserved in CSR build

### Risks

- **Off-by-one in offset calculation**: `lsOffset` has N+1 elements. `lsOffset V.! (n+1)` should equal `VU.length lsAdj`. Must verify this invariant in build.
- **Dangling edge handling**: If a neighbor index does not map to a valid node in the idx-to-node mapping, it must be stored as a self-loop. This logic is in the current `lsNeighbors` build and must be replicated exactly in CSR build.
- **Memory regression**: CSR is `2E + (N+1)` ints vs N vector-pointers + 2E ints. On solario (78K nodes, ~235K edges): CSR ≈ 470K + 78K ints ≈ 2.2MB; vector-of-vectors ≈ 470K ints + 78K × 24-byte header ≈ 2.8MB. CSR is smaller. The win is cache locality. If memory increases, the build is retaining thunks — add `deepseq` boundaries.
- **`lsNeighbors` removal**: Plan to keep `lsNeighbors` populated during transition for safe rollback. Can be removed in a follow-up cleanup once CSR is verified stable on large graphs.

## Result

pending — awaiting Do phase.
