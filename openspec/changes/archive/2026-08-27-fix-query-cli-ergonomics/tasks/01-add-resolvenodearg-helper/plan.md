# Task 1 — Add resolveNodeArg helper in Graphos.UseCase.Query — PLAN

**Task slug**: `01-add-resolvenodearg-helper`
**Attempt**: 1
**Status**: pending

## Summary

Introduce a pure node-argument resolver in the UseCase layer (`src/Graphos/UseCase/Query.hs`) that resolves a user-supplied `Text` argument to a `NodeId` via a three-step resolution order: exact id match → exact label match (via `giLabelIndex`) → case-insensitive label fallback.

## Detail

### Scope

- **New type** in `src/Graphos/UseCase/Query.hs`:
  ```haskell
  data NodeResolution
    = ResolvedSingle NodeId
    | Ambiguous [ScoredNode]
    | NotFound
  ```
  with `ToJSON` instance (needed for future JSON rendering; add instance now to avoid later refactors).

- **New function** in `src/Graphos/UseCase/Query.hs`:
  ```haskell
  resolveNodeArg :: Text -> Graph -> GraphIndex -> NodeResolution
  ```
  Resolution order:
  1. Exact id: `Map.lookup arg (gNodes g)`
  2. Exact label: use `giLabelIndex` from `GraphIndex` with the argument as-is (case-sensitive)
  3. Case-insensitive label: use `giLabelIndex` with `lower arg` (the label index keys are lowercased)

- **Export** `NodeResolution` and `resolveNodeArg` from `Graphos.UseCase.Query` module exports.

- **NO** signature change to `neighborhoodExpansion` — it keeps `NodeId -> Int -> Graph -> GraphIndex -> NeighborsResult`.

- **NO** call to `findMatchingNodes` fuzzy path — `resolveNodeArg` only uses `giLabelIndex`.

### Check Criteria

**Tests to run**:
- `cabal test` — Hspec cases in `tests/Graphos/UseCase/QuerySpec.hs` covering `resolveNodeArg`
- `cabal build --flag dev` with `-Wall -Werror --compat -Wincomplete-uni-patterns` — clean

**Spec scenarios satisfied**:
- `Scenario: Display name fallback resolves a single node` — neighbor-expansion spec
- `Scenario: Case-insensitive label fallback` — neighbor-expansion spec
- `Scenario: Ambiguous name lists candidates without traversal` — neighbor-expansion spec
- `Scenario: Unknown name fails explicitly` — neighbor-expansion spec

**PASS conditions**:
- `resolveNodeArg "mod_Graphos.UseCase.QuerySpec" g gIdx` returns `ResolvedSingle (NodeId "mod_Graphos.UseCase.QuerySpec")`
- `resolveNodeArg "Graphos.UseCase.QuerySpec" g gIdx` returns `ResolvedSingle (NodeId "...")` where that node has label `"Graphos.UseCase.QuerySpec"`
- `resolveNodeArg "graphos.usecase.queryspec" g gIdx` returns `ResolvedSingle (NodeId "...")` via case-insensitive fallback (same node as exact label above)
- `resolveNodeArg "parse" g gIdx` returns `Ambiguous [ScoredNode {snId = NodeId "mod_file1", snLabel = "parse", snFile = "..."}, ScoredNode {snId = NodeId "mod_file2", snLabel = "parse", snFile = "..."}]` when exactly two nodes share the label `"parse"`
- `resolveNodeArg "no_such_node" g gIdx` returns `NotFound` when no node id or label matches
- `resolveNodeArg` has an explicit type signature and is exported from `Graphos.UseCase.Query`
- `NodeResolution` is exported from `Graphos.UseCase.Query`
- No new compiler warnings under `-Wall -Wcompat -Wincomplete-uni-patterns -Werror --flag dev`

**FAIL boundaries**:
- If `resolveNodeArg` calls `findMatchingNodes` (the fuzzy path) instead of using `giLabelIndex`, the test fails — the spec explicitly forbids fuzzy traversal
- If `resolveNodeArg` changes the signature of `neighborhoodExpansion`, the test fails — the function must remain `NodeId -> ...`
- If `Ambiguous` does not include source file information in the candidate list, the test fails — the spec requires distinct source locations

### Affected modules

- **Modified**: `src/Graphos/UseCase/Query.hs` — add `NodeResolution` type, `resolveNodeArg` function, update exports
- **New (tests)**: `tests/Graphos/UseCase/QuerySpec.hs` — add Hspec cases for `resolveNodeArg` (5 scenarios)
- **Imports from**: `Data.Map.Strict` (gNodes, giLabelIndex lookups), `Data.Text` (lower for case-insensitive), `Graphos.Domain.Graph` (NodeId, Graph), `Graphos.Domain.Pipeline` (GraphIndex, ScoredNode)

### Prerequisites

- `GraphIndex` has a `giLabelIndex :: Map Text [ScoredNode]` field (already exists)
- `giNodes :: Map NodeId Node` is accessible on `Graph` (already exists)
- `ScoredNode` has fields for node id, label, and source file path
- `lower :: Text -> Text` is available from `Data.Text`

### Risks

- **Low**: Pure function addition — no IO, no side effects, clean architecture compliant
- **Low**: Reuses existing `giLabelIndex` — no new data structures needed
- **Low**: `NodeResolution` type is additive — no existing callers affected
- **Medium**: The label index may contain nodes with the same label from different files — the `Ambiguous` case must surface all candidates with their source files for disambiguation
