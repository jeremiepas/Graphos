<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Implement joinCommunitiesToNodes UseCase function — PLAN

**Task slug**: `02-implement-join-communities-to-nodes`
**Attempt**: 1
**Status**: <!-- pending | in-progress | PASS | FAIL -->

## Summary

The `joinCommunitiesToNodes` function already exists in `src/Graphos/UseCase/Cluster.hs:85-92`. This task is to verify its correctness, add comprehensive Hspec tests, and add a QuickCheck property test. The function is pure, O(N), and sets `nodeCommunityId` on each `Node` record based on the `CommunityMap`.

## Detail

### Scope

**Existing implementation** (`Cluster.hs:85-92`):
```haskell
joinCommunitiesToNodes :: Graph -> CommunityMap -> Graph
joinCommunitiesToNodes graph commMap =
  let invMap = invertCommunityMap' commMap
      updateNode n = n { nodeCommunityId = case Map.lookup (nodeId n) invMap of
                Just cid -> Just cid
                Nothing  -> Nothing }
      nodes = Map.map updateNode (gNodes graph)
  in graph { gNodes = nodes }
```

**Work items:**
1. Add Hspec tests in `tests/UseCaseSpec.hs` (or a new `tests/JoinCommunitiesSpec.hs`):
   - Test: a graph with 2 nodes in community 4 and 1 isolated node → after join, the 2 nodes have `community_id = Just 4`, the isolated stays `Nothing`
   - Test: empty graph → empty graph
   - Test: graph with nodes in multiple communities → all nodes get correct community IDs
2. Add QuickCheck property test:
   - `length (filter isJust (map nodeCommunityId (Map.elems (gNodes (joinCommunitiesToNodes g cm))))) == countNodesInCommunities cm`
   - Where `countNodesInCommunities cm = sum (map length (Map.elems cm))`
3. Verify the function has no `IO` in its type signature or body

### Check Criteria

**Tests/gates:**
- (a) `cabal build` with `-Werror` → exits 0
- (b) `cabal test --match "joinCommunities"` → all tests PASS
- (c) `cabal test --quickcheck "joinCommunities"` → property holds
- (d) `grep "IO" src/Graphos/UseCase/Cluster.hs | grep -c joinCommunities` → must be 0 (no IO in the function)
- (e) `grep -c "import.*IO" src/Graphos/UseCase/Cluster.hs` → verify no IO imports that could indicate accidental side effects

**Spec scenarios satisfied:**
- `node-schema/spec.md` — Scenario "Community ID populated after Leiden": node `n1` assigned to community `4` → `community_id` in JSON is `4` (not `null`)
- `node-schema/spec.md` — Scenario "Every community member has a non-null community_id": all 78,529 nodes have non-null `community_id` matching their assigned community
- `node-schema/spec.md` — Scenario "Nodes outside any community remain null": isolated nodes stay `Nothing`

**PASS conditions:**
- (a) `cabal build` exits with code 0
- (b) All Hspec tests pass
- (c) QuickCheck property holds for 100 test cases
- (d) No `IO` in the function body or type signature

**FAIL boundaries:**
- (a) Compilation error → FAIL
- (b) Any Hspec test fails → FAIL
- (c) QuickCheck property fails for any input → FAIL
- (d) Function has `IO` in its type → FAIL (violates Domain purity constraint)

### Affected Modules

- `src/Graphos/UseCase/Cluster.hs` — verify existing implementation (no changes expected to the function itself)
- `tests/UseCaseSpec.hs` or `tests/JoinCommunitiesSpec.hs` — new test file with Hspec tests and QuickCheck properties

### Prerequisites

- `CommunityMap`, `Graph`, `NodeId`, `Node` types available
- `invertCommunityMap'` helper already exists in `Cluster.hs`
- Test framework (Hspec + QuickCheck) already configured

### Risks

- **Existing implementation correctness**: The function looks correct but has no tests. The risk is that the existing implementation is actually correct and this task is purely about adding tests. Verify the implementation before writing tests to avoid test bias.
- **Test fixture setup**: Building test graphs with specific community assignments requires constructing `Graph` and `CommunityMap` values manually. Use the existing test helpers if available, or create minimal fixtures.
- **Performance concern**: The O(N) inverted map lookup is acceptable at 158K nodes (~10MB). No changes needed to address this — it was already assessed as acceptable in the original task.
