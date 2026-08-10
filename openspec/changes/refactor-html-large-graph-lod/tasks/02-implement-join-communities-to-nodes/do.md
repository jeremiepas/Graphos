<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Implement joinCommunitiesToNodes UseCase function — DO

**Task slug**: `02-implement-join-communities-to-nodes`
**Attempt**: 1
**Status**: pending

## Summary

Add Hspec tests and QuickCheck property tests for the existing `joinCommunitiesToNodes` function in `src/Graphos/UseCase/Cluster.hs:85-92`. The function implementation is already correct — this task verifies it through tests.

## Detail

### Concrete Changes

**File: `tests/UseCaseSpec.hs` (or new `tests/JoinCommunitiesSpec.hs`)**

1. Hspec test: community join sets correct `community_id`
   ```haskell
   describe "joinCommunitiesToNodes" $ do
     it "sets community_id for nodes in a community, leaves isolated nodes as Nothing" $
       let nodes = Map.fromList
             [ (NodeId "n1", Node (NodeId "n1") "a" ... { nodeCommunityId = Nothing })
             , (NodeId "n2", Node (NodeId "n2") "b" ... { nodeCommunityId = Nothing })
             , (NodeId "n3", Node (NodeId "n3") "c" ... { nodeCommunityId = Nothing })
             ]
           commMap = Map.fromList [ (4, [NodeId "n1", NodeId "n2"]) ]
           graph = Graph nodes Map.empty
           result = joinCommunitiesToNodes graph commMap
       in do
           (nodeCommunityId $ Map.find (NodeId "n1") (gNodes result)) `shouldBe` Just 4
           (nodeCommunityId $ Map.find (NodeId "n2") (gNodes result)) `shouldBe` Just 4
           (nodeCommunityId $ Map.find (NodeId "n3") (gNodes result)) `shouldBe` Nothing
   ```

2. Hspec test: empty graph returns empty graph
   ```haskell
   it "returns unchanged graph for empty community map" $
     let graph = Graph Map.empty Map.empty
         result = joinCommunitiesToNodes graph Map.empty
     in gNodes result `shouldBe` Map.empty
   ```

3. Hspec test: multiple communities
   ```haskell
   it "assigns correct community_id across multiple communities" $
     let commMap = Map.fromList [ (1, [NodeId "n1"]), (2, [NodeId "n2"]) ]
         -- ... build graph ...
         result = joinCommunitiesToNodes graph commMap
     in do
           (nodeCommunityId $ Map.find (NodeId "n1") (gNodes result)) `shouldBe` Just 1
           (nodeCommunityId $ Map.find (NodeId "n2") (gNodes result)) `shouldBe` Just 2
   ```

4. QuickCheck property:
   ```haskell
   prop_joinCommunities_count :: Property
   prop_joinCommunities_count =
     let countNodesInCommunities cm = sum (map length (Map.elems cm))
     in forAll (arbitrary :: Gen (Graph, CommunityMap)) $ \(g, cm) ->
           let result = joinCommunitiesToNodes g cm
               joined = length (filter isJust (map nodeCommunityId (Map.elems (gNodes result))))
           in joined === countNodesInCommunities cm
   ```

### Key Decisions

- **Test file location**: Add to existing `tests/UseCaseSpec.hs` if it has room, otherwise create `tests/JoinCommunitiesSpec.hs` as a dedicated test file. Follow the existing test file pattern in the project.
- **Fixture construction**: Use `Node` smart constructors or raw constructors with `nodeCommunityId = Nothing` as the starting point. The `Graph` type uses `Map NodeId Node` for nodes and `Map EdgeId Edge` for edges.
- **No changes to the function itself**: The existing implementation (`Cluster.hs:85-92`) is correct. Only tests are added.

### Dependencies

- Reads: `tasks/02-implement-join-communities-to-nodes/plan.md`
- Unlocks: `tasks/02-implement-join-communities-to-nodes/check.md`
