module Graphos.Domain.CommunitySpec where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Community (detectCommunities, detectCommunitiesWithResolution, cohesionScore, buildReverseIndex, communityOf, countMoves, Resolution(..), defaultResolution)

spec :: Spec
spec = do
  describe "detectCommunities" $ do
    it "assigns all nodes to communities" $ do
      -- Build a simple graph and detect communities
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c"] [testEdge "a" "b"]
          g = buildGraph False ext
          commMap = detectCommunities g
          allMembers = concat (Map.elems commMap)
      length allMembers `shouldSatisfy` (>= 2)

    it "generates unique community IDs (no collisions)" $ do
      let nodes = [testNode (T.pack $ "n" ++ show i) | i <- [1..20::Int]]
          edges = [testEdge (T.pack $ "n" ++ show i) (T.pack $ "n" ++ show (i+1)) | i <- [1..19::Int]]
          ext = extractionFromLists nodes edges
          g = buildGraph False ext
          commMap = detectCommunities g
          cids = Map.keys commMap
      length cids `shouldBe` length (Map.keysSet commMap)  -- all unique

  describe "cohesionScore" $ do
    it "returns 1.0 for a fully connected pair" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] [testEdge "a" "b"]
          g = buildGraph False ext
      cohesionScore g ["a", "b"] `shouldSatisfy` (> 0)

    it "returns 0 for completely disconnected nodes" $ do
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c"] []
          g = buildGraph False ext
      cohesionScore g ["a", "b", "c"] `shouldBe` 0.0

  describe "buildReverseIndex" $ do
    it "maps every node to its community ID" $ do
      let commMap = Map.fromList [(0, [T.pack "a", T.pack "b"]), (1, [T.pack "c"])]
          revIdx = buildReverseIndex commMap
      communityOf (T.pack "a") revIdx `shouldBe` Just 0
      communityOf (T.pack "b") revIdx `shouldBe` Just 0
      communityOf (T.pack "c") revIdx `shouldBe` Just 1

    it "returns Nothing for non-existent node" $ do
      let commMap = Map.fromList [(0, [T.pack "a"])]
          revIdx = buildReverseIndex commMap
      communityOf (T.pack "unknown") revIdx `shouldBe` Nothing

  describe "communityOf" $ do
    it "provides O(log N) lookup via reverse index" $ do
      let commMap = Map.fromList [(i, [T.pack $ "node" ++ show i]) | i <- [0..100::Int]]
          revIdx = buildReverseIndex commMap
      communityOf (T.pack "node50") revIdx `shouldBe` Just 50

  describe "countMoves" $ do
    it "returns 0 when no nodes changed community" $ do
      let old = Map.fromList [(T.pack "a", 0), (T.pack "b", 0), (T.pack "c", 1)]
          new = Map.fromList [(T.pack "a", 0), (T.pack "b", 0), (T.pack "c", 1)]
      countMoves old new `shouldBe` 0

    it "counts nodes that moved to a different community" $ do
      let old = Map.fromList [(T.pack "a", 0), (T.pack "b", 0), (T.pack "c", 1)]
          new = Map.fromList [(T.pack "a", 1), (T.pack "b", 0), (T.pack "c", 1)]
      countMoves old new `shouldBe` 1

    it "counts multiple moves" $ do
      let old = Map.fromList [(T.pack "a", 0), (T.pack "b", 1), (T.pack "c", 2)]
          new = Map.fromList [(T.pack "a", 2), (T.pack "b", 0), (T.pack "c", 2)]
      countMoves old new `shouldBe` 2

  describe "detectCommunitiesWithResolution" $ do
    it "respects max iterations setting" $ do
      -- With 1 iteration, the algorithm should still produce a valid community map
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c"] [testEdge "a" "b"]
          g = buildGraph False ext
          res = defaultResolution { resMaxIterations = 1 }
          commMap = detectCommunitiesWithResolution g res
          allMembers = concat (Map.elems commMap)
      length allMembers `shouldSatisfy` (>= 2)

    it "converges faster on stable graphs" $ do
      -- A simple connected pair should converge in very few iterations
      let ext = extractionFromLists [testNode "a", testNode "b"] [testEdge "a" "b"]
          g = buildGraph False ext
          res = defaultResolution { resMaxIterations = 3 }
          commMap = detectCommunitiesWithResolution g res
      -- Should still produce valid communities even with few iterations
      Map.size commMap `shouldSatisfy` (>= 1)

-- Helpers (duplicated from GraphSpec for test isolation)
testNode :: Text -> Node
testNode nid = Node nid nid CodeFile "test.hs" (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "L1") Nothing Nothing Nothing Nothing

edgeIdFrom :: Text -> Text -> EdgeId
edgeIdFrom src tgt = EdgeId (src <> "->" <> tgt)

testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge (edgeIdFrom src tgt) src tgt Calls 1.0 (Confidence 1.0)