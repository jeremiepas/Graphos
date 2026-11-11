{-# LANGUAGE OverloadedStrings #-}
-- | Bunch modularization quality (openspec @bunch-search-based-modularization@,
-- spec @modularization-quality@). The two-triangle fixture and its value
-- @84/49@ are the ones proved in the Lean arm
-- (@formalization/lean/GraphosFormalization/Bunch/MQ.lean@, @twoTriangles_mq@).
module Graphos.Domain.Community.MQSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph)
import Graphos.Domain.Community.MQ

spec :: Spec
spec = describe "modularizationQuality (Bunch TurboMQ)" $ do
  describe "cluster factor" $ do
    it "paper Fig. 2, subsystem 2: 1 intra-edge, 3 inter-edges -> 0.4" $
      clusterFactor 1 3 `shouldBe` 0.4

    it "closed cluster (intra-edges, no external edge) -> 1" $
      clusterFactor 5 0 `shouldBe` 1

    it "no intra-edge -> 0 whatever the external weight" $
      clusterFactor 0 7 `shouldBe` 0

  describe "two triangles joined by one edge (Lean fixture)" $ do
    let g = twoTriangles 1.0
        cm = Map.fromList [(0, ["0", "1", "2"]), (1, ["3", "4", "5"])]
        r = modularizationQuality g cm

    it "MQ = 84/49 within 1e-12" $
      abs (mqValue r - 84 / 49) `shouldSatisfy` (< 1e-12)

    it "each triangle has CF = 6/7, mu = 3, ext = 1" $ do
      let cf0 = mqFactors r Map.! 0
      cfIntra cf0 `shouldBe` 3
      cfExternal cf0 `shouldBe` 1
      abs (cfValue cf0 - 6 / 7) `shouldSatisfy` (< 1e-12)
      mqFactors r Map.! 1 `shouldBe` cf0

    it "intra-edge fraction = 6/7, total weight 7, two clusters" $ do
      abs (mqIntraFraction r - 6 / 7) `shouldSatisfy` (< 1e-12)
      mqTotalWeight r `shouldBe` 7
      mqClusterCount r `shouldBe` 2

  describe "degenerate partitions" $ do
    it "all edges internal -> intra fraction 1 and MQ = 1 for one cluster" $ do
      let g = twoTriangles 1.0
          r = modularizationQuality g (Map.singleton 0 ["0", "1", "2", "3", "4", "5"])
      mqIntraFraction r `shouldBe` 1
      mqValue r `shouldBe` 1

    it "every node its own cluster -> MQ 0 and intra fraction 0" $ do
      let g = twoTriangles 1.0
          r = modularizationQuality g (Map.fromList [ (i, [T.pack (show i)]) | i <- [0 .. 5 :: Int] ])
      mqValue r `shouldBe` 0
      mqIntraFraction r `shouldBe` 0
      mqClusterCount r `shouldBe` 6

    it "empty graph -> MQ 0, intra fraction 0" $ do
      let g = buildGraph False (extractionFromLists [] [])
          r = modularizationQuality g Map.empty
      mqValue r `shouldBe` 0
      mqIntraFraction r `shouldBe` 0
      mqClusterCount r `shouldBe` 0

  describe "weights" $ do
    it "doubling every edge weight leaves MQ unchanged under EdgeWeights" $ do
      let cm = Map.fromList [(0, ["0", "1", "2"]), (1, ["3", "4", "5"])]
          a = modularizationQualityWith EdgeWeights (twoTriangles 1.5) cm
          b = modularizationQualityWith EdgeWeights (twoTriangles 3.0) cm
      abs (mqValue a - mqValue b) `shouldSatisfy` (< 1e-12)
      mqTotalWeight b `shouldBe` 2 * mqTotalWeight a

    it "UnitWeights ignores edgeWeight" $ do
      let cm = Map.fromList [(0, ["0", "1", "2"]), (1, ["3", "4", "5"])]
          r = modularizationQualityWith UnitWeights (twoTriangles 9.0) cm
      abs (mqValue r - 84 / 49) `shouldSatisfy` (< 1e-12)

  describe "ranges (QuickCheck)" $ do
    it "every CF lies in [0,1] and MQ lies in [0,k]" $
      property $ forAll genGraphAndPartition $ \(g, cm) ->
        let r = modularizationQuality g cm
            k = fromIntegral (mqClusterCount r) :: Double
        in all (\cf -> cfValue cf >= 0 && cfValue cf <= 1) (Map.elems (mqFactors r))
           && mqValue r >= 0 && mqValue r <= k + 1e-9
           && mqIntraFraction r >= 0 && mqIntraFraction r <= 1 + 1e-12

    it "is deterministic: scoring twice gives the same result" $
      property $ forAll genGraphAndPartition $ \(g, cm) ->
        modularizationQuality g cm == modularizationQuality g cm

-- ───────────────────────────────────────────────
-- Fixtures
-- ───────────────────────────────────────────────

node :: Text -> Node
node nid = Node
  { nodeId = nid
  , nodeLabel = fromText nid
  , nodeFileType = CodeFile
  , nodeSourceFile = fromText "test.hs"
  , nodeSource = Nothing
  , nodeLineStart = Just 1
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodePresentBits = 0
  }

edge :: Double -> Text -> Text -> Edge
edge w u v = Edge
  { edgeId = EdgeId (u <> "->" <> v)
  , edgeSource = u
  , edgeTarget = v
  , edgeRelation = Calls
  , edgeWeight = w
  , edgeConfidence = Confidence 1.0
  , edgeExtra = Nothing
  }

-- | Nodes 0,1,2 and 3,4,5 form two triangles joined by the edge 2-3; every
-- edge carries weight @w@.
twoTriangles :: Double -> Graph
twoTriangles w =
  buildGraph False $ extractionFromLists
    (map (node . T.pack . show) [0 .. 5 :: Int])
    [ edge w "0" "1", edge w "1" "2", edge w "0" "2"
    , edge w "3" "4", edge w "4" "5", edge w "3" "5"
    , edge w "2" "3" ]

genGraphAndPartition :: Gen (Graph, CommunityMap)
genGraphAndPartition = do
  n <- choose (1, 12 :: Int)
  let ids = [ T.pack (show i) | i <- [0 .. n - 1] ]
  m <- choose (0, 20 :: Int)
  pairs <- vectorOf m ((,) <$> choose (0, n - 1) <*> choose (0, n - 1))
  let es = [ edge 1.0 (ids !! i) (ids !! j) | (i, j) <- pairs, i /= j ]
      g = buildGraph False (extractionFromLists (map node ids) es)
  k <- choose (1, n)
  assign <- vectorOf n (choose (0, k - 1))
  let cm = Map.fromListWith (++) [ (c, [nid]) | (nid, c) <- zip ids assign ]
  pure (g, cm)
