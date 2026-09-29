{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Domain.AnalysisSpec where

import Control.DeepSeq (force)
import Control.Exception (ErrorCall, evaluate, try)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Graph.Analysis (biconnectedComponents, biconnectedComponentCount)
import Graphos.Domain.Community (detectCommunities, scoreAllCohesion)
import Graphos.Domain.Analysis (analyze, selectTopNOn, suggestQuestions, surprisingConnections)

spec :: Spec
spec = do
  describe "analyze" $ do
    it "produces analysis with god nodes" $ do
      let ext = extractionFromLists [testNode "hub", testNode "leaf1", testNode "leaf2"] [testEdge "hub" "leaf1", testEdge "hub" "leaf2"]
          g = buildGraph False ext
          commMap = detectCommunities g
          cohesion = scoreAllCohesion g commMap
          analysis = analyze g commMap cohesion
      length (analysisGodNodes analysis) `shouldSatisfy` (>= 1)

    it "carries shared connectivity artifacts (bounded-report-export)" $ do
      -- Chain a-b-c: b is the articulation point; two biconnected components.
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c"]
                                    [testEdge "a" "b", testEdge "b" "c"]
          g = buildGraph False ext
          analysis = analyze g Map.empty Map.empty
      analysisArticulation analysis `shouldBe` ["b"]
      analysisBccCount analysis `shouldBe` 2

    it "is fully evaluable to normal form (NFData depth reaches surprises)" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] [testEdge "a" "b"]
          g = buildGraph False ext
          boom = SurprisingConnection (error "boom") "t" [] (Confidence 1.0) "calls" "why"
          analysis = (analyze g Map.empty Map.empty) { analysisSurprises = [boom] }
      r <- try (evaluate (force analysis)) :: IO (Either ErrorCall Analysis)
      case r of
        Left _  -> pure ()
        Right _ -> expectationFailure "deepseq did not reach the surprises payload"

  describe "selectTopNOn (bounded-report-export D3)" $ do
    it "equals take n . sortOn key, including tie order" $
      property $ \(xs :: [(Int, Int)]) (NonNegative n) ->
        selectTopNOn n fst xs === take n (sortOn fst xs)

    it "returns nothing for non-positive n" $ do
      selectTopNOn 0 id [3, 1, 2 :: Int] `shouldBe` []
      selectTopNOn (-1) id [3, 1, 2 :: Int] `shouldBe` []

  describe "suggestQuestions cohesion source (bounded-report-export)" $ do
    let members = ["m1", "m2", "m3", "m4", "m5"]
        ext = extractionFromLists (map testNode members) [testEdge "m1" "m2"]
        g = buildGraph False ext
        commMap = Map.fromList [(0 :: CommunityId, members)]
        lowCohesionQs cm = [q | q <- suggestQuestions g commMap cm Map.empty, sqType q == "low_cohesion"]

    it "raises a low-cohesion question from the provided map" $
      length (lowCohesionQs (Map.fromList [(0, 0.05)])) `shouldBe` 1

    it "stays quiet when the map reports high cohesion" $
      lowCohesionQs (Map.fromList [(0, 0.9)]) `shouldBe` []

    it "treats a missing community as cohesive instead of recomputing" $
      lowCohesionQs Map.empty `shouldBe` []

  describe "cross-community surprises (bounded dedup selection)" $ do
    it "keeps the highest-confidence edge per community pair, ordered by confidence" $ do
      -- Single-source graph (all nodes share test.hs) -> cross-community path.
      -- Pairs: (0,1) has conf 0.4 and 0.9 edges; (0,2) has conf 0.6.
      let nodes = map testNode ["a1", "a2", "b1", "c1"]
          edges = [ testEdgeConf "a1" "b1" 0.4
                  , testEdgeConf "a2" "b1" 0.9
                  , testEdgeConf "a1" "c1" 0.6
                  ]
          g = buildGraph False (extractionFromLists nodes edges)
          commMap = Map.fromList [(0, ["a1", "a2"]), (1, ["b1"]), (2, ["c1"])]
          surprises = surprisingConnections g commMap 5
      [(scSource s, scTarget s, scConfidence s) | s <- surprises]
        `shouldBe` [ ("a2", "b1", Confidence 0.9)
                   , ("a1", "c1", Confidence 0.6)
                   ]

  describe "biconnectedComponentCount" $ do
    -- fgl's `bcc` (the list version) is documented for CONNECTED graphs only:
    -- on disconnected input it lumps everything into one component. The
    -- counter computes the standard per-component block count instead, so the
    -- two agree exactly on connected graphs and the counter is the correct
    -- figure on disconnected ones (bounded-report-export).
    it "equals length of biconnectedComponents on a connected fixture" $ do
      let ext = extractionFromLists (map testNode ["a", "b", "c", "d"])
                                    [testEdge "a" "b", testEdge "b" "c", testEdge "c" "a", testEdge "c" "d"]
          g = buildGraph False ext
      biconnectedComponentCount g `shouldBe` length (biconnectedComponents g)

    it "counts one block per component edge group on a disconnected graph" $ do
      -- Two disjoint edges = two blocks (fgl's connected-only bcc reports 1).
      let ext = extractionFromLists (map testNode ["a", "b", "x", "y"])
                                    [testEdge "a" "b", testEdge "x" "y"]
          g = buildGraph False ext
      biconnectedComponentCount g `shouldBe` 2

    it "isolated vertices contribute no blocks" $ do
      -- Triangle (one block) + a lonely vertex (no edges, no block).
      let ext = extractionFromLists (map testNode ["a", "b", "c", "lonely"])
                                    [testEdge "a" "b", testEdge "b" "c", testEdge "c" "a"]
          g = buildGraph False ext
      biconnectedComponentCount g `shouldBe` 1

    it "agrees with the fgl-based component list on random connected graphs" $
      property $ \(edgePairs :: [(Word, Word)]) ->
        let names = [T.pack ("n" ++ show i) | i <- [0 :: Word .. 11]]
            backbone = [testEdge (names !! i) (names !! (i + 1)) | i <- [0 .. 10]]
            extra = [ testEdge (names !! fromIntegral (a `mod` 12)) (names !! fromIntegral (b `mod` 12))
                    | (a, b) <- edgePairs
                    , a `mod` 12 /= b `mod` 12  -- no self-loops
                    ]
            g = buildGraph False (extractionFromLists (map testNode names) (backbone ++ extra))
        in biconnectedComponentCount g === length (biconnectedComponents g)

-- Helpers
edgeIdFrom :: Text -> Text -> EdgeId
edgeIdFrom src tgt = EdgeId (src <> "->" <> tgt)

testNode :: Text -> Node
testNode nid = Node nid (fromText nid) CodeFile (fromText "test.hs") (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge (edgeIdFrom src tgt) src tgt Calls 1.0 (Confidence 1.0) Nothing

testEdgeConf :: Text -> Text -> Double -> Edge
testEdgeConf src tgt c = Edge (edgeIdFrom src tgt) src tgt Calls 1.0 (Confidence c) Nothing
