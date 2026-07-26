{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.InferSpec where

import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Domain.Analysis (dedupOn)
import Graphos.Domain.Graph (buildGraph)
import Graphos.UseCase.Infer (inferCommunityBridges, inferCodeDocEdges)

-- Helpers
testNode :: Text -> Node
testNode nid = Node nid nid CodeFile "test.hs" (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

docNode :: Text -> Text -> Node
docNode nid lbl = Node nid lbl DocFile "doc.md" (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

codeNode :: Text -> Text -> Node
codeNode nid lbl = Node nid lbl CodeFile "code.hs" (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge (EdgeId (src <> "->" <> tgt)) src tgt Calls 1.0 (Confidence 1.0)

spec :: Spec
spec = do
  describe "dedupOn" $ do
    it "keeps the first occurrence and preserves order" $ do
      dedupOn fst [(1 :: Int, "a" :: String), (2, "b"), (1, "c"), (3, "d"), (2, "e")]
        `shouldBe` [(1, "a"), (2, "b"), (3, "d")]

    it "matches nubBy semantics (QuickCheck property)" $ property $
      \(xs :: [(Int, Int)]) ->
        dedupOn fst xs == nubBy (\a b -> fst a == fst b) xs

  describe "inferCommunityBridges" $ do
    -- Two star communities centered on a and d (highest degree → centroids),
    -- connected by one real inter-community edge between LEAF members (b-e),
    -- so the centroid pair a-d has no existing edge. Third star community
    -- {g,h,i,z} is disconnected from both.
    let star c leaves = [testEdge c l | l <- leaves]
        ns = map testNode ["a","b","c","x","d","e","f","y","g","h","i","z"]
        es = star "a" ["b","c","x"] ++ star "d" ["e","f","y"] ++ star "g" ["h","i","z"]
             ++ [testEdge "b" "e"]
        g = buildGraph False (extractionFromLists ns es)
        commMap = Map.fromList [ (0, ["a","b","c","x"])
                               , (1, ["d","e","f","y"])
                               , (2, ["g","h","i","z"]) ]
        bridges = inferCommunityBridges g commMap

    it "bridges communities connected by a real edge exactly once" $ do
      length bridges `shouldBe` 1

    it "does not bridge disconnected communities" $ do
      let endpoints = concatMap (\e -> [edgeSource e, edgeTarget e]) bridges
      any (`elem` (["g","h","i","z"] :: [Text])) endpoints `shouldBe` False

    it "produces no bridges when all communities are disconnected" $ do
      let es' = star "a" ["b","c","x"] ++ star "d" ["e","f","y"]
          g' = buildGraph False (extractionFromLists (map testNode ["a","b","c","x","d","e","f","y"]) es')
          cm' = Map.fromList [(0, ["a","b","c","x"]), (1, ["d","e","f","y"])]
      inferCommunityBridges g' cm' `shouldBe` []

  describe "inferCodeDocEdges" $ do
    it "links a doc label matching few code nodes" $ do
      let doc = docNode "doc1" "parseConfig"
          codes = [codeNode "c1" "parseConfig", codeNode "c2" "other"]
          g = buildGraph False (extractionFromLists (doc : codes) [])
          edges = inferCodeDocEdges g
      length edges `shouldBe` 1
      edgeRelation <$> edges `shouldBe` [References]

    it "skips labels exceeding the fan-out cap" $ do
      let doc = docNode "doc1" "Config"
          codes = [codeNode (T.pack ("c" ++ show i)) "Config" | i <- [1 .. 30 :: Int]]
          g = buildGraph False (extractionFromLists (doc : codes) [])
      inferCodeDocEdges g `shouldBe` []

    it "emits no duplicate (source, target) pairs" $ do
      let doc = docNode "doc1" "parseConfig"
          codes = [codeNode "c1" "parseConfig"]
          g = buildGraph False (extractionFromLists (doc : codes) [])
          edges = inferCodeDocEdges g
          pairs = map (\e -> (edgeSource e, edgeTarget e)) edges
      length pairs `shouldBe` length (dedupOn id pairs)
