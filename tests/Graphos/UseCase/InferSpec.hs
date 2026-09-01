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
import Graphos.Domain.Config (SemanticEdgesConfig(..), defaultSemanticEdgesConfig)
import Graphos.Domain.Graph (buildGraph, gEmbeddings)
import Graphos.UseCase.Infer (inferCommunityBridges, inferCodeDocEdges, inferSemanticCodeDocEdges, SemanticMode(..), semanticMode, isSingleCorpus)

-- Helpers
testNode :: Text -> Node
testNode nid = Node nid nid CodeFile "test.hs" (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

docNode :: Text -> Text -> Node
docNode nid lbl = Node nid lbl DocFile "doc.md" (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

codeNode :: Text -> Text -> Node
codeNode nid lbl = Node nid lbl CodeFile "code.hs" (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge (EdgeId (src <> "->" <> tgt)) src tgt Calls 1.0 (Confidence 1.0) Nothing

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

  describe "isSingleCorpus" $ do
    it "returns True for empty graph" $ do
      isSingleCorpus (buildGraph False (extractionFromLists [] [])) `shouldBe` True

    it "returns True when all nodes are CodeFile" $ do
      let ns = [codeNode "c1" "a", codeNode "c2" "b"]
          g = buildGraph False (extractionFromLists ns [])
      isSingleCorpus g `shouldBe` True

    it "returns True when all nodes are DocFile" $ do
      let ns = [docNode "d1" "a", docNode "d2" "b"]
          g = buildGraph False (extractionFromLists ns [])
      isSingleCorpus g `shouldBe` True

    it "returns False for mixed CodeFile and DocFile" $ do
      let ns = [codeNode "c1" "a", docNode "d1" "b"]
          g = buildGraph False (extractionFromLists ns [])
      isSingleCorpus g `shouldBe` False

  describe "semanticMode" $ do
    let seOn = defaultSemanticEdgesConfig
        seDisabled = defaultSemanticEdgesConfig { seEnabled = False }
        codeOnly = buildGraph False (extractionFromLists [codeNode "c1" "a", codeNode "c2" "b"] [])
        mixed = buildGraph False (extractionFromLists [codeNode "c1" "a", docNode "d1" "b"] [])

    it "returns SemanticDisabled when seEnabled is False" $ do
      semanticMode seDisabled False codeOnly `shouldBe` SemanticDisabled
      semanticMode seDisabled True codeOnly `shouldBe` SemanticDisabled

    it "returns SemanticForced when force is True (and enabled)" $ do
      semanticMode seOn True codeOnly `shouldBe` SemanticForced
      semanticMode seOn True mixed `shouldBe` SemanticForced

    it "returns SemanticAutoSkip for single-corpus graph" $ do
      semanticMode seOn False codeOnly `shouldBe` SemanticAutoSkip

    it "returns SemanticEnabled for mixed corpus under scale cap" $ do
      semanticMode seOn False mixed `shouldBe` SemanticEnabled

  describe "inferSemanticCodeDocEdges" $ do
    it "returns empty list when embeddings are empty" $ do
      let g = buildGraph False (extractionFromLists [codeNode "c1" "a", docNode "d1" "b"] [])
      inferSemanticCodeDocEdges defaultSemanticEdgesConfig g Map.empty `shouldBe` []

    it "creates References edges for similar doc-code pairs" $ do
      let g = (buildGraph False (extractionFromLists [codeNode "c1" "a", docNode "d1" "b"] []))
                { gEmbeddings = Just (Map.fromList [("c1", [1.0, 0.0]), ("d1", [1.0, 0.0])]) }
          se = defaultSemanticEdgesConfig
          edges = inferSemanticCodeDocEdges se g (Map.fromList [("c1", [1.0, 0.0]), ("d1", [1.0, 0.0])])
      length edges `shouldBe` 1
      case edges of
        [e] -> do
          edgeRelation e `shouldBe` References
          edgeSource e `shouldBe` "c1"
          edgeTarget e `shouldBe` "d1"
        _ -> fail "expected exactly one edge"

    it "filters out pairs below threshold" $ do
      let embs = Map.fromList [("c1", [1.0, 0.0]), ("d1", [0.0, 1.0])]
          g = (buildGraph False (extractionFromLists [codeNode "c1" "a", docNode "d1" "b"] []))
                { gEmbeddings = Just embs }
          se = defaultSemanticEdgesConfig
      inferSemanticCodeDocEdges se g embs `shouldBe` []

    it "respects maxFanOut cap" $ do
      let codes = [codeNode (T.pack ("c" ++ show i)) (T.pack ("a" ++ show i)) | i <- [1..10 :: Int]]
          doc = docNode "d1" "doc"
          g = (buildGraph False (extractionFromLists (doc : codes) []))
                { gEmbeddings = Just (Map.fromList [("d1", [1.0, 0.0])]) }
          se = defaultSemanticEdgesConfig { seMaxFanOut = 3 }
          embs = Map.fromList ([("d1", [1.0, 0.0])] ++ [(T.pack ("c" ++ show i), [1.0, 0.0]) | i <- [1..10 :: Int]])
      length (inferSemanticCodeDocEdges se g embs) `shouldBe` 3

    it "emits References edge with confidence equal to cosine similarity" $ do
      let b = sqrt (1 - 0.82 * 0.82)
          embs = Map.fromList [("c1", [0.82, b]), ("d1", [1.0, 0.0])]
          g = (buildGraph False (extractionFromLists [codeNode "c1" "a", docNode "d1" "b"] []))
                { gEmbeddings = Just embs }
          edges = inferSemanticCodeDocEdges defaultSemanticEdgesConfig g embs
      case edges of
        [e] -> case edgeConfidence e of
          Confidence c -> c `shouldSatisfy` (\v -> abs (v - 0.82) < 1e-9)
        _ -> fail "expected exactly one edge"

    it "emits no edge for doc node with empty-vector embedding" $ do
      let embs = Map.fromList [("c1", [1.0, 0.0]), ("d1", [])]
          g = (buildGraph False (extractionFromLists [codeNode "c1" "a", docNode "d1" "b"] []))
                { gEmbeddings = Just embs }
      inferSemanticCodeDocEdges defaultSemanticEdgesConfig g embs `shouldBe` []
