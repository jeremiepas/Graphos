module Graphos.UseCase.FormatContextSpec where

import Test.Hspec
import qualified Data.Text as T

import Data.Text.Short (fromText)
import Graphos.Domain.Types (Node(..), FileType(..), Edge(..), EdgeId(..), Relation(..), Confidence(..))
import Graphos.UseCase.FormatContext

mkTestEdge :: T.Text -> Relation -> Double -> Edge
mkTestEdge tgt rel conf =
  Edge (EdgeId ("src->" <> tgt)) "src" tgt rel conf (Confidence conf) Nothing

spec :: Spec
spec = describe "FormatContext" $ do
  describe "countContextTokens" $ do
    it "approximates token count from text" $ do
      countContextTokens "hello world" `shouldSatisfy` (> 0)

    it "returns 0 for empty text" $ do
      countContextTokens "" `shouldBe` 0

  describe "formatNodeCompact" $ do
    it "includes label and file type" $ do
      let node = Node
            { nodeId           = "test_node"
            , nodeLabel        = fromText "TestNode"
            , nodeFileType     = CodeFile
            , nodeSourceFile   = fromText "src/Test.hs"
            , nodeLineStart    = Just 42
            , nodeCommunityId  = Nothing
            , nodeDegree       = Nothing
            , nodeIsBridge     = Nothing
            , nodeExtra        = Nothing
            , nodeLineEnd      = Just 50
            , nodeKind         = Nothing
            , nodeSignature    = Nothing
            , nodePresentBits  = 0
            }
          result = formatNodeCompact "test_node" node
      T.isInfixOf "TestNode" result `shouldBe` True
      T.isInfixOf "code" result `shouldBe` True
      T.isInfixOf "src:src/Test.hs:42-50" result `shouldBe` True

    it "omits location when line fields are absent" $ do
      let node = Node
            { nodeId           = "test_node2"
            , nodeLabel        = fromText "TestNode"
            , nodeFileType     = CodeFile
            , nodeSourceFile   = fromText "src/Test.hs"
            , nodeLineStart    = Nothing
            , nodeCommunityId  = Nothing
            , nodeDegree       = Nothing
            , nodeIsBridge     = Nothing
            , nodeExtra        = Nothing
            , nodeLineEnd      = Nothing
            , nodeKind         = Nothing
            , nodeSignature    = Nothing
            , nodePresentBits  = 0
            }
          result = formatNodeCompact "test_node2" node
      T.isInfixOf "src:src/Test.hs:42" result `shouldBe` False


  describe "filterAndRankEdges (semantic edge set)" $ do
    it "drops low-confidence inferred edges in semantic mode" $ do
      let lowInferred = mkTestEdge "trgt" Inferred 0.5
      filterAndRankEdges Semantic [lowInferred] `shouldBe` []

    it "keeps low-confidence inferred edges in all mode" $ do
      let lowInferred = mkTestEdge "trgt" Inferred 0.5
      filterAndRankEdges All [lowInferred] `shouldBe` [lowInferred]

    it "keeps documents edges in semantic mode regardless of confidence" $ do
      -- Deterministic doc-code edges survive the semantic (non-ambiguous)
      -- filter: they are classified into the semantic edge set.
      let docEdge = mkTestEdge "lib" Documents 0.5
      filterAndRankEdges Semantic [docEdge] `shouldBe` [docEdge]

    it "keeps documents edges even when the target looks like trivia" $ do
      let docEdge = mkTestEdge "null" Documents 0.9
      filterAndRankEdges Semantic [docEdge] `shouldBe` [docEdge]

    it "still drops ambiguous non-documents edges that look like trivia" $ do
      let triviaInferred = mkTestEdge "null" Inferred 0.5
      filterAndRankEdges Semantic [triviaInferred] `shouldBe` []

    it "keeps high-confidence inferred edges in semantic mode" $ do
      let strongInferred = mkTestEdge "trgt" Inferred 0.85
      filterAndRankEdges Semantic [strongInferred] `shouldBe` [strongInferred]
