module Graphos.UseCase.Query.RefineSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (uncons)

import Data.Text.Short (fromText)
import Graphos.Domain.Types
import Graphos.Domain.Graph.Score (ScoredNode(..))
import Graphos.UseCase.Query.Refine

testNode :: Text -> Text -> Maybe Int -> Text -> Node
testNode nid lbl line src = Node
  { nodeId           = nid
  , nodeLabel        = fromText lbl
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText src
  , nodeCommunityId  = Nothing
  , nodeDegree       = Just 3
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineStart    = line
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodePresentBits  = 0
  }

testScoredNode :: Text -> Text -> Text -> Double -> ScoredNode
testScoredNode nid lbl src score = ScoredNode
  { snNodeId      = nid
  , snLabel       = lbl
  , snScore       = score
   , snSourceFile  = src
   , snKind        = ""
   , snCommunityId = Nothing
   }

spec :: Spec
spec = do
  describe "refineEdges" $ do
    it "drops trivia-target contains edges in semantic mode" $ do
      let nodeMap = Map.fromList
            [ ("n1", testNode "n1" "MyModule" (Just 1) "src/Main.hs")
            , ("n2", testNode "n2" "undefined" (Just 5) "src/Main.hs")
            ]
          edges = [ ("n1", "n2", "contains" :: Text, 1.0)
                  , ("n1", "n3", "calls", 1.0)
                  ]
          result = refineEdges Semantic nodeMap edges
      result `shouldBe` [("n1", "n3", "calls", 1.0)]

    it "keeps trivia-target edges in all mode" $ do
      let nodeMap = Map.fromList
            [ ("n1", testNode "n1" "MyModule" (Just 1) "src/Main.hs")
            , ("n2", testNode "n2" "undefined" (Just 5) "src/Main.hs")
            ]
          edges = [ ("n1", "n2", "contains" :: Text, 1.0)
                  , ("n1", "n3", "calls", 1.0)
                  ]
          result = refineEdges All nodeMap edges
      length result `shouldBe` 2

    it "keeps non-contains edges in semantic mode" $ do
      let nodeMap = Map.fromList
            [ ("n1", testNode "n1" "MyModule" (Just 1) "src/Main.hs")
            , ("n2", testNode "n2" "undefined" (Just 5) "src/Main.hs")
            ]
          edges = [ ("n1", "n2", "calls" :: Text, 1.0) ]
          result = refineEdges Semantic nodeMap edges
      result `shouldBe` edges

  describe "collapseSelfEdges" $ do
    it "removes self-edges" $ do
      let edges = [ ("n1", "n1", "contains" :: Text, 1.0)
                  , ("n1", "n2", "calls", 1.0)
                  ]
          result = collapseSelfEdges edges
      result `shouldBe` [("n1", "n2", "calls", 1.0)]

    it "keeps non-self edges" $ do
      let edges = [ ("n1", "n2", "calls" :: Text, 1.0) ]
          result = collapseSelfEdges edges
      result `shouldBe` edges

  describe "dedupDeclarations" $ do
    it "merges nodes with same file+line and declaration-prefix variants" $ do
      let nodeMap = Map.fromList
            [ ("n1", testNode "n1" "export const foo" (Just 10) "src/A.ts")
            , ("n2", testNode "n2" "const foo" (Just 10) "src/A.ts")
            , ("n3", testNode "n3" "foo" (Just 10) "src/A.ts")
            ]
          nodes = [ testScoredNode "n1" "export const foo" "src/A.ts" 0.8
                  , testScoredNode "n2" "const foo" "src/A.ts" 0.8
                  , testScoredNode "n3" "foo" "src/A.ts" 0.8
                  ]
          edges = [ ("n1", "n4", "calls" :: Text, 1.0)
                  , ("n2", "n5", "imports", 1.0)
                  ]
          (dedupedNodes, _dedupedEdges) = dedupDeclarations nodeMap nodes edges
      length dedupedNodes `shouldBe` 1
      case uncons dedupedNodes of
        Just (rep, _) -> snLabel rep `shouldBe` "foo"
        Nothing        -> expectationFailure "expected at least one deduped node"

    it "keeps nodes with different files separate" $ do
      let nodeMap = Map.fromList
            [ ("n1", testNode "n1" "parse" (Just 1) "src/A.hs")
            , ("n2", testNode "n2" "parse" (Just 1) "src/B.hs")
            ]
          nodes = [ testScoredNode "n1" "parse" "src/A.hs" 0.5
                  , testScoredNode "n2" "parse" "src/B.hs" 0.5
                  ]
          (dedupedNodes, _) = dedupDeclarations nodeMap nodes []
      length dedupedNodes `shouldBe` 2

    it "keeps nodes with same name but different lines separate" $ do
      let nodeMap = Map.fromList
            [ ("n1", testNode "n1" "handler" (Just 10) "src/A.ts")
            , ("n2", testNode "n2" "handler" (Just 20) "src/A.ts")
            ]
          nodes = [ testScoredNode "n1" "handler" "src/A.ts" 0.5
                  , testScoredNode "n2" "handler" "src/A.ts" 0.5
                  ]
          (dedupedNodes, _) = dedupDeclarations nodeMap nodes []
      length dedupedNodes `shouldBe` 2

  describe "elideLabel" $ do
    it "keeps short labels unchanged" $ do
      elideLabel 120 "short label" `shouldBe` "short label"

    it "elides long labels at word boundary" $ do
      let longLabel = T.replicate 30 "word " :: Text
          result = elideLabel 40 longLabel
      T.length result `shouldSatisfy` (\l -> l <= 42)
      T.isSuffixOf "…" result `shouldBe` True

    it "preserves content under width" $ do
      let label = "moderate sized label" :: Text
      elideLabel 120 label `shouldBe` label

    it "elides at word boundary with ellipsis" $ do
      let label = "this is a very long label that should be truncated at a word boundary because it exceeds the width" :: Text
          result = elideLabel 40 label
      T.isSuffixOf "…" result `shouldBe` True