module Graphos.Domain.Community.LabelSpec where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Community.Label (suggestCommunityLabels, labelFromNodes)

-- Helper: create a test node
testNode :: Text -> Node
testNode nid = Node
  { nodeId           = nid
  , nodeLabel        = nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = "test.hs"
  , nodeLineStart    = Nothing
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeSourceLocation = Just "L1"
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodeSourceUrl    = Nothing
  , nodeCapturedAt   = Nothing
  , nodeAuthor       = Nothing
  , nodeContributor  = Nothing
  }

-- Helper: create a test edge
edgeIdFrom :: Text -> Text -> EdgeId
edgeIdFrom src tgt = EdgeId (src <> "->" <> tgt)

testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge
  { edgeId        = edgeIdFrom src tgt
  , edgeSource    = src
  , edgeTarget    = tgt
  , edgeRelation  = Imports
  , edgeConfidence = Confidence 1.0
  , edgeWeight    = 1.0
  }

spec :: Spec
spec = do
  describe "labelFromNodes" $ do
    it "returns label for nodes with labels" $ do
      let ext = extractionFromLists [testNode "Auth", testNode "Database"] [testEdge "Auth" "Database"]
          g = buildGraph False ext
      let label = labelFromNodes g ["Auth", "Database"]
      label `shouldSatisfy` (not . T.null)

    it "returns Unnamed Community for empty node list" $ do
      let ext = extractionFromLists [] []
          g = buildGraph False ext
      labelFromNodes g [] `shouldBe` "Unnamed Community"

    it "handles nodes with zero degree without division by zero" $ do
      let ext = extractionFromLists [testNode "Module.Authentication"] []
          g = buildGraph False ext
      -- This should NOT crash with division by zero
      let label = labelFromNodes g ["Module.Authentication"]
      label `shouldSatisfy` (not . T.null)

  describe "suggestCommunityLabels" $ do
    it "generates labels for communities" $ do
      let ext = extractionFromLists [testNode "Auth", testNode "Database", testNode "Router"] [testEdge "Auth" "Database", testEdge "Auth" "Router"]
          g = buildGraph False ext
          commMap = Map.fromList [(0, ["Auth", "Database", "Router"])]
          labels = suggestCommunityLabels g commMap
      Map.size labels `shouldSatisfy` (>= 1)

    it "handles empty community gracefully" $ do
      let ext = extractionFromLists [] []
          g = buildGraph False ext
          commMap = Map.fromList [(0, [])]
          labels = suggestCommunityLabels g commMap
      -- Should not crash on empty community
      Map.size labels `shouldBe` 1