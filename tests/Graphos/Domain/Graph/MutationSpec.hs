{-# LANGUAGE OverloadedStrings #-}
module Graphos.Domain.Graph.MutationSpec where

import Test.Hspec
import Data.Text (Text)
import Data.Text.Short (fromText)
import qualified Data.Map.Strict as Map

import Graphos.Domain.Types
import Graphos.Domain.Graph
import Graphos.Domain.Graph.Mutation (rebuildAdjacency)

node :: Text -> Node
node nid = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "test.hs"
  , nodeSource       = Nothing
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodePresentBits  = 0
  }

edge :: Text -> Text -> Edge
edge s t = Edge
  { edgeId        = EdgeId (s <> "->" <> t)
  , edgeSource    = s
  , edgeTarget    = t
  , edgeRelation  = Calls
  , edgeWeight    = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra     = Nothing
  }

spec :: Spec
spec = do
  describe "deleteNodeIncident canonical adjacency" $ do
    it "matches rebuildAdjacency after deletion (directed)" $ do
      let dir = True
          ns = [node "a", node "b", node "c", node "d"]
          es = [edge "a" "b", edge "b" "c", edge "c" "d", edge "d" "b"]
          g  = buildGraph dir (extractionFromLists ns es)
          (g', _) = deleteNodeIncident "b" g
          canon = rebuildAdjacency g'
      gAdjFwd g' `shouldBe` gAdjFwd canon
      gAdjBack g' `shouldBe` gAdjBack canon
    it "matches rebuildAdjacency after deletion (undirected)" $ do
      let dir = False
          ns = [node "a", node "b", node "c", node "d"]
          es = [edge "a" "b", edge "b" "c", edge "c" "d"]
          g0 = buildGraph dir (extractionFromLists ns es)
          -- Start from canonical adjacency so undirected rows include both
          -- directions (matches 'rebuildAdjacency' semantics).
          g  = rebuildAdjacency g0
          -- delete a leaf whose removal empties adjacency entries
          (g', _) = deleteNodeIncident "a" g
          canon = rebuildAdjacency g'
      gAdjFwd g' `shouldBe` gAdjFwd canon
      gAdjBack g' `shouldBe` gAdjBack canon
    it "drops empty adjacency entries (star centre removal)" $ do
      let dir = True
          ns = [node "a", node "b", node "c"]
          es = [edge "a" "b", edge "a" "c"]
          g  = buildGraph dir (extractionFromLists ns es)
          (g', _) = deleteNodeIncident "a" g
      Map.member "b" (gAdjFwd g') `shouldBe` False
      Map.member "c" (gAdjFwd g') `shouldBe` False
      Map.member "b" (gAdjBack g') `shouldBe` False
      Map.member "c" (gAdjBack g') `shouldBe` False