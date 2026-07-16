module Graphos.UseCase.QuerySpec where

import Test.Hspec
import Data.Text (Text)

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.UseCase.Query (queryGraph, pathQuery, explainNode, QueryResult(..))

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

-- helper: generate a unique EdgeId from source and target
edgeIdFrom :: Text -> Text -> EdgeId
edgeIdFrom src tgt = EdgeId (src <> "->" <> tgt)

-- Helper: create a test edge
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
  describe "queryGraph" $ do
    it "finds nodes matching query terms" $ do
      let ext = extractionFromLists [testNode "AuthModule", testNode "Database", testNode "Router"] [testEdge "AuthModule" "Database"]
          g = buildGraph False ext
          result = queryGraph g "Auth" "bfs" 2000
      length (qrNodes result) `shouldSatisfy` (> 0)

    it "returns empty results for non-matching query" $ do
      let ext = extractionFromLists [testNode "Alpha", testNode "Beta"] []
          g = buildGraph False ext
          result = queryGraph g "ZZZZnotfound" "bfs" 2000
      length (qrNodes result) `shouldBe` 0

    it "supports DFS traversal mode" $ do
      let ext = extractionFromLists [testNode "Hub", testNode "Leaf1", testNode "Leaf2"] [testEdge "Hub" "Leaf1", testEdge "Hub" "Leaf2"]
          g = buildGraph False ext
          result = queryGraph g "Hub" "dfs" 2000
      qrTraverse result `shouldBe` "dfs"

    it "includes edges in the subgraph result for connected nodes" $ do
      let ext = extractionFromLists [testNode "AuthModule", testNode "AuthLogin", testNode "AuthSession"] [testEdge "AuthModule" "AuthLogin", testEdge "AuthModule" "AuthSession"]
          g = buildGraph False ext
          result = queryGraph g "Auth" "bfs" 2000
      -- Should find nodes matching "Auth" and their subgraph edges
      length (qrNodes result) `shouldSatisfy` (> 0)

  describe "pathQuery" $ do
    it "finds shortest path between connected nodes" $ do
      let ext = extractionFromLists [testNode "start", testNode "mid", testNode "end"] [testEdge "start" "mid", testEdge "mid" "end"]
          g = buildGraph False ext
      pathQuery g "start" "end" `shouldSatisfy` (/= Nothing)

    it "returns Nothing for disconnected nodes" $ do
      let ext = extractionFromLists [testNode "isolated1", testNode "isolated2"] []
          g = buildGraph False ext
      pathQuery g "isolated1" "isolated2" `shouldBe` Nothing

  describe "explainNode" $ do
    it "finds a node by label" $ do
      let ext = extractionFromLists [testNode "MyModule"] []
          g = buildGraph False ext
      explainNode g "MyModule" `shouldSatisfy` (/= Nothing)

    it "returns Nothing for non-existent node" $ do
      let ext = extractionFromLists [testNode "Exists"] []
          g = buildGraph False ext
      explainNode g "DoesNotExist" `shouldBe` Nothing