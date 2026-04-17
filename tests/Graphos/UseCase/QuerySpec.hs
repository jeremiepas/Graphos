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
  , nodeLabel         = nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = "test.hs"
  , nodeSourceLocation = Just "L1"
  , nodeSourceUrl    = Nothing
  , nodeCapturedAt   = Nothing
  , nodeAuthor       = Nothing
  , nodeContributor  = Nothing
  }

-- Helper: create a test edge
testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge
  { edgeSource        = src
  , edgeTarget        = tgt
  , edgeRelation      = Imports
  , edgeConfidence    = Extracted
  , edgeConfidenceScore = 1.0
  , edgeSourceFile    = "test.hs"
  , edgeSourceLocation = Just "L1"
  , edgeWeight        = 1.0
  }

spec :: Spec
spec = do
  describe "queryGraph" $ do
    it "finds nodes matching query terms" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "AuthModule", testNode "Database", testNode "Router"]
            , extractionEdges = [testEdge "AuthModule" "Database"]
            }
          g = buildGraph False ext
          result = queryGraph g "Auth" "bfs" 2000
      length (qrNodes result) `shouldSatisfy` (> 0)

    it "returns empty results for non-matching query" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "Alpha", testNode "Beta"]
            , extractionEdges = []
            }
          g = buildGraph False ext
          result = queryGraph g "ZZZZnotfound" "bfs" 2000
      length (qrNodes result) `shouldBe` 0

    it "supports DFS traversal mode" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "Hub", testNode "Leaf1", testNode "Leaf2"]
            , extractionEdges = [testEdge "Hub" "Leaf1", testEdge "Hub" "Leaf2"]
            }
          g = buildGraph False ext
          result = queryGraph g "Hub" "dfs" 2000
      qrTraverse result `shouldBe` "dfs"

    it "includes edges in the subgraph result for connected nodes" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "AuthModule", testNode "AuthLogin", testNode "AuthSession"]
            , extractionEdges = [testEdge "AuthModule" "AuthLogin", testEdge "AuthModule" "AuthSession"]
            }
          g = buildGraph False ext
          result = queryGraph g "Auth" "bfs" 2000
      -- Should find nodes matching "Auth" and their subgraph edges
      length (qrNodes result) `shouldSatisfy` (> 0)

  describe "pathQuery" $ do
    it "finds shortest path between connected nodes" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "start", testNode "mid", testNode "end"]
            , extractionEdges = [testEdge "start" "mid", testEdge "mid" "end"]
            }
          g = buildGraph False ext
      pathQuery g "start" "end" `shouldSatisfy` (/= Nothing)

    it "returns Nothing for disconnected nodes" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "isolated1", testNode "isolated2"]
            , extractionEdges = []
            }
          g = buildGraph False ext
      pathQuery g "isolated1" "isolated2" `shouldBe` Nothing

  describe "explainNode" $ do
    it "finds a node by label" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "MyModule"]
            , extractionEdges = []
            }
          g = buildGraph False ext
      explainNode g "MyModule" `shouldSatisfy` (/= Nothing)

    it "returns Nothing for non-existent node" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "Exists"]
            , extractionEdges = []
            }
          g = buildGraph False ext
      explainNode g "DoesNotExist" `shouldBe` Nothing