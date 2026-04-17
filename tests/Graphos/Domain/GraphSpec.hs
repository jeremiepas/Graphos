{-# OPTIONS_GHC -Wno-x-partial #-}
module Graphos.Domain.GraphSpec where

import Test.Hspec
import Data.Maybe (fromJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Graphos.Domain.Types
import Graphos.Domain.Graph

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

-- Helper: create a test node with custom label
testNodeWithLabel :: Text -> Text -> Node
testNodeWithLabel nid label = Node
  { nodeId           = nid
  , nodeLabel        = label
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
  , edgeRelation      = Calls
  , edgeConfidence    = Extracted
  , edgeConfidenceScore = 1.0
  , edgeSourceFile    = "test.hs"
  , edgeSourceLocation = Just "L1"
  , edgeWeight        = 1.0
  }

-- Helper: create a test edge with custom confidence
testEdgeWithConfidence :: Text -> Text -> Confidence -> Edge
testEdgeWithConfidence src tgt conf = Edge
  { edgeSource        = src
  , edgeTarget        = tgt
  , edgeRelation      = References
  , edgeConfidence    = conf
  , edgeConfidenceScore = confidenceScore conf
  , edgeSourceFile    = "test.hs"
  , edgeSourceLocation = Just "L1"
  , edgeWeight        = 1.0
  }

-- Helper: create a test node with file type
testNodeWithFile :: Text -> FileType -> Text -> Node
testNodeWithFile nid ft srcFile = Node
  { nodeId           = nid
  , nodeLabel        = nid
  , nodeFileType     = ft
  , nodeSourceFile   = srcFile
  , nodeSourceLocation = Just "L1"
  , nodeSourceUrl    = Nothing
  , nodeCapturedAt   = Nothing
  , nodeAuthor       = Nothing
  , nodeContributor  = Nothing
  }

spec :: Spec
spec = do
  describe "buildGraph" $ do
    it "creates an empty graph from empty extraction" $ do
      let g = buildGraph False emptyExtraction
      Map.size (gNodes g) `shouldBe` 0

    it "creates nodes from extraction" $ do
      let ext = emptyExtraction { extractionNodes = [testNode "a", testNode "b"] }
          g = buildGraph False ext
      Map.size (gNodes g) `shouldBe` 2

  describe "mergeExtractions" $ do
    it "deduplicates nodes by id" $ do
      let a = emptyExtraction { extractionNodes = [testNode "x"] }
          b = emptyExtraction { extractionNodes = [testNode "x", testNode "y"] }
          merged = mergeExtractions a b
      length (extractionNodes merged) `shouldBe` 2

  describe "degree" $ do
    it "returns 0 for isolated nodes" $ do
      let g = buildGraph False emptyExtraction { extractionNodes = [testNode "a"] }
      degree g "a" `shouldBe` 0

  describe "shortestPath" $ do
    it "returns Just for connected nodes" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "a", testNode "b", testNode "c"]
            , extractionEdges = [testEdge "a" "b", testEdge "b" "c"]
            }
          g = buildGraph False ext
      shortestPath g "a" "c" `shouldBe` Just ["a", "b", "c"]

    it "returns Nothing for disconnected nodes" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "a", testNode "b"]
            }
          g = buildGraph False ext
      shortestPath g "a" "b" `shouldBe` Nothing

  describe "godNodes" $ do
    it "returns list sorted by degree descending" $ do
      let ext = emptyExtraction
            { extractionNodes = 
                [ testNodeWithLabel "a" "Alpha"
                , testNodeWithLabel "b" "Beta" 
                , testNodeWithLabel "c" "Gamma"
                ]
            , extractionEdges = 
                [ testEdge "a" "b"
                , testEdge "a" "c"
                , testEdge "b" "c"
                ]
            }
          g = buildGraph False ext
          result = godNodes g 10
      -- In a fully connected 3-node undirected graph, all have degree 2
      -- All non-file, non-concept nodes with degree > 0 are included
      length result `shouldBe` 3
      gnEdges (fromJust (listToMaybe result)) `shouldBe` 2

    it "excludes file nodes from results" $ do
      let fileNode = Node
            { nodeId = "test.hs"
            , nodeLabel = "test.hs"
            , nodeFileType = CodeFile
            , nodeSourceFile = "test.hs"
            , nodeSourceLocation = Just "L1"
            , nodeSourceUrl = Nothing
            , nodeCapturedAt = Nothing
            , nodeAuthor = Nothing
            , nodeContributor = Nothing
            }
          ext = emptyExtraction
            { extractionNodes = [fileNode, testNode "func"]
            , extractionEdges = [testEdge "test.hs" "func"]
            }
          g = buildGraph False ext
          result = godNodes g 10
      -- File node should be excluded
      map gnId result `shouldNotContain` ["test.hs"]

    it "respects topN parameter" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "a", testNode "b", testNode "c", testNode "d"]
            , extractionEdges = [testEdge "a" "b", testEdge "a" "c", testEdge "a" "d", testEdge "b" "c"]
            }
          g = buildGraph False ext
          result = godNodes g 2
      length result `shouldBe` 2
      gnId (fromJust (listToMaybe result)) `shouldBe` "a"  -- highest degree

  describe "neighbors" $ do
    it "returns connected nodes" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "a", testNode "b", testNode "c"]
            , extractionEdges = [testEdge "a" "b", testEdge "a" "c"]
            }
          g = buildGraph False ext
          nbs = neighbors g "a"
      Set.size nbs `shouldBe` 2
      Set.member "b" nbs `shouldBe` True
      Set.member "c" nbs `shouldBe` True

    it "returns empty set for isolated nodes" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "a", testNode "b"]
            }
          g = buildGraph False ext
          nbs = neighbors g "a"
      Set.size nbs `shouldBe` 0

  describe "graphDiff" $ do
    it "detects new nodes" $ do
      let old = emptyExtraction { extractionNodes = [testNode "a", testNode "b"] }
          new = emptyExtraction { extractionNodes = [testNode "a", testNode "b", testNode "c"] }
          gOld = buildGraph False old
          gNew = buildGraph False new
          diff = graphDiff gOld gNew
      length (gdNewNodes diff) `shouldBe` 1
      nodeId (fromJust (listToMaybe (gdNewNodes diff))) `shouldBe` "c"

    it "detects removed nodes" $ do
      let old = emptyExtraction { extractionNodes = [testNode "a", testNode "b"] }
          new = emptyExtraction { extractionNodes = [testNode "a"] }
          gOld = buildGraph False old
          gNew = buildGraph False new
          diff = graphDiff gOld gNew
      length (gdRemovedNodes diff) `shouldBe` 1
      fst (fromJust (listToMaybe (gdRemovedNodes diff))) `shouldBe` "b"

    it "detects new edges" $ do
      let old = emptyExtraction
            { extractionNodes = [testNode "a", testNode "b"]
            , extractionEdges = [testEdge "a" "b"]
            }
          new = emptyExtraction
            { extractionNodes = [testNode "a", testNode "b", testNode "c"]
            , extractionEdges = [testEdge "a" "b", testEdge "b" "c"]
            }
          gOld = buildGraph False old
          gNew = buildGraph False new
          diff = graphDiff gOld gNew
      length (gdNewEdges diff) `shouldBe` 1

    it "returns no changes when graphs are equal" $ do
      let ext = emptyExtraction
            { extractionNodes = [testNode "a", testNode "b"]
            , extractionEdges = [testEdge "a" "b"]
            }
          g = buildGraph False ext
          diff = graphDiff g g
      gdSummary diff `shouldBe` "no changes"

  describe "mergeGraphs" $ do
    it "combines nodes from both graphs" $ do
      let ext1 = emptyExtraction { extractionNodes = [testNode "a"] }
          ext2 = emptyExtraction { extractionNodes = [testNode "b"] }
          g1 = buildGraph False ext1
          g2 = buildGraph False ext2
          merged = mergeGraphs g1 g2
      Map.size (gNodes merged) `shouldBe` 2

    it "old graph takes precedence for duplicates" $ do
      let n1 = testNodeWithLabel "a" "Original"
          n2 = testNodeWithLabel "a" "Updated"
          ext1 = emptyExtraction { extractionNodes = [n1] }
          ext2 = emptyExtraction { extractionNodes = [n2] }
          g1 = buildGraph False ext1
          g2 = buildGraph False ext2
          merged = mergeGraphs g1 g2
      -- Old graph takes precedence (<>) - first argument wins
      nodeLabel (fromJust (Map.lookup "a" (gNodes merged))) `shouldBe` "Original"

  describe "isFileNode" $ do
    it "identifies file nodes by label matching source" $ do
      let n = Node
            { nodeId = "test.hs"
            , nodeLabel = "test.hs"
            , nodeFileType = CodeFile
            , nodeSourceFile = "test.hs"
            , nodeSourceLocation = Nothing
            , nodeSourceUrl = Nothing
            , nodeCapturedAt = Nothing
            , nodeAuthor = Nothing
            , nodeContributor = Nothing
            }
          ext = emptyExtraction { extractionNodes = [n] }
          g = buildGraph False ext
      isFileNode g n `shouldBe` True

    it "identifies method stubs" $ do
      let n = Node
            { nodeId = ".foo()"
            , nodeLabel = ".foo()"
            , nodeFileType = CodeFile
            , nodeSourceFile = "test.hs"
            , nodeSourceLocation = Nothing
            , nodeSourceUrl = Nothing
            , nodeCapturedAt = Nothing
            , nodeAuthor = Nothing
            , nodeContributor = Nothing
            }
          ext = emptyExtraction { extractionNodes = [n] }
          g = buildGraph False ext
      isFileNode g n `shouldBe` True

  describe "isConceptNode" $ do
    it "returns True for empty source file" $ do
      let n = testNodeWithFile "concept" CodeFile ""
      isConceptNode n `shouldBe` True

    it "returns False for real source file" $ do
      let n = testNodeWithFile "func" CodeFile "model.py"
      isConceptNode n `shouldBe` False