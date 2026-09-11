{-# OPTIONS_GHC -Wno-x-partial #-}
module Graphos.Domain.GraphSpec where

import Test.Hspec
import Data.Aeson (Value(..), eitherDecode, encode)
import Data.Either (fromRight, isRight)
import Data.Maybe (fromJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text.Short (fromText)

import Graphos.Domain.Types
import Graphos.Domain.Graph

-- Helper: create a test node
testNode :: Text -> Node
testNode nid = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "test.hs"

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

-- Helper: create a test node with custom label
testNodeWithLabel :: Text -> Text -> Node
testNodeWithLabel nid label = Node
  { nodeId           = nid
  , nodeLabel        = fromText label
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "test.hs"

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

-- helper: generate a unique EdgeId from source and target
edgeIdFrom :: Text -> Text -> EdgeId
edgeIdFrom src tgt = EdgeId (src <> "->" <> tgt)

-- Helper: create a test edge
testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge
  { edgeId        = edgeIdFrom src tgt
  , edgeSource    = src
  , edgeTarget    = tgt
  , edgeRelation  = Calls
  , edgeConfidence = Confidence 1.0
  , edgeWeight    = 1.0
  , edgeExtra     = Nothing
  }

testEdgeWithConfidence :: Text -> Text -> Confidence -> Edge
testEdgeWithConfidence src tgt conf = Edge
  { edgeId        = edgeIdFrom src tgt
  , edgeSource    = src
  , edgeTarget    = tgt
  , edgeRelation  = References
  , edgeConfidence = conf
  , edgeWeight    = 1.0
  , edgeExtra     = Nothing
  }

-- | Edge with an explicit relation (used by the LG-C3 feasibility gate below).
mkRelEdge :: Text -> Text -> Relation -> Edge
mkRelEdge src tgt rel = Edge
  { edgeId        = edgeIdFrom src tgt
  , edgeSource    = src
  , edgeTarget    = tgt
  , edgeRelation  = rel
  , edgeConfidence = Confidence 1.0
  , edgeWeight    = 1.0
  , edgeExtra     = Nothing
  }

-- Helper: create a test node with file type
testNodeWithFile :: Text -> FileType -> Text -> Node
testNodeWithFile nid ft srcFile = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = ft
  , nodeSourceFile   = fromText srcFile

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

spec :: Spec
spec = do
  describe "buildGraph" $ do
    it "creates an empty graph from empty extraction" $ do
      let g = buildGraph False emptyExtraction
      Map.size (gNodes g) `shouldBe` 0

    it "creates nodes from extraction" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] []
          g = buildGraph False ext
      Map.size (gNodes g) `shouldBe` 2

  describe "mergeExtractions" $ do
    it "deduplicates nodes by id" $ do
      let a = extractionFromLists [testNode "x"] []
          b = extractionFromLists [testNode "x", testNode "y"] []
          merged = mergeExtractions a b
      length (extractionNodes merged) `shouldBe` 2

  describe "degree" $ do
    it "returns 0 for isolated nodes" $ do
      let g = buildGraph False (extractionFromLists [testNode "a"] [])
      degree g "a" `shouldBe` 0

  describe "shortestPath" $ do
    it "returns Just for connected nodes" $ do
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c"] [testEdge "a" "b", testEdge "b" "c"]
          g = buildGraph False ext
      shortestPath g "a" "c" `shouldBe` Just ["a", "b", "c"]

    it "returns Nothing for disconnected nodes" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] []
          g = buildGraph False ext
      shortestPath g "a" "b" `shouldBe` Nothing

  describe "godNodes" $ do
    it "returns list sorted by degree descending" $ do
      let ext = extractionFromLists [ testNodeWithLabel "a" "Alpha"
                , testNodeWithLabel "b" "Beta" 
                , testNodeWithLabel "c" "Gamma"
                ] [ testEdge "a" "b"
                , testEdge "a" "c"
                , testEdge "b" "c"
                ]
          g = buildGraph False ext
          result = godNodes g 10
      -- In a fully connected 3-node undirected graph, all have degree 2
      -- All non-file, non-concept nodes with degree > 0 are included
      length result `shouldBe` 3
      gnEdges (fromJust (listToMaybe result)) `shouldBe` 2

    it "excludes file nodes from results" $ do
      let fileNode = Node
            { nodeId = "test.hs"
            , nodeLabel = fromText "test.hs"
            , nodeFileType = CodeFile
            , nodeSourceFile = fromText "test.hs"

  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
            , nodeLineStart    = Just 1
            , nodeLineEnd = Nothing
            , nodeKind = Nothing
            , nodeSignature = Nothing
            , nodePresentBits  = 0
            }
          ext = extractionFromLists [fileNode, testNode "func"] [testEdge "test.hs" "func"]
          g = buildGraph False ext
          result = godNodes g 10
      -- File node should be excluded
      map gnId result `shouldNotContain` ["test.hs"]

    it "respects topN parameter" $ do
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c", testNode "d"] [testEdge "a" "b", testEdge "a" "c", testEdge "a" "d", testEdge "b" "c"]
          g = buildGraph False ext
          result = godNodes g 2
      length result `shouldBe` 2
      gnId (fromJust (listToMaybe result)) `shouldBe` "a"  -- highest degree

  describe "neighbors" $ do
    it "returns connected nodes" $ do
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c"] [testEdge "a" "b", testEdge "a" "c"]
          g = buildGraph False ext
          nbs = neighbors g "a"
      Set.size nbs `shouldBe` 2
      Set.member "b" nbs `shouldBe` True
      Set.member "c" nbs `shouldBe` True

    it "returns empty set for isolated nodes" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] []
          g = buildGraph False ext
          nbs = neighbors g "a"
      Set.size nbs `shouldBe` 0

  describe "graphDiff" $ do
    it "detects new nodes" $ do
      let old = extractionFromLists [testNode "a", testNode "b"] []
          new = extractionFromLists [testNode "a", testNode "b", testNode "c"] []
          gOld = buildGraph False old
          gNew = buildGraph False new
          diff = graphDiff gOld gNew
      length (diffAddedNodes diff) `shouldBe` 1
      nodeId (head $ Map.elems $ diffAddedNodes diff) `shouldBe` "c"

    it "detects removed nodes" $ do
      let old = extractionFromLists [testNode "a", testNode "b"] []
          new = extractionFromLists [testNode "a"] []
          gOld = buildGraph False old
          gNew = buildGraph False new
          diff = graphDiff gOld gNew
      length (diffRemovedNodes diff) `shouldBe` 1
      fst (head $ Map.toList $ diffRemovedNodes diff) `shouldBe` "b"

    it "detects new edges" $ do
      let old = extractionFromLists [testNode "a", testNode "b"] [testEdge "a" "b"]
          new = extractionFromLists [testNode "a", testNode "b", testNode "c"] [testEdge "a" "b", testEdge "b" "c"]
          gOld = buildGraph False old
          gNew = buildGraph False new
          diff = graphDiff gOld gNew
      length (diffAddedEdges diff) `shouldBe` 1

    it "returns no changes when graphs are equal" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] [testEdge "a" "b"]
          g = buildGraph False ext
          diff = graphDiff g g
      -- No changes means all added/removed maps are empty
      Map.null (diffAddedNodes diff) `shouldBe` True
      Map.null (diffRemovedNodes diff) `shouldBe` True
      Map.null (diffAddedEdges diff) `shouldBe` True
      Map.null (diffRemovedEdges diff) `shouldBe` True

  describe "mergeGraphs" $ do
    it "combines nodes from both graphs" $ do
      let ext1 = extractionFromLists [testNode "a"] []
          ext2 = extractionFromLists [testNode "b"] []
          g1 = buildGraph False ext1
          g2 = buildGraph False ext2
          merged = mergeGraphs g1 g2
      Map.size (gNodes merged) `shouldBe` 2

    it "old graph takes precedence for duplicates" $ do
      let n1 = testNodeWithLabel "a" "Original"
          n2 = testNodeWithLabel "a" "Updated"
          ext1 = extractionFromLists [n1] []
          ext2 = extractionFromLists [n2] []
          g1 = buildGraph False ext1
          g2 = buildGraph False ext2
          merged = mergeGraphs g1 g2
      -- Old graph takes precedence (<>) - first argument wins
      nodeLabel (fromJust (Map.lookup "a" (gNodes merged))) `shouldBe` "Original"

  describe "mergeGraphs — LG-C3 feasibility gate (identification)" $ do

    describe "LG-C2 (agreeing shared node key maps identically under both cocone legs)" $ do
      it "Agree(A,B): identical node value is carried by both operands and the merge" $ do
        let eA = extractionFromLists
              [ testNodeWithLabel "shared" "Alpha", testNode "a" ] []
            eB = extractionFromLists
              [ testNodeWithLabel "shared" "Alpha", testNode "b" ] []
            ga = buildGraph False eA
            gb = buildGraph False eB
            gm = mergeGraphs ga gb
            k = "shared" :: Text
        Map.lookup k (gNodes gm) `shouldBe` Map.lookup k (gNodes ga)
        Map.lookup k (gNodes gm) `shouldBe` Map.lookup k (gNodes gb)

      it "LG-C2 holds for every agreeing node key simultaneously" $ do
        let eA = extractionFromLists
              [ testNodeWithLabel "s1" "Alpha", testNodeWithLabel "s2" "Beta", testNode "a" ] []
            eB = extractionFromLists
              [ testNodeWithLabel "s1" "Alpha", testNodeWithLabel "s2" "Beta", testNode "b" ] []
            ga = buildGraph False eA
            gb = buildGraph False eB
            gm = mergeGraphs ga gb
        Map.lookup "s1" (gNodes gm) `shouldBe` Map.lookup "s1" (gNodes ga)
        Map.lookup "s1" (gNodes gm) `shouldBe` Map.lookup "s1" (gNodes gb)
        Map.lookup "s2" (gNodes gm) `shouldBe` Map.lookup "s2" (gNodes ga)
        Map.lookup "s2" (gNodes gm) `shouldBe` Map.lookup "s2" (gNodes gb)

    describe "LG-C3 (no identification beyond shared identical labels)" $ do
      it "|keys(gNodes gm)| == |keys(gNodes ga) ∪ keys(gNodes gb)| — no over-identification of distinct keys" $ do
        let eA = extractionFromLists
              [ testNodeWithLabel "shared" "Alpha", testNode "a", testNode "c" ] []
            eB = extractionFromLists
              [ testNodeWithLabel "shared" "Alpha", testNode "b", testNode "d" ] []
            ga = buildGraph False eA
            gb = buildGraph False eB
            gm = mergeGraphs ga gb
            keysA = Map.keysSet (gNodes ga)
            keysB = Map.keysSet (gNodes gb)
            keysUnion = keysA `Set.union` keysB
        Map.size (gNodes gm) `shouldBe` Set.size keysUnion
        -- every merged key is present in at least one operand (nothing collapsed away)
        all (\k -> Set.member k keysA || Set.member k keysB) (Map.keys (gNodes gm)) `shouldBe` True

      it "agreeing edge key is carried by both operands" $ do
        let eA = extractionFromLists
              [ testNode "a", testNode "b" ] [ testEdge "a" "b" ]
            eB = extractionFromLists
              [ testNode "a", testNode "b" ] [ testEdge "a" "b" ]
            ga = buildGraph False eA
            gb = buildGraph False eB
            gm = mergeGraphs ga gb
            ek = ("a", "b") :: (NodeId, NodeId)
        Map.lookup ek (gEdges gm) `shouldBe` Map.lookup ek (gEdges ga)
        Map.lookup ek (gEdges gm) `shouldBe` Map.lookup ek (gEdges gb)
        edgeRelation (fromJust (Map.lookup ek (gEdges gm))) `shouldBe` Calls

      it "edges with distinct endpoint pairs never spuriously merge" $ do
        let eA = extractionFromLists
              [ testNode "a", testNode "b", testNode "c" ]
              [ testEdge "a" "b", testEdge "b" "c" ]
            eB = extractionFromLists
              [ testNode "x", testNode "y", testNode "z" ]
              [ testEdge "x" "y", testEdge "y" "z" ]
            ga = buildGraph False eA
            gb = buildGraph False eB
            gm = mergeGraphs ga gb
        Map.size (gEdges gm) `shouldBe` 4
        Map.member ("a", "b") (gEdges gm) `shouldBe` True
        Map.member ("b", "c") (gEdges gm) `shouldBe` True
        Map.member ("x", "y") (gEdges gm) `shouldBe` True
        Map.member ("y", "z") (gEdges gm) `shouldBe` True

    describe "LG-C4 (conflicting key over-identifies: pushout but NOT pullback — expected)" $ do
      it "same node key, differing value collapses to one entry (over-identification), not a correctness violation" $ do
        let eA = extractionFromLists [ testNodeWithLabel "shared" "Alpha", testNode "a" ] []
            eB = extractionFromLists [ testNodeWithLabel "shared" "Gamma", testNode "b" ] []
            ga = buildGraph False eA
            gb = buildGraph False eB
            gm = mergeGraphs ga gb
        -- conflicting key collapses to a single entry (fiber product over-identifies)
        Map.size (gNodes gm) `shouldBe` 3
        let kept = fromJust (Map.lookup "shared" (gNodes gm))
            fromA  = fromJust (Map.lookup "shared" (gNodes ga))
            fromB  = fromJust (Map.lookup "shared" (gNodes gb))
        kept `shouldBe` fromA
        kept /= fromB `shouldBe` True
        nodeLabel kept `shouldBe` "Alpha"

      it "same edge endpoint pair, differing relation collapses to one edge (documented non-pullback)" $ do
        let eA = extractionFromLists [ testNode "a", testNode "b" ] [ testEdge "a" "b" ]
            eB = extractionFromLists [ testNode "a", testNode "b" ] [ mkRelEdge "a" "b" Imports ]
            ga = buildGraph False eA
            gb = buildGraph False eB
            gm = mergeGraphs ga gb
            ek = ("a", "b") :: (NodeId, NodeId)
        Map.size (gEdges gm) `shouldBe` 1
        edgeRelation (fromJust (Map.lookup ek (gEdges gm))) `shouldBe` Calls

  describe "embeddings_path JSON round-trip" $ do
    it "omits embeddings_path when gEmbeddingsPath is Nothing" $ do
      let g = buildGraph False (extractionFromLists [testNode "a"] [])
      let decoded = eitherDecode (encode g) :: Either String (Map.Map Text Value)
      decoded `shouldSatisfy` isRight
      Map.member "embeddings_path" (fromRight Map.empty decoded) `shouldBe` False

    it "includes embeddings_path when set" $ do
      let g = (buildGraph False (extractionFromLists [testNode "a"] []))
            { gEmbeddingsPath = Just "embeddings.json" }
      let decoded = eitherDecode (encode g) :: Either String (Map.Map Text Value)
      decoded `shouldSatisfy` isRight
      let m = fromRight Map.empty decoded
      Map.member "embeddings_path" m `shouldBe` True
      m Map.! "embeddings_path" `shouldBe` String "embeddings.json"

    it "decodes embeddings_path and leaves gEmbeddings as Nothing" $ do
      let g = (buildGraph False (extractionFromLists [testNode "a"] []))
            { gEmbeddingsPath = Just "embeddings.json" }
      let decoded = eitherDecode (encode g) :: Either String Graph
      decoded `shouldSatisfy` isRight
      let g' = fromRight (error "decode failed") decoded
      gEmbeddingsPath g' `shouldBe` Just "embeddings.json"
      gEmbeddings g' `shouldBe` Nothing

    it "decodes graphs without embeddings_path" $ do
      let g = buildGraph False (extractionFromLists [testNode "a"] [])
      let decoded = eitherDecode (encode g) :: Either String Graph
      decoded `shouldSatisfy` isRight
      let g' = fromRight (error "decode failed") decoded
      gEmbeddingsPath g' `shouldBe` Nothing
      gEmbeddings g' `shouldBe` Nothing

  describe "isFileNode" $ do
    it "identifies file nodes by label matching source" $ do
      let n = Node
            { nodeId = "test.hs"
            , nodeLabel = fromText "test.hs"
            , nodeFileType = CodeFile
            , nodeSourceFile = fromText "test.hs"
            , nodeLineStart    = Just 1
            , nodeCommunityId  = Nothing
            , nodeDegree       = Nothing
            , nodeIsBridge     = Nothing
            , nodeExtra        = Nothing
            , nodeLineEnd      = Nothing
            , nodeKind         = Nothing
            , nodeSignature    = Nothing
            , nodePresentBits  = 0
            }
          ext = extractionFromLists [n] []
          g = buildGraph False ext
      isFileNode g n `shouldBe` True

    it "identifies method stubs" $ do
      let n = Node
            { nodeId = ".foo()"
            , nodeLabel = fromText ".foo()"
            , nodeFileType = CodeFile
            , nodeSourceFile = fromText "test.hs"
            , nodeLineStart    = Just 1
            , nodeCommunityId  = Nothing
            , nodeDegree       = Nothing
            , nodeIsBridge     = Nothing
            , nodeExtra        = Nothing
            , nodeLineEnd      = Nothing
            , nodeKind         = Nothing
            , nodeSignature    = Nothing
            , nodePresentBits  = 0
            }
          ext = extractionFromLists [n] []
          g = buildGraph False ext
      isFileNode g n `shouldBe` True

  describe "isConceptNode" $ do
    it "returns True for empty source file" $ do
      let n = testNodeWithFile "concept" CodeFile ""
      isConceptNode n `shouldBe` True

    it "returns False for real source file" $ do
      let n = testNodeWithFile "func" CodeFile "model.py"
      isConceptNode n `shouldBe` False

  describe "addEdges" $ do
    it "returns the same graph when given an empty list" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] [testEdge "a" "b"]
          g = buildGraph False ext
          g' = addEdges g []
      gNodes g' `shouldBe` gNodes g
      gEdges g' `shouldBe` gEdges g
      gAdjFwd g' `shouldBe` gAdjFwd g
      gAdjBack g' `shouldBe` gAdjBack g

    it "adds a single edge to the graph" $ do
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c"] [testEdge "a" "b"]
          g = buildGraph False ext
          newEdge = testEdge "b" "c"
          g' = addEdges g [newEdge]
      Map.size (gEdges g') `shouldBe` 2
      Set.member "c" (Map.findWithDefault Set.empty "b" (gAdjFwd g')) `shouldBe` True
      Set.member "b" (Map.findWithDefault Set.empty "c" (gAdjBack g')) `shouldBe` True

    it "adds multiple edges to the graph" $ do
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c", testNode "d"] [testEdge "a" "b"]
          g = buildGraph False ext
          edges = [testEdge "b" "c", testEdge "c" "d"]
          g' = addEdges g edges
      Map.size (gEdges g') `shouldBe` 3
      Set.member "c" (Map.findWithDefault Set.empty "b" (gAdjFwd g')) `shouldBe` True
      Set.member "d" (Map.findWithDefault Set.empty "c" (gAdjFwd g')) `shouldBe` True

    it "handles duplicate edges (same source and target)" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] []
          g = buildGraph False ext
          e1 = testEdge "a" "b"
          e2 = testEdgeWithConfidence "a" "b" (Confidence 0.5)
          g' = addEdges g [e1, e2]
      Map.size (gEdges g') `shouldBe` 1
      edgeConfidence (Map.findWithDefault (error "missing") ("a", "b") (gEdges g')) `shouldBe` Confidence 0.5

    it "drops dangling edges (source not in graph)" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] []
          g = buildGraph False ext
          danglingEdge = testEdge "z" "a"
          g' = addEdges g [danglingEdge]
      Map.size (gEdges g') `shouldBe` 0
      Map.size (gAdjFwd g') `shouldBe` 0

    it "drops dangling edges (target not in graph)" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] []
          g = buildGraph False ext
          danglingEdge = testEdge "a" "z"
          g' = addEdges g [danglingEdge]
      Map.size (gEdges g') `shouldBe` 0
      Map.size (gAdjFwd g') `shouldBe` 0

    it "updates adjacency correctly for directed graphs" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] []
          g = buildGraph True ext
          newEdge = testEdge "a" "b"
          g' = addEdges g [newEdge]
      Set.member "b" (Map.findWithDefault Set.empty "a" (gAdjFwd g')) `shouldBe` True
      Set.member "a" (Map.findWithDefault Set.empty "b" (gAdjBack g')) `shouldBe` True
      Set.size (Map.findWithDefault Set.empty "b" (gAdjFwd g')) `shouldBe` 0
      Set.size (Map.findWithDefault Set.empty "a" (gAdjBack g')) `shouldBe` 0

    it "updates adjacency bidirectionally for undirected graphs" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] []
          g = buildGraph False ext
          newEdge = testEdge "a" "b"
          g' = addEdges g [newEdge]
      Set.member "b" (Map.findWithDefault Set.empty "a" (gAdjFwd g')) `shouldBe` True
      Set.member "a" (Map.findWithDefault Set.empty "b" (gAdjFwd g')) `shouldBe` True
      Set.member "b" (Map.findWithDefault Set.empty "a" (gAdjBack g')) `shouldBe` True
      Set.member "a" (Map.findWithDefault Set.empty "b" (gAdjBack g')) `shouldBe` True

    it "preserves existing edges and nodes" $ do
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c"] [testEdge "a" "b"]
          g = buildGraph False ext
          newEdge = testEdge "b" "c"
          g' = addEdges g [newEdge]
      Map.size (gNodes g') `shouldBe` 3
      Map.member ("a", "b") (gEdges g') `shouldBe` True
      edgeRelation (Map.findWithDefault (error "missing") ("a", "b") (gEdges g')) `shouldBe` Calls

    it "preserves graph metadata (directed, embeddings)" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] []
          g = buildGraph True ext
          g' = addEdges g [testEdge "a" "b"]
      gDirected g' `shouldBe` True
      gEmbeddings g' `shouldBe` gEmbeddings g
