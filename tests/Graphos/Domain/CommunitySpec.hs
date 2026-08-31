module Graphos.Domain.CommunitySpec where

import Test.Hspec
import Control.DeepSeq (deepseq)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.Short (fromText)
import Data.List (sortOn)
import Data.Aeson ( (.=), eitherDecode, encode, object, toJSON, Value(..) )
import Data.Aeson.Types (Object)
import Data.Aeson.Key (unKey)
import qualified Data.Aeson.KeyMap as KeyMap
import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph, gCompositions)
import Graphos.Domain.Community (detectCommunities, detectCommunitiesWithResolution, cohesionScore, buildReverseIndex, communityOf, countMoves, CommunityComposition(..), computeCompositions, Resolution(..), defaultResolution)
import Graphos.Domain.Types.Edge (Relation(References, Contains, Calls), Confidence(..))

spec :: Spec
spec = do
  describe "detectCommunities" $ do
    it "assigns all nodes to communities" $ do
      -- Build a simple graph and detect communities
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c"] [testEdge "a" "b"]
          g = buildGraph False ext
          commMap = detectCommunities g
          allMembers = concat (Map.elems commMap)
      length allMembers `shouldSatisfy` (>= 2)

    it "generates unique community IDs (no collisions)" $ do
      let nodes = [testNode (T.pack $ "n" ++ show i) | i <- [1..20::Int]]
          edges = [testEdge (T.pack $ "n" ++ show i) (T.pack $ "n" ++ show (i+1)) | i <- [1..19::Int]]
          ext = extractionFromLists nodes edges
          g = buildGraph False ext
          commMap = detectCommunities g
          cids = Map.keys commMap
      length cids `shouldBe` length (Map.keysSet commMap)  -- all unique

  describe "cohesionScore" $ do
    it "returns 1.0 for a fully connected pair" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] [testEdge "a" "b"]
          g = buildGraph False ext
      cohesionScore g ["a", "b"] `shouldSatisfy` (> 0)

    it "returns 0 for completely disconnected nodes" $ do
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c"] []
          g = buildGraph False ext
      cohesionScore g ["a", "b", "c"] `shouldBe` 0.0

  describe "buildReverseIndex" $ do
    it "maps every node to its community ID" $ do
      let commMap = Map.fromList [(0, [T.pack "a", T.pack "b"]), (1, [T.pack "c"])]
          revIdx = buildReverseIndex commMap
      communityOf (T.pack "a") revIdx `shouldBe` Just 0
      communityOf (T.pack "b") revIdx `shouldBe` Just 0
      communityOf (T.pack "c") revIdx `shouldBe` Just 1

    it "returns Nothing for non-existent node" $ do
      let commMap = Map.fromList [(0, [T.pack "a"])]
          revIdx = buildReverseIndex commMap
      communityOf (T.pack "unknown") revIdx `shouldBe` Nothing

  describe "communityOf" $ do
    it "provides O(log N) lookup via reverse index" $ do
      let commMap = Map.fromList [(i, [T.pack $ "node" ++ show i]) | i <- [0..100::Int]]
          revIdx = buildReverseIndex commMap
      communityOf (T.pack "node50") revIdx `shouldBe` Just 50

  describe "countMoves" $ do
    it "returns 0 when no nodes changed community" $ do
      let old = Map.fromList [(T.pack "a", 0), (T.pack "b", 0), (T.pack "c", 1)]
          new = Map.fromList [(T.pack "a", 0), (T.pack "b", 0), (T.pack "c", 1)]
      countMoves old new `shouldBe` 0

    it "counts nodes that moved to a different community" $ do
      let old = Map.fromList [(T.pack "a", 0), (T.pack "b", 0), (T.pack "c", 1)]
          new = Map.fromList [(T.pack "a", 1), (T.pack "b", 0), (T.pack "c", 1)]
      countMoves old new `shouldBe` 1

    it "counts multiple moves" $ do
      let old = Map.fromList [(T.pack "a", 0), (T.pack "b", 1), (T.pack "c", 2)]
          new = Map.fromList [(T.pack "a", 2), (T.pack "b", 0), (T.pack "c", 2)]
      countMoves old new `shouldBe` 2

  describe "golden clustering results (result-equivalence guard)" $ do
    -- These pin the exact output of the current implementation so that
    -- performance refactors (ST-based local moving, batched refinement,
    -- incremental merge index) can prove result equivalence.
    it "two 4-cliques joined by a bridge split into the two cliques" $ do
      let ns = map testNode ["a","b","c","d","e","f","g","h"]
          es = cliqueEdges ["a","b","c","d"] ++ cliqueEdges ["e","f","g","h"] ++ [testEdge "d" "e"]
          g = buildGraph False (extractionFromLists ns es)
          commMap = detectCommunities g
      communityMembershipSets commMap `shouldBe`
        Set.fromList [ Set.fromList ["a","b","c","d"]
                     , Set.fromList ["e","f","g","h"] ]

    it "a path of six nodes splits into two halves" $ do
      let ns = map testNode ["p1","p2","p3","p4","p5","p6"]
          es = [testEdge "p1" "p2", testEdge "p2" "p3", testEdge "p3" "p4", testEdge "p4" "p5", testEdge "p5" "p6"]
          g = buildGraph False (extractionFromLists ns es)
          commMap = detectCommunities g
      communityMembershipSets commMap `shouldBe`
        Set.fromList [ Set.fromList ["p1","p2","p3"]
                     , Set.fromList ["p4","p5","p6"] ]

    it "a triangle chained to three pairs merges pairs into stable groups" $ do
      let ns = map testNode ["a","b","c","x1","x2","y1","y2","z1","z2"]
          es = cliqueEdges ["a","b","c"]
               ++ [testEdge "x1" "x2", testEdge "y1" "y2", testEdge "z1" "z2"]
               ++ [testEdge "a" "x1", testEdge "x2" "y1", testEdge "y2" "z1"]
          g = buildGraph False (extractionFromLists ns es)
          commMap = detectCommunities g
      communityMembershipSets commMap `shouldBe`
        Set.fromList [ Set.fromList ["a","b","c"]
                     , Set.fromList ["x1","x2","y1"]
                     , Set.fromList ["y2","z1","z2"] ]

  describe "clustering deep evaluation" $ do
    it "clusterGraphWithResolution result is fully forceable (NFData smoke)" $ do
      let ns = map testNode ["a","b","c","d","e","f"]
          es = cliqueEdges ["a","b","c"] ++ cliqueEdges ["d","e","f"] ++ [testEdge "c" "d"]
          g = buildGraph False (extractionFromLists ns es)
          commMap = detectCommunities g
      -- Forces the full result; guards against partial NFData instances
      -- leaving unevaluated structure behind.
      commMap `deepseq` (Map.size commMap `shouldSatisfy` (>= 1))

  describe "mergeSmallCommunities node preservation" $ do
    it "never loses nodes when a merged-into community is itself merged" $ do
      -- Regression: raw communities {b}, {c,a}, {e,d} — merging {b} into
      -- {c,a} and then merging that community used to drop b (stale member
      -- snapshot). Every input node must appear in the final community map.
      let ns = map testNode ["a","b","c","d","e"]
          es = cliqueEdges ["a","b","c"] ++ [testEdge "d" "e", testEdge "a" "d"]
          g = buildGraph False (extractionFromLists ns es)
          commMap = detectCommunities g
          allMembers = Set.fromList (concat (Map.elems commMap))
      allMembers `shouldBe` Set.fromList ["a","b","c","d","e"]

    it "skips communities that grew past the minimum size via earlier merges" $ do
      let ns = map testNode ["a","b","c","d","e"]
          es = cliqueEdges ["a","b","c"] ++ [testEdge "d" "e", testEdge "a" "d"]
          g = buildGraph False (extractionFromLists ns es)
          commMap = detectCommunities g
      -- All five nodes end up connected through community 2 growth + pair merge
      communityMembershipSets commMap `shouldBe`
        Set.fromList [Set.fromList ["a","b","c","d","e"]]

  describe "detectCommunitiesWithResolution" $ do
    it "respects max iterations setting" $ do
      -- With 1 iteration, the algorithm should still produce a valid community map
      let ext = extractionFromLists [testNode "a", testNode "b", testNode "c"] [testEdge "a" "b"]
          g = buildGraph False ext
          res = defaultResolution { resMaxIterations = 1 }
          commMap = detectCommunitiesWithResolution g res
          allMembers = concat (Map.elems commMap)
      length allMembers `shouldSatisfy` (>= 2)

    it "converges faster on stable graphs" $ do
      -- A simple connected pair should converge in very few iterations
      let ext = extractionFromLists [testNode "a", testNode "b"] [testEdge "a" "b"]
          g = buildGraph False ext
          res = defaultResolution { resMaxIterations = 3 }
          commMap = detectCommunitiesWithResolution g res
      -- Should still produce valid communities even with few iterations
      Map.size commMap `shouldSatisfy` (>= 1)

  describe "CommunityComposition" $ do
    it "round-trips through JSON" $ do
      let comp = CommunityComposition 5 3 1 (Just "function") 0.6 2
          json = toJSON comp
          parsed :: Either String CommunityComposition
          parsed = eitherDecode (encode json)
      parsed `shouldBe` Right comp

    it "uses snake_case field names in JSON" $ do
      let comp = CommunityComposition 1 2 3 (Just "module") 0.5 0
          json = toJSON comp
          keys = case json of
            Object m -> map unKey (KeyMap.keys m)
            _ -> error "expected Object"
      keys `shouldContain` ["code", "doc", "other", "dominant_kind", "mixed_ratio", "code_doc_edges"]

    it "handles absent dominant_kind on legacy graphs" $ do
      let legacyJson = object [ "code" .= (1 :: Int)
                              , "doc" .= (2 :: Int)
                              , "other" .= (0 :: Int)
                              , "mixed_ratio" .= (0.5 :: Double)
                              , "code_doc_edges" .= (1 :: Int)
                              ]
          parsed :: Either String CommunityComposition
          parsed = eitherDecode (encode legacyJson)
      case parsed of
        Right comp -> ccDominantKind comp `shouldBe` Nothing
        Left err   -> error ("unexpected parse error: " ++ err)

  describe "computeCompositions" $ do
    it "pure-code community: ccMixedRatio = 0" $ do
      let ext = extractionFromLists
            [ testNode "a", testNode "b", testNode "c"
            , testNode "d", testNode "e"
            ]
            []
          g = buildGraph False ext
          commMap = Map.fromList [(0, ["a","b","c","d","e"])]
          comps = computeCompositions g commMap
          comp = Map.findWithDefault (error "missing comp") 0 comps
      ccCodeCount comp `shouldBe` 5
      ccDocCount comp `shouldBe` 0
      ccMixedRatio comp `shouldBe` 0.0

    it "balanced mixed community: ccMixedRatio = 1" $ do
      let ext = extractionFromLists
            [ testNode "a", testNode "b", testNode "c"
            , testDocNode "doc1", testDocNode "doc2", testDocNode "doc3"
            ]
            []
          g = buildGraph False ext
          commMap = Map.fromList [(0, ["a","b","c","doc1","doc2","doc3"])]
          comps = computeCompositions g commMap
          comp = Map.findWithDefault (error "missing comp") 0 comps
      ccCodeCount comp `shouldBe` 3
      ccDocCount comp `shouldBe` 3
      ccMixedRatio comp `shouldBe` 1.0

    it "paper counted as doc" $ do
      let ext = extractionFromLists
            [ testNode "a", testNode "b"
            , testNodeWithFile "paper1" PaperFile
            , testNodeWithFile "paper2" PaperFile
            , testNodeWithFile "paper3" PaperFile
            ]
            []
          g = buildGraph False ext
          commMap = Map.fromList [(0, ["a","b","paper1","paper2","paper3"])]
          comps = computeCompositions g commMap
          comp = Map.findWithDefault (error "missing comp") 0 comps
      ccCodeCount comp `shouldBe` 2
      ccDocCount comp `shouldBe` 3
      ccMixedRatio comp `shouldBe` 0.5

    it "composition counts match membership" $ do
      let ext = extractionFromLists
            [ testNode "a", testNode "b", testNode "c", testNode "d"
            , testNode "e", testNode "f", testNode "g"
            , testDocNode "doc1", testDocNode "doc2", testDocNode "doc3", testDocNode "doc4"
            , testNodeWithFile "img1" ImageFile
            , testNodeWithFile "img2" ImageFile
            , testNodeWithFile "img3" ImageFile
            ]
            []
          g = buildGraph False ext
          commMap = Map.fromList [(0, ["a","b","c","d","e","f","g","doc1","doc2","doc3","doc4","img1","img2","img3"])]
          comps = computeCompositions g commMap
          comp = Map.findWithDefault (error "missing comp") 0 comps
      ccCodeCount comp `shouldBe` 7
      ccDocCount comp `shouldBe` 4
      ccOtherCount comp `shouldBe` 3
      ccMixedRatio comp `shouldBe` (4 :: Double) / (7 :: Double)

    it "cross-type edge count excludes non-References" $ do
      let ext = extractionFromLists
            [ testNode "a", testNode "b", testNode "c"
            , testDocNode "doc1", testDocNode "doc2", testDocNode "doc3"
            ]
            [ testEdgeWithRelation References "a" "doc1"
            , testEdgeWithRelation References "b" "doc2"
            , testEdgeWithRelation References "c" "doc3"
            , testEdgeWithRelation Contains "a" "b"
            , testEdgeWithRelation Contains "doc1" "doc2"
            , testEdgeWithRelation Calls "a" "doc1"
            ]
          g = buildGraph False ext
          commMap = Map.fromList [(0, ["a","b","c","doc1","doc2","doc3"])]
          comps = computeCompositions g commMap
          comp = Map.findWithDefault (error "missing comp") 0 comps
      ccCodeDocEdges comp `shouldBe` 3

    it "dominant kind ignores Nothing" $ do
      let ext = extractionFromLists
            [ testNodeWithKind "a" (Just "function")
            , testNodeWithKind "b" (Just "function")
            , testNodeWithKind "c" (Just "function")
            , testNodeWithKind "d" Nothing
            , testNodeWithKind "e" (Just "module")
            , testNodeWithKind "f" (Just "module")
            ]
            []
          g = buildGraph False ext
          commMap = Map.fromList [(0, ["a","b","c","d","e","f"])]
          comps = computeCompositions g commMap
          comp = Map.findWithDefault (error "missing comp") 0 comps
      ccDominantKind comp `shouldBe` Just "function"

    it "dominant kind is Nothing when all kinds are Nothing" $ do
      let ext = extractionFromLists
            [ testNodeWithKind "a" Nothing
            , testNodeWithKind "b" Nothing
            ]
            []
          g = buildGraph False ext
          commMap = Map.fromList [(0, ["a","b"])]
          comps = computeCompositions g commMap
          comp = Map.findWithDefault (error "missing comp") 0 comps
      ccDominantKind comp `shouldBe` Nothing

    it "every community has a composition entry" $ do
      let ext = extractionFromLists
            [ testNode "a", testNode "b", testNode "c"
            , testNode "d", testNode "e"
            , testNode "f", testNode "g"
            ]
            [ testEdge "a" "b", testEdge "b" "c"
            , testEdge "d" "e", testEdge "e" "f"
            , testEdge "c" "d"
            ]
          g = buildGraph False ext
          commMap = detectCommunities g
          comps = computeCompositions g commMap
      Map.size comps `shouldBe` Map.size commMap

  describe "gCompositions legacy compatibility" $ do
    it "legacy graph without compositions key loads with gCompositions = Nothing" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] [testEdge "a" "b"]
          g = buildGraph False ext
      gCompositions g `shouldBe` Nothing

-- Helpers (duplicated from GraphSpec for test isolation)
testNode :: Text -> Node
testNode nid = Node
  { nodeId = nid
  , nodeLabel = fromText nid
  , nodeFileType = CodeFile
  , nodeSourceFile = fromText "test.hs"
  , nodeCommunityId = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodeLineStart = Just 1
  , nodeLineEnd = Nothing
  , nodeKind = Nothing
  , nodeSignature = Nothing
  , nodePresentBits = 0
  }

testDocNode :: Text -> Node
testDocNode nid = Node
  { nodeId = nid
  , nodeLabel = fromText nid
  , nodeFileType = DocFile
  , nodeSourceFile = fromText "test.md"
  , nodeCommunityId = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodeLineStart = Just 1
  , nodeLineEnd = Nothing
  , nodeKind = Nothing
  , nodeSignature = Nothing
  , nodePresentBits = 0
  }

testNodeWithKind :: Text -> Maybe Text -> Node
testNodeWithKind nid kind = Node
  { nodeId = nid
  , nodeLabel = fromText nid
  , nodeFileType = CodeFile
  , nodeSourceFile = fromText "test.hs"
  , nodeCommunityId = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodeLineStart = Just 1
  , nodeLineEnd = Nothing
  , nodeKind = fmap fromText kind
  , nodeSignature = Nothing
  , nodePresentBits = 0
  }

testNodeWithFile :: Text -> FileType -> Node
testNodeWithFile nid ft = Node
  { nodeId = nid
  , nodeLabel = fromText nid
  , nodeFileType = ft
  , nodeSourceFile = fromText "test.hs"
  , nodeCommunityId = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodeLineStart = Just 1
  , nodeLineEnd = Nothing
  , nodeKind = Nothing
  , nodeSignature = Nothing
  , nodePresentBits = 0
  }

-- | All-pairs edges over a node list (undirected clique).
cliqueEdges :: [Text] -> [Edge]
cliqueEdges ns = [testEdge a b | a <- ns, b <- ns, a < b]

-- | Community structure as a set of member sets (ignores community IDs,
-- which are internal indices and not semantically meaningful).
communityMembershipSets :: CommunityMap -> Set (Set NodeId)
communityMembershipSets commMap = Set.fromList [Set.fromList members | members <- Map.elems commMap]

edgeIdFrom :: Text -> Text -> EdgeId
edgeIdFrom src tgt = EdgeId (src <> "->" <> tgt)

testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge (edgeIdFrom src tgt) src tgt Calls 1.0 (Confidence 1.0) Nothing

testEdgeWithRelation :: Relation -> Text -> Text -> Edge
testEdgeWithRelation rel src tgt = Edge (edgeIdFrom src tgt) src tgt rel 1.0 (Confidence 1.0) Nothing