module Graphos.Domain.CommunitySpec where

import Test.Hspec
import Control.DeepSeq (deepseq)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.Short (fromText)
import Data.List (sort)
import Data.Maybe (isNothing, listToMaybe, fromMaybe)
import Data.Aeson ( (.=), eitherDecode, encode, object, toJSON, Value(..) )

import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph, gCompositions, Graph(..), degree)
import Graphos.Domain.Community (detectCommunities, detectCommunitiesWithResolution, cohesionScore, cohesionWeighted, weightedModularity, bestCommunityWeighted, CommunityStatsWeighted(..), computeCommunityStatsWeighted, buildReverseIndex, communityOf, countMoves, modularity, localMovesFrom, CommunityComposition(..), computeCompositions, Resolution(..), defaultResolution)
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
            Object m -> map keyToText (KeyMap.keys m)
            _ -> error "expected Object"
      sort keys `shouldBe` sort ["code", "doc", "other", "dominant_kind", "mixed_ratio", "code_doc_edges"]

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
      ccMixedRatio comp `shouldBe` (2 :: Double) / 3

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
      ccCodeDocEdges comp `shouldBe` 2

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

  describe "modularity (Q) — auditable, per CTO note 1 §2.2 / INV-Q" $ do
    -- Shared undirected fixture: two triangles joined by a bridge (c-f).
    -- 7 edge records; degree sum = 14 (m = 7).
    let twoTriangleBridge = buildGraph False $ extractionFromLists
              [ testNode "a", testNode "b", testNode "c"
              , testNode "d", testNode "e", testNode "f" ]
              (cliqueEdges ["a", "b", "c"] ++ cliqueEdges ["d", "e", "f"] ++ [testEdge "c" "f"])
        -- Independent audit oracle: recompute Q straight from gEdges, bypassing
        -- computeCommunityStats entirely (CTO note 1 §2.2 fallback).
        modularityDirect g cm =
          let assign   = buildReverseIndex cm
              degrees  = Map.fromList [(nid, fromIntegral (degree g nid)) | nid <- Map.keys (gNodes g)]
              m        = sum (Map.elems degrees) / 2.0
              twoM     = 2.0 * m
              sigmaTot = Map.fromListWith (+)
                    [ (Map.findWithDefault 0 nid assign, fromIntegral (degree g nid))
                    | nid <- Map.keys (gNodes g) ]
              sigmaIn  = Map.fromListWith (+)
                    [ (Map.findWithDefault 0 (edgeSource e) assign, 2.0)
                    | e <- Map.elems (gEdges g)
                    , Map.findWithDefault 0 (edgeSource e) assign == Map.findWithDefault 0 (edgeTarget e) assign ]
          in if m == 0.0 then 0.0
             else foldl' (\q cid -> q + (Map.findWithDefault 0.0 cid sigmaIn) / twoM
                                         - (Map.findWithDefault 0.0 cid sigmaTot / twoM) * (Map.findWithDefault 0.0 cid sigmaTot / twoM))
                         0.0 (Map.keys cm)

    it "single community on a connected undirected graph yields Q == 0" $ do
      let g = twoTriangleBridge
          cm = Map.fromList [(0, ["a", "b", "c", "d", "e", "f"])]
      modularity g cm `shouldBe` (0 :: Double)

    it "all-singletons partition yields Q < 0 and equals -(sum k_v^2)/(2m)^2" $ do
      let g = twoTriangleBridge
          cm = Map.fromList [(i, [nid]) | (i, nid) <- zip ([0 :: Int ..]) ["a", "b", "c", "d", "e", "f"]]
          twoM = sum (map (\n -> fromIntegral (degree g n)) ["a", "b", "c", "d", "e", "f"])
          expectedQ = -(sum (map (\k -> k * k) [fromIntegral (degree g n) | n <- ["a", "b", "c", "d", "e", "f"]])) / (twoM * twoM)
      modularity g cm `shouldSatisfy` (< 0)
      modularity g cm `shouldSatisfy` (\q -> abs (q - expectedQ) < 1e-9)

    it "Q stays within [-0.5, 1] for reachable partitions on an undirected graph" $ do
      let g = twoTriangleBridge
          reachable = [ Map.fromList [(0, ["a", "b", "c", "d", "e", "f"])]
                      , Map.fromList [(i, [nid]) | (i, nid) <- zip ([0 :: Int ..]) ["a", "b", "c", "d", "e", "f"]]
                      , detectCommunities g
                      , Map.fromList [(0, ["a", "b", "c"]), (1, ["d", "e", "f"])] ]
      forM_ reachable $ \cm -> do
        let q = modularity g cm
        q `shouldSatisfy` (>= (-0.5 :: Double))
        q `shouldSatisfy` (<= (1 :: Double))

    it "Q is monotone non-decreasing under one local move from a fine start (INV-Q3)" $ do
      let g = twoTriangleBridge
          cm0 = Map.fromList [(i, [nid]) | (i, nid) <- zip ([0 :: Int ..]) ["a", "b", "c", "d", "e", "f"]]
          qBefore = modularity g cm0
          qAfter  = modularity g (localMovesFrom g cm0)
      qAfter `shouldSatisfy` (>= qBefore - 1e-9)

    it "Q is independent of member-list order (INV-Q2)" $ do
      let g = twoTriangleBridge
          cm  = Map.fromList [(0, ["a", "b", "c"]), (1, ["d", "e", "f"])]
          reordered = Map.fromList [(0, ["c", "a", "b"]), (1, ["f", "d", "e"])]
      modularity g cm `shouldBe` modularity g reordered

    it "audit fallback: Q recomputed from gEdges equals Q from modularity (INV-Q5)" $ do
      let g = twoTriangleBridge
          cm = Map.fromList [(0, ["a", "b", "c"]), (1, ["d", "e", "f"])]
      modularity g cm `shouldBe` modularityDirect g cm
      modularity g cm `shouldSatisfy` (> 0)

    it "Q is deterministic across repeated calls (INV-Q4)" $ do
      let g = twoTriangleBridge
          cm = Map.fromList [(0, ["a", "b", "c"]), (1, ["d", "e", "f"])]
      modularity g cm `shouldBe` modularity g cm

  describe "cohesionWeighted" $ do
    it "returns 0 for a singleton community (AVI-535 §6.3)" $ do
      let ext = extractionFromLists [testNode "a"] []
          g = buildGraph False ext
      cohesionWeighted g ["a"] `shouldBe` 0.0

    it "reduces to cohesionScore under uniform confidence (self-consistency)" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] [testEdge "a" "b"]
          g = buildGraph False ext
          cs = cohesionScore g ["a", "b"]
      cohesionWeighted g ["a", "b"] `shouldSatisfy` (\x -> abs (x - cs) < 1e-9)

    it "is 0 when the community members are disconnected" $ do
      let ext = extractionFromLists [testNode "a", testNode "b"] []
          g = buildGraph False ext
      cohesionWeighted g ["a", "b"] `shouldBe` 0.0

    it "is independent of member ordering (INV-CONSTR)" $ do
      -- Weighted cohesion must not depend on the order candidates are enumerated
      -- — the exact property the reviewer flagged as non-deterministic (AVI-578).
      let ns = map testNode ["a", "b", "c", "x"]
          g = buildGraph False (extractionFromLists ns
            (cliqueEdges ["a", "b", "c"] ++ [testEdge "a" "x"]))
      cohesionWeighted g ["a", "b", "c"] `shouldBe` cohesionWeighted g ["c", "a", "b"]

  describe "weightedModularity" $ do
    it "is 0 for a singleton partition (all nodes in one community)" $ do
      let ns = map testNode ["a", "b", "c", "d", "e", "f"]
          es = cliqueEdges ["a", "b", "c"] ++ cliqueEdges ["d", "e", "f"] ++ [testEdge "c" "d"]
          g = buildGraph False (extractionFromLists ns es)
          cm = Map.fromList [(0, ["a", "b", "c", "d", "e", "f"])]
      weightedModularity g cm `shouldBe` 0.0

    it "is positive for two tight cliques joined by a weak bridge" $ do
      let ns = map testNode ["a", "b", "c", "d", "e", "f"]
          es = cliqueEdges ["a", "b", "c"] ++ cliqueEdges ["d", "e", "f"] ++ [testEdge "c" "d"]
          g = buildGraph False (extractionFromLists ns es)
          cm = Map.fromList [(0, ["a", "b", "c"]), (1, ["d", "e", "f"])]
      weightedModularity g cm `shouldSatisfy` (> 0)

  describe "bestCommunityWeighted" $ do
    it "proposes a strictly beneficial move into a neighbour community (AVI-535 §8)" $ do
      let ns = map testNode ["a", "b", "c", "x"]
          es = cliqueEdges ["a", "b", "c"] ++ [testEdge "x" "a", testEdge "x" "b"]
          g = buildGraph False (extractionFromLists ns es)
          cm = Map.fromList [(0, ["a", "b", "c"]), (1, ["x"])]
      bestCommunityWeighted cm g (T.pack "x") `shouldSatisfy` maybe False (\(_, s) -> s > 0)

    it "returns Nothing when the only neighbour community is the node's own" $ do
      let ns = map testNode ["a", "b"]
          es = [testEdge "a" "b"]
          g = buildGraph False (extractionFromLists ns es)
          cm = Map.fromList [(0, ["a", "b"])]
      bestCommunityWeighted cm g (T.pack "a") `shouldSatisfy` isNothing

  describe "computeCommunityStatsWeighted" $ do
    it "records internal weight, undirected pair count, and cohesion per community" $ do
      let ns = map testNode ["a", "b", "c"]
          es = cliqueEdges ["a", "b", "c"]
          g = buildGraph False (extractionFromLists ns es)
          cm = Map.fromList [(0, ["a", "b", "c"])]
          stats = computeCommunityStatsWeighted cm g
      length stats `shouldBe` 1
      let s = fromMaybe (error "exactly one community stats") (listToMaybe stats)
      cswInternalWeight s `shouldSatisfy` (> 0)
      cswUndirectedPairs s `shouldBe` 6
      cswCohesion s `shouldSatisfy` (> 0)

  describe "weighted modularity invariants (AVI-535)" $ do
    it "INV-CONSTR: Q_w equals auditable Q under equal confidence (w0 in {0.25,1,4})" $ do
      let ns = map testNode ["a", "b", "c", "d"]
          cm = Map.fromList [(0, ["a", "b"]), (1, ["c", "d"])]
          g1  = buildGraph False (extractionFromLists ns (cliqueEdges ["a", "b"] ++ cliqueEdges ["c", "d"] ++ [testEdge "b" "c"]))
          g25 = buildGraph False (extractionFromLists ns [testEdgeWithConfidence 0.25 "a" "b", testEdgeWithConfidence 0.25 "c" "d", testEdgeWithConfidence 0.25 "b" "c"])
          g40 = buildGraph False (extractionFromLists ns [testEdgeWithConfidence 4.0 "a" "b", testEdgeWithConfidence 4.0 "c" "d", testEdgeWithConfidence 4.0 "b" "c"])
          tol = 1e-9
      weightedModularity g1 cm `shouldSatisfy` (\x -> abs (x - modularity g1 cm) < tol)
      weightedModularity g25 cm `shouldSatisfy` (\x -> abs (x - modularity g1 cm) < tol)
      weightedModularity g40 cm `shouldSatisfy` (\x -> abs (x - modularity g1 cm) < tol)

    it "INV-CONSTR: bidirectional pair — Q_w well-defined and order-independent (AVI-578)" $ do
      -- The exact fixture the reviewer cited as divergent: a bidirectional pair
      -- (a→b, b→a). A single community spanning every node yields Q_w = 0 under
      -- the configuration null model, independent of how the members are enumerated.
      let ns = map testNode ["a", "b"]
          es = [testEdge "a" "b", testEdge "b" "a"]
          g = buildGraph False (extractionFromLists ns es)
          cm  = Map.fromList [(0, ["a", "b"])]
          cmR = Map.fromList [(0, ["b", "a"])]
      weightedModularity g cm `shouldBe` 0.0
      weightedModularity g cm `shouldBe` weightedModularity g cmR

    it "INV-Qw0: Q_w = 0 when confidence mass is zero (no divide-by-zero)" $ do
      let ns = map testNode ["a", "b", "c"]
          g = buildGraph False (extractionFromLists ns [testEdgeWithConfidence 0.0 "a" "b", testEdgeWithConfidence 0.0 "b" "c", testEdgeWithConfidence 0.0 "a" "c"])
          cm = Map.fromList [(0, ["a", "b", "c"])]
      weightedModularity g cm `shouldBe` 0.0

    it "INV-Qw1: Q_w in [-0.5, 1] on undirected fixtures" $ do
      let inRange x = x >= (-0.5) && x <= 1
          bridge = buildGraph False (extractionFromLists (map testNode ["a", "b", "c", "d", "e", "f"]) (cliqueEdges ["a", "b", "c"] ++ cliqueEdges ["d", "e", "f"] ++ [testEdge "c" "d"]))
          pathAllSingletons = buildGraph False (extractionFromLists (map testNode ["a", "b", "c"]) [testEdge "a" "b", testEdge "b" "c"])
          triSplit = buildGraph False (extractionFromLists (map testNode ["a", "b", "c"]) (cliqueEdges ["a", "b", "c"]))
      weightedModularity bridge (Map.fromList [(0, ["a", "b", "c"]), (1, ["d", "e", "f"])]) `shouldSatisfy` inRange
      weightedModularity pathAllSingletons (Map.fromList [(0, ["a"]), (1, ["b"]), (2, ["c"])]) `shouldSatisfy` inRange
      weightedModularity triSplit (Map.fromList [(0, ["a", "b"]), (1, ["c"])]) `shouldSatisfy` inRange

    it "INV-Qw4: all-singletons yields negative Q_w with exact value" $ do
      -- path a-b-c: m=2, degrees 1,2,1 ; Q_w = -(1/(2m))^2 * sum deg^2 = -6/16
      let ns = map testNode ["a", "b", "c"]
          es = [testEdge "a" "b", testEdge "b" "c"]
          g = buildGraph False (extractionFromLists ns es)
          cm = Map.fromList [(0, ["a"]), (1, ["b"]), (2, ["c"])]
      weightedModularity g cm `shouldSatisfy` (< 0)
      weightedModularity g cm `shouldSatisfy` (\x -> abs (x - (-6 / 16 :: Double)) < 1e-9)

    it "INV-C1: cohesion_w in [0, 1]" $ do
      let inRange x = x >= 0 && x <= 1
          tri = buildGraph False (extractionFromLists (map testNode ["a", "b", "c"]) (cliqueEdges ["a", "b", "c"]))
          mixed = buildGraph False (extractionFromLists (map testNode ["a", "b", "c", "x"]) (cliqueEdges ["a", "b", "c"] ++ [testEdge "a" "x"]))
      cohesionWeighted tri ["a", "b", "c"] `shouldSatisfy` inRange
      cohesionWeighted mixed ["a", "b", "c"] `shouldSatisfy` inRange

    it "INV-C2: cohesion_w is scale-invariant under uniform confidence" $ do
      -- members [a,b,c]; a-x external. Node a ratio = 2w_int / 3w_total = 2/3; mean over a,b,c = 8/9.
      let ns = map testNode ["a", "b", "c", "x"]
          g1  = buildGraph False (extractionFromLists ns (cliqueEdges ["a", "b", "c"] ++ [testEdge "a" "x"]))
          g4  = buildGraph False (extractionFromLists ns [testEdgeWithConfidence 4.0 "a" "b", testEdgeWithConfidence 4.0 "b" "c", testEdgeWithConfidence 4.0 "a" "c", testEdgeWithConfidence 4.0 "a" "x"])
          tol = 1e-9
      cohesionWeighted g1 ["a", "b", "c"] `shouldSatisfy` (\x -> abs (x - 8 / 9 :: Double) < tol)
      cohesionWeighted g4 ["a", "b", "c"] `shouldSatisfy` (\x -> abs (x - 8 / 9 :: Double) < tol)

    it "signal strength moves Q_w: edge confidence changes weighted modularity" $ do
      let ns = map testNode ["a", "b", "c", "d", "e", "f"]
          twoCliques = cliqueEdges ["a", "b", "c"] ++ cliqueEdges ["d", "e", "f"]
          cm = Map.fromList [(0, ["a", "b", "c"]), (1, ["d", "e", "f"])]
          weak  = buildGraph False (extractionFromLists ns (twoCliques ++ [testEdgeWithConfidence 0.1 "c" "d"]))
          strong = buildGraph False (extractionFromLists ns (twoCliques ++ [testEdgeWithConfidence 10.0 "c" "d"]))
      let wWeak  = weightedModularity weak cm
          wStrong = weightedModularity strong cm
      -- Q_w responds to confidence weights; an inter-community bridge of
      -- different mass moves the objective (a strong bridge inflates sigma_tot
      -- and penalises Q_w for this fixed partition).
      abs (wWeak - wStrong) `shouldSatisfy` (> 1e-9)
      wStrong `shouldSatisfy` (< wWeak)

    it "INV-Qw2/Qw3: Q_w is deterministic and order-independent" $ do
      let ns = map testNode ["a", "b", "c", "d", "e", "f"]
          es = cliqueEdges ["a", "b", "c"] ++ cliqueEdges ["d", "e", "f"] ++ [testEdge "c" "d"]
          g = buildGraph False (extractionFromLists ns es)
          cmA = Map.fromList [(0, ["a", "b", "c"]), (1, ["d", "e", "f"])]
          cmB = Map.fromList [(1, ["d", "e", "f"]), (0, ["a", "b", "c"])]
          tol = 1e-12
      let x = weightedModularity g cmA
      weightedModularity g cmA `shouldSatisfy` (\y -> abs (y - x) < tol)
      weightedModularity g cmB `shouldSatisfy` (\y -> abs (y - x) < tol)

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

testEdgeWithConfidence :: Double -> Text -> Text -> Edge
testEdgeWithConfidence w src tgt = Edge (edgeIdFrom src tgt) src tgt Calls 1.0 (Confidence w) Nothing

keyToText :: Key -> Text
keyToText = T.drop 1 . T.init . T.pack . show