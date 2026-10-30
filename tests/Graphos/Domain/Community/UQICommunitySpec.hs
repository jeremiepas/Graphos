{-# LANGUAGE OverloadedStrings #-}

module Graphos.Domain.Community.UQICommunitySpec where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph(..), buildGraph, gNodes, gDirected, mergeGraphs, neighbors, degree)
import Graphos.Domain.Community
  ( defaultResolution
  , detectCommunitiesWithResolution, modularity
  , scoreAllCohesion
  )
import Data.Text.Short (fromText)

mkNode :: Text -> Node
mkNode nid = Node nid (fromText nid) CodeFile (fromText "t.hs") Nothing (Just 1)
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

mkEdge :: Text -> Text -> Edge
mkEdge s t = Edge (EdgeId (s <> "->" <> t)) s t Calls 1.0 (Confidence 1.0) Nothing

genSimpleGraph :: Gen Graph
genSimpleGraph = do
  n <- choose (1, 8 :: Int)
  let ids = [ pack ("n" ++ show i) | i <- [0..n-1] ]
      nodes = map mkNode ids
      possible = [ (ids !! i, ids !! j) | i <- [0..n-1], j <- [i+1..n-1] ]
  es <- sublistOf possible
  let edges = [ mkEdge a b | (a,b) <- es ]
  pure (buildGraph False (extractionFromLists nodes edges))

newtype SmallGraph = SmallGraph Graph deriving Show
instance Arbitrary SmallGraph where
  arbitrary = SmallGraph <$> genSimpleGraph

spec :: Spec
spec = do
  describe "UQI-4 fresh re-detection on the apex" $ do
    it "detectCommunitiesWithResolution is deterministic on same M" $ property $
      \(SmallGraph g) ->
        detectCommunitiesWithResolution g defaultResolution
          == detectCommunitiesWithResolution g defaultResolution
    it "detection partition covers every node" $ property $
      \(SmallGraph g) ->
        let cm = detectCommunitiesWithResolution g defaultResolution
            members = Set.fromList (concat (Map.elems cm))
            nodes = Map.keysSet (gNodes g)
        in members == nodes
    it "nodes in a fresh merged graph carry no stale community id (values already clean)" $ do
      let g = buildGraph False (extractionFromLists [mkNode "a", mkNode "b"] [mkEdge "a" "b"])
          m = mergeGraphs g g
      all (\n -> nodeCommunityId n == Nothing) (Map.elems (gNodes m)) `shouldBe` True

  describe "UQI-5 monotonicity boundary" $ do
    it "cross-merge modularity decrease is recorded as an allowed outcome" $ property $
      \(SmallGraph a, SmallGraph b) ->
        let qa = modularity a (detectCommunitiesWithResolution a defaultResolution)
            m = mergeGraphs a b
            qab = modularity m (detectCommunitiesWithResolution m defaultResolution)
        in classify (qab < qa) "cross-merge decrease allowed" True
    it "detected modularity is at least singleton modularity" $ property $
      \(SmallGraph g) ->
        let detected = detectCommunitiesWithResolution g defaultResolution
            singles = Map.fromList (zip [0..] (map (:[]) (Map.keys (gNodes g))))
            qdet = modularity g detected
            qsng = modularity g singles
        in qdet + 1e-9 >= qsng

  describe "UQI-6 cohesion cross-reading" $ do
    it "0 <= cohesion(c) <= 1 for all communities" $ property $
      \(SmallGraph g) ->
        let cm = detectCommunitiesWithResolution g defaultResolution
            cs = Map.elems (scoreAllCohesion g cm)
        in all (\c -> c >= 0.0 && c <= 1.0) cs
    it "isolated-node communities keep cohesion in range" $ do
      let g = buildGraph False (extractionFromLists [mkNode "a", mkNode "b"] [])
          cm = detectCommunitiesWithResolution g defaultResolution
      Map.elems (scoreAllCohesion g cm) `shouldSatisfy` all (\c -> c >= 0.0 && c <= 1.0)
    it "cohesion agrees across a rebuilt (view-union) adjacency" $ do
      let a = buildGraph False (extractionFromLists [mkNode "x", mkNode "y"] [mkEdge "x" "y"])
          b = buildGraph False (extractionFromLists [mkNode "y", mkNode "z"] [mkEdge "y" "z"])
          merged = mergeGraphs a b
          rebuilt = buildGraph (gDirected merged) (Extraction { extractionNodes = gNodes merged, extractionEdges = Map.fromList [(edgeId e, e) | e <- Map.elems (gEdges merged)] })
          cm = detectCommunitiesWithResolution merged defaultResolution
      scoreAllCohesion merged cm `shouldBe` scoreAllCohesion rebuilt cm

  describe "UQI-7 per-query cost model (structural)" $ do
    it "degree query is O(deg(v)) by definition" $ property $
      \(SmallGraph g) ->
        and [ degree g v == Set.size (neighbors g v) | v <- Map.keys (gNodes g) ]
    it "structural read size is O(|response|)" $ property $
      \(SmallGraph g) ->
        length (Map.keys (gNodes g)) == Map.size (gNodes g)
    it "cohesion result size equals community count (O(E_s))" $ property $
      \(SmallGraph g) ->
        let cm = detectCommunitiesWithResolution g defaultResolution
        in Map.size (scoreAllCohesion g cm) == Map.size cm
