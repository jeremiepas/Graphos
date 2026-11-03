{-# LANGUAGE OverloadedStrings #-}

module Graphos.Domain.Graph.UQISupportSpec where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph(..), buildGraph, mergeGraphs, gNodes, gEdges, degree)
import Data.Text.Short (fromText)

mkNode :: Text -> Node
mkNode nid = Node nid (fromText nid) CodeFile (fromText "t.hs") Nothing (Just 1)
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

mkEdge :: Text -> Text -> Edge
mkEdge s t = Edge (EdgeId (s <> "->" <> t)) s t Calls 1.0 (Confidence 1.0) Nothing

genSimpleGraph :: Gen Graph
genSimpleGraph = do
  n <- choose (1, 12 :: Int)
  let ids = [ pack ("n" ++ show i) | i <- [0..n-1] ]
      nodes = map mkNode ids
      possible = [ (ids !! i, ids !! j) | i <- [0..n-1], j <- [i+1..n-1] ]
  es <- sublistOf possible
  let edges = [ mkEdge a b | (a,b) <- es ]
  pure (buildGraph False (extractionFromLists nodes edges))

newtype SimpleGraph = SimpleGraph Graph deriving Show
instance Arbitrary SimpleGraph where
  arbitrary = SimpleGraph <$> genSimpleGraph

spec :: Spec
spec = do
  describe "UQI-3 support-graph grounding" $ do
    it "merged support vertex set equals the colimit node union" $ property $
      \(SimpleGraph a, SimpleGraph b) ->
        let m = mergeGraphs a b
        in Map.keysSet (gNodes m) == Set.union (Map.keysSet (gNodes a)) (Map.keysSet (gNodes b))

    it "|V(G_s)| equals Map.size (gNodes M) (colimit node count)" $ property $
      \(SimpleGraph a, SimpleGraph b) ->
        let m = mergeGraphs a b
            ua = Set.union (Map.keysSet (gNodes a)) (Map.keysSet (gNodes b))
        in Map.size (gNodes m) == Set.size ua

    it "dangling edges are dropped: every edge endpoint lies in gNodes M" $ property $
      \(SimpleGraph a, SimpleGraph b) ->
        let m = mergeGraphs a b
        in all (\e -> Map.member (edgeSource e) (gNodes m) && Map.member (edgeTarget e) (gNodes m))
               (Map.elems (gEdges m))

    it "m == (sum_v degree M v) / 2 on the merged support graph" $ property $
      \(SimpleGraph a, SimpleGraph b) ->
        let m = mergeGraphs a b
            degSum = sum [ degree m v | v <- Map.keys (gNodes m) ]
        in degSum `mod` 2 == 0 && (degSum `div` 2) == Map.size (gEdges m)

    it "degree is non-negative and bounded by the node count" $ property $
      \(SimpleGraph g) ->
        all (\v -> degree g v >= 0 && degree g v <= Map.size (gNodes g)) (Map.keys (gNodes g))
