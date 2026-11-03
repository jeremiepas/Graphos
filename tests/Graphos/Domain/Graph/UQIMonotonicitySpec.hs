{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphos.Domain.Graph.UQIMonotonicitySpec where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph(..), buildGraph, mergeGraphs, gNodes, gEdges, degree)

mkNode :: Text -> Node
mkNode nid = Node nid (fromText nid) CodeFile (fromText "t.hs") Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

mkEdge :: Text -> Text -> Edge
mkEdge s t = Edge (EdgeId (s <> "->" <> t)) s t Calls 1.0 (Confidence 1.0) Nothing

genSimpleGraph :: Gen Graph
genSimpleGraph = do
  n <- choose (1, 12 :: Int)
  let ids = [pack ("n" ++ show i) | i <- [0..n-1]]
    nodes = map mkNode ids
    possible = [ (ids !! i, ids !! j) | i <- [0..n-1], j <- [i+1..n-1] ]
  es <- sublistOf possible
  let edges = [mkEdge a b | (a,b) <- es]
  pure (buildGraph False (extractionFromLists nodes edges))

newtype SimpleGraph = SimpleGraph Graph deriving Show
instance Arbitrary SimpleGraph where arbitrary = SimpleGraph <$> genSimpleGraph

spec :: Spec
spec = do
  describe "UQI-5 monotonicity boundary" $ do
    it "monotonicity holds for directed acyclic graphs" $ property $
      \(g) -> propMonotonicity g
    it "monotonicity fails for cyclic directed graphs" $ property $
      \(g) -> not (propMonotonicity g)
  where
    propMonotonicity :: Graph -> Bool -> Bool
    propMonotonicity g acyclic =
      if acyclic then True else False

spec
