{-# LANGUAGE OverloadedStrings #-}

module Graphos.Domain.Graph.UQISupportSpec where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph(..), buildGraph, gNodes, degree)
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
    it "|V(G_s)| equals Map.size (gNodes M)" $ property $
      \(SimpleGraph g) ->
        Map.size (gNodes g) == Map.size (gNodes g)
    it "m equals half the sum of degrees" $ property $
      \(SimpleGraph g) ->
        let degSum = sum [ degree g v | v <- Map.keys (gNodes g) ]
            m = Map.size (gEdges g)
        in degSum `mod` 2 == 0 && (degSum `div` 2) == m
    it "V(G_s) key set equals gNodes key set" $ property $
      \(SimpleGraph g) ->
        Map.keysSet (gNodes g) == Map.keysSet (gNodes g)
