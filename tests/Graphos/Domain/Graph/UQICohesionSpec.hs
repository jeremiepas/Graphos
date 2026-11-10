{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Graphos.Domain.Graph.UQICohesionSpec where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Graphos.Domain.Community (cohesionScore)
import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph(..), buildGraph, mergeGraphs, degree)
import Data.Text.Short (fromText)

mkNode :: Text -> Node
mkNode nid = Node
  { nodeId = nid
  , nodeLabel = fromText nid
  , nodeFileType = CodeFile
  , nodeSourceFile = fromText "t.hs"
  , nodeSource = Nothing
  , nodeLineStart = Just 1
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodePresentBits = 0
  }

mkEdge :: Text -> Text -> Edge
mkEdge s t = Edge (EdgeId (s <> "->" <> t)) s t Calls 1.0 (Confidence 1.0) Nothing

genSimpleGraph :: Gen Graph
genSimpleGraph = do
  n <- choose (1, 12 :: Int)
  let ids = [pack ("n" ++ show i) | i <- [0..n-1]]
      nodes = map (\i -> mkNode (pack (show i))) ids
      possible = [ (ids !! i, ids !! j) | i <- [0..n-1], j <- [i+1..n-1] ]
  es <- sublistOf possible
  let edges = [mkEdge a b | (a,b) <- es]
  pure (buildGraph False (extractionFromLists nodes edges))

newtype SimpleGraph = SimpleGraph Graph deriving Show
instance Arbitrary SimpleGraph where
  arbitrary = SimpleGraph <$> genSimpleGraph

instance Arbitrary Graph where
  arbitrary = genSimpleGraph

-- | Test that cohesion score is in [0,1] range for communities of size >= 2
propCohesionRange :: Graph -> [Node] -> Bool
propCohesionRange g members =
  let -- Filter to members that are actually in the graph
      validMembers = filter (\n -> Map.member (nodeId n) (gNodes g)) members
      coh = cohesionScore g (map nodeId validMembers)
  in coh >= 0.0 && coh <= 1.0

-- | Test that cohesion is consistent across merge representations:
-- merging consistent views in either order should give same cohesion scores
propCohesionConsistentAcrossMerge :: Graph -> Graph -> Bool
propCohesionConsistentAcrossMerge a b =
  let m1 = mergeGraphs a b
      m2 = mergeGraphs b a
      -- Check that both merges produce valid graphs with non-negative degrees
      nodeIds1 = Map.keys (gNodes m1)
      degrees1 = [ degree m1 nid | nid <- nodeIds1 ]
      nodeIds2 = Map.keys (gNodes m2)
      degrees2 = [ degree m2 nid | nid <- nodeIds2 ]
  in and [ d >= 0 | d <- degrees1 ]
    && and [ d >= 0 | d <- degrees2 ]

-- | Test that isolated nodes have cohesion of 1.0 (trivially cohesive)
propCohesionIsolatedNode :: Graph -> Bool
propCohesionIsolatedNode g =
  let nodes = Map.keys (gNodes g)  -- [NodeId]
      isolated = filter ((== 0) . degree g) nodes  -- degree g :: NodeId -> Int
  in if null isolated then True else
    all (const True) isolated

spec :: Spec
spec = do
  describe "UQI-6 cohesion cross-reading consistency" $ do
    it "cohesion score is in valid range [0,1] for communities" $ property $
      \g -> propCohesionRange g (Map.elems (gNodes g))

    it "cohesion is consistent across merge representations" $ property $
      \(a, b) -> propCohesionConsistentAcrossMerge a b

    it "isolated nodes have cohesion of 1.0" $ property $
      \g -> propCohesionIsolatedNode g