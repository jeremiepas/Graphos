{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphos.Domain.Graph.UQIFreshRedetectionSpec where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort, sortOn)
import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph(..), buildGraph, mergeGraphs)
import Graphos.Domain.Community (detectCommunitiesWithResolution, Resolution(..), MergeStrategy(..))
import Data.Text.Short (fromText)

-- | Canonical form for community map comparison: sort communities by min NodeId,
-- sort members within each community.
canonCommMap :: Map.Map CommunityId [NodeId] -> Map.Map Int [NodeId]
canonCommMap m =
  let ordered = sortOn minimum (Map.elems m)
  in Map.fromList (zip [0 ..] (map sort ordered))

mkNode :: Text -> Node
mkNode nid = Node nid (fromText nid) CodeFile (fromText "t.hs") Nothing (Just 1)
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

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
instance Arbitrary SimpleGraph where
  arbitrary = SimpleGraph <$> genSimpleGraph

spec :: Spec
spec = do
  describe "UQI-4 fresh re-detection on merge" $ do
    it "no source-view community id survives a merge - community IDs are recomputed" $ property $
      \(SimpleGraph a, SimpleGraph b) ->
        let m = mergeGraphs a b
        in all (\n -> nodeCommunityId n == Nothing) (Map.elems (gNodes m))

    it "fresh community detection covers all nodes on merged graph" $ property $
      \(SimpleGraph a, SimpleGraph b) ->
        let m = mergeGraphs a b
            -- Use the default resolution for detection
            comms = detectCommunitiesWithResolution m (Resolution 1.0 1 MergeToNeighbor 1)
            members = Set.fromList (concat (Map.elems comms))
            nodeSet = Map.keysSet (gNodes m)
        in members `shouldSatisfy` (\s -> Set.isSubsetOf s nodeSet && Set.size s == Set.size nodeSet)

    it "community IDs are not inherited from source views on merge" $ property $
      \(SimpleGraph a, SimpleGraph b) ->
        let m = mergeGraphs a b
            -- Check that no node has a community ID inherited from either view
            allFreed = all (\n -> nodeCommunityId n == Nothing) (Map.elems (gNodes m))
        in allFreed

  describe "UQI-4b community re-detection determinism" $ do
    it "detectCommunitiesWithResolution is deterministic on same graph" $ property $
      \(SimpleGraph g) ->
        let cm1 = detectCommunitiesWithResolution g (Resolution 1.0 1 MergeToNeighbor 1)
            cm2 = detectCommunitiesWithResolution g (Resolution 1.0 1 MergeToNeighbor 1)
        in canonCommMap cm1 == canonCommMap cm2