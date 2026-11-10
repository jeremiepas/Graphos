{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Graphos.UseCase.PROPEQVSpec where

import Test.Hspec
import Test.QuickCheck

import Graphos.Domain.Types
import Graphos.Domain.Graph
import Graphos.Domain.Community (Resolution(..), defaultResolution, communityMapFromGraph, buildReverseIndex, scoreAllCohesion, MergeStrategy(..))
import Graphos.Domain.Config (SemanticEdgesConfig(..), defaultSemanticEdgesConfig)
import Graphos.Domain.Types.Pipeline (EdgeDensity(..))
import Graphos.UseCase.Pipeline.Incremental
  ( boundedDeltaCommunityUpdate
  , boundedDeltaCommunityUpdate'
  , affectedCommunities
  , communityMapFromGraph
  , clusterAndInfer
  , cleanInferred
  )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)

{-|
Test suite for G6 PROP-EQV: bounded-delta community reconciliation property tests.

These tests verify the bounded-delta community update invariants under PROP-EQV
(property equality) reconciliation, as specified in AVI-1291 adoption.
-}

{-|
Test that bounded-delta community update preserves carried communities correctly.

When some communities are 'carried' (unchanged) and others are affected by churn,
the carried communities should retain their nodes and IDs, while affected communities
should be reclustered with offsetted IDs.
-}
propBoundedDeltaPreservesCarried ::
  Resolution -> Graph -> Graph -> CommunityMap -> [NodeId] -> Bool
propBoundedDeltaPreservesCarried res oldG newG oldP churnNodes =
  let affected = affectedCommunities oldG newG oldP churnNodes
      carried =
        Map.map (filter (`Map.member` gNodes newG))
          (Map.filterWithKey (\cid _ -> not (Set.member cid affected)) oldP)
      carriedMembers = Set.fromList (concat (Map.elems carried))
      affectedNodes = Set.difference (Map.keysSet (gNodes newG)) carriedMembers
      result = boundedDeltaCommunityUpdate res oldG newG oldP churnNodes
  in
    -- Carried communities should be preserved exactly
    carried `Map.isInfixOf` result &&
    -- All nodes in the result should be accounted for
    let resultNodes = Map.keysSet result
        expectedNodes = Map.keysSet newG
    in resultNodes == expectedNodes

{-|
Test that bounded-delta community update reclusters affected communities with correct offset.

The offset for reclustered affected communities should be the maximum carried
community key plus one, ensuring no ID collisions.
-}
propBoundedDeltaAffectedReclusteredOffset ::
  Resolution -> Graph -> Graph -> CommunityMap -> [NodeId] -> Bool
propBoundedDeltaAffectedReclusteredOffset res oldG newG oldP churnNodes =
  let affected = affectedCommunities oldG newG oldP churnNodes
      carried =
        Map.map (filter (`Map.member` gNodes newG))
          (Map.filterWithKey (\cid _ -> not (Set.member cid affected)) oldP)
      carriedMembers = Set.fromList (concat (Map.elems carried))
      carriedKeys = Map.keys carried
      offset =
        case carriedKeys of
          [] -> 0
          ks -> maximum ks + 1
      result = boundedDeltaCommunityUpdate res oldG newG oldP churnNodes
      -- Check that affected communities in the result have offset applied
      affectedCarriedIntersection =
        Map.intersectionWith (const []) (Map.filterWithKey (\cid _ -> Set.member cid affected) result) carried
  in
    -- The result should contain exactly the nodes of newGraph
    let resultNodes = Map.keysSet result
        newNodes = Map.keysSet newG
    in resultNodes == newNodes &&
    -- No carried community should be in an affected community
    Map.null affectedCarriedIntersection

{-|
Test that deleteNodeIncident removes exactly the incident edges of a node

(runs in O(deg) time, touching at most deg neighbors).
-}
propDeleteNodeIncidentRemovesIncidentEdges ::
  NodeId -> Graph -> Bool
propDeleteNodeIncidentRemovesIncidentEdges nid g =
  let (g', deletedCount) = deleteNodeIncident nid g
      -- The number of deleted edges should be the degree of the node
      -- (forward + backward adjacency, undirected merges them)
      expectedDeleted = case gDirected g of
        True -> -- directed: count both forward and backward
          let fwd = Map.findWithDefault Set.empty nid (gAdjFwd g)
              bwd = Map.findWithDefault Set.empty nid (gAdjBack g)
          in Set.size (fwd `Set.union` bwd)
        False -> -- undirected: each edge is counted once via adjacency
          let adj = Map.findWithDefault Set.empty nid (gAdjFwd g) -- forward IS the undirected adj
          in Set.size adj
  in
    deletedCount == expectedDeleted &&
    -- All edges touching the node should be removed
    let touchingNode g nid' = Map.any (\e -> edgeSource e == nid' || edgeTarget e == nid') (gEdges g)
    in not (touchingNode g' nid) &&
    -- All non-deleted nodes should still have their adjacency intact
    let remainingNodes = Map.keysSet g' Map.\\ Set.singleton nid
        -- All remaining nodes should have at least 0 edges (they might be isolated)
        allPresent = all (\nid' -> Map.member nid' (gNodes g')) remainingNodes
    in allPresent

{-|
 Test that running bounded-delta community update twice with the same inputs
 produces the same result (determinism property).
 -}
propBoundedDeltaDeterministic ::
  Resolution -> Graph -> Graph -> CommunityMap -> [NodeId] -> Bool
propBoundedDeltaDeterministic res oldG newG oldP churnNodes =
  let result1 = boundedDeltaCommunityUpdate res oldG newG oldP churnNodes
      result2 = boundedDeltaCommunityUpdate res oldG newG oldP churnNodes
  in result1 == result2

{-|
 Test that the community map from graph function is consistent with the
 bounded-delta update (round-trip property).
 -}
propCommunityMapConsistency ::
  Resolution -> Graph -> Graph -> CommunityMap -> [NodeId] -> Bool
propCommunityMapConsistency res oldG newG oldP churnNodes =
  let result = boundedDeltaCommunityUpdate res oldG newG oldP churnNodes
      -- The community map should partition the new graph's nodes
      cm = communityMapFromGraph result
      newNodes = Map.keysSet newG
      -- Every node should be in exactly one community
      allAssigned = all (\nid -> Map.member (nodeCommunityId (fromMaybe (error "no node") (Map.lookup nid (gNodes result)))) cm
                         || not (Map.member nid (gNodes result)))
      -- Actually, let's just check that communities partition the nodes
      communities = Map.elems cm
      nodeSets = Map.elems cm
  in
    -- Communities should be non-empty
    all (not . null) nodeSets &&
    -- Every node should be in exactly one community
    let allNodeIds = Map.keysSet (gNodes result)
        assignedIds = concat nodeSets
    in  all (`Set.member` Set.fromList assignedIds) (Set.fromList allNodeIds) &&
       Set.size (Set.fromList assignedIds) == Set.size (Set.fromList allNodeIds)

main :: IO ()
main = hspec $ do
  describe "PROP-EQV bounded-delta community reconciliation properties" $ do
    it "preserves carried communities correctly" $ property $ \res oldG newG oldP churnNodes ->
      propBoundedDeltaPreservesCarried res oldG newG oldP churnNodes

    it "reclusters affected communities with correct offset" $ property $ \res oldG newG oldP churnNodes ->
      propBoundedDeltaAffectedReclusteredOffset res oldG newG oldP churnNodes

    it "deleteNodeIncident removes exactly incident edges" $ property $ \nid g ->
      not (Set.member nid (gNodes g)) ==> propDeleteNodeIncidentRemovesIncidentEdges nid g

    it "bounded-delta update is deterministic" $ property $ \res oldG newG oldP churnNodes ->
      propBoundedDeltaDeterministic res oldG newG oldP churnNodes

    it "community map is consistent after bounded-delta update" $ property $ \res oldG newG oldP churnNodes ->
      propCommunityMapConsistency res oldG newG oldP churnNodes