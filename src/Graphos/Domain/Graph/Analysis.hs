-- | Advanced graph analysis — structural properties and centrality.
-- Pure functions over the domain types.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Domain.Graph.Analysis
  ( godNodes
  , articulationPoints
  , biconnectedComponents
  , dominators
  , edgeBetweenness
  ) where

import Data.List (sortOn, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Graph.Inductive.Graph (labNodes)
import qualified Data.Graph.Inductive.Graph as FGL
import Data.Graph.Inductive.Query.ArtPoint (ap)
import Data.Graph.Inductive.Query.BCC (bcc)
import Data.Graph.Inductive.Query.Dominators (dom)
import Data.Graph.Inductive.Query.BFS (esp)

import Graphos.Domain.Types
import Graphos.Domain.Graph.Core (Graph(..), isFileNode, isConceptNode)
import Graphos.Domain.Graph.FGL (toFGL, FGLGraph)

-- ───────────────────────────────────────────────
-- Internal: Graph -> FGL conversion
-- ───────────────────────────────────────────────

-- | Convert a Graphos Graph to an fgl Gr for algorithm use
toFGL' :: Graph -> FGLGraph
toFGL' g = toFGL (gNodes g) (gEdges g)

-- | Build a node ID lookup: fgl Int -> Graphos NodeId
nidLookup :: FGLGraph -> Map Int NodeId
nidLookup gr = Map.fromList [(idx, nid) | (idx, (nid, _)) <- labNodes gr]

-- | Find the fgl Int index for a Graphos NodeId
findFglIdx :: FGLGraph -> NodeId -> Maybe Int
findFglIdx gr nid = lookup nid idxList
  where idxList = [(nid', idx) | (idx, (nid', _)) <- labNodes gr]

-- ───────────────────────────────────────────────
-- Analysis queries
-- ───────────────────────────────────────────────

-- | Find god nodes (highest-degree nodes, excluding file hubs and concepts)
godNodes :: Graph -> Int -> [GodNode]
godNodes g topN =
  let degrees = [(nid, Set.size (neighbors' g nid), n) | (nid, n) <- Map.toList (gNodes g)]
      filtered = filter (\(_, deg, n) -> not (isFileNode g n) && not (isConceptNode n) && deg > 0) degrees
      sorted = sortOn (\(_, deg, _) -> negate deg) filtered
  in take topN [GodNode { gnId = nid, gnLabel = nodeLabel n, gnEdges = deg }
               | (nid, deg, n) <- sorted]
  where
    neighbors' g' nid =
      let fwd = Map.findWithDefault Set.empty nid (gAdjFwd g')
          bwd = Map.findWithDefault Set.empty nid (gAdjBack g')
      in if gDirected g' then fwd else fwd `Set.union` bwd

-- | Find articulation points (bridge nodes) whose removal would disconnect the graph.
-- Uses fgl's ap algorithm internally.
articulationPoints :: Graph -> [NodeId]
articulationPoints g =
  let gr = toFGL' g
      nidMap = nidLookup gr
      artPointIdxs = ap gr
  in [Map.findWithDefault (T.pack "???") idx nidMap | idx <- artPointIdxs]

-- | Find biconnected components of the graph.
-- Each component is a list of NodeIds forming a maximal subgraph
-- with no articulation point.
-- Uses fgl's bcc algorithm internally.
biconnectedComponents :: Graph -> [[NodeId]]
biconnectedComponents g =
  let gr = toFGL' g
      nidMap = nidLookup gr
      components = bcc gr
  in [nub [Map.findWithDefault (T.pack "???") idx nidMap | idx <- FGL.nodes comp] | comp <- components]

-- | Compute the dominator tree for a given start node.
-- Returns a map from each node to its immediate dominator.
-- Uses fgl's dom algorithm internally.
dominators :: Graph -> NodeId -> Map NodeId (Maybe NodeId)
dominators g start =
  let gr = toFGL' g
      nidMap = nidLookup gr
  in case findFglIdx gr start of
       Just startIdx ->
         let domList = dom gr startIdx
         in Map.fromList [(Map.findWithDefault n idx nidMap
                          , case Map.lookup idom nidMap of
                              Just d -> Just d
                              Nothing -> Nothing)
                          | (idx, idomList) <- domList
                          , n <- [Map.findWithDefault start idx nidMap]
                          , idom <- idomList]
       Nothing -> Map.empty

-- | Compute edge betweenness centrality using fgl shortest paths
edgeBetweenness :: Graph -> Map (NodeId, NodeId) Double
edgeBetweenness g =
  let gr = toFGL' g
      nidMap = nidLookup gr
      allNodeIndices = [(idx, nid) | (idx, (nid, _)) <- labNodes gr]
      -- For each pair of nodes, find shortest path and count edge traversals
      pathEdges = [edge
                  | (srcIdx, _) <- allNodeIndices
                  , (tgtIdx, _) <- allNodeIndices
                  , srcIdx < tgtIdx
                  , let path = esp srcIdx tgtIdx gr
                  , not (null path)
                  , edge <- zip path (drop 1 path)]
      edgeCounts = Map.fromListWith (+) [
        ((Map.findWithDefault (T.pack "???") s nidMap,
          Map.findWithDefault (T.pack "???") t nidMap), 1.0)
        | (s, t) <- pathEdges]
      n = fromIntegral (length allNodeIndices)
      normalization = if n > 1 then 2.0 / (n * (n - 1)) else 1.0
  in fmap (* normalization) edgeCounts