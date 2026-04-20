-- | Graph query operations — traversal and search.
-- Pure functions over the domain types.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Domain.Graph.Query
  ( neighbors
  , degree
  , shortestPath
  , breadthFirstSearch
  , depthFirstSearch
  , subgraph
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph.Inductive.Graph (labNodes)
import Data.Graph.Inductive.Query.BFS (bfs, esp)
import Data.Graph.Inductive.Query.DFS (dfs)

import Graphos.Domain.Types
import Graphos.Domain.Graph.Core (Graph(..))
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
-- Queries
-- ───────────────────────────────────────────────

-- | Get neighbor node IDs.
-- For directed graphs: forward neighbors only.
-- For undirected graphs: union of forward and backward adjacency.
neighbors :: Graph -> NodeId -> Set NodeId
neighbors g nid =
  let fwd = Map.findWithDefault Set.empty nid (gAdjFwd g)
      bwd = Map.findWithDefault Set.empty nid (gAdjBack g)
  in if gDirected g then fwd else fwd `Set.union` bwd

-- | Get degree of a node
degree :: Graph -> NodeId -> Int
degree g nid = Set.size $ neighbors g nid

-- | Breadth-first search from a start node, returns visited node IDs
-- Uses fgl's BFS algorithm internally
breadthFirstSearch :: Graph -> NodeId -> Int -> Set NodeId
breadthFirstSearch g start _maxDepth =
  let gr = toFGL' g
      nidMap = nidLookup gr
  in case findFglIdx gr start of
       Just startIdx -> Set.fromList [Map.findWithDefault start idx nidMap | idx <- bfs startIdx gr]
       Nothing -> Set.empty

-- | Depth-first search from a start node, returns visited node IDs
-- Uses fgl's DFS algorithm internally
depthFirstSearch :: Graph -> NodeId -> Int -> Set NodeId
depthFirstSearch g start _maxDepth =
  let gr = toFGL' g
      nidMap = nidLookup gr
  in case findFglIdx gr start of
       Just startIdx -> Set.fromList [Map.findWithDefault start idx nidMap | idx <- dfs [startIdx] gr]
       Nothing -> Set.empty

-- | Shortest path between two nodes (BFS)
-- Uses fgl's ESP (shortest path by edge count) algorithm internally
shortestPath :: Graph -> NodeId -> NodeId -> Maybe [NodeId]
shortestPath g src tgt =
  let gr = toFGL' g
      nidMap = nidLookup gr
  in case (findFglIdx gr src, findFglIdx gr tgt) of
       (Just srcIdx, Just tgtIdx) ->
         let path = esp srcIdx tgtIdx gr
         in if null path then Nothing
            else Just [Map.findWithDefault src idx nidMap | idx <- path]
       _ -> Nothing

-- | Extract a subgraph around given nodes
subgraph :: Graph -> Set NodeId -> Graph
subgraph g nodeSet =
  let nodes' = Map.filterWithKey (\k _ -> k `Set.member` nodeSet) (gNodes g)
      edges' = Map.filterWithKey (\(s, t) _ -> s `Set.member` nodeSet && t `Set.member` nodeSet) (gEdges g)
      fwd' = Map.map (`Set.intersection` nodeSet) $ Map.filterWithKey (\k _ -> k `Set.member` nodeSet) (gAdjFwd g)
      bwd' = Map.map (`Set.intersection` nodeSet) $ Map.filterWithKey (\k _ -> k `Set.member` nodeSet) (gAdjBack g)
  in Graph { gNodes = nodes', gEdges = edges', gAdjFwd = fwd', gAdjBack = bwd', gDirected = gDirected g }