-- | Graph query operations — traversal and search.
-- Pure functions over the domain types.
--
-- Memory optimization: Uses CachedFGL to share a single FGL conversion
-- across all algorithm calls, saving ~600MB on 100k-node graphs.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Domain.Graph.Query
  ( neighbors
  , degree
  , shortestPath
  , shortestPathWithCached
  , breadthFirstSearch
  , breadthFirstSearchWithCached
  , depthFirstSearch
  , depthFirstSearchWithCached
  , subgraph
  ) where

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Graph.Inductive.Query.BFS (bfs, esp)
import Data.Graph.Inductive.Query.DFS (dfs)

import Graphos.Domain.Types
import Graphos.Domain.Graph.Core (Graph(..), computeGraphHash)
import Graphos.Domain.Graph.Analysis (CachedFGL(..), toCachedFGL, cachedFindIdx)

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
breadthFirstSearch g start maxDepth = breadthFirstSearchWithCached (toCachedFGL g) start maxDepth

-- | Breadth-first search using a pre-built CachedFGL
breadthFirstSearchWithCached :: CachedFGL -> NodeId -> Int -> Set NodeId
breadthFirstSearchWithCached cfg start _maxDepth =
  let gr = cfgGraph cfg
      nidMap = cfgNidMap cfg
  in case cachedFindIdx cfg start of
       Just startIdx -> Set.fromList [nidMap V.! idx | idx <- bfs startIdx gr]
       Nothing -> Set.empty

-- | Depth-first search from a start node, returns visited node IDs
-- Uses fgl's DFS algorithm internally
depthFirstSearch :: Graph -> NodeId -> Int -> Int -> Set NodeId
depthFirstSearch g start maxDepth budget = depthFirstSearchWithCached (toCachedFGL g) start maxDepth budget

-- | Depth-first search using a pre-built CachedFGL
depthFirstSearchWithCached :: CachedFGL -> NodeId -> Int -> Int -> Set NodeId
depthFirstSearchWithCached cfg start _maxDepth budget =
  let gr = cfgGraph cfg
      nidMap = cfgNidMap cfg
      result = case cachedFindIdx cfg start of
                 Just startIdx -> Set.fromList [nidMap V.! idx | idx <- dfs [startIdx] gr]
                 Nothing -> Set.empty
  in if Set.size result > budget
     then Set.fromList (take budget (Set.toList result))
     else result

-- | Shortest path between two nodes (BFS)
-- Uses fgl's ESP (shortest path by edge count) algorithm internally
shortestPath :: Graph -> NodeId -> NodeId -> Maybe [NodeId]
shortestPath g src tgt = shortestPathWithCached (toCachedFGL g) src tgt

-- | Shortest path using a pre-built CachedFGL
shortestPathWithCached :: CachedFGL -> NodeId -> NodeId -> Maybe [NodeId]
shortestPathWithCached cfg src tgt =
  let gr = cfgGraph cfg
      nidMap = cfgNidMap cfg
  in case (cachedFindIdx cfg src, cachedFindIdx cfg tgt) of
       (Just srcIdx, Just tgtIdx) ->
         let path = esp srcIdx tgtIdx gr
         in if null path then Nothing
            else Just [nidMap V.! idx | idx <- path]
       _ -> Nothing

-- | Extract a subgraph around given nodes
subgraph :: Graph -> Set NodeId -> Graph
subgraph g nodeSet =
  let nodes' = Map.filterWithKey (\k _ -> k `Set.member` nodeSet) (gNodes g)
      edges' = Map.filterWithKey (\(s, t) _ -> s `Set.member` nodeSet && t `Set.member` nodeSet) (gEdges g)
      fwd' = Map.map (`Set.intersection` nodeSet) $ Map.filterWithKey (\k _ -> k `Set.member` nodeSet) (gAdjFwd g)
      bwd' = Map.map (`Set.intersection` nodeSet) $ Map.filterWithKey (\k _ -> k `Set.member` nodeSet) (gAdjBack g)
      embs' = case gEmbeddings g of
                Nothing -> Nothing
                Just m  -> Just (Map.filterWithKey (\k _ -> k `Set.member` nodeSet) m)
    in Graph { gNodes = nodes', gEdges = edges', gAdjFwd = fwd', gAdjBack = bwd', gDirected = gDirected g, gCompositions = Nothing, gHash = computeGraphHash nodes' edges', gEmbeddings = embs', gEmbeddingsPath = gEmbeddingsPath g }