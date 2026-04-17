-- | Graph operations - pure functions over the domain types.
-- Build, merge, query, and diff knowledge graphs.
-- 
-- This module uses fgl (Functional Graph Library) internally for graph
-- algorithms (BFS, DFS, shortest path, betweenness centrality) while
-- keeping the public API unchanged with Map/Set-based representation.

{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Domain.Graph
  ( -- * Types
    Graph
  , gNodes
  , gEdges
  
    -- * Construction
  , buildGraph
  , mergeExtractions
  , mergeGraphs
  
    -- * Queries
  , godNodes
  , neighbors
  , degree
  , shortestPath
  , breadthFirstSearch
  , depthFirstSearch
  , subgraph
  
    -- * Advanced queries (fgl-powered)
  , articulationPoints
  , biconnectedComponents
  , dominators
  
    -- * Analysis helpers
  , isFileNode
  , isConceptNode
  , edgeBetweenness
  
    -- * Diff
  , graphDiff
  ) where

import Data.List (sortOn, nubBy, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Graph.Inductive.Graph (labNodes)
import qualified Data.Graph.Inductive.Graph as FGL
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.BFS (bfs, esp)
import Data.Graph.Inductive.Query.DFS (dfs)
import Data.Graph.Inductive.Query.ArtPoint (ap)
import Data.Graph.Inductive.Query.BCC (bcc)
import Data.Graph.Inductive.Query.Dominators (dom)

import Graphos.Domain.Types
import Graphos.Domain.Graph.FGL
  ( toFGL, fromFGL, FGLNodeLabel, FGLEdgeLabel, FGLGraph, nidToInt )

-- ───────────────────────────────────────────────
-- Internal graph representation
-- ───────────────────────────────────────────────

-- | Adjacency-list graph with node and edge attributes
data Graph = Graph
  { gNodes    :: Map NodeId Node
  , gEdges    :: Map (NodeId, NodeId) Edge
  , gAdjFwd   :: Map NodeId (Set NodeId)   -- forward adjacency
  , gAdjBack  :: Map NodeId (Set NodeId)   -- backward adjacency (for undirected queries)
  , gDirected :: Bool
  } deriving (Eq, Show)

-- ───────────────────────────────────────────────
-- Construction
-- ───────────────────────────────────────────────

-- | Build a graph from an Extraction result
buildGraph :: Bool -> Extraction -> Graph
buildGraph directed extraction =
  let nodes = Map.fromList [(nodeId n, n) | n <- extractionNodes extraction]
      edgeMap = Map.fromList [((edgeSource e, edgeTarget e), e) | e <- extractionEdges extraction]
      fwdAdj = Map.fromListWith Set.union
          [(edgeSource e, Set.singleton (edgeTarget e)) | e <- extractionEdges extraction]
      bwdAdj = if directed
          then Map.fromListWith Set.union
            [(edgeTarget e, Set.singleton (edgeSource e)) | e <- extractionEdges extraction]
          else Map.fromListWith Set.union
            [(edgeTarget e, Set.singleton (edgeSource e)) | e <- extractionEdges extraction]
              <> fwdAdj  -- undirected: edges go both ways
  in Graph
    { gNodes    = nodes
    , gEdges    = edgeMap
    , gAdjFwd   = fwdAdj
    , gAdjBack  = bwdAdj
    , gDirected = directed
    }

-- | Merge two extractions (dedup nodes by id, combine edges)
mergeExtractions :: Extraction -> Extraction -> Extraction
mergeExtractions a b =
  let allNodes = nubBy (\x y -> nodeId x == nodeId y) (extractionNodes a ++ extractionNodes b)
      allEdges = extractionEdges a ++ extractionEdges b
      allHyper = extractionHyperedges a ++ extractionHyperedges b
  in Extraction
    { extractionNodes      = allNodes
    , extractionEdges      = allEdges
    , extractionHyperedges = allHyper
    , extractionInputTokens  = extractionInputTokens a + extractionInputTokens b
    , extractionOutputTokens = extractionOutputTokens a + extractionOutputTokens b
    }

-- | Merge two graphs (new graph takes precedence for overlapping nodes)
mergeGraphs :: Graph -> Graph -> Graph
mergeGraphs old new =
  let mergedNodes = gNodes old <> gNodes new
      mergedEdges = gEdges old <> gEdges new
      mergedFwd   = Map.unionWith Set.union (gAdjFwd old) (gAdjFwd new)
      mergedBwd   = Map.unionWith Set.union (gAdjBack old) (gAdjBack new)
  in Graph
    { gNodes    = mergedNodes
    , gEdges    = mergedEdges
    , gAdjFwd   = mergedFwd
    , gAdjBack  = mergedBwd
    , gDirected = gDirected old
    }

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

-- | Find god nodes (highest-degree nodes, excluding file hubs and concepts)
godNodes :: Graph -> Int -> [GodNode]
godNodes g topN =
  let degrees = [(nid, Set.size (neighbors g nid), n) | (nid, n) <- Map.toList (gNodes g)]
      filtered = filter (\(_, deg, n) -> not (isFileNode g n) && not (isConceptNode n) && deg > 0) degrees
      sorted = sortOn (\(_, deg, _) -> negate deg) filtered
  in take topN [GodNode { gnId = nid, gnLabel = nodeLabel n, gnEdges = deg }
               | (nid, deg, n) <- sorted]

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

-- ───────────────────────────────────────────────
-- Advanced queries (fgl-powered)
-- ───────────────────────────────────────────────

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

-- ───────────────────────────────────────────────
-- Analysis helpers
-- ───────────────────────────────────────────────

-- | Check if a node is a file-level hub (synthetic AST node)
isFileNode :: Graph -> Node -> Bool
isFileNode g n =
  let label = nodeLabel n
      srcFile = nodeSourceFile n
  in -- Method stub: starts with '.' and ends with ')'
     (not (T.null label) && T.singleton (T.head label) == "." && T.last label == ')')
     -- Low-degree function stub
     || (not (T.null label) && T.last label == ')' && degree g (nodeId n) <= 1)
     -- Label matches source filename
     || (not (T.null srcFile) && not (T.null label) && label == T.pack (takeFileName (T.unpack srcFile)))
  where
    takeFileName path = case T.breakOnEnd "/" (T.pack path) of
      (_, "") -> path
      (_, name) -> T.unpack $ T.dropWhile (== '/') name

-- | Check if a node is a concept node (injected semantic annotation)
isConceptNode :: Node -> Bool
isConceptNode n =
  let src = nodeSourceFile n
  in T.null src || (T.null $ T.takeWhileEnd (/= '.') src)

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

-- ───────────────────────────────────────────────
-- Diff
-- ───────────────────────────────────────────────

-- | Compare two graph snapshots
graphDiff :: Graph -> Graph -> GraphDiff
graphDiff old new =
  let oldNodeIds = Map.keysSet (gNodes old)
      newNodeIds = Map.keysSet (gNodes new)
      addedIds   = newNodeIds `Set.difference` oldNodeIds
      removedIds = oldNodeIds `Set.difference` newNodeIds
      newNodes   = [n | (nid, n) <- Map.toList (gNodes new), nid `Set.member` addedIds]
      removedNodes = [(nid, nodeLabel n) | (nid, n) <- Map.toList (gNodes old), nid `Set.member` removedIds]
      oldEdgeKeys = Map.keysSet (gEdges old)
      newEdgeKeys = Map.keysSet (gEdges new)
      addedEdgeKeys = newEdgeKeys `Set.difference` oldEdgeKeys
      removedEdgeKeys = oldEdgeKeys `Set.difference` newEdgeKeys
      newEdges   = [e | (k, e) <- Map.toList (gEdges new), k `Set.member` addedEdgeKeys]
      removedEs  = [e | (k, e) <- Map.toList (gEdges old), k `Set.member` removedEdgeKeys]
      parts = []
        <> (if null newNodes then [] else [T.pack (show (length newNodes) ++ " new node(s)")])
        <> (if null newEdges then [] else [T.pack (show (length newEdges) ++ " new edge(s)")])
        <> (if null removedNodes then [] else [T.pack (show (length removedNodes) ++ " node(s) removed")])
        <> (if null removedEs then [] else [T.pack (show (length removedEs) ++ " edge(s) removed")])
  in GraphDiff
    { gdNewNodes    = newNodes
    , gdRemovedNodes = removedNodes
    , gdNewEdges    = newEdges
    , gdRemovedEdges = removedEs
    , gdSummary     = if null parts then "no changes" else T.intercalate ", " parts
    }