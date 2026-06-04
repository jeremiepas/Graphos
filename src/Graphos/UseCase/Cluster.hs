-- | Community detection orchestration.
--
-- Two modes:
--   1. Full Leiden clustering on the entire graph (existing)
--   2. Fast single-node clustering: extract a bounded subgraph around a node,
--      then run Leiden only on that subgraph. Much faster for incremental ingestion.
module Graphos.UseCase.Cluster
  ( clusterGraph
  , clusterGraphWithResolution
  , clusterSingle
  ) where

import qualified Data.Map.Strict as Map

import Graphos.Domain.Types (CommunityMap, CohesionMap, NodeId)
import Graphos.Domain.Graph (Graph, gNodes)
import Graphos.Domain.Graph.Query (breadthFirstSearch, subgraph)
import Graphos.Domain.Community (detectCommunitiesWithResolution, Resolution(..), defaultResolution, scoreAllCohesion)

-- | Run community detection with default resolution and compute cohesion scores
clusterGraph :: Graph -> (CommunityMap, CohesionMap)
clusterGraph g = clusterGraphWithResolution g defaultResolution

-- | Run community detection with a custom resolution and compute cohesion scores.
-- Handles edge cases: empty graphs and single-node graphs return empty maps.
clusterGraphWithResolution :: Graph -> Resolution -> (CommunityMap, CohesionMap)
clusterGraphWithResolution g res
  | Map.null (gNodes g) = (Map.empty, Map.empty)
  | Map.size (gNodes g) == 1 = (Map.singleton 0 [fst (Map.findMin (gNodes g))], Map.singleton 0 1.0)
  | otherwise =
      let commMap = detectCommunitiesWithResolution g res
          cohesion = scoreAllCohesion g commMap
      in (commMap, cohesion)

-- | Fast clustering for a single ingested node.
--
-- Extracts a bounded BFS subgraph (up to 'maxDepth' hops) around the given
-- node, then runs Leiden only on that subgraph. This is O(|subgraph|) instead
-- of O(|full graph|), making it fast for incremental/single-file ingestion.
--
-- Returns the community map and cohesion for the subgraph only.
-- The node may be assigned a different community than it would get in the
-- full graph, but this is acceptable for quick incremental updates.
--
-- Returns (empty, empty) if the seed node is not in the graph.
clusterSingle :: Graph -> NodeId -> Int -> Resolution -> (CommunityMap, CohesionMap)
clusterSingle graph seedNodeId maxDepth res
  | not (Map.member seedNodeId (gNodes graph)) = (Map.empty, Map.empty)
  | otherwise =
      let reachable = breadthFirstSearch graph seedNodeId maxDepth
          sub = subgraph graph reachable
      in if Map.null (gNodes sub)
         then (Map.empty, Map.empty)
         else clusterGraphWithResolution sub res