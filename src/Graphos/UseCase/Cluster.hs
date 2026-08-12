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
  , joinCommunitiesToNodes
  , computeCommunityAggregates
  , colorForCommunity
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing)

import Graphos.Domain.Types (CommunityAggregate(..), CommunityId, CommunityMap, CohesionMap, NodeId, Node(..), Edge(..))
import Graphos.Domain.Graph (Graph, gNodes, gEdges)
import Graphos.Domain.Graph.Query (breadthFirstSearch, subgraph)
import Graphos.Domain.Community (detectCommunitiesWithResolution, Resolution(..), defaultResolution, scoreAllCohesion)
import qualified Data.Set as Set

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

-- | Community color palette (must match HTML.hs)
communityColors :: [Text]
communityColors =
  [ "#7dd3fc", "#f472b6", "#34d399", "#fbbf24", "#a78bfa"
  , "#fb923c", "#2dd4bf", "#f87171", "#818cf8", "#4ade80"
  , "#e879f9", "#38bdf8", "#facc15", "#fb7185", "#22d3ee"
  , "#c084fc"
  ]

colorForCommunity :: Int -> Text
colorForCommunity cid = communityColors !! (cid `mod` length communityColors)

-- | Build an inverted lookup map: NodeId -> CommunityId
invertCommunityMap' :: CommunityMap -> Map.Map NodeId CommunityId
invertCommunityMap' commMap = Map.fromList
  [ (nid, cid) | (cid, nids) <- Map.toList commMap, nid <- nids ]

-- | Join community IDs to nodes by setting nodeCommunityId.
-- Pure O(N) function that maps over all nodes, looks up each NodeId in the
-- inverted CommunityMap, and sets nodeCommunityId = Just cid when found.
-- Nodes not in any community retain Nothing.
joinCommunitiesToNodes :: Graph -> CommunityMap -> Graph
joinCommunitiesToNodes graph commMap =
  let invMap = invertCommunityMap' commMap
      updateNode n = n { nodeCommunityId = case Map.lookup (nodeId n) invMap of
                Just cid -> Just cid
                Nothing  -> Nothing }
      nodes = Map.map updateNode (gNodes graph)
  in graph { gNodes = nodes }

-- | Compute community aggregates from graph, community map, cohesion map,
-- and optional LLM labels.
computeCommunityAggregates
  :: Graph
  -> CommunityMap
  -> CohesionMap
  -> [NodeId]  -- articulation points (bridge nodes)
  -> Maybe (Map.Map CommunityId Text)  -- LLM labels
  -> [CommunityAggregate]
computeCommunityAggregates graph commMap cohesionMap artPoints mLabels =
  let invMap = invertCommunityMap' commMap
      artSet = Set.fromList artPoints
      edgeList = Map.elems (gEdges graph)
      -- Count inter-community edges: source community -> target community -> count
      interEdgeCounts :: Map.Map CommunityId (Map.Map CommunityId Int)
      interEdgeCounts = foldl' step Map.empty edgeList
        where
          step :: Map.Map CommunityId (Map.Map CommunityId Int) -> Edge -> Map.Map CommunityId (Map.Map CommunityId Int)
          step m e =
            let src = edgeSource e
                tgt = edgeTarget e
            in case (Map.lookup src invMap, Map.lookup tgt invMap) of
              (Just cidSrc, Just cidTgt) | cidSrc /= cidTgt ->
                let current = Map.findWithDefault Map.empty cidSrc m
                    updated = Map.insertWith (+) cidTgt 1 current
                in Map.insert cidSrc updated m
              _ -> m
      -- Count bridge nodes (articulation points that are in a community)
      countBridge :: [NodeId] -> Int
      countBridge members = length [m | m <- members, Set.member m artSet]
      -- Extract a clean label from representative labels
      cleanLabel :: Int -> [Text] -> Text
      cleanLabel commId labels =
        let filtered = filter (\l -> T.length l > 3 && T.length l < 100) labels
        in case filtered of
            [] -> T.pack ("Community " ++ show commId)
            good -> case headMay good of
              Just clean -> clean
              Nothing -> T.pack ("Community " ++ show commId)
          
      -- Safe head for lists
      headMay :: [a] -> Maybe a
      headMay [] = Nothing
      headMay (x:_) = Just x
          
      -- Get representative labels (up to 3)
      representativeLabels :: [NodeId] -> [Text]
      representativeLabels nids =
        let nodesInComm = concatMap (\nid -> case Map.lookup nid (gNodes graph) of
                Just n -> [n]
                Nothing -> []) nids
            sorted = sortBy (comparing nodeLabel) nodesInComm
        in take 3 [nodeLabel n | n <- sorted]
    in map (\(cid, members) ->
          let repLabels = representativeLabels members
              lbl = cleanLabel cid repLabels
          in CommunityAggregate
          { caId                     = T.pack (show cid)
          , caMemberCount            = length members
          , caCohesion               = Map.findWithDefault 0.0 cid cohesionMap
          , caBridgeCount            = countBridge members
          , caColor                  = colorForCommunity cid
          , caLabel                  = case mLabels of
                Just labels -> Map.findWithDefault lbl cid labels
                Nothing   -> lbl
          , caRepresentativeLabels   = repLabels
           , caInterCommunityEdges    = Map.toList (Map.findWithDefault Map.empty cid interEdgeCounts)
          , caDominantKind           = Nothing
          , caMixedRatio             = 0.0
          , caCodeDocEdges           = 0
          }
        ) (Map.toList commMap)
