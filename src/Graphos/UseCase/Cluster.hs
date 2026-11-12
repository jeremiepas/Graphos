-- | Community detection orchestration.
--
-- Two modes:
--   1. Full Leiden clustering on the entire graph (existing)
--   2. Fast single-node clustering: extract a bounded subgraph around a node,
--      then run Leiden only on that subgraph. Much faster for incremental ingestion.
--
-- The stage also dispatches on a selectable clustering algorithm
-- (openspec change @bunch-search-based-modularization@): 'Leiden' (the
-- default, byte-identical to the pre-existing path) or 'BunchAlgorithm',
-- which folds the graph to its file-level MDG, runs the seeded hill-climbing
-- search of "Graphos.Domain.Community.Bunch", and pushes the file partition
-- back to every symbol node. Above the file cap the stage skips Bunch,
-- reports the skip and runs Leiden instead.
module Graphos.UseCase.Cluster
  ( clusterGraph
  , clusterGraphWithResolution
  , clusterSingle
  , joinCommunitiesToNodes
  , computeCommunityAggregates
  , colorForCommunity
    -- * Selectable clustering algorithm
  , ClusterAlgorithm(..)
  , BunchSettings(..)
  , defaultBunchSettings
  , ClusterQuality(..)
  , BunchSkip(..)
  , runClustering
  , partitionQuality
  , clusterAlgorithmFromConfig
  , defaultBunchFileCap
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (toText)
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing)

import Graphos.Domain.Types (CommunityAggregate(..), CommunityId, CommunityMap, CohesionMap, NodeId, Node(..), Edge(..), ClusterQuality(..), BunchSkip(..))
import Graphos.Domain.Config.Clustering (ClusteringConfig(..), ClusteringAlgorithmName(..), BunchFileConfig(..))
import Graphos.Domain.Graph (Graph, gNodes, gEdges)
import Graphos.Domain.Graph.Query (breadthFirstSearch, subgraph)
import Graphos.Domain.Community (detectCommunitiesWithResolution, Resolution(..), defaultResolution, scoreAllCohesion, undirectedView, modularity)
import Graphos.Domain.Community.MQ (Weighting(..), modularizationQuality, mqValue, mqIntraFraction)
import Graphos.Domain.Community.Bunch
  ( BunchConfig(..)
  , BunchResult(..)
  , BunchRun(..)
  , FileMDG(..)
  , bunchCluster
  , defaultBunchConfig
  , foldToFileMDG
  , mdgFileCount
  )
import qualified Data.Set as Set
import qualified Data.Vector as V

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
      -- Cohesion is scored on the same undirected projection the partition
      -- was computed on, so a directed graph does not under-count the
      -- internal edges of its communities.
      let commMap = detectCommunitiesWithResolution g res
          cohesion = scoreAllCohesion (undirectedView g) commMap
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
        in take 3 [toText (nodeLabel n) | n <- sorted]
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

-- ───────────────────────────────────────────────
-- Selectable clustering algorithm (bunch-search-based-modularization)
-- ───────────────────────────────────────────────

-- | The clustering algorithm of the build stage. 'Leiden' is the default and
-- is byte-identical to the pre-existing path; 'BunchAlgorithm' runs the
-- seeded search over the file-level MDG and pushes the file partition back
-- to every symbol node.
data ClusterAlgorithm = Leiden | BunchAlgorithm BunchSettings
  deriving (Eq, Show)

-- | Stage-level Bunch settings: the Domain search configuration plus the
-- two knobs that live above the search — the file cap and the weighting of
-- the MDG fold.
data BunchSettings = BunchSettings
  { bsConfig   :: BunchConfig -- ^ seed, population, η, annealing, iteration cap
  , bsMaxFiles :: Int         -- ^ file count above which Leiden is run instead
  , bsWeighted :: Bool        -- ^ fold the MDG with 'EdgeWeights' instead of unit weights
  } deriving (Eq, Show)

-- | Default file cap (design D2): 5,000 files.
defaultBunchFileCap :: Int
defaultBunchFileCap = 5000

defaultBunchSettings :: BunchSettings
defaultBunchSettings = BunchSettings defaultBunchConfig defaultBunchFileCap False

-- | Run the stage for the selected algorithm. Pure and deterministic: the
-- Leiden path is exactly 'clusterGraphWithResolution'; the Bunch path folds
-- the file MDG, checks the cap (falling back to Leiden with a skip entry)
-- and runs the population search. The returned partition is the Bunch
-- result pushed back to symbols, *without* the community-size cap — the
-- caller applies 'capCommunitySizes' to it exactly as it does for Leiden,
-- and never 'mergeSmallCommunities' (MQ already penalises singletons).
runClustering :: ClusterAlgorithm -> Resolution -> Graph
              -> (CommunityMap, CohesionMap, ClusterQuality)
runClustering algo res g = case algo of
  Leiden -> leidenRun Nothing
  BunchAlgorithm bs
    | n > bsMaxFiles bs -> leidenRun (Just (BunchSkip n (bsMaxFiles bs)))
    | otherwise         -> bunchRun bs mdg
  where
    weighting = case algo of
      BunchAlgorithm bs | bsWeighted bs -> EdgeWeights
      _ -> UnitWeights
    mdg = foldToFileMDG weighting g
    n   = mdgFileCount mdg

    leidenRun skip =
      let (comm, coh) = clusterGraphWithResolution g res
      in (comm, coh, partitionQuality "leiden" g comm skip)

    bunchRun bs mdg0 =
      let best  = bresBest (bunchCluster (bsConfig bs) mdg0)
          nodesByFile = Map.fromListWith (flip (++))
            [ (nodeSourceFile nd, [nodeId nd]) | nd <- Map.elems (gNodes g) ]
          comm = Map.fromList
            [ (cid, concatMap (\f -> Map.findWithDefault [] f nodesByFile)
                              (map (mdgFiles mdg0 V.!) members))
            | (cid, members) <- zip [0 :: Int ..] (brPartition best) ]
      in (comm, scoreAllCohesion (undirectedView g) comm, partitionQuality "bunch" g comm Nothing)
      -- Cohesion is scored on the same undirected projection as for Leiden
      -- (per-community intra-edge density), so the analysis and aggregates
      -- downstream stay uniform across algorithms.


-- | Score a partition of the graph: MQ (unit weights), modularity and
-- intra-edge fraction, tagged with the algorithm name and the skip entry.
-- The pipeline re-scores the partition after the size-cap post-pass so the
-- report describes exactly the communities the export writes.
partitionQuality :: Text -> Graph -> CommunityMap -> Maybe BunchSkip -> ClusterQuality
partitionQuality name g cm skip =
  let mq = modularizationQuality g cm
  in ClusterQuality
       { cqAlgorithm     = name
       , cqMQ            = mqValue mq
       , cqModularity    = modularity g cm
       , cqIntraFraction = mqIntraFraction mq
       , cqClusters      = Map.size cm
       , cqSkip          = skip
       }

-- | The stage algorithm selected by the effective clustering configuration
-- (@graphos.yaml@ with CLI overrides applied).
clusterAlgorithmFromConfig :: ClusteringConfig -> ClusterAlgorithm
clusterAlgorithmFromConfig cc = case ccAlgorithm cc of
  AlgoLeiden -> Leiden
  AlgoBunch  -> BunchAlgorithm BunchSettings
    { bsConfig   = defaultBunchConfig
        { bcSeed          = fromIntegral (ccSeed cc)
        , bcPopulation    = bfPopulation b
        , bcThreshold     = bfThreshold b
        , bcAnnealing     = bfAnnealing b
        , bcMaxIterations = bfMaxIterations b
        }
    , bsMaxFiles = bfMaxFiles b
    , bsWeighted = bfWeighted b
    }
  where b = ccBunch cc
