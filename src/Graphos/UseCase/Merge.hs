-- | Merge orchestration — combines two graphs and re-clusters.
--
-- Pure composition of Domain functions:
--   mergeGraphs → inferEdges → clusterGraph → analyzeGraph
--
-- IO is not performed here; this module only orchestrates pure logic.
module Graphos.UseCase.Merge
  ( MergeResult(..)
  , mergeGraphsAndAnalyze
  ) where

import qualified Data.Map.Strict as Map

import Graphos.Domain.Types
import Graphos.Domain.Config (SemanticEdgesConfig)
import Graphos.Domain.Graph (Graph, gNodes, gEdges, mergeGraphs)
import Graphos.Domain.Community (Resolution(..), detectCommunitiesWithResolution, scoreAllCohesion)
import Graphos.UseCase.Infer (inferEdges)
import Graphos.UseCase.Analyze (analyzeGraph)
import Graphos.UseCase.Build (buildGraphFromExtractions)

-- | Result of a merge operation
data MergeResult = MergeResult
  { mrGraph        :: Graph
  , mrCommunities  :: CommunityMap
  , mrCohesion     :: CohesionMap
  , mrAnalysis     :: Analysis
  } deriving (Eq, Show)

-- | Merge two graphs, re-cluster, infer edges, and analyze.
--
-- After merging, community IDs from the two source graphs no longer
-- align, so we re-detect communities on the combined graph.
-- Edge inference and analysis run on the enriched merged graph.
--
-- The 'first' graph's directed flag is preserved.
mergeGraphsAndAnalyze :: Graph -> Graph -> EdgeDensity -> Resolution -> SemanticEdgesConfig -> Bool -> MergeResult
mergeGraphsAndAnalyze graphA graphB density res seCfg force =
  let -- Step 1: Merge graphs (Domain pure function)
      merged = mergeGraphs graphA graphB

      -- Step 2: Cluster the merged graph
      (commMap, _cohesion) = clusterGraphWithResolution' merged res

      -- Step 3: Infer additional edges
      allInferred = inferEdges density seCfg force merged commMap
      enriched = if null allInferred
        then merged
        else buildGraphFromExtractions False
             [Extraction
               { extractionNodes = gNodes merged
               , extractionEdges = Map.fromList [(edgeId e, e) | e <- Map.elems (gEdges merged) ++ allInferred]
               }]

      -- Step 4: Re-cluster on enriched graph and analyze
      (finalComm, finalCohes) = clusterGraphWithResolution' enriched res
      analysis = analyzeGraph enriched finalComm finalCohes

  in MergeResult
       { mrGraph       = enriched
       , mrCommunities = finalComm
       , mrCohesion    = finalCohes
       , mrAnalysis    = analysis
       }
  where
    clusterGraphWithResolution' g r =
      let cm = detectCommunitiesWithResolution g r
          co = scoreAllCohesion g cm
      in (cm, co)