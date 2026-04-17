-- | Community detection orchestration
module Graphos.UseCase.Cluster
  ( clusterGraph
  , clusterGraphWithResolution
  ) where

import Graphos.Domain.Types (CommunityMap, CohesionMap)
import Graphos.Domain.Graph (Graph)
import Graphos.Domain.Community (detectCommunitiesWithResolution, Resolution(..), defaultResolution, scoreAllCohesion)

-- | Run community detection with default resolution and compute cohesion scores
clusterGraph :: Graph -> (CommunityMap, CohesionMap)
clusterGraph g = clusterGraphWithResolution g defaultResolution

-- | Run community detection with a custom resolution and compute cohesion scores
clusterGraphWithResolution :: Graph -> Resolution -> (CommunityMap, CohesionMap)
clusterGraphWithResolution g res =
  let commMap = detectCommunitiesWithResolution g res
      cohesion = scoreAllCohesion g commMap
  in (commMap, cohesion)