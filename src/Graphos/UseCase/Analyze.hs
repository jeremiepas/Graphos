-- | Analysis orchestration
module Graphos.UseCase.Analyze
  ( analyzeGraph
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph)
import Graphos.Domain.Analysis (analyze)

-- | Run full analysis on a graph with communities
analyzeGraph :: Graph -> CommunityMap -> CohesionMap -> Analysis
analyzeGraph = analyze