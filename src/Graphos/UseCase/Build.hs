-- | Build a graph from extraction results
module Graphos.UseCase.Build
  ( buildGraphFromExtractions
  ) where

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph, mergeExtractions)

-- | Build a graph from one or more extractions
buildGraphFromExtractions :: Bool -> [Extraction] -> Graph
buildGraphFromExtractions directed extractions =
  let merged = foldr mergeExtractions emptyExtraction extractions
  in buildGraph directed merged