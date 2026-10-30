-- | Build a graph from extraction results
module Graphos.UseCase.Build
  ( buildGraphFromExtractions
  , buildGraphFromExtractionsWithStats
  , buildGraphFromExtractionsStats
  , buildGraphFromExtractionsCounts
  , buildGraphFromExtractionsWithCounts
  , buildGraphFromExtractionsDrops
  ) where

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph, buildGraphWithStats, buildGraphStats, mergeExtractions)

-- | Build a graph from one or more extractions
buildGraphFromExtractions :: Bool -> [Extraction] -> Graph
buildGraphFromExtractions directed extractions =
  let merged = foldr mergeExtractions emptyExtraction extractions
  in buildGraph directed merged

-- | Build a graph from one or more extractions, also reporting the number of
-- edges dropped for unknown endpoints and collapsed for duplicate (source,target)
-- keys (both as a pair @(unknownEndpointDrops, duplicateKeyDrops)@).
buildGraphFromExtractionsWithStats :: Bool -> [Extraction] -> (Graph, (Int, Int))
buildGraphFromExtractionsWithStats directed extractions =
  let merged = foldr mergeExtractions emptyExtraction extractions
  in buildGraphWithStats directed merged

buildGraphFromExtractionsStats :: Bool -> [Extraction] -> (Int, Int)
buildGraphFromExtractionsStats directed =
  snd . buildGraphFromExtractionsWithStats directed

buildGraphFromExtractionsCounts :: Bool -> [Extraction] -> (Int, Int)
buildGraphFromExtractionsCounts = buildGraphFromExtractionsStats

buildGraphFromExtractionsWithCounts :: Bool -> [Extraction] -> (Graph, (Int, Int))
buildGraphFromExtractionsWithCounts = buildGraphFromExtractionsWithStats

buildGraphFromExtractionsDrops :: Bool -> [Extraction] -> (Int, Int)
buildGraphFromExtractionsDrops = buildGraphFromExtractionsStats