-- | JSON export - graph.json output and incremental checkpoints
module Graphos.Infrastructure.Export.JSON
  ( exportGraph
  , exportGraphWithLabels
  , saveCheckpoint
  ) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges)

-- | Export graph as JSON
exportGraph :: Graph -> Analysis -> FilePath -> IO ()
exportGraph g analysis path =
  exportGraphWithLabels g analysis Nothing path

-- | Export graph as JSON with community labels
exportGraphWithLabels :: Graph -> Analysis -> Maybe (Map Int Text) -> FilePath -> IO ()
exportGraphWithLabels g analysis mLabels path = do
  let base = [ "nodes"      .= Map.elems (gNodes g)
              , "edges"      .= Map.elems (gEdges g)
              , "communities" .= analysisCommunities analysis
              , "cohesion"   .= analysisCohesion analysis
              , "god_nodes"  .= analysisGodNodes analysis
              ]
      withLabels = case mLabels of
        Just labels -> base ++ ["community_labels" .= labels]
        Nothing    -> base
  BSL.writeFile path (encode (object withLabels))

-- | Save a checkpoint of the graph during pipeline execution.
-- Writes nodes and edges extracted so far; communities/analysis are empty.
-- The "checkpoint" flag signals this is a partial snapshot, not a final export.
-- If the pipeline crashes, the checkpoint file remains on disk for recovery.
saveCheckpoint :: Graph -> FilePath -> IO ()
saveCheckpoint g path = do
  let emptyCommMap = Map.empty :: CommunityMap
      emptyCohMap   = Map.empty :: CohesionMap
      payload = [ "nodes"       .= Map.elems (gNodes g)
                , "edges"       .= Map.elems (gEdges g)
                , "communities" .= emptyCommMap
                , "cohesion"    .= emptyCohMap
                , "god_nodes"   .= ([] :: [GodNode])
                , "checkpoint" .= True
                ]
  BSL.writeFile path (encode (object payload))