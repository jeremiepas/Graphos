-- | JSON export - graph.json output and incremental checkpoints
module Graphos.Infrastructure.Export.JSON
  ( exportGraph
  , exportGraphWithLabels
  , exportSubgraphJSON
  , saveCheckpoint
  , loadCheckpointInputSource
  ) where

import Data.Aeson (encode, object, (.=), Value(..), eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map as M
import Data.Text (Text)
import System.Directory (doesFileExist)

import Graphos.Domain.Types
import qualified Graphos.Domain.Types.Graph as G (LabeledGraph(..))
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

-- | Export a subgraph (a 'LabeledGraph') in the standard graph.json format so
-- it is directly consumable via @--graph@. Community/analysis sections are
-- written empty: the query family only needs the node/edge payload.
exportSubgraphJSON :: G.LabeledGraph -> FilePath -> IO ()
exportSubgraphJSON g path = do
  let payload = [ "nodes"            .= Map.elems (G.gNodes g)
                , "edges"            .= Map.elems (G.gEdges g)
                , "communities"      .= (Map.empty :: CommunityMap)
                , "cohesion"         .= (Map.empty :: CohesionMap)
                , "god_nodes"        .= ([] :: [GodNode])
                , "community_labels" .= (Map.empty :: Map Int Text)
                ]
  BSL.writeFile path (encode (object payload))

-- | Save a checkpoint of the graph during pipeline execution.
--
-- The checkpoint is written to @<output-dir>/graph.checkpoint.json@ — the canonical
-- checkpoint location shared by the incremental-run and @--cluster-only@ paths. It is a
-- partial snapshot: nodes and edges extracted so far, with communities, cohesion,
-- god-nodes, and analysis all empty. The @"checkpoint": true@ flag distinguishes it
-- from a final graph export; if the pipeline crashes the file remains on disk for
-- recovery.
--
-- The payload carries a @"schema_version"@ (currently @"1"@; bump on breaking changes)
-- and the originating @"input_source"@ provenance, so a resumed / @--cluster-only@ run
-- can validate forward compatibility and warn when it was built from a different input.
saveCheckpoint :: Graph -> FilePath -> Text -> IO ()
saveCheckpoint g path provenance = do
  let emptyCommMap = Map.empty :: CommunityMap
      emptyCohMap   = Map.empty :: CohesionMap
      payload = [ "nodes"          .= Map.elems (gNodes g)
                , "edges"          .= Map.elems (gEdges g)
                , "communities"    .= emptyCommMap
                , "cohesion"       .= emptyCohMap
                , "god_nodes"      .= ([] :: [GodNode])
                , "checkpoint"     .= True
                , "schema_version" .= checkpointSchemaVersion
                , "input_source"   .= provenance
                ]
  BSL.writeFile path (encode (object payload))

-- | Current checkpoint schema major version. Bump on breaking changes; the
-- loader treats a missing version as this major (forward-compatible).
checkpointSchemaVersion :: Text
checkpointSchemaVersion = "1"

-- | Read the recorded "input_source" provenance from a checkpoint file.
-- Returns Nothing when the file is absent or has no provenance record, so
-- callers can treat a missing value as "no mismatch to report".
loadCheckpointInputSource :: FilePath -> IO (Maybe Text)
loadCheckpointInputSource path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      bs <- BSL.readFile path
      case eitherDecode bs :: Either String (Map Text Value) of
        Left _   -> pure Nothing
        Right obj -> case M.lookup "input_source" obj of
          Just (String s) -> pure (Just s)
          _               -> pure Nothing