-- | JSON export - graph.json output and incremental checkpoints.
--
-- The canonical on-disk format is a JSON Graph Format (JGF) document
-- (@application\/vnd.jgf+json@): a top-level @graph@ object wrapping
-- @nodes@ (object keyed by id) and @edges@ (array), with graphos-specific
-- data under @graph.metadata.graphos@. See 'Graphos.Domain.Types.JGF'.
module Graphos.Infrastructure.Export.JSON
  ( exportGraph
  , exportGraphWithLabels
  , exportSubgraphJSON
  , saveCheckpoint
  , loadCheckpointInputSource
  ) where

import Data.Aeson (Value(..), encode, eitherDecode, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Vector as V
import System.Directory (doesFileExist)

import Graphos.Domain.Types
  ( Analysis(..)
  , CommunityMap, CohesionMap
  , GodNode(..)
  , Node, Edge
  , NullModel(..)
  )
import qualified Graphos.Domain.Types.JGF as JGF
  ( jgfDocument, jgfGraphDirectives, jgfNodesObject, edgeToJGF, graphosMetadata
  )
import qualified Graphos.Domain.Types.Graph as G (LabeledGraph(..))
import Graphos.Domain.Graph (Graph, gNodes, gEdges, gHash)
import Graphos.Infrastructure.FileSystem.AtomicWrite (writeFileAtomic)

-- | Export graph as JSON
exportGraph :: Graph -> Analysis -> FilePath -> IO ()
exportGraph g analysis path =
  exportGraphWithLabels g analysis Nothing path

-- | Export graph as JSON with community labels.
-- Emits the JGF envelope: @{ "graph": { directed, type, nodes, edges,
-- metadata: { graphos: { ... } } } }@.
exportGraphWithLabels :: Graph -> Analysis -> Maybe (Map Int Text) -> FilePath -> IO ()
exportGraphWithLabels g analysis mLabels path =
  writeFileAtomic path (encode (graphDocument nodes edges graphosMeta))
  where
    nodes = Map.elems (gNodes g)
    edges = Map.elems (gEdges g)
    graphosMeta =
      [ ("communities",      toJSON (analysisCommunities analysis))
      , ("null_model",       toJSON (analysisNullModel analysis))
      , ("cohesion",         toJSON (analysisCohesion analysis))
      , ("god_nodes",        toJSON (analysisGodNodes analysis))
      , ("community_labels", toJSON (maybe (Map.empty :: Map Int Text) id mLabels))
      , ("graph_hash",       toJSON (gHash g))
      ]

-- | Assemble the @graph@ object: JGF directives + node/edge payload +
-- @metadata.graphos@.
graphDocument :: [Node] -> [Edge] -> [(Text, Value)] -> Value
graphDocument nodes edges graphosMeta = JGF.jgfDocument $
  JGF.jgfGraphDirectives
    ++ [ ("nodes", JGF.jgfNodesObject nodes)
       , ("edges", Array (V.fromList (map JGF.edgeToJGF edges)))
       , ("metadata", object [ Key.fromText "graphos" .= JGF.graphosMetadata graphosMeta ])
       ]

-- | Empty analysis used by partial exports (subgraph, checkpoint).
emptyAnalysis :: Analysis
emptyAnalysis = Analysis
  { analysisCommunities = Map.empty
  , analysisNullModel   = DefaultNullModel
  , analysisCohesion    = Map.empty
  , analysisGodNodes    = []
  , analysisSurprises   = []
  , analysisQuestions   = []
  , analysisArticulation = []
  , analysisBccCount    = 0
  }

-- | Export a subgraph (a 'G.LabeledGraph') in the standard graph.json format so
-- it is directly consumable via @--graph@. Community/analysis sections are
-- written empty: the query family only needs the node/edge payload.
exportSubgraphJSON :: G.LabeledGraph -> FilePath -> IO ()
exportSubgraphJSON g path =
  writeFileAtomic path (encode (graphDocument nodes edges graphosMeta))
  where
    nodes = M.elems (G.gNodes g)
    edges = M.elems (G.gEdges g)
    graphosMeta =
      [ ("communities",      toJSON (Map.empty :: CommunityMap))
      , ("cohesion",         toJSON (Map.empty :: CohesionMap))
      , ("god_nodes",        toJSON ([] :: [GodNode]))
      , ("community_labels", toJSON (Map.empty :: Map Int Text))
      ]

-- | Save a checkpoint of the graph during pipeline execution.
--
-- The checkpoint is written to @<output-dir>/graph.checkpoint.json@ — the canonical
-- checkpoint location shared by the incremental-run and @--cluster-only@ paths. It is a
-- partial snapshot: nodes and edges extracted so far, with communities, cohesion,
-- god-nodes, and analysis all empty. The @checkpoint@ flag distinguishes it
-- from a final graph export; if the pipeline crashes the file remains on disk for
-- recovery.
--
-- The payload carries the JGF envelope with a @schemaVersion@ under
-- @graph.metadata.graphos@ and the originating @input_source@ provenance, so a
-- resumed / @--cluster-only@ run can validate forward compatibility and warn when
-- it was built from a different input.
saveCheckpoint :: Graph -> FilePath -> Text -> IO ()
saveCheckpoint g path provenance =
  writeFileAtomic path (encode (graphDocument nodes edges graphosMeta))
  where
    nodes = Map.elems (gNodes g)
    edges = Map.elems (gEdges g)
    graphosMeta =
      [ ("communities",  toJSON (Map.empty :: CommunityMap))
      , ("cohesion",     toJSON (Map.empty :: CohesionMap))
      , ("god_nodes",    toJSON ([] :: [GodNode]))
      , ("checkpoint",   Bool True)
      , ("input_source", toJSON provenance)
      , ("graph_hash",   toJSON (gHash g))
      ]
    _ = emptyAnalysis  -- partial snapshot: analysis sections are empty

-- | Read the recorded @input_source@ provenance from a checkpoint file.
-- Returns Nothing when the file is absent or has no provenance record, so
-- callers can treat a missing value as "no mismatch to report".
--
-- Accepts both the JGF envelope (@graph.metadata.graphos.input_source@) and
-- the legacy top-level key during the deprecation window.
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
          _               -> pure (graphosInputSource obj)

-- | Extract @input_source@ from the JGF envelope:
-- @graph.metadata.graphos.input_source@.
graphosInputSource :: Map Text Value -> Maybe Text
graphosInputSource obj = do
  Object gkm  <- M.lookup "graph" obj
  Object mm   <- KM.lookup (Key.fromText "metadata") gkm
  Object gm   <- KM.lookup (Key.fromText "graphos") mm
  String s    <- KM.lookup (Key.fromText "input_source") gm
  pure s