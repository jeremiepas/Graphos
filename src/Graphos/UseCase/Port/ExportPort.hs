-- | Port interface for export operations.
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Only Domain types appear in signatures.
module Graphos.UseCase.Port.ExportPort
  ( -- * Export result
    ExportResult(..)
    -- * Export port
  , ExportPort(..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Graphos.Domain.Types (Analysis, CommunityId, PipelineConfig, Detection, NodeId, CommunityMap, CohesionMap, Node, Edge, GodNode, CommunityAggregate)
import Graphos.Domain.Graph (Graph)


-- | Result of an export operation.
data ExportResult = ExportResult
  { erReport    :: FilePath
  , erJSON      :: FilePath
  , erHTML      :: Maybe FilePath
  , erObsidian  :: Maybe FilePath
  , erNeo4j     :: Maybe FilePath
  , erMemgraph  :: Maybe FilePath
  } deriving (Eq, Show)

-- | Record-of-functions port for export operations.
data ExportPort = ExportPort
  { -- | Individual export functions
    epExportHTML        :: Graph -> Analysis -> FilePath -> IO ()
  , epExportObsidian    :: Graph -> Analysis -> FilePath -> IO ()
  , epExportReport      :: Text -> FilePath -> IO ()
  , epExportCypher      :: Graph -> FilePath -> IO ()
  , epExportMemgraphCypher :: Graph -> FilePath -> IO ()
    -- | Neo4j push functions
  , epPushToNeo4jFull        :: Graph -> CommunityMap -> CohesionMap -> Text -> Text -> Text -> IO (Text, Int, Int)
  , epPushToNeo4jSubgraph    :: Graph -> CommunityMap -> CohesionMap -> Int -> [NodeId] -> Text -> Text -> Text -> IO (Text, Int, Int)
  , epPushToNeo4jCommunity   :: Graph -> CommunityMap -> CohesionMap -> Text -> Text -> Text -> IO (Text, Int, Int)
    -- | Neo4j edge repair
  , epPushEdgeRepair         :: Graph -> Text -> Text -> Text -> IO (Text, Int, Int)
    -- | Memgraph push functions
  , epPushToMemgraphFull        :: Graph -> CommunityMap -> CohesionMap -> Text -> Text -> Text -> IO (Text, Int, Int)
  , epPushToMemgraphSubgraph    :: Graph -> CommunityMap -> CohesionMap -> Int -> [NodeId] -> Text -> Text -> Text -> IO (Text, Int, Int)
  , epPushToMemgraphCommunity   :: Graph -> CommunityMap -> CohesionMap -> Text -> Text -> Text -> IO (Text, Int, Int)
    -- | Incremental JSON writer
  , epOpenIncrementalWriter   :: FilePath -> IO ()
  , epWriteNodes              :: () -> [Node] -> IO ()
  , epWriteEdges              :: () -> [Edge] -> IO ()
  , epWriteCommunities        :: () -> CommunityMap -> IO ()
  , epWriteCohesion           :: () -> CohesionMap -> IO ()
  , epWriteGodNodes           :: () -> [GodNode] -> IO ()
  , epWriteAnalysisTail       :: () -> Maybe (Map CommunityId Text) -> IO ()
  , epWriteCommunityAggregates :: () -> [CommunityAggregate] -> IO ()
  , epFlushWriter             :: () -> IO ()
  , epCloseWriter             :: () -> IO ()
    -- | Community graph export
  , epExportCommunityGraph    :: Graph -> CommunityMap -> FilePath -> IO ()
    -- | Checkpoint save
  , epSaveCheckpoint          :: Graph -> FilePath -> IO ()
    -- | Full export orchestration
  , epExportAll :: Graph -> FilePath -> Analysis -> PipelineConfig -> Detection -> Maybe (Map CommunityId Text) -> IO ExportResult
  }