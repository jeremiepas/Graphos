-- | Port interface for export operations.
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Only Domain types appear in signatures.
module Graphos.UseCase.Port.ExportPort
  ( -- * Export port
    ExportPort(..)
  , ExportResult(..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Graphos.Domain.Types (Analysis, CommunityId, PipelineConfig, Detection)

-- | Result of an export operation.
data ExportResult = ExportResult
  { erReport    :: FilePath
  , erJSON      :: FilePath
  , erHTML      :: Maybe FilePath
  , erObsidian  :: Maybe FilePath
  , erNeo4j     :: Maybe FilePath
  } deriving (Eq, Show)

-- | Record-of-functions port for export operations.
data ExportPort = ExportPort
  { -- | Export all formats (HTML, Obsidian, Report, JSON, Neo4j, Memgraph, CommunityGraph, SVG, GraphML)
    epExportAll :: FilePath -> Analysis -> PipelineConfig -> Detection -> Maybe (Map CommunityId Text) -> IO ExportResult
  }