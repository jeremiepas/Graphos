-- | Port interface for extraction operations.
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Infrastructure.Wiring provides the concrete implementations.
-- Only Domain types appear in signatures — no Infrastructure types.
module Graphos.UseCase.Port.ExtractionPort
  ( -- * Extraction port
    ExtractionPort(..)
  ) where

import Data.ByteString (ByteString)
import Graphos.Domain.Types (Extraction, PipelineConfig, Detection)

-- | Record-of-functions port for extraction operations.
-- Each field corresponds to an Infrastructure capability that UseCase.Extract needs.
-- Inject via AppEnv; mock for testing.
--
-- Note: 'PipelineConfig' already contains extractor mode, granularity,
-- file extension, and LSP config — no separate LSP client parameter needed.
-- The concrete implementation in Wiring will handle LSP client lifecycle internally.
data ExtractionPort = ExtractionPort
  { -- | Extract all files matching the detection config
    epExtractAll          :: PipelineConfig -> Detection -> IO Extraction
    -- | Extract a group of files with a specific language server command
  , epExtractGroup       :: FilePath -> PipelineConfig -> (String, [FilePath]) -> IO [Extraction]
    -- | Extract a single file (stub / fallback)
  , epExtractFromFile   :: FilePath -> IO Extraction
    -- | Extract image from file path
  , epExtractImageFile  :: PipelineConfig -> FilePath -> IO Extraction
    -- | Extract image from raw bytes
  , epExtractImageBytes :: PipelineConfig -> FilePath -> ByteString -> IO Extraction
    -- | Extract changed files (incremental mode)
  , epExtractChanged    :: PipelineConfig -> [FilePath] -> IO Extraction
  }