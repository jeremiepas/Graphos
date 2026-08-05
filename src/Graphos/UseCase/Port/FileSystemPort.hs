-- | Port interface for file system operations.
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Only Domain types appear in signatures.
module Graphos.UseCase.Port.FileSystemPort
  ( -- * File system port
    FileSystemPort(..)
  ) where

import Graphos.Domain.Types.Pipeline (PipelineCheckpoint)
import Graphos.Infrastructure.FileSystem.Ignore (AnnotatedPattern)

-- | Record-of-functions port for file system operations.
data FileSystemPort = FileSystemPort
  { -- | Load pipeline checkpoint from output directory
    fspLoadCheckpoint    :: FilePath -> IO (Maybe PipelineCheckpoint)
    -- | Save pipeline checkpoint to output directory
  , fspSaveCheckpoint    :: FilePath -> PipelineCheckpoint -> IO ()
    -- | Clear pipeline checkpoint
  , fspClearCheckpoint   :: FilePath -> IO ()
    -- | Load ignore patterns from config and .gitignore
  , fspLoadIgnorePatterns :: FilePath -> IO [AnnotatedPattern]
    }