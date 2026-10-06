-- | Port interface for file system operations.
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Only Domain types appear in signatures.
module Graphos.UseCase.Port.FileSystemPort
  ( -- * File system port
    FileSystemPort(..)
    -- * Pattern types for ignore patterns
  , AnnotatedPattern(..)
  , IgnorePattern(..)
  ) where

import Data.Text (Text)
import Graphos.Domain.Types (Extraction)
import Graphos.Domain.Types.Pipeline (PipelineCheckpoint)
import Graphos.Infrastructure.FileSystem.Ignore (AnnotatedPattern(..), IgnorePattern(..))

-- | Record-of-functions port for file system operations.
-- Only Domain types appear in signatures (the cache fingerprint arrives as a
-- precomputed text so Infrastructure stays UseCase-free).
data FileSystemPort = FileSystemPort
  { -- | Load pipeline checkpoint from output directory
    fspLoadCheckpoint    :: FilePath -> IO (Maybe PipelineCheckpoint)
    -- | Save pipeline checkpoint to output directory
  , fspSaveCheckpoint    :: FilePath -> PipelineCheckpoint -> IO ()
    -- | Clear pipeline checkpoint
  , fspClearCheckpoint   :: FilePath -> IO ()
    -- | Load ignore patterns from config and .gitignore
  , fspLoadIgnorePatterns :: FilePath -> IO [AnnotatedPattern]
    -- | Check if a path should be ignored given patterns (pure)
    -- First arg is the scan root for relativizing paths against ignore patterns
  , fspShouldIgnore      :: FilePath -> [AnnotatedPattern] -> FilePath -> Bool
    -- | Consult the persistent extraction cache for a file under the
    -- fingerprinted content key (wire-incremental-update 4.1). Nothing = miss.
  , fspLoadCachedExtraction :: Text -> FilePath -> FilePath -> IO (Maybe Extraction)
    -- | Write an extraction through to the persistent extraction cache under
    -- the fingerprinted content key.
  , fspSaveCachedExtraction :: Text -> FilePath -> Extraction -> FilePath -> IO ()
  }
