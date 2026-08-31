-- | Atomic file operations for Graphos
module Graphos.Infrastructure.IO.Atomic
  ( writeFileAtomic
  ) where

import qualified Data.ByteString.Lazy as BSL
import System.Directory (renameFile, removeFile)
import System.FilePath (takeDirectory)
import System.IO (openTempFile, hClose)
import Control.Exception (bracketOnError)

-- | Writes a lazy ByteString to a file atomically using a temporary file and rename.
-- This ensures that partial writes are not observed and crashes leave the previous version intact.
writeFileAtomic :: FilePath -> BSL.ByteString -> IO ()
writeFileAtomic path content = do
  let dir = takeDirectory path
  bracketOnError
    (openTempFile dir ".graphos-tmp")
    (\(tmpPath, h) -> hClose h >> removeFile tmpPath)
    (\(tmpPath, h) -> do
       BSL.hPut h content
       hClose h
       renameFile tmpPath path)
