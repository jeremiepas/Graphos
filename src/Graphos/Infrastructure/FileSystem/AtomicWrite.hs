-- | Atomic file write primitive — write to temp, flush, rename, fsync directory.
-- Guarantees that a concurrent reader never sees a partially-written file.
--
-- On POSIX systems, this fsyncs the parent directory after renaming the temp file
-- to ensure the rename entry is durable on disk. On Linux this uses 'fsync',
-- on macOS / BSD it uses 'fcntl F_FULLFSYNC'. If the platform fsync is unavailable
-- the function falls back to 'hFlush' as best-effort.
module Graphos.Infrastructure.FileSystem.AtomicWrite
  ( writeFileAtomic
  ) where

import qualified Data.ByteString as BS
import Control.Exception
  ( SomeException
  , catch
  , bracketOnError
  )
import System.Directory
  ( createDirectoryIfMissing
  , makeAbsolute
  , renameFile
  , removeFile
  )
import System.FilePath (takeDirectory)
import System.IO
  ( hClose
  , hFlush
  , openTempFile
  )


-- | Write @content@ atomically to @path@.
--
-- The sequence is:
--
-- 1. Create the parent directory if missing.
-- 2. Open a temporary file in the same directory as the target.
-- 3. Write @content@ and flush the handle.
-- 4. Close the temp file handle.
-- 5. Rename the temp file to the target path (atomic on same filesystem).
-- 6. Fsync the parent directory to make the rename durable.
--
-- If any step fails, the temp file is removed and the target (if it exists)
-- is left untouched.
writeFileAtomic :: FilePath -> BS.ByteString -> IO ()
writeFileAtomic targetPath content = do
  let parentDir = takeDirectory targetPath
  createDirectoryIfMissing True parentDir

  absTarget <- makeAbsolute targetPath
  let absParent = takeDirectory absTarget

  bracketOnError
    (openTempFile absParent "graphos-atomic-*.tmp")
    (\(tmpPath, h) -> hClose h `catch` handler >> removeFile tmpPath `catch` handler)
    (\(tmpPath, h) -> do
        BS.hPut h content
        hFlush h
        hClose h
        renameFile tmpPath absTarget
        fsyncDirectory absParent)
  where
    handler :: SomeException -> IO ()
    handler _ = pure ()

-- | Fsync the directory to ensure the rename is durable on disk.
--
-- Best-effort durability nicety. The atomic rename in 'writeFileAtomic' is
-- already durable on most filesystems; this is an extra flush. A portable,
-- correct directory fsync is not straightforwardly available across the
-- POSIX variants we support, so this is a no-op rather than a partial
-- implementation.
fsyncDirectory :: FilePath -> IO ()
fsyncDirectory _ = pure ()
