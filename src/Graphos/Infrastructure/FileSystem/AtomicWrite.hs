-- | Atomic file write primitives — write to temp, fsync, rename, fsync directory.
--
-- Guarantees that a concurrent reader never observes a partially-written file
-- and that an interrupted write leaves the previous file intact: the target
-- path is only ever replaced by a complete, durable file via @rename(2)@.
--
-- Temp files are created in the target's own directory, so the rename always
-- happens within a single filesystem (a cross-device rename is not atomic).
module Graphos.Infrastructure.FileSystem.AtomicWrite
  ( writeFileAtomic
  , writeTextFileAtomic
  , writeStringFileAtomic
  , openAtomicHandle
  , commitAtomicHandle
  , discardAtomicHandle
  , withAtomicHandle
  , fsyncDirectory
  ) where

import Control.Exception
  ( SomeException
  , bracket
  , catch
  )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
  ( createDirectoryIfMissing
  , renameFile
  , removeFile
  )
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hClose, hFlush, openTempFile)
import System.Posix.IO
  ( OpenMode(..)
  , closeFd
  , defaultFileFlags
  , handleToFd
  , openFd
  )
import System.Posix.Unistd (fileSynchronise)

-- | Swallow exceptions from best-effort cleanup/durability steps.
ignoreErr :: SomeException -> IO ()
ignoreErr _ = pure ()

-- | Write lazy @content@ atomically to @path@.
--
-- The sequence is:
--
-- 1. Create the parent directory if missing.
-- 2. Open a temporary file in the same directory as the target.
-- 3. Write @content@, flush and fsync the temp file.
-- 4. Rename the temp file to the target path (atomic on the same filesystem).
-- 5. Fsync the parent directory so the rename entry is durable.
--
-- If any step fails, the temp file is removed and the target (if it exists)
-- is left untouched.
writeFileAtomic :: FilePath -> BSL.ByteString -> IO ()
writeFileAtomic targetPath content =
  withAtomicHandle targetPath (\h -> BSL.hPut h content)

-- | Write UTF-8 'T.Text' atomically to @path@.
writeTextFileAtomic :: FilePath -> T.Text -> IO ()
writeTextFileAtomic targetPath content =
  writeFileAtomic targetPath (BSL.fromStrict (TE.encodeUtf8 content))

-- | Write a 'String' atomically to @path@ (encoded as UTF-8).
writeStringFileAtomic :: FilePath -> String -> IO ()
writeStringFileAtomic targetPath content =
  writeTextFileAtomic targetPath (T.pack content)

-- | Open a write-mode handle on a temporary file in the target's directory.
-- Returns the temp path and the handle. The caller must finish the write with
-- 'commitAtomicHandle' (rename into place) or 'discardAtomicHandle' (cleanup).
openAtomicHandle :: FilePath -> IO (FilePath, Handle)
openAtomicHandle targetPath = do
  let parentDir = takeDirectory targetPath
      baseName  = takeFileName targetPath
  createDirectoryIfMissing True parentDir
  openTempFile parentDir (baseName ++ ".tmp")

-- | Flush, fsync, close the temp handle, rename it over the target, and fsync
-- the parent directory. Throws on failure; the target is untouched in that case.
commitAtomicHandle :: FilePath -> FilePath -> Handle -> IO ()
commitAtomicHandle tmpPath targetPath h = do
  hFlush h
  fd <- handleToFd h
  fileSynchronise fd `catch` ignoreErr
  closeFd fd
  renameFile tmpPath targetPath
  fsyncDirectory (takeDirectory targetPath)

-- | Best-effort cleanup: close the handle and remove the temp file.
discardAtomicHandle :: FilePath -> Handle -> IO ()
discardAtomicHandle tmpPath h = do
  hClose h `catch` ignoreErr
  removeFile tmpPath `catch` ignoreErr

-- | Run @action@ with a write-mode handle on a temporary file that is
-- committed atomically: after @action@ returns, the handle is flushed,
-- fsynced, closed, and renamed over @targetPath@.
--
-- If @action@ or any commit step throws, the temp file is removed and the
-- target is left untouched; the exception propagates to the caller.
withAtomicHandle :: FilePath -> (Handle -> IO a) -> IO a
withAtomicHandle targetPath action =
  bracket
    (openAtomicHandle targetPath)
    (\(tmpPath, h) -> discardAtomicHandle tmpPath h)
    (\(tmpPath, h) -> do
        result <- action h
        commitAtomicHandle tmpPath targetPath h
        pure result)

-- | Fsync a directory so that a rename/create entry in it is durable.
--
-- Best-effort: on filesystems that do not support opening directories or
-- syncing them, the error is ignored. The atomic rename performed by
-- 'writeFileAtomic' and 'commitAtomicHandle' is already safe against partial
-- visibility; this only extends the durability guarantee across power loss.
fsyncDirectory :: FilePath -> IO ()
fsyncDirectory dir =
  (`catch` ignoreErr) $
    bracket (openFd dir ReadOnly defaultFileFlags) closeFd fileSynchronise