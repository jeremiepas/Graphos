-- | Crash-safe atomic writes for Graphos output artifacts.
--
-- The single buffered entry point, 'writeFileAtomic', writes content to a
-- temporary file in the target directory, fsyncs the file and its parent
-- directory, then renames the temp file into place. A reader therefore never
-- observes a partially written artifact: the final path is always either the
-- previous version or the complete new version, never a truncation.
--
-- The streaming helpers 'openAtomicTemp' and 'placeAtomicStreamed' let a caller
-- stream content incrementally into a same-directory temp file and place it
-- atomically on close, without buffering the whole payload in memory. This is
-- used for the (potentially large) graph.json artifact.
--
-- Every temp file is created in the target directory, so it shares the target
-- filesystem and the rename is atomic. If the temp file ever ends up on a
-- different filesystem the write logs a warning, because a cross-filesystem
-- rename would not be atomic.
--
-- All platform-specific durability (directory @fsync@ on Linux,
-- @fcntl(F_FULLFSYNC)@ on macOS / BSD, and filesystem detection) is hidden
-- behind the FFI helpers in "Graphos.Infrastructure.FileSystem.AtomicWrite.C".
{-# LANGUAGE BlockArguments #-}

module Graphos.Infrastructure.FileSystem.AtomicWrite
  ( writeFileAtomic
  , openAtomicTemp
  , placeAtomicStreamed
  , AtomicWriteFailure(..)
  ) where

import qualified Data.ByteString as BS
import Control.Exception (Exception, SomeException, catch, bracketOnError, displayException, throwIO)
import Control.Monad (when)
import qualified Data.IORef as IORef
import System.IO.Unsafe (unsafePerformIO)
import System.IO (Handle, openFile, hClose, IOMode(WriteMode))
import System.Directory
  ( createDirectoryIfMissing
  , makeAbsolute
  , renameFile
  , removeFile
  )
import System.FilePath (takeDirectory)
import System.Posix.Process (getProcessID)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CLong(..))

-- | @fsync@ a path read-only. @True@ selects the directory open flag and the
-- macOS / BSD @F_FULLFSYNC@ behaviour; returns 0 on success, -1 on failure.
foreign import ccall unsafe "hs_graphos_fsync_path"
  hsFsyncPath :: CString -> Bool -> IO CInt

-- | The @st_dev@ id of @path@, or -1 when @path@ cannot be stat'd.
foreign import ccall unsafe "hs_graphos_device_of"
  hsDeviceOf :: CString -> IO CLong

-- | Raised when an atomic write cannot guarantee crash-safe placement.
-- The message describes the step that failed (typically a filesystem flush).
data AtomicWriteFailure = AtomicWriteFailure !String

instance Show AtomicWriteFailure where
  show (AtomicWriteFailure m) = "AtomicWriteFailure: " ++ m

instance Exception AtomicWriteFailure

-- | Abort a write with a descriptive 'AtomicWriteFailure'.
raiseAtomicWriteFailure :: String -> IO a
raiseAtomicWriteFailure = throwIO . AtomicWriteFailure

-- WARNING (unsafePerformIO): tmpCounter is a module-local mutable counter used
-- only to make temp-file names unique. unsafePerformIO is safe here because the
-- IORef is created exactly once and only ever incremented; atomicModifyIORef'
-- hands out distinct values even under intra-process concurrency, so no caller
-- observes a value that depends on cross-thread ordering. Its only effect is a
-- unique suffix in a temporary filename, which has no externally observable
-- side effect.
tmpCounter :: IORef.IORef Integer
tmpCounter = unsafePerformIO (IORef.newIORef 0)

-- | Build a unique temp file path inside @dir@.
nextTempName :: FilePath -> String -> IO FilePath
nextTempName dir prefix = do
  pid <- getProcessID
  n   <- IORef.atomicModifyIORef' tmpCounter (\c -> (c + 1, c))
  return (dir ++ "/" ++ prefix ++ "-" ++ show pid ++ "-" ++ show n ++ ".tmp")

-- | Open a fresh temp file in the target directory for incremental streaming.
--
-- The temp shares the target directory (and therefore the target filesystem),
-- so the eventual 'placeAtomicStreamed' rename is atomic. The caller streams
-- into the returned handle and later places or discards the file; on failure
-- the caller is responsible for cleaning the temp up.
openAtomicTemp :: FilePath -> IO (FilePath, Handle)
openAtomicTemp targetPath = do
  absTarget <- makeAbsolute targetPath
  let absParent = takeDirectory absTarget
  createDirectoryIfMissing True absParent
  tmpPath <- nextTempName absParent "graphos-atomic"
  h <- openFile tmpPath WriteMode
  checkSameFs tmpPath absParent
  pure (tmpPath, h)

-- | Atomically place a streamed temp file into @target@.
--
-- The sequence is: fsync the temp file, rename it into place, then fsync the
-- parent directory. The caller must have closed the handle first. If any step
-- fails the target (if it already exists) is left untouched with its previous
-- contents intact, and an 'AtomicWriteFailure' is raised.
placeAtomicStreamed :: FilePath -> FilePath -> IO ()
-- first argument: the streamed temp file path; second: the destination path
placeAtomicStreamed tmpPath targetPath = do
  absTarget <- makeAbsolute targetPath
  let absParent = takeDirectory absTarget
  checkSameFs tmpPath absParent
  fsyncFile tmpPath
  renameFile tmpPath absTarget `catch` (handlePlaceFailure ("placeAtomicStreamed: failed to place file at " ++ absTarget))
  fsyncDirectory absParent

-- | Write @content@ atomically to @path@.
--
-- The sequence is:
--
-- > create parent dir -> write temp file in the target dir
-- > fsync the file -> rename into place -> fsync the directory
--
-- If any step fails, the temp file is removed and the target (if it already
-- exists) is left untouched with its previous contents intact.
writeFileAtomic :: FilePath -> BS.ByteString -> IO ()
writeFileAtomic targetPath content = do
  (tmpPath, h) <- openAtomicTemp targetPath
  bracketOnError
    (do
        BS.hPutStr h content
        hClose h
        pure tmpPath)
    (\tp -> removeFile tp `catch` ignoreException)
    \(tp) -> placeAtomicStreamed tp targetPath

ignoreException :: SomeException -> IO ()
ignoreException _ = pure ()

handlePlaceFailure :: String -> SomeException -> IO ()
handlePlaceFailure msg e = raiseAtomicWriteFailure (msg ++ ": " ++ displayException e)

checkSameFs :: FilePath -> FilePath -> IO ()
checkSameFs tmpPath absParent = withCString tmpPath $ \ct ->
  withCString absParent $ \cp -> do
    tmpDev     <- hsDeviceOf ct
    parentDev  <- hsDeviceOf cp
    when (isCrossFs tmpDev parentDev) $
      putStrLn
        "writeFileAtomic: warning: temp file is on a different filesystem than the target directory; the rename may not be atomic"

fsyncFile :: FilePath -> IO ()
fsyncFile path = withCString path $ \ct -> do
  ok <- hsFsyncPath ct False
  when (ok /= 0) (raiseAtomicWriteFailure ("writeFileAtomic: failed to fsync file " ++ path))

fsyncDirectory :: FilePath -> IO ()
fsyncDirectory dir = withCString dir $ \ct -> do
  ok <- hsFsyncPath ct True
  when (ok /= 0) (raiseAtomicWriteFailure ("writeFileAtomic: failed to fsync directory " ++ dir))

isCrossFs :: CLong -> CLong -> Bool
isCrossFs a b = a /= -1 && b /= -1 && a /= b
