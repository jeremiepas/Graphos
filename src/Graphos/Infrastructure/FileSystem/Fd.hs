-- | Conversion between raw POSIX file descriptors and Haskell 'Handle's.
--
-- This exists so that code holding a 'Handle' on a temp file can reach the
-- raw descriptor for POSIX-level operations (fsync, raw close) without
-- leaving the Handle half-closed. GHC's 'handleToFd' detaches the descriptor
-- from the handle; 'hToFd' pairs it with an explicit 'hClose' so both
-- resources are released exactly once.
module Graphos.Infrastructure.FileSystem.Fd
  ( hToFd
  , fdSyncAndClose
  ) where

import Control.Exception (SomeException, catch)
import System.IO (Handle, hClose)
import System.Posix.IO (closeFd, handleToFd)
import System.Posix.Types (Fd)
import System.Posix.Unistd (fileSynchronise)

-- | Swallow exceptions from best-effort durability steps.
ignoreErr :: SomeException -> IO ()
ignoreErr _ = pure ()

-- | Detach the underlying POSIX file descriptor from a 'Handle' and close
-- the handle.
--
-- After a successful 'handleToFd' the descriptor is solely owned by the
-- caller and the handle no longer references it, so the subsequent 'hClose'
-- is a no-op on the descriptor and cannot fail on it in practice; any
-- 'hClose' error propagates to the caller. The returned 'Fd' must be
-- released exactly once by the caller (e.g. with 'fdSyncAndClose').
hToFd :: Handle -> IO Fd
hToFd h = do
  fd <- handleToFd h
  hClose h
  pure fd

-- | Flush a descriptor's kernel buffers to disk, then close it.
--
-- Best-effort durability: a sync failure (e.g. a filesystem that does not
-- support directory/file syncing) is ignored — matching the atomic-write
-- contract, where the atomic rename already guards against partial
-- visibility and only the power-loss window is affected. The descriptor is
-- always closed exactly once.
fdSyncAndClose :: Fd -> IO ()
fdSyncAndClose fd = do
  fileSynchronise fd `catch` ignoreErr
  closeFd fd