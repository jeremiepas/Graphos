{-# LANGUAGE ScopedTypeVariables #-}
-- | Staged full-rebuild support.
--
-- A full rebuild writes its artifacts into a staging directory (sibling of the
-- real output directory, same filesystem). Only when the rebuild completes
-- successfully is the staging directory swapped into place via directory
-- renames. On any failure the staging directory is removed and the existing
-- output is left untouched.
--
-- The swap is two renames: existing @out@ → @out.prev-...@, staging → @out@,
-- then the old directory is deleted. If the process dies between the two
-- renames, the next run detects the missing output plus a leftover
-- @out.prev-*@ and rolls it back before sweeping stale staging dirs.
--
-- Multi-writer coordination is out of scope (see design non-goals): stale
-- staging/prev directories are swept at the start of each staged run.
module Graphos.UseCase.Pipeline.Staging
  ( withStagedOutput
  , stagingDirPrefix
  , prevDirPrefix
  , relocateStagedPath
  , carryOverEntries
  ) where

import Control.Exception
  ( SomeException
  , catch
  , throwIO
  )
import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , listDirectory
  , removeDirectoryRecursive
  , renameDirectory
  )
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Posix.Process (getProcessID)

import Graphos.Infrastructure.FileSystem.AtomicWrite (fsyncDirectory)

-- | Swallow exceptions from best-effort cleanup steps.
ignoreErr :: SomeException -> IO ()
ignoreErr _ = pure ()

-- | Prefix of the staging directories for a given final output directory.
stagingDirPrefix :: FilePath -> String
stagingDirPrefix final = takeFileName final ++ ".staging-"

-- | Prefix of the backup directories used while swapping.
prevDirPrefix :: FilePath -> String
prevDirPrefix final = takeFileName final ++ ".prev-"

-- | Run @action@ with a fresh staging directory path, then:
--
-- * @Right@ result  → swap the staging dir into @final@ and return.
-- * @Left@ result   → delete the staging dir and return the error.
-- * exception       → delete the staging dir and rethrow.
--
-- The existing @final@ directory is never modified unless the swap succeeds.
withStagedOutput :: FilePath -> (FilePath -> IO (Either Text a)) -> IO (Either Text a)
withStagedOutput final action = do
  recoverInterruptedSwap final
  sweepStaleDirs final
  parentDir <- pure (takeDirectory final)
  createDirectoryIfMissing True parentDir
  staging <- newStagingPath final
  createDirectoryIfMissing False staging
  result <- action staging `catch` \e -> cleanupStaging staging >> throwIO (e :: SomeException)
  case result of
    Left err -> do
      cleanupStaging staging
      pure (Left err)
    Right a -> do
      exists <- doesDirectoryExist final
      when exists (carryOverState final staging)
      swapIntoPlace staging final
      pure (Right a)

-- | Move persistent state (cache, memory, ...) from the old output directory
-- into the staging directory before the swap, so a rebuild does not lose it.
-- Best-effort: entries that cannot be moved are left in place.
carryOverState :: FilePath -> FilePath -> IO ()
carryOverState oldOut staging =
  mapM_ moveOne carryOverEntries
  where
    moveOne name = do
      let src = oldOut </> name
          dst = staging </> name
      srcExists <- doesDirectoryExist src
      when srcExists $
        renameDirectory src dst `catch` ignoreErr

-- | Unique staging directory path: @<final>.staging-<timestamp>-<pid>@.
newStagingPath :: FilePath -> IO FilePath
newStagingPath final = do
  now <- getCurrentTime
  pid <- getProcessID
  let stamp = formatTime defaultTimeLocale "%Y%m%dT%H%M%S%q" now
  pure (takeDirectory final </> (stagingDirPrefix final ++ stamp ++ "-" ++ show (toInteger pid)))

-- | Best-effort removal of a staging directory.
cleanupStaging :: FilePath -> IO ()
cleanupStaging staging =
  removeDirectoryRecursive staging `catch` ignoreErr

-- | Swap a fully-built staging directory into the final location.
swapIntoPlace :: FilePath -> FilePath -> IO ()
swapIntoPlace staging final = do
  exists <- doesDirectoryExist final
  if not exists
    then renameDirectory staging final
    else do
      prev <- newPrevPath final
      renameDirectory final prev
      renameDirectory staging final `catch` \e -> do
        -- Rollback: put the old output back in place.
        finalThere <- doesDirectoryExist final
        when finalThere (removeDirectoryRecursive final `catch` ignoreErr)
        renameDirectory prev final
        throwIO (e :: SomeException)
      removeDirectoryRecursive prev `catch` ignoreErr
  fsyncDirectory (takeDirectory final)

-- | Unique backup directory path: @<final>.prev-<timestamp>-<pid>@.
newPrevPath :: FilePath -> IO FilePath
newPrevPath final = do
  now <- getCurrentTime
  pid <- getProcessID
  let stamp = formatTime defaultTimeLocale "%Y%m%dT%H%M%S%q" now
  pure (takeDirectory final </> (prevDirPrefix final ++ stamp ++ "-" ++ show (toInteger pid)))

-- | If a previous run died between the two swap renames, the final directory
-- is missing while a @.prev-*@ backup still exists. Restore it.
recoverInterruptedSwap :: FilePath -> IO ()
recoverInterruptedSwap final = do
  finalExists <- doesDirectoryExist final
  unless finalExists $ do
    siblings <- listDirectory (takeDirectory final)
      `catch` \(_ :: SomeException) -> pure []
    case filter (isPrefixOf (prevDirPrefix final)) siblings of
      (prev:_) -> renameDirectory (takeDirectory final </> prev) final `catch` ignoreErr
      [] -> pure ()
  where
    isPrefixOf p s = take (length p) s == p

-- | Remove leftover staging and backup directories from earlier runs.
sweepStaleDirs :: FilePath -> IO ()
sweepStaleDirs final = do
  siblings <- listDirectory (takeDirectory final)
    `catch` \(_ :: SomeException) -> pure []
  let stale = filter (\d -> isPrefixOf (stagingDirPrefix final) d || isPrefixOf (prevDirPrefix final) d) siblings
  mapM_ (\d -> removeDirectoryRecursive (takeDirectory final </> d) `catch` ignoreErr) stale
  where
    isPrefixOf p s = take (length p) s == p

-- | Rewrite a path that pointed into the staging directory to point into the
-- final output directory (used to fix up result paths after the swap).
relocateStagedPath :: FilePath -> FilePath -> FilePath -> FilePath
relocateStagedPath staging final p =
  let (pre, rest) = splitAt (length staging) p
  in if pre == staging then final ++ rest else p

-- | Persistent state entries inside the output directory that a rebuild does
-- not regenerate: extraction cache, conversation memory, debug traces,
-- Q&A memory, cost log. These are moved from the old output into the staging
-- directory right before the swap so they survive the rebuild.
carryOverEntries :: [FilePath]
carryOverEntries = ["cache", "memory", "debug", "traces"]