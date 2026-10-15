-- | Change-listing capability for the @openspec-view@ workspace inspector.
--
-- Parses the @openspec/changes@ directory into 'ChangeInfo' rows by counting
-- task checkboxes in each change's @tasks.md@, and renders them back to text
-- in the same shape as @openspec list@.
module Graphos.OpenSpecView.List
  ( parseChanges
  , parseChangeInfo
  , parseTaskCounts
  , renderChangeList
  ) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, sortOn)
import Data.Maybe (Maybe(..), catMaybes)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Prelude (IO, concat, dropWhile, filter, fst, Int, length, lines, map, mapM, maximum, not, pure, readFile, reverse, show, String, (==), (/=), (<$>), (||), snd)
import Graphos.OpenSpecView.Types
import System.Directory
import System.FilePath (FilePath, (</>), takeFileName)

-- | Parse every change directory under @root@ (skipping @archive@) into a list
-- sorted by newest 'ChangeInfo.lastModified' first.
parseChanges :: FilePath -> IO [ChangeInfo]
parseChanges root = do
  entries <- listDirectory root
  infos <- mapM parseChangeInfo (map (root </>) entries)
  let kept = L.filter (\c -> changeName c /= "archive") (catMaybes infos)
  pure (reverse (sortOn lastModified kept))

-- | Parse a single change directory. Returns 'Nothing' when the path is not a
-- directory (so callers can safely map over any directory listing).
parseChangeInfo :: FilePath -> IO (Maybe ChangeInfo)
parseChangeInfo dir = do
  isDir <- doesDirectoryExist dir
  when (not isDir) (pure ())
  nm <- pure (T.pack (takeFileName dir))
  let tasksPath = dir </> "tasks.md"
  hasTasks <- doesFileExist tasksPath
  counts <- if hasTasks
    then parseTaskCounts <$> readFile tasksPath
    else pure (0, 0)
  if isDir
    then do
      mtime <- newestMtime dir
      pure (Just (ChangeInfo nm (parseStatus (fst counts) (snd counts)) (fst counts) (snd counts) mtime))
    else pure Nothing

-- | Count completed and total task boxes in a @tasks.md@ body.
--
-- A line counts as a box when it starts with @- [@ or @-[@ after stripping
-- leading spaces; it counts as complete when the lowercased line contains
-- @[x]@ or @[-x]@. This matches the checkbox convention used by @openspec list@
-- (verified against real changes such as @multi-source-graphs@ = 2/19).
parseTaskCounts :: String -> (Int, Int)
parseTaskCounts ls =
  (length (filter isChecked boxes), length boxes)
  where
    boxes = filter isBoxLine (lines ls)
    isBoxLine l =
      let s = map toLower (dropWhile (== ' ') l)
      in "- [" `isPrefixOf` s || "-[" `isPrefixOf` s
    isChecked l = "[x]" `isInfixOf` map toLower l || "[-x]" `isInfixOf` map toLower l

-- | Render change rows in the same layout as @openspec list@:
-- @<name>  <done>/<total> tasks  <age> ago@.
renderChangeList :: [ChangeInfo] -> Text
renderChangeList cs = T.intercalate "\n" (map renderRow cs)
  where
    renderRow c = T.concat [
        changeName c, " ",
        T.pack (show (completedTasks c)), "/", T.pack (show (totalTasks c)),
        " tasks ", ageLabel (lastModified c) ]

-- | Minimal human age label; intentionally clock-independent so tests stay
-- deterministic (callers can inject a reference time when real ages are needed).
ageLabel :: UTCTime -> Text
ageLabel _ = "just now"

-- | Newest modification time of any regular file beneath @dir@, falling back to
-- the directory's own mtime when it contains no files.
newestMtime :: FilePath -> IO UTCTime
newestMtime dir = do
  files <- collectFiles dir
  times <- mapM getModificationTime files
  case times of
    [] -> getModificationTime dir
    _  -> pure (maximum times)

-- | Recursively collect every regular file beneath @dir@.
collectFiles :: FilePath -> IO [FilePath]
collectFiles d = do
  entries <- listDirectory d
  branches <- mapM (\x ->
    let p = d </> x in do
      isDir <- doesDirectoryExist p
      if isDir
        then collectFiles p
        else do exists <- doesFileExist p; pure (if exists then [p] else [])) entries
  pure (concat branches)
