module OpenSpecView.ChangeList
  ( ChangeName
  , TaskStatus
  , TaskCount
  , ChangeInfo(..)
  , parseTasksMd
  , computeStatus
  , readChangeInfo
  , readAllChangeInfos
  ) where

import Control.Monad (forM, filterM)
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Time (defaultTimeLocale, formatTime, UTCTime(..), diffUTCTime, fromGregorian)
import Data.Ord (comparing, Down(..))
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime,
                         listDirectory)

type ChangeName = String
type TaskStatus = String
type TaskCount = Int

data ChangeInfo = ChangeInfo
  { changeName         :: ChangeName
  , changeStatus       :: TaskStatus
  , changeCompleted    :: TaskCount
  , changeTotal        :: TaskCount
  , changeLastModified :: Maybe String
  } deriving (Eq, Show)

classifyTask :: String -> Maybe Bool
classifyTask s = case dropWhile (== ' ') s of
  '-':rest ->
    let rest' = dropWhile (== ' ') rest
    in case rest' of
       '[':c:']':_ -> Just (c == 'x' || c == 'X')
       _ -> Nothing
  _ -> Nothing

parseTaskCounts :: String -> (TaskCount, TaskCount)
parseTaskCounts = foldl' step (0, 0) . lines
  where
    step (done, total) line =
      case classifyTask line of
        Just True  -> (done + 1, total + 1)
        Just False -> (done, total + 1)
        Nothing    -> (done, total)

computeStatus :: TaskCount -> TaskCount -> TaskStatus
computeStatus done total
  | total == 0    = "no-tasks"
  | done == total = "complete"
  | otherwise      = "in-progress"

parseTasksMd :: ChangeName -> String -> ChangeInfo
parseTasksMd name content =
  let (done, total) = parseTaskCounts content
  in ChangeInfo
        { changeName = name
        , changeStatus = computeStatus done total
        , changeCompleted = done
        , changeTotal = total
        , changeLastModified = Nothing
        }

combine' :: FilePath -> FilePath -> FilePath
combine' a b
  | not (null a) && last a == '/' = a ++ b
  | null a = b
  | otherwise = a ++ "/" ++ b

takeBaseName :: FilePath -> FilePath
takeBaseName fp = reverse (takeWhile (/= '/') (reverse fp))

pad3 :: Integer -> String
pad3 n = take 3 (show n ++ "000")

-- Encode an mtime the way `openspec list --json` reports lastModified: a UTC
-- timestamp rounded to milliseconds with a trailing Z. Callers must pass the
-- max mtime across a change's nested files.
formatModTime :: UTCTime -> IO String
formatModTime t = do
  let msTotal = round (diffUTCTime t epoch * 1000) :: Integer
  pure (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" t
        ++ "." ++ pad3 (msTotal `mod` 1000) ++ "Z")

-- Unix epoch (1970-01-01T00:00:00Z) used as the mtime reference.
epoch :: UTCTime
epoch = UTCTime { utctDay = fromGregorian 1970 1 1, utctDayTime = 0 }

-- Recursively collect every regular file beneath a directory.
listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively path = do
  entries <- listDirectory path
  chunks <- forM entries (\e ->
    let p = combine' path e in
    doesDirectoryExist p >>= \isDir ->
      if isDir then listFilesRecursively p else return [p])
  return (concat chunks)

-- Max mtime across all files in a change directory (recursive).
maxMtime :: FilePath -> IO (Maybe UTCTime)
maxMtime changeDir = do
  files <- listFilesRecursively changeDir
  if null files
    then return Nothing
    else do
      mtimes <- forM files (\f -> fmap Just (getModificationTime f))
      pure (Just (maximum (catMaybes mtimes)))

readChangeInfo :: FilePath -> IO (Maybe ChangeInfo)
readChangeInfo changeDir = do
  let tasksPath = combine' changeDir "tasks.md"
  hasTasks <- doesFileExist tasksPath
  content <- if hasTasks then readFile tasksPath else return ""
  mtime <- maxMtime changeDir
  info0 <- pure (parseTasksMd (takeBaseName changeDir) content)
  mtimeStr <- case mtime of
    Just t -> fmap Just (formatModTime t)
    Nothing -> pure Nothing
  pure (Just (info0 { changeLastModified = mtimeStr }))

readAllChangeInfos :: FilePath -> IO [ChangeInfo]
readAllChangeInfos changesRoot = do
  entries <- listDirectory changesRoot
  dirs <- filterM (\e -> do
                    isDir <- doesDirectoryExist (combine' changesRoot e)
                    pure (isDir && e /= "archive")) entries
  ordered <- forM dirs (\e -> readChangeInfo (combine' changesRoot e))
  let allInfos = [ x | Just x <- ordered ]
  -- Sort by lastModified descending, breaking ties by name ascending so the
  -- ordering matches `openspec list --json` exactly.
  pure (sortBy (comparing (fmap Down changeLastModified) <> comparing changeName) allInfos)
