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
import Data.List (sort)
import Data.Time (defaultTimeLocale, formatTime, UTCTime)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)

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
computeStatus 0 _ = "not_started"
computeStatus done total | done == total = "complete"
                         | otherwise       = "in_progress"

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

formatModTime :: UTCTime -> String
formatModTime t = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t

readChangeInfo :: FilePath -> IO (Maybe ChangeInfo)
readChangeInfo changeDir = do
  let tasksPath = combine' changeDir "tasks.md"
  exists <- doesFileExist tasksPath
  if not exists
    then return Nothing
    else do
      content <- readFile tasksPath
      mtime <- getModificationTime tasksPath
      let info = parseTasksMd (takeBaseName changeDir) content
          withMtime = info { changeLastModified = Just (formatModTime mtime) }
      return (Just withMtime)

readAllChangeInfos :: FilePath -> IO [ChangeInfo]
readAllChangeInfos changesRoot = do
  entries <- listDirectory changesRoot
  changeDirs <- filterM (\e -> doesDirectoryExist (combine' changesRoot e)) entries
  ordered <- forM (sort changeDirs) (\e -> readChangeInfo (combine' changesRoot e))
  return [ x | Just x <- ordered ]
