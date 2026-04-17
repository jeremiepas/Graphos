-- | File detection - scan directory for supported files
module Graphos.UseCase.Detect
  ( detectFiles
  , allSupportedExtensions
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import Graphos.Domain.Types

-- | All supported file extensions organized by category
allSupportedExtensions :: Map FileCategory [String]
allSupportedExtensions = Map.fromList
  [ (CodeFiles, codeExts)
  , (DocFiles, docExts)
  , (PaperFiles, paperExts)
  , (ImageFiles, imageExts)
  , (VideoFiles, videoExts)
  ]
  where
    codeExts = [".py", ".ts", ".tsx", ".js", ".jsx", ".go", ".rs", ".java", ".c", ".cpp", ".h", ".hpp"
               ,".rb", ".cs", ".kt", ".kts", ".scala", ".php", ".swift", ".lua", ".zig", ".hs", ".lhs"
               ,".ex", ".exs", ".m", ".mm", ".jl", ".vue", ".svelte", ".dart", ".ps1"]
    docExts = [".md", ".txt", ".rst", ".adoc", ".org"]
    paperExts = [".pdf"]
    imageExts = [".png", ".jpg", ".jpeg", ".webp", ".gif"]
    videoExts = [".mp4", ".mov", ".mkv", ".webm", ".avi", ".m4v", ".mp3", ".wav", ".m4a", ".ogg"]

-- | Detect files in a directory
detectFiles :: FilePath -> IO Detection
detectFiles root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure Detection
      { detectionTotalFiles  = 0
      , detectionTotalWords  = 0
      , detectionNeedsGraph  = False
      , detectionWarning     = Just $ T.pack $ "Directory not found: " ++ root
      , detectionFiles        = Map.empty
      }
    else do
      files <- findAllFiles root
      let categorized = categorizeFiles files
          totalFiles = sum (length <$> Map.elems categorized)
      pure Detection
        { detectionTotalFiles  = totalFiles
        , detectionTotalWords  = 0  -- word counting requires file reading
        , detectionNeedsGraph  = totalFiles > 0
        , detectionWarning     = if totalFiles > 200
                                 then Just $ T.pack $ "Large corpus: " ++ show totalFiles ++ " files"
                                 else Nothing
        , detectionFiles       = categorized
        }

-- | Find all files recursively
findAllFiles :: FilePath -> IO [FilePath]
findAllFiles dir = do
  entries <- listDirectory dir
  fmap concat $ mapM (\entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir && not (isIgnored entry)
      then findAllFiles path
      else if isSupported entry
           then pure [path]
           else pure []
    ) entries
  where
    isIgnored entry = entry `elem` [".git", "node_modules", "__pycache__", ".venv", "dist", "dist-newstyle", "build", ".stack-work", "graphos-out", ".opencode", ".tmp", ".obsidian", ".github"]
    isSupported f = takeExtension f `elem` concat (Map.elems allSupportedExtensions)

-- | Categorize files by type
categorizeFiles :: [FilePath] -> Map FileCategory [FilePath]
categorizeFiles files = Map.fromList
  [ (cat, filter (\f -> takeExtension f `elem` exts) files)
  | (cat, exts) <- Map.toList allSupportedExtensions
  ]