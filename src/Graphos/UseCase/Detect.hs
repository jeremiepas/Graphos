-- | File detection - scan directory for supported files
module Graphos.UseCase.Detect
  ( detectFiles
  , detectFilesWithExtensions
  , allSupportedExtensions
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import Graphos.Domain.Types

-- | All supported file extensions organized by category
-- Prefer using config-driven extensions from GraphosConfig when available.
-- This hardcoded default serves as fallback.
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
               ,".ex", ".exs", ".m", ".mm", ".jl", ".vue", ".svelte", ".dart", ".ps1"
               ,".nix", ".json"]  -- NEW: Nix and JSON
    docExts = [".md", ".txt", ".rst", ".adoc", ".org"
              ,".text", ".raml"]  -- NEW: plain text and RAML
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

-- | Detect files in a directory using config-driven extension categories.
detectFilesWithExtensions :: FilePath -> Map FileCategory [String] -> IO Detection
detectFilesWithExtensions root extMap = do
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
      files <- findAllFilesWith root extMap
      let categorized = categorizeFilesWith files extMap
          totalFiles = sum (length <$> Map.elems categorized)
      pure Detection
        { detectionTotalFiles  = totalFiles
        , detectionTotalWords  = 0
        , detectionNeedsGraph  = totalFiles > 0
        , detectionWarning     = if totalFiles > 200
                                 then Just $ T.pack $ "Large corpus: " ++ show totalFiles ++ " files"
                                 else Nothing
        , detectionFiles       = categorized
        }

-- | Find all files recursively (using default extensions)
findAllFiles :: FilePath -> IO [FilePath]
findAllFiles dir = findAllFilesWith dir allSupportedExtensions

-- | Find all files recursively using config-driven extension map
findAllFilesWith :: FilePath -> Map FileCategory [String] -> IO [FilePath]
findAllFilesWith dir extMap = do
  entries <- listDirectory dir
  fmap concat $ mapM (\entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir && not (isIgnored entry)
      then findAllFilesWith path extMap
      else if isSupportedWith entry extMap
           then pure [path]
           else pure []
    ) entries

-- | Categorize files by type (using default extensions)
categorizeFiles :: [FilePath] -> Map FileCategory [FilePath]
categorizeFiles files = categorizeFilesWith files allSupportedExtensions

-- | Categorize files by type using config-driven extension map
categorizeFilesWith :: [FilePath] -> Map FileCategory [String] -> Map FileCategory [FilePath]
categorizeFilesWith files extMap = Map.fromList
  [ (cat, filter (\f -> takeExtension f `elem` exts) files)
  | (cat, exts) <- Map.toList extMap
  ]

-- | Check if a directory entry should be ignored
isIgnored :: String -> Bool
isIgnored entry = entry `elem` [".git", "node_modules", "__pycache__", ".venv", "dist", "dist-newstyle", "build", ".stack-work", "graphos-out", ".opencode", ".tmp", ".obsidian", ".github"]

-- | Check if a file has a supported extension (using config-driven extensions)
isSupportedWith :: String -> Map FileCategory [String] -> Bool
isSupportedWith f extMap = takeExtension f `elem` concat (Map.elems extMap)