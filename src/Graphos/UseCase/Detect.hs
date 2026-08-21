-- | File detection - scan directory for supported files
module Graphos.UseCase.Detect
  ( detectFiles
  , detectFilesWithExtensions
  , detectFilesWithExtensionsAndIgnore
  , detectFilesWithExtensionsAndIgnore'
  , allSupportedExtensions
  , hardcodedIgnoreDirNames
  , rootAnchoredIgnoreDirs
  , depthIndependentIgnoreDirs
  , isIgnoredEntry
  , isIgnoredEntryRoot
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import Graphos.Domain.Types
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..), AnnotatedPattern(..))

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
  , (OfficeFiles, officeExts)
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
    officeExts = [".docx", ".pptx", ".xlsx", ".doc", ".ppt"]

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
detectFilesWithExtensions :: FileSystemPort -> FilePath -> Map FileCategory [String] -> IO Detection
detectFilesWithExtensions fsp root extMap = detectFilesWithExtensionsAndIgnore fsp root extMap

-- | Detect files in a directory using config-driven extension categories and ignore patterns.
-- This is the primary entry point for the pipeline — it applies .gitignore and .graphosignore
-- patterns in addition to hardcoded directory ignores.
detectFilesWithExtensionsAndIgnore :: FileSystemPort -> FilePath -> Map FileCategory [String] -> IO Detection
detectFilesWithExtensionsAndIgnore fsp root extMap = do
  ignorePatterns <- fspLoadIgnorePatterns fsp root
  detectFilesWithExtensionsAndIgnore' fsp root extMap ignorePatterns

-- | Detect files in a directory using config-driven extension categories and ignore patterns.
-- Internal version that takes pre-loaded ignore patterns.
detectFilesWithExtensionsAndIgnore' :: FileSystemPort -> FilePath -> Map FileCategory [String] -> [AnnotatedPattern] -> IO Detection
detectFilesWithExtensionsAndIgnore' fsp root extMap ignorePatterns = do
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
      files <- findAllFilesWithAndIgnore root root (fspShouldIgnore fsp) root extMap ignorePatterns
      let categorized = categorizeFilesWith files extMap
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

-- | Find all files recursively (using default extensions)
findAllFiles :: FilePath -> IO [FilePath]
findAllFiles dir = findAllFilesWith dir allSupportedExtensions

-- | Find all files recursively using config-driven extension map
findAllFilesWith :: FilePath -> Map FileCategory [String] -> IO [FilePath]
findAllFilesWith dir extMap = findAllFilesWithAndIgnore dir dir (\_ _ -> False) dir extMap []

-- | Find all files recursively using config-driven extension map and ignore patterns.
-- The first argument is the scan root (for root-anchored ignore matching); the second
-- is the current directory being walked.
findAllFilesWithAndIgnore
  :: FilePath            -- ^ scan root, used to anchor build-output directory names
  -> FilePath            -- ^ current directory being walked
  -> ([AnnotatedPattern] -> FilePath -> Bool)
  -> FilePath            -- ^ unused legacy parameter (kept for backward compatibility)
  -> Map FileCategory [String]
  -> [AnnotatedPattern]
  -> IO [FilePath]
findAllFilesWithAndIgnore scanRoot dir shouldIgnoreFn _ extMap ignorePatterns = do
  entries <- listDirectory dir
  fmap concat $ mapM (\entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir && not (isIgnoredEntryRoot scanRoot shouldIgnoreFn entry dir path ignorePatterns)
      then findAllFilesWithAndIgnore scanRoot path shouldIgnoreFn dir extMap ignorePatterns
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

-- | Build-output directory names that are pruned only when they appear as a
-- direct child of the scan root. A directory named @build@ nested inside a
-- source tree (e.g. @src\/domain\/build\/@) is a legitimate source directory
-- and SHALL be extracted. Users who want deep pruning can declare it
-- explicitly in @.graphosignore@.
rootAnchoredIgnoreDirs :: [String]
rootAnchoredIgnoreDirs =
  [ "build", "out", "target", "dist", "dist-newstyle", "DerivedData", ".build" ]

-- | Hardcoded directory names that are ignored at any depth (tooling/VCS state).
-- These never represent legitimate source directories.
depthIndependentIgnoreDirs :: [String]
depthIndependentIgnoreDirs =
  -- Version control
  [ ".git", ".svn", ".hg"
  -- Dependency/package directories
  , "node_modules", "bower_components", "vendor"
  , "__pypackages__", ".pnpm-store", ".yarn"
  -- Build outputs that are NOT root-anchored (caches, not the canonical build dir)
  , ".cache", ".sass-cache"
  -- Python caches
  , "__pycache__", ".pytest_cache", ".mypy_cache", ".tox"
  , ".venv", ".env"
  -- Haskell/Scala/Java
  , ".stack-work", ".gradle"
  -- JS/TS frameworks
  , ".next", ".nuxt"
  -- Rust
  , ".cargo"
  -- IDE/editor
  , ".idea", ".vscode", ".lsp", ".elixir_ls", ".clj-kondo"
  -- Nix
  , ".direnv"
  -- Graphos
  , "graphos-out", ".opencode", ".tmp", ".obsidian"
  -- Other
  , ".github", ".DS_Store", ".pdm-build"
  ]

-- | Hardcoded directory names that should always be ignored.
-- Kept for backward compatibility; prefer 'rootAnchoredIgnoreDirs' and
-- 'depthIndependentIgnoreDirs'. This is the union of the two classes.
hardcodedIgnoreDirNames :: [String]
hardcodedIgnoreDirNames = rootAnchoredIgnoreDirs ++ depthIndependentIgnoreDirs

-- | Check if a directory entry should be ignored (backward-compatible wrapper).
-- Defaults the scan root to "." and derives the parent directory from the
-- entry path. Prefer 'isIgnoredEntryRoot' when the scan root is known.
isIgnoredEntry
  :: ([AnnotatedPattern] -> FilePath -> Bool)  -- ^ file-based ignore matcher
  -> String        -- ^ directory entry (basename)
  -> FilePath       -- ^ full path to the directory entry
  -> [AnnotatedPattern]
  -> Bool
isIgnoredEntry shouldIgnoreFn entry entryPath ignorePatterns =
  let parentPath = reverse (dropWhile (/= '/') (reverse entryPath))
  in isIgnoredEntryRoot parentPath shouldIgnoreFn entry parentPath entryPath ignorePatterns

-- | Check if a directory entry should be ignored, given the scan root.
-- The scan root is used to anchor build-output directory names: @build@,
-- @out@, @target@, @dist@, @dist-newstyle@, @DerivedData@ and @.build@ are
-- pruned only when they are a direct child of the scan root.
isIgnoredEntryRoot
  :: FilePath       -- ^ scan root
  -> ([AnnotatedPattern] -> FilePath -> Bool)  -- ^ file-based ignore matcher
  -> String        -- ^ directory entry (basename)
  -> FilePath       -- ^ path of the parent directory containing the entry
  -> FilePath       -- ^ full path to the directory entry
  -> [AnnotatedPattern]
  -> Bool
isIgnoredEntryRoot scanRoot shouldIgnoreFn entry parentPath entryPath ignorePatterns =
  entry `elem` depthIndependentIgnoreDirs
  || (entry `elem` rootAnchoredIgnoreDirs && parentPath == scanRoot)
  || shouldIgnoreFn ignorePatterns entryPath

-- | Check if a file has a supported extension (using config-driven extensions)
isSupportedWith :: String -> Map FileCategory [String] -> Bool
isSupportedWith f extMap = takeExtension f `elem` concat (Map.elems extMap)
