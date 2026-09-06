-- | File detection - scan directory for supported files
module Graphos.UseCase.Detect
  ( detectFiles
  , detectFilesWithExtensions
  , detectFilesWithExtensionsAndIgnore
  , detectFilesWithExtensionsAndIgnore'
  , categorizeFiles
  , categorizeFilesWith
  , categorizeFilesWithConfig
  , classifyFile
  , fileMetaFromPath
  , allSupportedExtensions
  , hardcodedIgnoreDirNames
  , rootAnchoredIgnoreDirs
  , depthIndependentIgnoreDirs
  , isIgnoredEntry
  , isIgnoredEntryRoot
  , findAllFilesWithExclusions
  ) where

import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Control.Exception (try, IOException)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.IO (withFile, IOMode(..), hGetLine)
import System.FilePath (takeExtension, (</>))

import Graphos.Domain.Types
import Graphos.Domain.Types.Pipeline
  ( FileClass(..), DetectionConfig(..)
  , defaultDetectionConfig, FileMeta(..)
  , isSourceClass )
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..), AnnotatedPattern(..), IgnorePattern(..))
import Graphos.Infrastructure.FileSystem.Ignore (matches, matchingPattern)

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
    codeExts = [".py", ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs", ".go", ".rs", ".java", ".c", ".cpp", ".h", ".hpp"
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
      , detectionClassification = Map.empty
      , detectionExclusions   = emptyExclusionCounts
      }
    else do
      files <- findAllFiles root
      (categorized, classification) <- categorizeFilesWithConfig files allSupportedExtensions defaultDetectionConfig
      let totalFiles = sum (length <$> Map.elems categorized)
      pure Detection
        { detectionTotalFiles       = totalFiles
        , detectionTotalWords       = 0  -- word counting requires file reading
        , detectionNeedsGraph       = totalFiles > 0
        , detectionWarning          = if totalFiles > 200
                                        then Just $ T.pack $ "Large corpus: " ++ show totalFiles ++ " files"
                                        else Nothing
        , detectionFiles            = categorized
        , detectionClassification   = classification
        , detectionExclusions       = emptyExclusionCounts
        }

-- | Detect files in a directory using config-driven extension categories.
detectFilesWithExtensions :: FileSystemPort -> FilePath -> Map FileCategory [String] -> IO Detection
detectFilesWithExtensions fsp root extMap = detectFilesWithExtensionsAndIgnore fsp root extMap (const (pure ()))

-- | Detect files in a directory using config-driven extension categories and ignore patterns.
-- This is the primary entry point for the pipeline — it applies .gitignore and .graphosignore
-- patterns in addition to hardcoded directory ignores.
detectFilesWithExtensionsAndIgnore :: FileSystemPort -> FilePath -> Map FileCategory [String] -> (T.Text -> IO ()) -> IO Detection
detectFilesWithExtensionsAndIgnore fsp root extMap logDebug = do
  ignorePatterns <- fspLoadIgnorePatterns fsp root
  detectFilesWithExtensionsAndIgnore' fsp root extMap ignorePatterns logDebug

-- | Detect files in a directory using config-driven extension categories and ignore patterns.
-- Internal version that takes pre-loaded ignore patterns.
detectFilesWithExtensionsAndIgnore' :: FileSystemPort -> FilePath -> Map FileCategory [String] -> [AnnotatedPattern] -> (T.Text -> IO ()) -> IO Detection
detectFilesWithExtensionsAndIgnore' fsp root extMap ignorePatterns logDebug = do
  exists <- doesDirectoryExist root
  if not exists
    then pure Detection
      { detectionTotalFiles  = 0
      , detectionTotalWords  = 0
      , detectionNeedsGraph  = False
      , detectionWarning     = Just $ T.pack $ "Directory not found: " ++ root
      , detectionFiles        = Map.empty
      , detectionClassification = Map.empty
      , detectionExclusions   = emptyExclusionCounts
      }
    else do
      (files, excs) <- findAllFilesWithExclusions root root (fspShouldIgnore fsp) extMap ignorePatterns logDebug
      (categorized, classification) <- categorizeFilesWithConfig files extMap defaultDetectionConfig
      let totalFiles = sum (length <$> Map.elems categorized)
      pure Detection
        { detectionTotalFiles       = totalFiles
        , detectionTotalWords       = 0  -- word counting requires file reading
        , detectionNeedsGraph       = totalFiles > 0
        , detectionWarning          = if totalFiles > 200
                                        then Just $ T.pack $ "Large corpus: " ++ show totalFiles ++ " files"
                                        else Nothing
        , detectionFiles            = categorized
        , detectionClassification   = classification
        , detectionExclusions       = excs
        }

-- | Find all files recursively (using default extensions)
findAllFiles :: FilePath -> IO [FilePath]
findAllFiles dir = findAllFilesWith dir allSupportedExtensions

-- | Find all files recursively using config-driven extension map
findAllFilesWith :: FilePath -> Map FileCategory [String] -> IO [FilePath]
findAllFilesWith dir extMap = findAllFilesWithAndIgnore dir dir (\_ _ _ -> False) dir extMap [] (const (pure ()))

-- | Find all files recursively using config-driven extension map and ignore patterns.
-- The first argument is the scan root (for root-anchored ignore matching); the second
-- is the current directory being walked.
findAllFilesWithAndIgnore
  :: FilePath            -- ^ scan root, used to anchor build-output directory names
  -> FilePath            -- ^ current directory being walked
  -> (FilePath -> [AnnotatedPattern] -> FilePath -> Bool)
  -> FilePath            -- ^ unused legacy parameter (kept for backward compatibility)
  -> Map FileCategory [String]
  -> [AnnotatedPattern]
  -> (T.Text -> IO ())
  -> IO [FilePath]
findAllFilesWithAndIgnore scanRoot dir shouldIgnoreFn _ extMap ignorePatterns logDebug =
  fst <$> findAllFilesWithExclusions scanRoot dir shouldIgnoreFn extMap ignorePatterns logDebug

-- | Like 'findAllFilesWithAndIgnore' but also returns per-class exclusion counts.
-- A directory is counted once per class that excluded it. The counts let the
-- run report explain missing files without re-running the scan.
findAllFilesWithExclusions
  :: FilePath
  -> FilePath
  -> (FilePath -> [AnnotatedPattern] -> FilePath -> Bool)
  -> Map FileCategory [String]
  -> [AnnotatedPattern]
  -> (T.Text -> IO ())
  -> IO ([FilePath], ExclusionCounts)
findAllFilesWithExclusions scanRoot dir shouldIgnoreFn extMap ignorePatterns logDebug = do
  entries <- listDirectory dir
  results <- mapM (\entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then if isIgnoredEntryRoot scanRoot shouldIgnoreFn entry dir path ignorePatterns
             then pure ([], classifyExclusion scanRoot shouldIgnoreFn entry dir path ignorePatterns)
             else do
               (subFiles, subExc) <- findAllFilesWithExclusions scanRoot path shouldIgnoreFn extMap ignorePatterns logDebug
               pure (subFiles, subExc)
      else if isSupportedWith entry extMap
             then if shouldIgnoreFn scanRoot ignorePatterns path
                    then do
                      logIgnoredFile scanRoot ignorePatterns path logDebug
                      pure ([], emptyExclusionCounts { excIgnoredFiles = 1 })
                    else pure ([path], emptyExclusionCounts)
             else pure ([], emptyExclusionCounts)
    ) entries
  let (files, excs) = unzip results
      totalExc = foldr addExclusionCounts emptyExclusionCounts excs
  pure (concat files, totalExc)

-- | Log a per-file ignore decision at DEBUG: the file path and the pattern
-- that caused it to be ignored.
logIgnoredFile :: FilePath -> [AnnotatedPattern] -> FilePath -> (T.Text -> IO ()) -> IO ()
logIgnoredFile scanRoot ignorePatterns path logDebug =
  let relPath = relativize scanRoot path
      mPat = matchingPattern ignorePatterns relPath
  in case mPat of
    Just ap -> logDebug $ T.pack $ "Ignored " ++ path ++ " (matched pattern: " ++ show (apPattern ap) ++ ")"
    Nothing -> logDebug $ T.pack $ "Ignored " ++ path

-- | Classify a pruned directory into an exclusion class.
-- Determines which rule class caused the directory to be pruned, for
-- per-class accounting in the run report.
classifyExclusion
  :: FilePath
  -> (FilePath -> [AnnotatedPattern] -> FilePath -> Bool)
  -> String
  -> FilePath
  -> FilePath
  -> [AnnotatedPattern]
  -> ExclusionCounts
classifyExclusion scanRoot _shouldIgnoreFn entry parentPath entryPath ignorePatterns
  | entry `elem` depthIndependentIgnoreDirs = emptyExclusionCounts { excDepthIndependent = 1 }
  | entry `elem` rootAnchoredIgnoreDirs && parentPath == scanRoot = emptyExclusionCounts { excRootAnchored = 1 }
  | otherwise =
      let relPath = relativize scanRoot entryPath
          gitMatch = any (\ap -> apPriority ap == 1 && matches relPath (apPattern ap)) ignorePatterns
          graphosMatch = any (\ap -> apPriority ap == 2 && matches relPath (apPattern ap)) ignorePatterns
      in if graphosMatch
            then emptyExclusionCounts { excGraphosignore = 1 }
            else if gitMatch
                   then emptyExclusionCounts { excGitignore = 1 }
                   else emptyExclusionCounts { excUnexplained = 1 }

-- | Add two exclusion counts records element-wise.
addExclusionCounts :: ExclusionCounts -> ExclusionCounts -> ExclusionCounts
addExclusionCounts a b = ExclusionCounts
  { excRootAnchored     = excRootAnchored a + excRootAnchored b
  , excDepthIndependent = excDepthIndependent a + excDepthIndependent b
  , excGitignore        = excGitignore a + excGitignore b
  , excGraphosignore    = excGraphosignore a + excGraphosignore b
  , excUnexplained      = excUnexplained a + excUnexplained b
  , excIgnoredFiles     = excIgnoredFiles a + excIgnoredFiles b
  }

-- | Categorize files by type (using default extensions), excluding
-- generated/vendored/minified so only 'Source' files reach the Extract stage.
categorizeFiles :: [FilePath] -> IO (Map FileCategory [FilePath], Map FileClass [FilePath])
categorizeFiles files =
  categorizeFilesWithConfig files allSupportedExtensions defaultDetectionConfig

-- | Categorize files by type using config-driven extension map (Source-only).
categorizeFilesWith :: [FilePath] -> Map FileCategory [String] -> IO (Map FileCategory [FilePath], Map FileClass [FilePath])
categorizeFilesWith files extMap =
  categorizeFilesWithConfig files extMap defaultDetectionConfig

-- | Categorize files by type and provenance. Returns a pair:
--   * the 'Source'-only category map (what reaches Extract), and
--   * the full classification breakdown over every file.
-- Files classified as generated/vendored/minified are dropped from the
-- category map so they never reach extraction.
categorizeFilesWithConfig
  :: [FilePath]
  -> Map FileCategory [String]
  -> DetectionConfig
  -> IO (Map FileCategory [FilePath], Map FileClass [FilePath])
categorizeFilesWithConfig files extMap cfg = do
  classified <- mapM (\f -> do
        fm <- fileMetaFromPath cfg f
        pure (f, classifyFile cfg fm)
      ) files
  let sourceFiles = [ f | (f, c) <- classified, isSourceClass c ]
      byClass = Map.fromListWith (++ ) (reverse [ (c, [f]) | (f, c) <- classified ])
      categorized = Map.fromList
        [ (cat, filter (\f -> takeExtension f `elem` exts) sourceFiles)
        | (cat, exts) <- Map.toList extMap
        ]
  pure (categorized, byClass)

-- | Classify a single file by provenance using three heuristics. Pure and
-- side-effect free: it consumes only the 'FileMeta' view, so it is trivially
-- testable by constructing that record directly. Precedence is Vendored >
-- Generated > Minified > Source, with path-based vendored detection first
-- because it is structural and unambiguous.
classifyFile :: DetectionConfig -> FileMeta -> FileClass
classifyFile cfg fm =
  case catMaybes [classifyVendored cfg fm, classifyGenerated cfg fm, classifyMinified cfg fm] of
    (c : _) -> c
    []      -> Source

-- | Path-based vendored detection: any path segment matches a configured
-- vendored directory name (node_modules, vendor, third_party, ...).
classifyVendored :: DetectionConfig -> FileMeta -> Maybe FileClass
classifyVendored cfg fm =
  let parts = splitPath (metaPath fm)
  in if any (`elem` dcVendorSegments cfg) parts then Just Vendored else Nothing

-- | Header-sniff generated detection: a generator signature appears within the
-- leading content window, matched case-insensitively.
classifyGenerated :: DetectionConfig -> FileMeta -> Maybe FileClass
classifyGenerated cfg fm =
  let sigs = map T.toLower (dcGeneratorSignatures cfg)
      lines_ = map T.toLower (metaLeadingLines fm)
  in if any (\l -> any (`T.isInfixOf` l) sigs) lines_ then Just Generated else Nothing

-- | Minified detection: the longest line exceeds the configured threshold.
classifyMinified :: DetectionConfig -> FileMeta -> Maybe FileClass
classifyMinified cfg fm =
  if metaMaxLineLen fm > dcMinifiedLineThreshold cfg then Just Minified else Nothing

-- | Split a path into its '/'-separated components. Empty components (from
-- leading/trailing separators) are preserved; they never match a segment name.
splitPath :: FilePath -> [FilePath]
splitPath [] = []
splitPath p =
  let (seg, rest) = break (== '/') p
  in seg : case rest of
             ('/' : t) -> splitPath t
             _         -> []

-- | Build a 'FileMeta' for a path by reading only its leading content window.
-- IO boundary: reads at most 'dcLeadingLines' lines and stops at EOF or on a
-- non-UTF8 line rather than reading a huge generated file to completion.
fileMetaFromPath :: DetectionConfig -> FilePath -> IO FileMeta
fileMetaFromPath cfg path = do
  lines_ <- readLeadingLines path (dcLeadingLines cfg)
  let maxLen = foldr (\l mx -> max mx (T.length l)) 0 lines_
  pure FileMeta
    { metaPath = path
    , metaLeadingLines = lines_
    , metaMaxLineLen = maxLen
    }

-- | Read up to 'maxLines' lines from a file, stopping at EOF or on error.
readLeadingLines :: FilePath -> Int -> IO [T.Text]
readLeadingLines path maxLines = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else withFile path ReadMode $ \h -> loop maxLines [] h
  where
    loop 0 acc _ = pure (reverse acc)
    loop i acc h = do
      ln <- try (T.pack <$> hGetLine h) :: IO (Either IOException T.Text)
      case ln of
        Right l   -> loop (i - 1) (l : acc) h
        Left _    -> pure (reverse acc)

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
  :: (FilePath -> [AnnotatedPattern] -> FilePath -> Bool)  -- ^ file-based ignore matcher
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
--
-- Negation-first evaluation: a negation pattern (e.g. @!dist\/keep\/**@) from
-- @.graphosignore@ or @.gitignore@ is consulted before the hardcoded list,
-- so a user can re-include a directory that the hardcoded list would prune.
-- The hardcoded list is the lowest-priority layer, consistent with the
-- existing priority order (hardcoded 0, gitignore 1, graphosignore 2).
isIgnoredEntryRoot
  :: FilePath       -- ^ scan root
  -> (FilePath -> [AnnotatedPattern] -> FilePath -> Bool)  -- ^ file-based ignore matcher
  -> String        -- ^ directory entry (basename)
  -> FilePath       -- ^ path of the parent directory containing the entry
  -> FilePath       -- ^ full path to the directory entry
  -> [AnnotatedPattern]
  -> Bool
isIgnoredEntryRoot scanRoot shouldIgnoreFn entry parentPath entryPath ignorePatterns =
  let hardcodedMatch =
        entry `elem` depthIndependentIgnoreDirs
        || (entry `elem` rootAnchoredIgnoreDirs && parentPath == scanRoot)
  in if hardcodedMatch
       then not (negationCovers scanRoot ignorePatterns entryPath)
       else shouldIgnoreFn scanRoot ignorePatterns entryPath

-- | Check if any negation pattern in the list matches the given path, or
-- covers a path inside it (i.e. the directory is an ancestor of the
-- negation target). This is used to re-include a directory that the
-- hardcoded list would prune, so that files inside it can be reached by
-- negation patterns. For example, @!dist\/keep\/**@ means the @dist@
-- directory at the root should not be pruned, because the negation target
-- lives inside it.
negationCovers :: FilePath -> [AnnotatedPattern] -> FilePath -> Bool
negationCovers scanRoot patterns dirPath =
  let relDir = relativize scanRoot dirPath
  in any (\ap -> apNegate ap && coversPath (apPattern ap) relDir) patterns
  where
    -- A negation pattern covers a directory if it matches the directory
    -- path directly, or if the directory is an ancestor of the pattern's
    -- literal prefix (so files inside the directory would match the
    -- negation).
    coversPath (WildcardPattern p) path =
      matches path (WildcardPattern p)
      || dirIsAncestorOf p path
    coversPath other path = matches path other

-- | Make a path relative to the scan root, stripping any leading @./@.
relativize :: FilePath -> FilePath -> FilePath
relativize root path
  | root == "." = stripLeadingDotSlash path
  | root `isPrefixOf` path = drop (length root + 1) path  -- +1 for the /
  | otherwise = path
  where
    stripLeadingDotSlash ('.':'/':rest) = rest
    stripLeadingDotSlash s = s

-- | Check if a directory path is an ancestor of a pattern's literal path.
-- For example, @dist@ is an ancestor of @dist\/keep\/**@, so the @dist@
-- directory should not be pruned when a negation targets @dist\/keep\/**@.
dirIsAncestorOf :: String -> FilePath -> Bool
dirIsAncestorOf patternPath dirPath =
  let normDir = stripDotSlash dirPath
      normPat = stripDotSlash (takeLiteralPrefix patternPath)
  in not (null normPat) && not (null normDir)
     && normPat /= normDir
     && (normDir ++ "/") `isPrefixOf` normPat
  where
    stripDotSlash ('.':'/':rest) = rest
    stripDotSlash s = s
    -- Extract the literal (non-wildcard) prefix of a pattern path
    takeLiteralPrefix = takeWhile (/= '*')

-- | Check if a file has a supported extension (using config-driven extensions)
isSupportedWith :: String -> Map FileCategory [String] -> Bool
isSupportedWith f extMap = takeExtension f `elem` concat (Map.elems extMap)
