-- | Ignore pattern handling for .graphosignore and .gitignore files.
-- Supports layered pattern merging: hardcoded defaults → .gitignore → .graphosignore
-- with .graphosignore having highest priority (can negate lower-priority patterns).
module Graphos.Infrastructure.FileSystem.Ignore
  ( -- * Pattern types
    IgnorePattern(..)
  , NegatePattern(..)
  , AnnotatedPattern(..)

    -- * Loading patterns from files
  , loadGraphosignore
  , loadGitignore
  , loadIgnorePatterns

    -- * Pattern matching
  , shouldIgnore
  , matches
  , matchesAnnotated
  , ignoreMatches
  , relativize

    -- * Pattern merging
  , mergeIgnorePatterns

    -- * Parsing
  , parsePattern

    -- * Hardcoded defaults
    , hardcodedIgnorePatterns
    , rootAnchoredIgnorePatterns
    ) where

import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import System.Directory (doesFileExist)
import System.FilePath (takeFileName)

-- | A simplified ignore pattern
data IgnorePattern
  = PrefixPattern String    -- ^ e.g., "build/" matches any path containing "build/" as a directory prefix
  | SuffixPattern String    -- ^ e.g., ".min.js" matches any path ending with ".min.js"
  | ExactPattern String     -- ^ e.g., "node_modules" matches exactly as a directory name
  | GlobPattern String      -- ^ e.g., "*.log" — legacy suffix-match pattern (kept for backward compat)
  | WildcardPattern String  -- ^ e.g., "result-*", ".ghc.environment.*" — fnmatch-style wildcard matching
  deriving (Eq, Show)

-- | A negation pattern — re-includes a path that would otherwise be ignored.
-- Takes priority over positive patterns from lower-priority sources.
newtype NegatePattern = NegatePattern IgnorePattern
  deriving (Eq, Show)

-- | An annotated pattern that tracks its source priority.
-- Higher priority sources override lower ones.
-- Priority: hardcoded (0) < gitignore (1) < graphosignore (2)
data AnnotatedPattern = AnnotatedPattern
  { apPattern :: IgnorePattern
  , apNegate  :: Bool            -- ^ True means this pattern negates (re-includes)
  , apPriority :: Int            -- ^ Source priority: 0=hardcoded, 1=gitignore, 2=graphosignore
  } deriving (Eq, Show)

-- | Check if a file path should be ignored given a list of annotated patterns.
-- A path is ignored if any positive pattern matches AND no higher-priority
-- negation pattern matches. Negation patterns from higher-priority sources
-- override positive patterns from lower-priority sources.
shouldIgnore :: [AnnotatedPattern] -> FilePath -> Bool
shouldIgnore patterns path =
  let -- Check if any positive pattern matches
      positiveMatch = any (\ap -> not (apNegate ap) && matches path (apPattern ap)) patterns
      -- Check if any negation pattern with higher priority overrides
      negationOverride = any (\ap -> apNegate ap && matches path (apPattern ap)) patterns
  in positiveMatch && not negationOverride

-- | Check if a path matches a specific pattern
matches :: FilePath -> IgnorePattern -> Bool
matches path (PrefixPattern p)    = p `isPrefixOf` path || ("/" ++ p) `isInfixOf` path
matches path (SuffixPattern p)    = p `isSuffixOf` path
matches path (ExactPattern p)     = p == path
                                          || ("/" ++ p ++ "/") `isInfixOf` ("/" ++ path ++ "/")
                                          || (p ++ "/") `isPrefixOf` path
                                          || ("/" ++ p) `isSuffixOf` path
matches path (GlobPattern p)     = p `isSuffixOf` path
matches path (WildcardPattern p)
  -- If the pattern contains a path separator, match against the full path.
  -- Otherwise, match against the filename component only (git semantics: *.log matches
  -- any file named *.log at any depth, but * alone does not cross directory boundaries).
  | '/' `elem` p = wildcardMatch p path
  | otherwise     = wildcardMatch p (takeFileName path)

-- | Check if a path matches an annotated pattern (convenience wrapper)
matchesAnnotated :: FilePath -> AnnotatedPattern -> Bool
matchesAnnotated path ap = matches path (apPattern ap)

-- ───────────────────────────────────────────────
-- Wildcard matching (fnmatch-style)
-- ───────────────────────────────────────────────

-- | Token for wildcard pattern segmentation.
data WcToken = WcLit String | WcStar | WcDoubleStar
  deriving (Eq, Show)

-- | Fnmatch-style wildcard matching.
--   * matches zero or more characters within a single path segment (excluding \/)
--   ** matches zero or more characters including \/ (recursive directory match)
--
-- Pattern segments are matched left-to-right.  A literal segment must appear
-- in the path at the expected position; a * segment matches any run of
-- non-\/ characters; a ** segment matches any run of characters including \/.
wildcardMatch :: String -> FilePath -> Bool
wildcardMatch pat path = wcMatch (wcTokenize pat) (path ++ "/")

-- | Tokenize a wildcard pattern into literal, Star, and DoubleStar segments.
wcTokenize :: String -> [WcToken]
wcTokenize [] = []
wcTokenize ('*':'*':rest) = WcDoubleStar : wcTokenize rest
wcTokenize ('*':rest)     = WcStar : wcTokenize rest
wcTokenize cs =
  let (lit, rest) = break (== '*') cs
  in if null lit then wcTokenize rest else WcLit lit : wcTokenize rest

-- | Match a list of tokens against a path string.
--   We append a trailing \/ to the path so that patterns like "dir\/"
--   which produce a trailing Lit "slash" match correctly.
wcMatch :: [WcToken] -> String -> Bool
wcMatch [] _ = True
wcMatch (WcDoubleStar : rest) path = any (wcMatch rest . snd) (wcSplits path)
wcMatch (WcStar : rest) path =
  -- * matches zero or more characters (excluding /)
  wcMatch rest path  -- zero characters consumed
  || any (\(pref, suff) -> '/' `notElem` pref && wcMatch rest suff) (wcInitsSplits path)
wcMatch (WcLit lit : rest) path =
  let litLen = length lit
  in litLen <= length path
     && take litLen path == lit
     && wcMatch rest (drop litLen path)

-- | All ways to split a string into prefix and suffix (including empty prefix).
wcSplits :: String -> [(String, String)]
wcSplits [] = [("", "")]
wcSplits s = [("", s)] ++ [(take i s, drop i s) | i <- [1..length s]]

-- | Like wcSplits but excludes empty prefix (Star must consume at least one char before trying rest).
wcInitsSplits :: String -> [(String, String)]
wcInitsSplits [] = []
wcInitsSplits s = [(take i s, drop i s) | i <- [1..length s]]

-- ───────────────────────────────────────────────
-- Loading patterns from files
-- ───────────────────────────────────────────────

-- | Load .graphosignore patterns from root directory (priority 2)
-- Handles negation patterns (!) the same way .gitignore does.
loadGraphosignore :: FilePath -> IO [AnnotatedPattern]
loadGraphosignore root = do
  let ignoreFile = root ++ "/.graphosignore"
  exists <- doesFileExist ignoreFile
  if not exists
    then pure []
    else do
      contents <- readFile ignoreFile
      pure $ map (parseGitignoreLine 2) $ filter (not . isCommentOrBlank) (lines contents)

-- | Load .gitignore patterns from root directory (priority 1)
-- Supports a simplified subset of .gitignore syntax:
--   * Blank lines and comments (#) are skipped
--   * Lines starting with ! are negation patterns (re-include)
--   * Lines ending with / are directory patterns (PrefixPattern)
--   * Lines containing * are wildcard patterns (WildcardPattern, fnmatch-style)
--   * Lines starting with / are anchored exact patterns (ExactPattern)
--   * Everything else is an exact match pattern (ExactPattern)
loadGitignore :: FilePath -> IO [AnnotatedPattern]
loadGitignore root = do
  let ignoreFile = root ++ "/.gitignore"
  exists <- doesFileExist ignoreFile
  if not exists
    then pure []
    else do
      contents <- readFile ignoreFile
      pure $ map (parseGitignoreLine 1) $ filter (not . isCommentOrBlank) (lines contents)

-- | Load all ignore patterns from all sources, merged with priority ordering.
-- Returns patterns in priority order: hardcoded (0) → gitignore (1) → graphosignore (2)
loadIgnorePatterns :: FilePath -> IO [AnnotatedPattern]
loadIgnorePatterns root = do
  gitPatterns <- loadGitignore root
  graphosPatterns <- loadGraphosignore root
  pure $ hardcodedIgnorePatterns ++ gitPatterns ++ graphosPatterns

-- ───────────────────────────────────────────────
-- Merging patterns
-- ───────────────────────────────────────────────

-- | Merge multiple lists of annotated patterns.
-- Simply concatenates them; priority is determined by the apPriority field.
mergeIgnorePatterns :: [[AnnotatedPattern]] -> [AnnotatedPattern]
mergeIgnorePatterns = concat

-- ───────────────────────────────────────────────
-- Hardcoded ignore patterns (priority 0)
-- ───────────────────────────────────────────────

-- | Build-output directory names that are pruned only when they appear as a
-- direct child of the scan root. Mirrors 'Graphos.UseCase.Detect.rootAnchoredIgnoreDirs'.
-- These are emitted as hardcoded patterns (priority 0) so that a higher-priority
-- negation in @.graphosignore@ (e.g. @!src\/**\/build\/**@) can re-include them.
rootAnchoredIgnorePatterns :: [AnnotatedPattern]
rootAnchoredIgnorePatterns = map (annotatePattern 0) rootAnchoredDirs
  where
    rootAnchoredDirs = ["build", "out", "target", "dist", "dist-newstyle", "DerivedData", ".build"]

-- | Hardcoded directory names that should always be ignored at any depth.
-- These cover dependency directories, IDE folders, and cache directories across
-- all major ecosystems. The build-output names (@build@, @out@, @target@,
-- @dist@, @dist-newstyle@, @DerivedData@, @.build@) are deliberately excluded:
-- they are root-anchored in 'Graphos.UseCase.Detect' (pruned only at the scan
-- root) and must not be pruned at arbitrary depth by the pattern path.
hardcodedIgnorePatterns :: [AnnotatedPattern]
hardcodedIgnorePatterns = map (annotatePattern 0) hardcodedDirs
  where
    hardcodedDirs =
      -- Version control
      [ ".git", ".svn", ".hg"
      -- Dependency/package directories
      , "node_modules", "bower_components", "vendor"
      , "__pypackages__", ".pnpm-store", ".yarn"
       -- Build-output directory names (build, out, target, dist, dist-newstyle,
       -- DerivedData, .build) are intentionally NOT here: they are root-anchored in
       -- Graphos.UseCase.Detect and must not be pruned at arbitrary depth by the
       -- pattern path. Only depth-independent caches remain.
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

-- ───────────────────────────────────────────────
-- Parsing helpers
-- ───────────────────────────────────────────────

-- | Parse a .graphosignore line into an IgnorePattern
parsePattern :: String -> IgnorePattern
parsePattern line =
  let trimmed = reverse (dropWhile (== ' ') (reverse (dropWhile (== ' ') line)))
  in case trimmed of
    -- Wildcard patterns: * anywhere in the line → WildcardPattern (fnmatch-style)
    _ | '*' `elem` trimmed -> WildcardPattern trimmed
    -- Directory patterns: trailing / → PrefixPattern
    _ | lastOrDefault trimmed == '/' -> PrefixPattern (init trimmed)
    -- Anchored patterns: leading / → ExactPattern
    _ | headOrDefault trimmed == '/' -> ExactPattern trimmed
    -- Everything else → ExactPattern (filenames, directory names)
    _ -> ExactPattern trimmed
  where
    lastOrDefault [] = ' '
    lastOrDefault xs = last xs
    headOrDefault [] = ' '
    headOrDefault (x:_) = x

-- | Parse a .gitignore line into an AnnotatedPattern with given priority.
-- Handles negation (!) prefix by creating a negation pattern.
parseGitignoreLine :: Int -> String -> AnnotatedPattern
parseGitignoreLine priority line =
  let trimmed = dropWhile (== ' ') line
  in case trimmed of
    ('!':rest) -> let patStr = dropWhile (== ' ') rest
                  in AnnotatedPattern
                     { apPattern = parsePattern patStr
                     , apNegate = True
                     , apPriority = priority
                     }
    _ -> AnnotatedPattern
         { apPattern = parsePattern trimmed
         , apNegate = False
         , apPriority = priority
         }

-- | Annotate a simple pattern string with a priority (non-negated)
annotatePattern :: Int -> String -> AnnotatedPattern
annotatePattern priority line = AnnotatedPattern
  { apPattern = parsePattern line
  , apNegate = False
  , apPriority = priority
  }

-- | Check if a line is a comment or blank
isCommentOrBlank :: String -> Bool
isCommentOrBlank line =
  let trimmed = dropWhile (== ' ') line
  in null trimmed || case trimmed of ('#':_) -> True; _ -> False

-- | Make a path relative to the scan root, stripping any leading @./@.
relativize :: FilePath -> FilePath -> FilePath
relativize root path
  | root == "." = stripLeadingDotSlash path
  | root `isPrefixOf` path = drop (length root + 1) path  -- +1 for the /
  | otherwise = path
  where
    stripLeadingDotSlash ('.':'/':rest) = rest
    stripLeadingDotSlash s = s

-- | Check if a path should be ignored given patterns, relativizing the path
-- to the scan root first. This is the core function for matching .graphosignore
-- patterns against absolute or relative paths.
ignoreMatches :: FilePath -> [AnnotatedPattern] -> FilePath -> Bool
ignoreMatches scanRoot patterns path =
  let relPath = relativize scanRoot path
  in shouldIgnore patterns relPath