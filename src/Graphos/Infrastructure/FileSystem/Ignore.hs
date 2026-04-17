-- | Graphifyignore - read .graphifyignore files for exclusion patterns
module Graphos.Infrastructure.FileSystem.Ignore
  ( loadGraphifyignore
  , shouldIgnore
  , IgnorePattern
  ) where

import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)

-- | A simplified ignore pattern
data IgnorePattern
  = PrefixPattern String   -- ^ e.g., "build/" matches any path starting with "build/"
  | SuffixPattern String   -- ^ e.g., ".min.js" matches any path ending with ".min.js"
  | ExactPattern String     -- ^ e.g., "node_modules" matches exactly
  | GlobPattern String     -- ^ e.g., "*.log" - simplified glob
  deriving (Eq, Show)

-- | Load .graphifyignore patterns from root directory
loadGraphifyignore :: FilePath -> IO [IgnorePattern]
loadGraphifyignore root = do
  let ignoreFile = root ++ "/.graphifyignore"
  exists <- doesFileExist ignoreFile
  if not exists
    then pure []
    else do
      contents <- readFile ignoreFile
      pure $ map parsePattern $ filter (not . isCommentOrBlank) (lines contents)
  where
    isCommentOrBlank line =
      let trimmed = dropWhile (== ' ') line
      in null trimmed || head trimmed == '#'

-- | Parse a single ignore line into a pattern
parsePattern :: String -> IgnorePattern
parsePattern line =
  let trimmed = reverse (dropWhile (== ' ') (reverse (dropWhile (== ' ') line)))
  in case trimmed of
    ('*':rest) -> GlobPattern rest  -- *.log → GlobPattern ".log"
    _ | lastOrDefault trimmed == '/' -> PrefixPattern (init trimmed)
      | headOrDefault trimmed == '/' -> ExactPattern trimmed
      | '.' `elem` trimmed           -> SuffixPattern trimmed
      | otherwise                    -> ExactPattern trimmed
  where
    lastOrDefault [] = ' '
    lastOrDefault xs = last xs
    headOrDefault [] = ' '
    headOrDefault (x:_) = x

-- | Check if a file path should be ignored
shouldIgnore :: [IgnorePattern] -> FilePath -> Bool
shouldIgnore patterns path = any (matches path) patterns

-- | Check if a path matches a pattern
matches :: FilePath -> IgnorePattern -> Bool
matches path (PrefixPattern p)  = p `isPrefixOf` path || ("/" ++ p) `isInfixOf` path
matches path (SuffixPattern p)  = p `isSuffixOf` path
matches path (ExactPattern p)   = p `isInfixOf` path || ("/" ++ p ++ "/") `isInfixOf` ("/" ++ path ++ "/")
matches path (GlobPattern p)    = p `isSuffixOf` path