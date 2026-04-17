-- | Git hook - post-commit auto-rebuild
module Graphos.Infrastructure.Git.Hook
  ( installHook
  , uninstallHook
  , hookStatus
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.List (isInfixOf)

-- | Install post-commit hook
installHook :: FilePath -> IO (Either Text Text)
installHook repoRoot = do
  let hookDir = repoRoot </> ".git" </> "hooks"
      hookPath = hookDir </> "post-commit"
  gitDirExists <- doesDirectoryExist (repoRoot </> ".git")
  if not gitDirExists
    then pure (Left "Not a git repository: .git/ directory not found")
    else do
      createDirectoryIfMissing True hookDir
      existing <- doesFileExist hookPath
      graphosMarker <- if existing
        then do
          content <- readFile hookPath
          pure ("# graphos-auto-rebuild" `isInfixOf` content)
        else pure False
      if graphosMarker
        then pure (Right "Graphos hook already installed")
        else do
          let hookContent = generateHookContent
          appendFile hookPath hookContent
          pure (Right "Graphos post-commit hook installed")

-- | Uninstall graphos section from post-commit hook
uninstallHook :: FilePath -> IO (Either Text Text)
uninstallHook repoRoot = do
  let hookPath = repoRoot </> ".git" </> "hooks" </> "post-commit"
  exists <- doesFileExist hookPath
  if not exists
    then pure (Right "No post-commit hook found - nothing to uninstall")
    else do
      content <- readFile hookPath
      if not ("# graphos-auto-rebuild" `isInfixOf` content)
        then pure (Right "Graphos hook not found in post-commit")
        else do
          let cleaned = removeGraphosSection content
          writeFile hookPath cleaned
          pure (Right "Graphos section removed from post-commit hook")

-- | Check if graphos hook is installed
hookStatus :: FilePath -> IO (Either Text Text)
hookStatus repoRoot = do
  let hookPath = repoRoot </> ".git" </> "hooks" </> "post-commit"
  exists <- doesFileExist hookPath
  if not exists
    then pure (Right "No post-commit hook - graphos not installed")
    else do
      content <- readFile hookPath
      if "# graphos-auto-rebuild" `isInfixOf` content
        then pure (Right "Graphos post-commit hook is installed and active")
        else pure (Right "Post-commit hook exists but graphos section not found")

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Generate the hook script content
generateHookContent :: String
generateHookContent = unlines
  [ ""
  , "# graphos-auto-rebuild"
  , "# Rebuild graphos graph after each commit for changed code files"
  , "CHANGED=$(git diff HEAD~1 --name-only 2>/dev/null | grep -E '\\.(py|ts|js|go|rs|java|cpp|c|rb|swift|kt|cs|hs|scala|php)$' || true)"
  , "if [ -n \"$CHANGED\" ]; then"
  , "  echo '[graphos] Code files changed, rebuilding graph...'"
  , "  graphos . --update 2>/dev/null || true"
  , "fi"
  ]

-- | Remove the graphos section from hook content
removeGraphosSection :: String -> String
removeGraphosSection content =
  let ls = lines content
      go [] = []
      go (l:ls')
        | "# graphos-auto-rebuild" `isInfixOf` l =
            -- Skip until we hit a non-graphos line
            skipGraphosBlock ls'
        | otherwise = l : go ls'
      skipGraphosBlock [] = []
      skipGraphosBlock (l:ls')
        | "# graphos-auto-rebuild" `isInfixOf` l = skipGraphosBlock ls'
        | null l = skipGraphosBlock ls'  -- skip blank lines between graphos sections
        | "graphos" `isInfixOf` l || "CHANGED" `isInfixOf` l || "if [" `isInfixOf` l || "then" `isInfixOf` l || "fi" `isInfixOf` l || "echo" `isInfixOf` l = skipGraphosBlock ls'
        | otherwise = l : go ls'
  in unlines (go ls)