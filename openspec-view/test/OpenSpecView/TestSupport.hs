-- | Shared filesystem helpers for the @openspec-view@ test suite.
--
-- The package is meant to be checked out and tested on its own, so the
-- disk-scanning specs locate the OpenSpec workspace by walking up from the
-- current directory (Cabal runs a test executable from the package root,
-- which may be either a repository root or a subdirectory of one).
module OpenSpecView.TestSupport
  ( combine'
  , takeParent
  , findChangesRoot
  , findWorkspaceRoot
  ) where

import System.Directory (doesDirectoryExist, getCurrentDirectory)

-- | Join two path fragments without depending on @filepath@.
combine' :: FilePath -> FilePath -> FilePath
combine' a b
  | not (null a) && last a == '/' = a ++ b
  | null a = b
  | otherwise = a ++ "/" ++ b

takeParent :: FilePath -> FilePath
takeParent p =
  let stripped = drop 1 (dropWhile (/= '/') (reverse p))
      parent = reverse stripped
  in if null parent then "." else parent

-- | Directory that holds @openspec/@, if any ancestor has one.
findWorkspaceRoot :: IO (Maybe FilePath)
findWorkspaceRoot = go =<< getCurrentDirectory
  where
    go d = do
      exists <- doesDirectoryExist (combine' d "openspec/changes")
      if exists
        then pure (Just d)
        else
          let parent = takeParent d
          in if parent == "." || parent == d
               then pure Nothing
               else go parent

-- | The @openspec/changes@ directory, if any ancestor has one.
findChangesRoot :: IO (Maybe FilePath)
findChangesRoot = fmap (fmap (`combine'` "openspec/changes")) findWorkspaceRoot
