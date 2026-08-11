{-# LANGUAGE StrictData #-}
module Graphos.Infrastructure.Scaffold.Writer
  ( -- * Scaffold writer
    ScaffoldResult(..)
  , writeScaffold
  , gatherDetectionFacts
    -- * Install-skill writer
  , runInstallSkill
  , runInstallSkillWithRoot
  ) where

import System.Directory (doesDirectoryExist, createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath (takeDirectory, (</>))
import qualified Data.Text.IO as TIO
import Data.Text (Text)

import Graphos.Domain.Scaffold
import Graphos.UseCase.Scaffold

data ScaffoldResult = ScaffoldResult
  { srCreated :: [FilePath]
  , srSkipped :: [FilePath]
  } deriving (Eq, Show)

gatherDetectionFacts :: IO DetectionFacts
gatherDetectionFacts = do
  opencodeExists <- doesDirectoryExist ".opencode"
  claudeExists <- doesDirectoryExist ".claude"
  pure DetectionFacts
    { dfOpencodeDirExists = opencodeExists
    , dfClaudeDirExists = claudeExists
    }

writeScaffold :: [ScaffoldFile] -> IO ScaffoldResult
writeScaffold files = do
  (created, skipped) <- go [] [] files
  pure ScaffoldResult { srCreated = reverse created, srSkipped = reverse skipped }
  where
    go cr sk [] = pure (cr, sk)
    go cr sk (f:fs) = do
      let path = sfRelativePath f
          dir = takeDirectory path
      createDirectoryIfMissing True dir
      exists <- doesFileExist path
      if exists
        then do
          putStrLn $ "[init] " ++ path ++ " already exists. Skipping."
          go cr (path : sk) fs
        else do
          TIO.writeFile path (sfContent f)
          putStrLn $ "[init] Created " ++ path
          go (path : cr) sk fs

-- ───────────────────────────────────────────────
-- Install-skill writer
-- ───────────────────────────────────────────────

runInstallSkillWithRoot :: FilePath -> Text -> InstallSkillRequest -> CommandReference -> IO ScaffoldResult
runInstallSkillWithRoot rootDir ver req ref = do
  let files = installSkillPlan req ver ref
      targetDir = rootDir </> ".agents" </> "skills"
  createDirectoryIfMissing True targetDir
  (created, skipped) <- go targetDir [] [] files
  pure ScaffoldResult { srCreated = reverse created, srSkipped = reverse skipped }
  where
    go :: FilePath -> [FilePath] -> [FilePath] -> [ScaffoldFile] -> IO ([FilePath], [FilePath])
    go _ cr sk [] = pure (cr, sk)
    go td cr sk (f:fs) = do
      let filePath = td </> sfRelativePath f
          dir = takeDirectory filePath
      createDirectoryIfMissing True dir
      exists <- doesFileExist filePath
      if exists
        then do
          putStrLn $ "[install-skill] " ++ filePath ++ " already exists. Skipping."
          go td cr (filePath : sk) fs
        else do
          TIO.writeFile filePath (sfContent f)
          putStrLn $ "[install-skill] Created " ++ filePath
          go td (filePath : cr) sk fs

runInstallSkill :: Text -> InstallSkillTarget -> CommandReference -> IO ()
runInstallSkill ver target ref = do
  home <- getHomeDirectory
  result <- runInstallSkillWithRoot home ver (InstallSkillRequest { isrTarget = target }) ref
  let msg = case (srCreated result, srSkipped result) of
        ([], []) -> "No skills installed (empty plan)"
        (c, []) -> "Installed " ++ show (length c) ++ " skill(s)"
        (c, s) -> "Installed " ++ show (length c) ++ " skill(s), skipped " ++ show (length s) ++ " existing"
  putStrLn $ "[install-skill] " ++ msg