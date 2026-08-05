{-# LANGUAGE StrictData #-}
module Graphos.Infrastructure.Scaffold.Writer
  ( -- * Scaffold writer
    ScaffoldResult(..)
  , writeScaffold
  , gatherDetectionFacts
  ) where

import System.Directory (doesDirectoryExist, createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import qualified Data.Text.IO as TIO

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