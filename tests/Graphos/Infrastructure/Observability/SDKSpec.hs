{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Observability.SDKSpec where

import Test.Hspec
import Control.Exception (catch, SomeException)
import System.Directory (doesDirectoryExist, doesFileExist, getTemporaryDirectory, listDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Graphos.Infrastructure.Observability.SDK

-- | Remove a directory if it exists, ignoring errors if it doesn't.
cleanDir :: FilePath -> IO ()
cleanDir dir = catch (removeDirectoryRecursive dir) (\(_ :: SomeException) -> pure ())

spec :: Spec
spec = do
  describe "Debug trace directory creation" $ do
    it "does not create a directory when tracing is disabled" $ do
      tmp <- mkTempPath "disabled"
      cleanDir tmp
      env <- newDebugTraceEnv False tmp
      flushDebugTrace env
      doesDirectoryExist tmp `shouldReturn` False

    it "does not create a directory when tracing is enabled but no events were emitted" $ do
      tmp <- mkTempPath "empty"
      cleanDir tmp
      env <- newDebugTraceEnv True tmp
      flushDebugTrace env
      doesDirectoryExist tmp `shouldReturn` False

    it "creates the directory and a JSONL file when events were emitted" $ do
      tmp <- mkTempPath "events"
      cleanDir tmp
      env <- newDebugTraceEnv True tmp
      debugTraceEvent env "test_event" Map.empty
      flushDebugTrace env
      doesDirectoryExist tmp `shouldReturn` True
      files <- listDirectory tmp
      length files `shouldBe` 1
      let file = NE.head $ NE.fromList files
      doesFileExist (tmp </> file) `shouldReturn` True
      file `shouldSatisfy` (\f -> ".jsonl" `T.isSuffixOf` T.pack f)
      -- Clean up after test
      cleanDir tmp

mkTempPath :: String -> IO FilePath
mkTempPath tag = do
  base <- getTemporaryDirectory
  pure $ base </> ("graphos-sdk-test-" ++ tag)
