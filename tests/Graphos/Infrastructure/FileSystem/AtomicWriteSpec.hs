-- | Tests for Graphos.Infrastructure.FileSystem.AtomicWrite — writeFileAtomic.
module Graphos.Infrastructure.FileSystem.AtomicWriteSpec (spec) where

import Test.Hspec
import Control.Exception (try)
import Data.List (isSuffixOf)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hPutStr, hClose)

import Graphos.Infrastructure.FileSystem.AtomicWrite
  (writeFileAtomic, openAtomicTemp, placeAtomicStreamed, AtomicWriteFailure(..))

spec :: Spec
spec = do
  describe "writeFileAtomic" $ do
    it "replaces content on success and leaves no stray temp files" $
      withSystemTempDirectory "graphos-atomic-success-spec" $ \dir -> do
        let target = dir </> "artifact.txt"
        writeFileAtomic target "v1"
        v1 <- readFile target
        v1 `shouldBe` "v1"
        writeFileAtomic target "v2"
        v2 <- readFile target
        v2 `shouldBe` "v2"
        leftovers <- listDirectory dir
        leftovers
          `shouldSatisfy` \ls ->
            not (any (\f -> ".tmp" `isSuffixOf` f) ls)

    it "preserves prior state when the rename step fails (target as directory)" $
      withSystemTempDirectory "graphos-atomic-interrupt-spec" $ \dir -> do
        let target   = dir </> "artifact.txt"
            sentinel = target </> "keep.txt"
        createDirectoryIfMissing True target
        writeFile sentinel "sentinel"
        result <-
          (try (writeFileAtomic target "new-content")) :: IO (Either AtomicWriteFailure ())
        case result of
          Left _ -> pure ()
          Right () ->
            expectationFailure
              "expected AtomicWriteFailure when renaming onto a directory"
        s <- readFile sentinel
        s `shouldBe` "sentinel"
        leftovers <- listDirectory dir
        leftovers
          `shouldSatisfy` \ls ->
            not (any (\f -> ".tmp" `isSuffixOf` f) ls)

    it "fsyncs the parent directory when creating a new nested tree" $
      withSystemTempDirectory "graphos-atomic-fsync-spec" $ \dir -> do
        let target = dir </> "a" </> "b" </> "c" </> "artifact.txt"
        writeFileAtomic target "deep"
        deep <- readFile target
        deep `shouldBe` "deep"

    it "places streamed content atomically with no leftover temp files" $
      withSystemTempDirectory "graphos-stream-success-spec" $ \dir -> do
        let target = dir </> "stream.txt"
        (tmpPath, h) <- openAtomicTemp target
        hPutStr h "streamed"
        hClose h
        placeAtomicStreamed tmpPath target
        got <- readFile target
        got `shouldBe` "streamed"
        leftovers <- listDirectory dir
        leftovers
          `shouldSatisfy` \ls ->
            not (any (\f -> ".tmp" `isSuffixOf` f) ls)

    it "raises and preserves prior state when streamed placement renames onto a directory" $
      withSystemTempDirectory "graphos-stream-interrupt-spec" $ \dir -> do
        let target   = dir </> "stream.txt"
            sentinel = target </> "keep.txt"
        createDirectoryIfMissing True target
        writeFile sentinel "sentinel"
        (tmpPath, h) <- openAtomicTemp target
        hPutStr h "partial"
        hClose h
        result <-
          (try (placeAtomicStreamed tmpPath target)) :: IO (Either AtomicWriteFailure ())
        case result of
          Left _ -> pure ()
          Right () ->
            expectationFailure
              "expected AtomicWriteFailure when renaming onto a directory"
        s <- readFile sentinel
        s `shouldBe` "sentinel"
