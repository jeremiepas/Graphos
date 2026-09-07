{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.StagingSpec (spec) where

import Control.Exception (SomeException, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import Graphos.UseCase.Pipeline.Staging

spec :: Spec
spec = do
  describe "withStagedOutput" $ do
    it "swaps the staging directory into place on success" $ do
      withSystemTempDirectory "graphos-staging" $ \root -> do
        let out = root </> "graphos-out"
        result <- withStagedOutput out $ \staging -> do
          writeFile (staging </> "graph.json") "{\"ok\":true}"
          pure (Right ())
        result `shouldBe` Right ()
        goodOut <- doesFileExist (out </> "graph.json")
        goodOut `shouldBe` True
        content <- TIO.readFile (out </> "graph.json")
        content `shouldBe` "{\"ok\":true}"

    it "preserves an existing output when the rebuild fails" $ do
      withSystemTempDirectory "graphos-staging" $ \root -> do
        let out = root </> "graphos-out"
        -- Establish a prior good output.
        _ <- withStagedOutput out $ \staging -> do
          writeFile (staging </> "graph.json") "old-good"
          pure (Right ())
        -- Failed rebuild.
        result <- withStagedOutput out $ \staging -> do
          writeFile (staging </> "graph.json") "half-written"
          pure (Left "clustering blew up" :: Either Text ())
        result `shouldBe` Left "clustering blew up"
        content <- TIO.readFile (out </> "graph.json")
        content `shouldBe` "old-good"
        -- No staging leftovers.
        siblings <- listDirectory root
        siblings `shouldBe` ["graphos-out"]

    it "cleans up staging when the action throws" $ do
      withSystemTempDirectory "graphos-staging" $ \root -> do
        let out = root </> "graphos-out"
        _ <- withStagedOutput out $ \staging -> do
          writeFile (staging </> "graph.json") "old-good"
          pure (Right ())
        res <- try $ withStagedOutput out $ \staging -> do
          writeFile (staging </> "graph.json") "partial"
          ioError (userError "kaboom")
        case res of
          Left (_ :: SomeException) -> pure ()
          Right _ -> fail "expected exception"
        content <- TIO.readFile (out </> "graph.json")
        content `shouldBe` "old-good"
        siblings <- listDirectory root
        siblings `shouldBe` ["graphos-out"]

    it "carries persistent state (cache/memory) across the rebuild" $ do
      withSystemTempDirectory "graphos-staging" $ \root -> do
        let out = root </> "graphos-out"
        _ <- withStagedOutput out $ \staging -> do
          writeFile (staging </> "graph.json") "v1"
          createDir (staging </> "cache")
          writeFile (staging </> "cache" </> "abc.json") "{}"
          pure (Right ())
        _ <- withStagedOutput out $ \staging -> do
          writeFile (staging </> "graph.json") "v2"
          pure (Right ())
        cacheKept <- doesFileExist (out </> "cache" </> "abc.json")
        cacheKept `shouldBe` True
        graphV2 <- TIO.readFile (out </> "graph.json")
        graphV2 `shouldBe` "v2"

    it "runs on a missing output directory (first build)" $ do
      withSystemTempDirectory "graphos-staging" $ \root -> do
        let out = root </> "nested" </> "graphos-out"
        result <- withStagedOutput out $ \staging -> do
          writeFile (staging </> "graph.json") "first"
          pure (Right ())
        result `shouldBe` Right ()
        exists <- doesFileExist (out </> "graph.json")
        exists `shouldBe` True

    it "reports failure without creating the output on first build" $ do
      withSystemTempDirectory "graphos-staging" $ \root -> do
        let out = root </> "graphos-out"
        result <- withStagedOutput out $ \_ -> pure (Left "no files found" :: Either Text ())
        result `shouldBe` Left "no files found"
        outExists <- doesDirectoryExist out
        outExists `shouldBe` False
        siblings <- listDirectory root
        siblings `shouldBe` []

    it "exposes a staging path distinct from the final path" $ do
      withSystemTempDirectory "graphos-staging" $ \root -> do
        let out = root </> "graphos-out"
            takeFileName' = reverse . takeWhile (/= '/') . reverse
            startsWith p pre = take (length pre) p == pre
        seen <- newIORef ""
        _ <- withStagedOutput out $ \staging -> do
          writeIORef seen staging
          pure (Right ())
        staging <- readIORef seen
        staging `shouldSatisfy` (/= out)
        staging `shouldSatisfy` (\p -> takeFileName' p `startsWith` "graphos-out.staging-")

  describe "relocateStagedPath" $ do
    it "rewrites staged paths to final paths after the swap" $ do
      relocateStagedPath "/r/graphos-out.staging-1" "/r/graphos-out"
        "/r/graphos-out.staging-1/graph.json"
        `shouldBe` "/r/graphos-out/graph.json"

    it "leaves unrelated paths alone" $ do
      relocateStagedPath "/r/graphos-out.staging-1" "/r/graphos-out"
        "/somewhere/else.txt"
        `shouldBe` "/somewhere/else.txt"

createDir :: FilePath -> IO ()
createDir = createDirectoryIfMissing True