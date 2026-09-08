{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.StagingSpec (spec) where

import Control.Exception (SomeException, throwIO, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
  ( Permissions(writable)
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getPermissions
  , listDirectory
  , setPermissions
  )
import System.FilePath (takeDirectory, (</>))
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

    it "returns Left instead of throwing when the swap into place fails" $ do
      -- Regression (AVI-567): a post-success swap IO failure must come back
      -- through the Either channel, not escape as a thrown exception.
      withSystemTempDirectory "graphos-staging" $ \root -> do
        let out = root </> "out"
        -- Establish a prior good output so the swap takes the two-rename path.
        _ <- withStagedOutput out $ \staging -> do
          writeFile (staging </> "graph.json") "old-good"
          pure (Right ())
        -- Stage the new build inside a writable root so the action succeeds;
        -- the sabotage applies only to the swap itself.
        result <- rootSabotagedSwap root $ \staging -> do
          writeFile (staging </> "graph.json") "new"
          pure (Right ())
        case result of
          Left err ->
            err `shouldSatisfy` \t -> any (`T.isInfixOf` t) ["failed to swap", "permission denied"]
          Right () ->
            fail "expected the swap to fail"
        -- The prior output is intact and no staging leftovers remain.
        content <- TIO.readFile (out </> "graph.json")
        content `shouldBe` "old-good"
        siblings <- listDirectory root
        siblings `shouldSatisfy` all (== "out")

  describe "relocateStagedPath" $ do
    it "rewrites staged paths to final paths after the swap" $ do
      relocateStagedPath "/r/graphos-out.staging-1" "/r/graphos-out"
        "/r/graphos-out.staging-1/graph.json"
        `shouldBe` "/r/graphos-out/graph.json"

    it "leaves unrelated paths alone" $ do
      relocateStagedPath "/r/graphos-out.staging-1" "/r/graphos-out"
        "/somewhere/else.txt"
        `shouldBe` "/somewhere/else.txt"

    it "does not rewrite siblings that share the staging string prefix" $ do
      -- Adversarial (AVI-567): "out.staging-1-old" starts with the staging
      -- prefix "out.staging-1" but is not inside the staging directory.
      relocateStagedPath "/r/graphos-out.staging-1" "/r/graphos-out"
        "/r/graphos-out.staging-1-old/graph.json"
        `shouldBe` "/r/graphos-out.staging-1-old/graph.json"
      relocateStagedPath "/r/graphos-out.staging-1" "/r/graphos-out"
        "/r/graphos-out.staging-10/x.json"
        `shouldBe` "/r/graphos-out.staging-10/x.json"

    it "rewrites the staging directory itself and nested paths" $ do
      relocateStagedPath "/r/graphos-out.staging-1" "/r/graphos-out"
        "/r/graphos-out.staging-1"
        `shouldBe` "/r/graphos-out"
      relocateStagedPath "/r/graphos-out.staging-1" "/r/graphos-out"
        "/r/graphos-out.staging-1/a/b/c.json"
        `shouldBe` "/r/graphos-out/a/b/c.json"

  describe "interrupted-swap recovery" $ do
    it "restores the previous output from a leftover .prev directory" $ do
      -- Simulate a crash between the two swap renames: final is missing,
      -- a .prev-... backup (with the old content) is still present.
      withSystemTempDirectory "graphos-staging" $ \root -> do
        let out = root </> "graphos-out"
            prev = root </> (prevDirPrefix out ++ "20260908T000000-1")
        createDir prev
        writeFile (prev </> "graph.json") "recovered"
        result <- withStagedOutput out $ \staging -> do
          writeFile (staging </> "graph.json") "fresh"
          pure (Right ())
        result `shouldBe` Right ()
        -- The pre-crash content was restored, then replaced by the new build.
        content <- TIO.readFile (out </> "graph.json")
        content `shouldBe` "fresh"
        -- No .prev leftovers remain after the successful run.
        siblings <- listDirectory root
        siblings `shouldSatisfy` all (not . isPrefixOfStr (prevDirPrefix out))

    it "sweeps stale staging and prev directories at the start of a run" $ do
      withSystemTempDirectory "graphos-staging" $ \root -> do
        let out = root </> "graphos-out"
            staleStaging = root </> (stagingDirPrefix out ++ "20260907T000000-1")
            stalePrev = root </> (prevDirPrefix out ++ "20260907T000000-2")
        createDir out
        writeFile (out </> "graph.json") "current"
        createDir staleStaging
        writeFile (staleStaging </> "junk.json") "stale-staging"
        createDir stalePrev
        writeFile (stalePrev </> "junk.json") "stale-prev"
        result <- withStagedOutput out $ \staging -> do
          writeFile (staging </> "graph.json") "rebuilt"
          pure (Right ())
        result `shouldBe` Right ()
        -- Only the final output remains; stale dirs were swept.
        siblings <- listDirectory root
        siblings `shouldBe` ["graphos-out"]
        content <- TIO.readFile (out </> "graph.json")
        content `shouldBe` "rebuilt"

createDir :: FilePath -> IO ()
createDir = createDirectoryIfMissing True

isPrefixOfStr :: String -> FilePath -> Bool
isPrefixOfStr pre s = take (length pre) s == pre

-- | 'withStagedOutput' with the swap sabotaged: the parent directory is made
-- read-only once the build action returns (staging creation — the only
-- parent-directory write — has already happened by then), so the swap renames
-- fail with a POSIX permission error. Writability is restored after
-- 'withStagedOutput' returns, so its cleanup paths and later assertions work.
rootSabotagedSwap :: FilePath -> (FilePath -> IO (Either Text a)) -> IO (Either Text a)
rootSabotagedSwap final action = do
  rootPermsRef <- newIORef Nothing
  result <- try (withStagedOutput final (wrap rootPermsRef action))
  restore rootPermsRef
  case result of
    Left (e :: SomeException) -> throwIO e
    Right v -> pure v
  where
    rootDir = takeDirectory final
    wrap ref act staging = do
      perms <- getPermissions rootDir
      writeIORef ref (Just perms)
      r <- act staging
      setPermissions rootDir perms { writable = False }
      pure r
    restore ref = do
      saved <- readIORef ref
      case saved of
        Just perms -> setPermissions rootDir perms
        Nothing -> pure ()