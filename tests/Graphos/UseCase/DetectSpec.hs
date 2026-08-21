{-# LANGUAGE OverloadedStrings #-}
-- | Tests for UseCase.Detect — root-anchored build-output ignore names.
module Graphos.UseCase.DetectSpec (spec) where

import Test.Hspec
import System.Directory
  ( createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist
  )
import System.FilePath ((</>))

import Graphos.UseCase.Detect
  ( rootAnchoredIgnoreDirs
  , depthIndependentIgnoreDirs
  , hardcodedIgnoreDirNames
  , isIgnoredEntryRoot
  )

-- | Create a temporary test directory tree, run the action, then clean up.
withTestTree :: FilePath -> IO a -> IO a
withTestTree dir action = do
  createDirectoryIfMissing True dir
  result <- action
  exists <- doesDirectoryExist dir
  if exists then removeDirectoryRecursive dir else pure ()
  pure result

mkSubdirs :: FilePath -> [FilePath] -> IO ()
mkSubdirs parent = mapM_ (\d -> createDirectoryIfMissing True (parent </> d))

touch :: FilePath -> String -> IO ()
touch dir name = writeFile (dir </> name) "content"

spec :: Spec
spec = do
  describe "root-anchored build-output ignore names (fix-treesitter-graph-fidelity)" $ do
    it "rootAnchoredIgnoreDirs contains build, out, target, dist, dist-newstyle, DerivedData, .build" $ do
      all (`elem` rootAnchoredIgnoreDirs) ["build", "out", "target", "dist", "dist-newstyle", "DerivedData", ".build"] `shouldBe` True

    it "depthIndependentIgnoreDirs contains node_modules, .git, .stack-work, __pycache__" $ do
      all (`elem` depthIndependentIgnoreDirs) ["node_modules", ".git", ".stack-work", "__pycache__"] `shouldBe` True

    it "hardcodedIgnoreDirNames is the union of the two classes" $ do
      hardcodedIgnoreDirNames `shouldBe` rootAnchoredIgnoreDirs ++ depthIndependentIgnoreDirs

    it "./build/output.js is pruned when the scan root is ." $ do
      isIgnoredEntryRoot "." (\_ _ -> False) "build" "." "./build" []
        `shouldBe` True

    it "src/domain/build/build-ledger.ts is NOT pruned (build nested in source tree)" $ do
      isIgnoredEntryRoot "." (\_ _ -> False) "build" "./src/domain" "./src/domain/build" []
        `shouldBe` False

    it "src/services/phase/build/build-pipeline-executor.ts is NOT pruned" $ do
      isIgnoredEntryRoot "." (\_ _ -> False) "build" "./src/services/phase" "./src/services/phase/build" []
        `shouldBe` False

    it "packages/app/node_modules/left-pad/index.js is still pruned (depth-independent)" $ do
      isIgnoredEntryRoot "." (\_ _ -> False) "node_modules" "./packages/app" "./packages/app/node_modules" []
        `shouldBe` True

  describe "detectFiles (integration with real filesystem)" $ do
    it "does not prune nested build dirs but prunes top-level build" $ do
      let tmpDir = "/tmp/graphos-test-detect-spec-1"
      withTestTree tmpDir $ do
        mkSubdirs tmpDir [ "src" </> "domain" </> "build"
                        , "src" </> "services" </> "phase" </> "build"
                        , "build"
                        , "src" </> "lib" </> "build"
                        ]
        touch (tmpDir </> "src" </> "domain" </> "build") "build-ledger.ts"
        touch (tmpDir </> "src" </> "services" </> "phase" </> "build") "build-pipeline-executor.ts"
        touch (tmpDir </> "build") "output.js"
        touch (tmpDir </> "src" </> "lib" </> "build") "build-helper.ts"
        -- Nested build dirs are NOT pruned (parentPath /= scanRoot).
        isIgnoredEntryRoot tmpDir (\_ _ -> False) "build" (tmpDir </> "src" </> "domain") (tmpDir </> "src" </> "domain" </> "build") []
          `shouldBe` False
        isIgnoredEntryRoot tmpDir (\_ _ -> False) "build" (tmpDir </> "src" </> "services" </> "phase") (tmpDir </> "src" </> "services" </> "phase" </> "build") []
          `shouldBe` False
        isIgnoredEntryRoot tmpDir (\_ _ -> False) "build" (tmpDir </> "src" </> "lib") (tmpDir </> "src" </> "lib" </> "build") []
          `shouldBe` False
        -- Top-level build IS pruned (parentPath == scanRoot).
        isIgnoredEntryRoot tmpDir (\_ _ -> False) "build" tmpDir (tmpDir </> "build") []
          `shouldBe` True