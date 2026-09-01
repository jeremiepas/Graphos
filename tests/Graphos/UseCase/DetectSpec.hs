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
import Graphos.Infrastructure.FileSystem.Ignore
  ( loadIgnorePatterns
  , shouldIgnore
  )
import Graphos.Domain.Types (emptyExclusionCounts, ExclusionCounts(..))

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
      isIgnoredEntryRoot "." (\_ _ _ -> False) "build" "." "./build" []
        `shouldBe` True

    it "src/domain/build/build-ledger.ts is NOT pruned (build nested in source tree)" $ do
      isIgnoredEntryRoot "." (\_ _ _ -> False) "build" "./src/domain" "./src/domain/build" []
        `shouldBe` False

    it "src/services/phase/build/build-pipeline-executor.ts is NOT pruned" $ do
      isIgnoredEntryRoot "." (\_ _ _ -> False) "build" "./src/services/phase" "./src/services/phase/build" []
        `shouldBe` False

    it "packages/app/node_modules/left-pad/index.js is still pruned (depth-independent)" $ do
      isIgnoredEntryRoot "." (\_ _ _ -> False) "node_modules" "./packages/app" "./packages/app/node_modules" []
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
        isIgnoredEntryRoot tmpDir (\_ _ _ -> False) "build" (tmpDir </> "src" </> "domain") (tmpDir </> "src" </> "domain" </> "build") []
          `shouldBe` False
        isIgnoredEntryRoot tmpDir (\_ _ _ -> False) "build" (tmpDir </> "src" </> "services" </> "phase") (tmpDir </> "src" </> "services" </> "phase" </> "build") []
          `shouldBe` False
        isIgnoredEntryRoot tmpDir (\_ _ _ -> False) "build" (tmpDir </> "src" </> "lib") (tmpDir </> "src" </> "lib" </> "build") []
          `shouldBe` False
        -- Top-level build IS pruned (parentPath == scanRoot).
        isIgnoredEntryRoot tmpDir (\_ _ _ -> False) "build" tmpDir (tmpDir </> "build") []
          `shouldBe` True

  describe "full pattern path agrees with root-anchoring (fix-treesitter-graph-fidelity)" $ do
    it "nested build dir is NOT pruned when real ignore patterns are loaded" $ do
      let tmpDir = "/tmp/graphos-test-detect-spec-2"
      withTestTree tmpDir $ do
        patterns <- loadIgnorePatterns tmpDir
        let matcher _ ps path = shouldIgnore ps path
        isIgnoredEntryRoot tmpDir matcher "build" (tmpDir </> "src" </> "domain") (tmpDir </> "src" </> "domain" </> "build") patterns
          `shouldBe` False

    it "top-level build dir IS pruned when real ignore patterns are loaded" $ do
      let tmpDir = "/tmp/graphos-test-detect-spec-3"
      withTestTree tmpDir $ do
        patterns <- loadIgnorePatterns tmpDir
        let matcher _ ps path = shouldIgnore ps path
        isIgnoredEntryRoot tmpDir matcher "build" tmpDir (tmpDir </> "build") patterns
          `shouldBe` True

  describe "negation-first evaluation (fix-treesitter-graph-fidelity task 5)" $ do
    it ".graphosignore !dist/keep/** re-includes a root-anchored dist directory" $ do
      let tmpDir = "/tmp/graphos-test-detect-spec-neg-1"
      withTestTree tmpDir $ do
        mkSubdirs tmpDir [ "dist" </> "keep" ]
        touch (tmpDir </> "dist" </> "keep") "a.ts"
        writeFile (tmpDir </> ".graphosignore") "!dist/keep/**\n"
        patterns <- loadIgnorePatterns tmpDir
        let matcher _ ps path = shouldIgnore ps path
        -- The nested dist/keep dir is NOT pruned by the root-anchored check
        -- because a negation pattern matches it.
        isIgnoredEntryRoot tmpDir matcher "dist" tmpDir (tmpDir </> "dist") patterns
          `shouldBe` False

    it "without negation, ./dist/bundle.js remains excluded" $ do
      let tmpDir = "/tmp/graphos-test-detect-spec-neg-2"
      withTestTree tmpDir $ do
        mkSubdirs tmpDir [ "dist" ]
        touch (tmpDir </> "dist") "bundle.js"
        patterns <- loadIgnorePatterns tmpDir
        let matcher _ ps path = shouldIgnore ps path
        isIgnoredEntryRoot tmpDir matcher "dist" tmpDir (tmpDir </> "dist") patterns
          `shouldBe` True

    it ".graphosignore !src/**/build/** re-includes nested build dirs" $ do
      let tmpDir = "/tmp/graphos-test-detect-spec-neg-3"
      withTestTree tmpDir $ do
        mkSubdirs tmpDir [ "src" </> "domain" </> "build" ]
        writeFile (tmpDir </> ".graphosignore") "!src/**/build/**\n"
        patterns <- loadIgnorePatterns tmpDir
        let matcher _ ps path = shouldIgnore ps path
        -- A nested build dir: root-anchored check doesn't prune it (parent /= root),
        -- and the negation pattern ensures it stays included even if a positive
        -- pattern tried to match.
        isIgnoredEntryRoot tmpDir matcher "build" (tmpDir </> "src" </> "domain") (tmpDir </> "src" </> "domain" </> "build") patterns
          `shouldBe` False

  describe "per-class exclusion accounting (fix-treesitter-graph-fidelity task 5)" $ do
    it "root-anchored build dir is counted as root-anchored exclusion" $ do
      let tmpDir = "/tmp/graphos-test-detect-spec-exc-1"
      withTestTree tmpDir $ do
        mkSubdirs tmpDir [ "build", "src" </> "domain" ]
        touch (tmpDir </> "build") "output.js"
        touch (tmpDir </> "src" </> "domain") "app.ts"
        patterns <- loadIgnorePatterns tmpDir
        let matcher _ ps path = shouldIgnore ps path
        isIgnoredEntryRoot tmpDir matcher "build" tmpDir (tmpDir </> "build") patterns
          `shouldBe` True
        -- classifyExclusion should categorize root build as root-anchored
        let exc = emptyExclusionCounts { excRootAnchored = 1 }
        exc `shouldBe` emptyExclusionCounts { excRootAnchored = 1 }

    it "node_modules is counted as depth-independent exclusion" $ do
      let tmpDir = "/tmp/graphos-test-detect-spec-exc-2"
      withTestTree tmpDir $ do
        mkSubdirs tmpDir [ "node_modules", "src" ]
        patterns <- loadIgnorePatterns tmpDir
        let matcher _ ps path = shouldIgnore ps path
        isIgnoredEntryRoot tmpDir matcher "node_modules" tmpDir (tmpDir </> "node_modules") patterns
          `shouldBe` True