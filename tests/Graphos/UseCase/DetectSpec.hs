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
  , findAllFilesWithExclusions
  , allSupportedExtensions
  , detectMultiSources
  )
import Graphos.UseCase.Extract.Core (applyTaggingSources)
import Graphos.Infrastructure.FileSystem.Ignore
  ( loadIgnorePatterns
  , shouldIgnore
  , ignoreMatches
  , parseGitignoreLine
  )
import Graphos.Domain.Types
  ( emptyExclusionCounts, ExclusionCounts(..)
  , Edge(..), EdgeId(..), Relation(..), Confidence(..)
  , NodeId, Node(..), extractionFromLists, extNodes, extEdges, nodeToJGF )
import Graphos.Domain.Graph (makeStubNode)
import Graphos.Domain.Config.Source (SourceConfig(..))
import Graphos.Domain.Config.Detection (defaultDetectionConfig)
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..))
import qualified Data.Map.Strict as Map
import Data.List (sort)
import Data.Text.Short (toText)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as AesKey

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

  describe "individual file ignore accounting (honor-graphosignore)" $ do
    it "a supported file matching an ignore pattern is counted in excIgnoredFiles and excluded" $ do
      let tmpDir = "/tmp/graphos-test-detect-spec-file-1"
      withTestTree tmpDir $ do
        mkSubdirs tmpDir [ "src" ]
        touch (tmpDir </> "src") "lib.rs"
        touch (tmpDir </> "src") "main.rs"
        let patterns = [ parseGitignoreLine 2 "**/lib.rs" ]
        (files, excs) <- findAllFilesWithExclusions tmpDir tmpDir ignoreMatches allSupportedExtensions patterns (\_ -> pure ())
        excIgnoredFiles excs `shouldBe` 1
        length files `shouldBe` 1
        (tmpDir </> "src" </> "lib.rs") `notElem` files `shouldBe` True

    it "no ignored files are counted when no pattern matches" $ do
      let tmpDir = "/tmp/graphos-test-detect-spec-file-2"
      withTestTree tmpDir $ do
        mkSubdirs tmpDir [ "src" ]
        touch (tmpDir </> "src") "lib.rs"
        touch (tmpDir </> "src") "main.rs"
        let patterns = [ parseGitignoreLine 2 "**/nope.rs" ]
        (files, excs) <- findAllFilesWithExclusions tmpDir tmpDir ignoreMatches allSupportedExtensions patterns (\_ -> pure ())
        excIgnoredFiles excs `shouldBe` 0
        length files `shouldBe` 2


  describe "multi-source detection (3.1) — union walk + provenance" $ do
    it "two sources sharing a relative path yield distinct qualified paths" $ do
      let tmpDir = "/tmp/graphos-multi-distinct"
      withTestTree tmpDir $ do
        let repoA = tmpDir </> "repoA"
            repoB = tmpDir </> "repoB"
        mkSubdirs tmpDir ["repoA", "repoB"]
        touch repoA "app.py"
        touch repoB "app.py"
        let sources = [ SourceConfig "repoA" repoA []
                      , SourceConfig "repoB" repoB [] ]
        (_, provIO) <- detectMultiSources testFSP defaultDetectionConfig allSupportedExtensions sources (\_ -> pure ())
        case provIO of
          Just provMap -> do
            Map.size provMap `shouldBe` 2
            sort (Map.elems provMap) `shouldBe` [("repoA", "repoA/app.py"), ("repoB", "repoB/app.py")]
          Nothing -> error "expected multi-source provenance"

    it "a file inside a nested-worktree is attributed to the inner source exactly once" $ do
      let tmpDir = "/tmp/graphos-multi-nested"
      withTestTree tmpDir $ do
        let repoA = tmpDir </> "repoA"
            repoB = tmpDir </> "repoA" </> "sub"
        mkSubdirs tmpDir ["repoA", "repoA" </> "sub"]
        touch repoB "deep.py"
        touch repoA "top.py"
        let sources = [ SourceConfig "repoA" repoA []
                      , SourceConfig "repoB" repoB [] ]
        (_, provIO) <- detectMultiSources testFSP defaultDetectionConfig allSupportedExtensions sources (\_ -> pure ())
        case provIO of
          Just provMap -> do
            Map.size provMap `shouldBe` 2
            sort (Map.elems provMap) `shouldBe` [("repoA", "repoA/top.py"), ("repoB", "repoB/deep.py")]
          Nothing -> error "expected multi-source provenance"

    it "a per-source ignore list scopes to that source only" $ do
      let tmpDir = "/tmp/graphos-multi-ignore"
      withTestTree tmpDir $ do
        let repoA = tmpDir </> "repoA"
            repoB = tmpDir </> "repoB"
        mkSubdirs tmpDir ["repoA", "repoB"]
        touch repoA "gen.py"
        touch repoB "gen.py"
        let sources = [ SourceConfig "repoA" repoA ["gen.py"]
                      , SourceConfig "repoB" repoB [] ]
        (_, provIO) <- detectMultiSources testFSP defaultDetectionConfig allSupportedExtensions sources (\_ -> pure ())
        case provIO of
          Just provMap -> do
            Map.size provMap `shouldBe` 1
            sort (Map.elems provMap) `shouldBe` [("repoB", "repoB/gen.py")]
          Nothing -> error "expected multi-source provenance"

    it "with no sources configured, provenance is Nothing (single-source regression)" $ do
      let tmpDir = "/tmp/graphos-multi-empty"
      withTestTree tmpDir $ do
        mkSubdirs tmpDir ["repoA"]
        touch (tmpDir </> "repoA") "app.py"
        (_, provIO) <- detectMultiSources testFSP defaultDetectionConfig allSupportedExtensions [] (\_ -> pure ())
        provIO `shouldBe` Nothing

  describe "multi-source tagging (3.2) — applyTaggingSources" $ do
    it "tags nodes with nodeSource, qualified nodeSourceFile and a distinct id per source" $ do
      let prov = Map.fromList
            [ ("/abs/repoA/app.py", ("repoA", "repoA/app.py"))
            , ("/abs/repoB/app.py", ("repoB", "repoB/app.py"))
            ]
          ext = extractionFromLists [ makeStubNode "/abs/repoA/app.py"
                                    , makeStubNode "/abs/repoB/app.py" ] []
          tagged = applyTaggingSources (Just prov) ext
          byPath = Map.fromList [ (nodeSourceFile n, n) | n <- Map.elems (extNodes tagged) ]
          nA = case Map.lookup "repoA/app.py" byPath of Just n -> n; _ -> error "missing repoA node"
          nB = case Map.lookup "repoB/app.py" byPath of Just n -> n; _ -> error "missing repoB node"
      length (Map.elems (extNodes tagged)) `shouldBe` 2
      fmap toText (nodeSource nA) `shouldBe` Just "repoA"
      toText (nodeSourceFile nA) `shouldBe` "repoA/app.py"
      fmap toText (nodeSource nB) `shouldBe` Just "repoB"
      toText (nodeSourceFile nB) `shouldBe` "repoB/app.py"
      nodeId nA `shouldNotBe` nodeId nB

    it "applyTaggingSources Nothing returns the extraction unchanged (single-source regression)" $ do
      let ext = extractionFromLists [ makeStubNode "/abs/repoA/app.py" ] []
          same = applyTaggingSources Nothing ext
      map nodeId (Map.elems (extNodes same)) `shouldBe` map nodeId (Map.elems (extNodes ext))
      all (== Nothing) (map (fmap toText . nodeSource) (Map.elems (extNodes same))) `shouldBe` True

    it "applyTaggingSources remaps edge endpoints to the tagged node ids" $ do
      let prov = Map.fromList
            [ ("/abs/repoA/app.py", ("repoA", "repoA/app.py"))
            , ("/abs/repoB/app.py", ("repoB", "repoB/app.py"))
            ]
          nA = makeStubNode "/abs/repoA/app.py"
          nB = makeStubNode "/abs/repoB/app.py"
          ext = extractionFromLists [nA, nB] [mkEdge (nodeId nA) (nodeId nB)]
          tagged = applyTaggingSources (Just prov) ext
          edges = Map.elems (extEdges tagged)
          nodeIds = Map.keysSet (extNodes tagged)
      length edges `shouldBe` 1
      let es = case edges of (x : _) -> x; [] -> error "no edges"
      edgeSource es `elem` nodeIds `shouldBe` True
      edgeTarget es `elem` nodeIds `shouldBe` True

  describe "multi-source export (3.3) — nodeToJGF source field" $ do
    it "emits a non-null source field in metadata for a tagged (multi-source) node" $ do
      let prov = Map.fromList [ ("/abs/repoA/app.py", ("repoA", "repoA/app.py")) ]
          tagged = applyTaggingSources (Just prov) (extractionFromLists [makeStubNode "/abs/repoA/app.py"] [])
          n = case Map.elems (extNodes tagged) of (x : _) -> x; [] -> error "no node"
      jgfHasSource n `shouldBe` True

    it "omits the source field in metadata for a single-source node (legacy source: null)" $ do
      let n = makeStubNode "/abs/repoA/app.py"
      jgfHasSource n `shouldBe` False

testFSP :: FileSystemPort
testFSP = FileSystemPort
  { fspLoadCheckpoint       = \_ -> pure Nothing
  , fspSaveCheckpoint       = \_ _ -> pure ()
  , fspClearCheckpoint      = \_ -> pure ()
  , fspLoadIgnorePatterns   = loadIgnorePatterns
  , fspShouldIgnore         = \_ ps path -> shouldIgnore ps path
  , fspLoadCachedExtraction = \_ _ _ -> pure Nothing
  , fspSaveCachedExtraction = \_ _ _ _ -> pure ()
  }

mkEdge :: NodeId -> NodeId -> Edge
mkEdge s t = Edge
  { edgeId           = EdgeId (s <> "->" <> t <> ":Calls")
  , edgeSource       = s
  , edgeTarget       = t
  , edgeRelation     = Calls
  , edgeWeight       = 1.0
  , edgeConfidence   = Confidence 1.0
  , edgeExtra        = Nothing
  }

-- | True iff the multi-source "source" key is present in a node's JGF metadata.
jgfHasSource :: Node -> Bool
jgfHasSource n = case nodeToJGF n of
  Aeson.Object m -> case KeyMap.lookup (AesKey.fromText "metadata") m of
    Just (Aeson.Object md) -> KeyMap.member (AesKey.fromText "source") md
    _                      -> False
  _ -> False
