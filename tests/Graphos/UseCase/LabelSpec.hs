{-# LANGUAGE OverloadedStrings #-}

module Graphos.UseCase.LabelSpec where

import Test.Hspec
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
-- (no aeson import needed)
-- BSL not needed
import Graphos.Domain.Types (CommunityId, CommunityMap)
import Graphos.Domain.Graph (Graph(..))
import Graphos.Domain.Config (defaultLabelingConfig, LabelingConfig(..))
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Label (labelCommunities)
import Graphos.UseCase.Port.LLMPort (LLMPort(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
-- (FileSystemPort not needed directly)
import Graphos.Infrastructure.Wiring (productionFileSystemPort)
import Graphos.Infrastructure.FileSystem.Cache (loadLabelCache)
import Graphos.Domain.Labeling (LabelingResult(..), LabelCacheFile(..), lcfEntries)
import Graphos.Infrastructure.LLM.OpenAI (parseLabelsFromResponse)
-- TIO not needed
emptyGraph :: Graph
emptyGraph = Graph
  { gNodes     = Map.empty
  , gEdges     = Map.empty
  , gAdjFwd   = Map.empty
  , gAdjBack  = Map.empty
  , gDirected = False
  , gCompositions = Nothing
  , gHash     = ""
  , gEmbeddings = Nothing
  , gEmbeddingsPath = Nothing
  }

capturingLP :: IORef [T.Text] -> LoggingPort
capturingLP ref = LoggingPort
  { lpLogTrace = add
  , lpLogDebug = add
  , lpLogInfo  = add
  , lpLogWarn  = add
  , lpLogError = add
  }
  where add t = modifyIORef' ref (t :)

countingLabelLLM :: IORef Int -> LLMPort
countingLabelLLM callsRef =
  let base = LLMPort
        { lpCallLLM = \_cfg _prompt -> do
            modifyIORef' callsRef (+ 1)
            pure (Right "{\"1\":\"L1\",\"2\":\"L2\",\"3\":\"L3\"}")
        , lpParseLabelsFromResponse = parseLabelsFromResponse
        , lpGenerateEmbedding = \_ _ -> pure (Left "unused")
        , lpGenerateEmbeddings = \_ _ -> pure (Left "unused")
        , lpAnalyzeImage = \_ _ _ -> pure (Left "unused")
        , lpValidateUrl = pure
        }
  in base

mkAppEnv :: IORef [T.Text] -> IORef Int -> FilePath -> AppEnv
mkAppEnv logRef callsRef _outDir = AppEnv
  { extractionPort     = error "not used"
  , exportPort         = error "not used"
  , fileSystemPort     = productionFileSystemPort
  , loggingPort        = capturingLP logRef
  , observabilityPort  = error "not used"
  , llmPort            = countingLabelLLM callsRef
  }

spec :: Spec
spec = do
  describe "label cache load tolerance" $ do
    it "missing file -> empty" $ withSystemTempDirectory "label-cache-missing" $ \dir -> do
      r <- loadLabelCache (dir </> "out")
      r `shouldBe` LabelCacheFile []

    it "malformed file -> Left warning" $ withSystemTempDirectory "label-cache-bad" $ \dir -> do
      let outDir = dir </> "out"
      createDirectoryIfMissing True (outDir </> "cache")
      writeFile (outDir </> "cache" </> "label-cache.json") "not json"
      r <- loadLabelCache outDir
      r `shouldBe` LabelCacheFile []

  describe "labelCommunities cache behavior" $ do
    it "first run labels all; second run uses cache with zero LLM calls" $ withSystemTempDirectory "label-run" $ \dir -> do
      let outDir = dir </> "out"
      createDirectoryIfMissing True outDir
      logRef <- newIORef []
      callsRef <- newIORef (0 :: Int)
      let appEnv = mkAppEnv logRef callsRef outDir
          cfg = defaultLabelingConfig { labelingModel = "m1", labelingBatchSize = 20 }
          comm1 = Map.fromList [(1 :: CommunityId, ["a","b" :: T.Text]), (2 :: CommunityId, ["c","d" :: T.Text])] :: CommunityMap

      _ <- labelCommunities appEnv emptyGraph comm1 Map.empty cfg outDir
      c1 <- readIORef callsRef
      c1 `shouldBe` 1

      -- reset counter
      writeIORef callsRef 0
      _ <- labelCommunities appEnv emptyGraph comm1 Map.empty cfg outDir
      c2 <- readIORef callsRef
      c2 `shouldBe` 0

      logs <- readIORef logRef
      (any (T.isInfixOf "cache:") logs) `shouldBe` True

      -- cache contains exactly 2 communities
      cached <- loadLabelCache outDir
      length (lcfEntries cached) `shouldBe` 2

    it "one new community triggers exactly one LLM batch" $ withSystemTempDirectory "label-new" $ \dir -> do
      let outDir = dir </> "out"
      createDirectoryIfMissing True outDir
      logRef <- newIORef []
      callsRef <- newIORef (0 :: Int)
      let appEnv = mkAppEnv logRef callsRef outDir
          cfg = defaultLabelingConfig { labelingModel = "m1", labelingBatchSize = 20 }
          comm1 = Map.fromList [(1 :: CommunityId, ["a","b" :: T.Text]), (2 :: CommunityId, ["c","d" :: T.Text])] :: CommunityMap
      _ <- labelCommunities appEnv emptyGraph comm1 Map.empty cfg outDir
      writeIORef callsRef 0

      let comm2 = Map.fromList [(1 :: CommunityId, ["a","b" :: T.Text]), (2 :: CommunityId, ["c","d" :: T.Text]), (3 :: CommunityId, ["e","f" :: T.Text])] :: CommunityMap
      _ <- labelCommunities appEnv emptyGraph comm2 Map.empty cfg outDir
      c <- readIORef callsRef
      c `shouldBe` 1

      cached <- loadLabelCache outDir
      length (lcfEntries cached) `shouldBe` 3

    it "model change yields zero reuse" $ withSystemTempDirectory "label-model" $ \dir -> do
      let outDir = dir </> "out"
      createDirectoryIfMissing True outDir
      logRef <- newIORef []
      callsRef <- newIORef (0 :: Int)
      let appEnv = mkAppEnv logRef callsRef outDir
          cfg1 = defaultLabelingConfig { labelingModel = "m1", labelingBatchSize = 20 }
          cfg2 = defaultLabelingConfig { labelingModel = "m2", labelingBatchSize = 20 }
          comm = Map.fromList [(1 :: CommunityId, ["a","b" :: T.Text])] :: CommunityMap
      _ <- labelCommunities appEnv emptyGraph comm Map.empty cfg1 outDir
      writeIORef callsRef 0
      res <- labelCommunities appEnv emptyGraph comm Map.empty cfg2 outDir
      -- all re-labeled, at least one call
      c <- readIORef callsRef
      c `shouldSatisfy` (>= 1)
      Map.lookup 1 (lrLabels res) `shouldSatisfy` (/= Nothing)
