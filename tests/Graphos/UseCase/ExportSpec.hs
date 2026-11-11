module Graphos.UseCase.ExportSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, modifyIORef, writeIORef)
import System.IO.Temp (withSystemTempDirectory)
import Data.List (isInfixOf)

import Graphos.Domain.Types hiding (PushMode(..))
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Community.Quality (ClusterQuality(..), BunchSkip(..))
import Graphos.UseCase.Port.ExportPort
import Graphos.UseCase.Export (exportAll)
import Graphos.Infrastructure.Export.JSON (exportGraph)

spec :: Spec
spec = describe "exportAll" $ do
  it "passes mLabels to epExportHTML" $ do
    withSystemTempDirectory "graphos-export-test" $ \tmpDir -> do
      ref <- newIORef Nothing
      let g = buildGraph False (extractionFromLists [] [])
          analysis = Analysis
            { analysisCommunities   = Map.empty
            , analysisNullModel     = DefaultNullModel
            , analysisCohesion      = Map.empty
            , analysisGodNodes      = []
            , analysisSurprises     = []
            , analysisQuestions     = []
            , analysisArticulation  = []
            , analysisBccCount      = 0
            }
          detection = Detection
            { detectionTotalFiles = 0
            , detectionTotalWords = 0
            , detectionNeedsGraph = True
            , detectionWarning = Nothing
            , detectionFiles = Map.empty
            , detectionClassification = Map.empty
            , detectionExclusions = emptyExclusionCounts
            }
          labels = Just (Map.fromList [(1 :: Int, T.pack "Auth")])
          quality = ClusterQuality
            { cqAlgorithm = T.pack "leiden"
            , cqMQ = 0
            , cqModularity = 0
            , cqIntraFraction = 0
            , cqClusters = 0
            , cqSkip = Nothing
            }
          port = ExportPort
            { epExportHTML = \_g _analysis mLbl _aggregates _path -> modifyIORef ref (const (Just mLbl))
            , epExportObsidian = \_ _ _ -> pure ()
            , epExportReport = \_ _ -> pure ()
            , epExportCypher = \_ _ -> pure ()
            , epExportMemgraphCypher = \_ _ -> pure ()
            , epPushToNeo4jFull = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToNeo4jSubgraph = \_ _ _ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToNeo4jCommunity = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushEdgeRepair = \_ _ _ _ -> pure ("", 0, 0)
            , epPushToMemgraphFull = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToMemgraphSubgraph = \_ _ _ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToMemgraphCommunity = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epOpenIncrementalWriter = \_ -> pure undefined
            , epWriteNodes = \_ _ -> pure ()
            , epWriteEdges = \_ _ -> pure ()
            , epWriteCommunities = \_ _ -> pure ()
            , epWriteCohesion = \_ _ -> pure ()
            , epWriteGodNodes = \_ _ -> pure ()
            , epWriteAnalysisTail = \_ _ -> pure ()
            , epWriteCommunityAggregates = \_ _ -> pure ()
            , epWriteCompositions = \_ _ -> pure ()
            , epWriteEmbeddingsPath = \_ _ -> pure ()
            , epFlushWriter = \_ -> pure ()
            , epCloseWriter = \_ -> pure ()
            , epExportCommunityGraph = \_ _ _ -> pure ()
            , epSaveCheckpoint = \_ _ _ -> pure ()
            , epLoadCheckpoint = \_ -> pure (Left "no checkpoint")
            , epExportAll = undefined
            }
          config = defaultConfig { cfgOutputDir = tmpDir, cfgNoViz = False }
      _ <- exportAll port g analysis config detection labels [] quality
      recorded <- readIORef ref
      recorded `shouldBe` Just labels

  it "renders the passed quality, including a Bunch->Leiden fallback" $ do
    withSystemTempDirectory "graphos-export-quality-test" $ \tmpDir -> do
      reportRef <- newIORef T.empty
      let g = buildGraph False (extractionFromLists [] [])
          analysis = Analysis
            { analysisCommunities   = Map.empty
            , analysisNullModel     = DefaultNullModel
            , analysisCohesion      = Map.empty
            , analysisGodNodes      = []
            , analysisSurprises     = []
            , analysisQuestions     = []
            , analysisArticulation  = []
            , analysisBccCount      = 0
            }
          detection = Detection
            { detectionTotalFiles = 0
            , detectionTotalWords = 0
            , detectionNeedsGraph = True
            , detectionWarning = Nothing
            , detectionFiles = Map.empty
            , detectionClassification = Map.empty
            , detectionExclusions = emptyExclusionCounts
            }
          quality = ClusterQuality
            { cqAlgorithm = T.pack "leiden"
            , cqMQ = 1.0
            , cqModularity = 0.5
            , cqIntraFraction = 0.25
            , cqClusters = 2
            , cqSkip = Just (BunchSkip 12 10)
            }
          port = ExportPort
            { epExportHTML = \_ _ _ _ _ -> pure ()
            , epExportObsidian = \_ _ _ -> pure ()
            , epExportReport = \content _ -> writeIORef reportRef content
            , epExportCypher = \_ _ -> pure ()
            , epExportMemgraphCypher = \_ _ -> pure ()
            , epPushToNeo4jFull = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToNeo4jSubgraph = \_ _ _ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToNeo4jCommunity = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushEdgeRepair = \_ _ _ _ -> pure ("", 0, 0)
            , epPushToMemgraphFull = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToMemgraphSubgraph = \_ _ _ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToMemgraphCommunity = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epOpenIncrementalWriter = \_ -> pure undefined
            , epWriteNodes = \_ _ -> pure ()
            , epWriteEdges = \_ _ -> pure ()
            , epWriteCommunities = \_ _ -> pure ()
            , epWriteCohesion = \_ _ -> pure ()
            , epWriteGodNodes = \_ _ -> pure ()
            , epWriteAnalysisTail = \_ _ -> pure ()
            , epWriteCommunityAggregates = \_ _ -> pure ()
            , epWriteCompositions = \_ _ -> pure ()
            , epWriteEmbeddingsPath = \_ _ -> pure ()
            , epFlushWriter = \_ -> pure ()
            , epCloseWriter = \_ -> pure ()
            , epExportCommunityGraph = \_ _ _ -> pure ()
            , epSaveCheckpoint = \_ _ _ -> pure ()
            , epLoadCheckpoint = \_ -> pure (Left "no checkpoint")
            , epExportAll = undefined
            }
          -- Config says Bunch, but the stage fell back to Leiden above the
          -- file cap; the report must describe what actually ran.
          config = defaultConfig
            { cfgOutputDir = tmpDir
            , cfgNoViz = True
            , cfgClusterAlgorithm = Just (T.pack "bunch")
            }
      _ <- exportAll port g analysis config detection Nothing [] quality
      content <- readIORef reportRef
      ("algorithm=leiden" `T.isInfixOf` content) `shouldBe` True
      ("algorithm=bunch" `T.isInfixOf` content) `shouldBe` False
      ("bunch skipped: 12 files" `T.isInfixOf` content) `shouldBe` True

  describe "JSON null-model export (AF-5)" $ do
    it "records the chosen null model in graph.json" $ do
      withSystemTempDirectory "graphos-nullmodel-test" $ \tmpDir -> do
        let g = buildGraph False (extractionFromLists [] [])
            analysis = Analysis
              { analysisCommunities   = Map.empty
              , analysisNullModel     = DefaultNullModel
              , analysisCohesion      = Map.empty
              , analysisGodNodes      = []
              , analysisSurprises     = []
              , analysisQuestions     = []
              , analysisArticulation  = []
              , analysisBccCount      = 0
              }
            path = tmpDir ++ "/graph.json"
        exportGraph g analysis path
        contents <- readFile path
        ("\"null_model\"" `isInfixOf` contents) `shouldBe` True
