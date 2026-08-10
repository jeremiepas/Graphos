-- | Core pipeline orchestration — the full detect→extract→build→cluster→export flow.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Pipeline.Core
  ( runPipeline
  , PipelineResult(..)
  , edgeCollapseThreshold
  ) where

import Control.DeepSeq (deepseq)
import Control.Exception (catch, SomeException, evaluate)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (createDirectoryIfMissing)
import System.Mem (performGC)

import Graphos.Domain.Types hiding (PushMode(..))
import Graphos.Domain.Types.Pipeline (Neo4jStreamingConfig(..), PipelineStep(..), PipelineCheckpoint(..))
import Graphos.Domain.Config (FileExtensionConfig(..))
import Graphos.Domain.Graph (gNodes, gEdges)
import qualified Graphos.Domain.Graph.Analysis as GAnalysis
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.UseCase.Port.ObservabilityPort (ObservabilityPort(..), StartTime(..), EndTime(..))
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..))
import qualified Graphos.UseCase.Port.ExportPort as UEP
import Graphos.UseCase.Port.ExportPort (ExportPort(..))
import Graphos.UseCase.Detect (detectFilesWithExtensionsAndIgnore')
import Graphos.UseCase.Extract (extractAll)
import Graphos.UseCase.Build (buildGraphFromExtractions)
import Graphos.UseCase.Cluster (clusterGraphWithResolution, joinCommunitiesToNodes, computeCommunityAggregates)
import Graphos.Domain.Community (Resolution(..), MergeStrategy(..))
import Graphos.UseCase.Analyze (analyzeGraph)
import Graphos.UseCase.Infer (inferEdges)
import Graphos.UseCase.Report (generateReport)
import Graphos.UseCase.Label (labelCommunities)
import Graphos.Domain.Labeling (LabelingResult(..))

-- | Minimum ratio of edges to nodes for a code-dominant graph. Values below
-- this threshold after the build step indicate a likely edge-extraction
-- collapse and are logged as a prominent warning.
edgeCollapseThreshold :: Double
edgeCollapseThreshold = 0.05

-- | Pipeline result
data PipelineResult = PipelineResult
  { prNodes        :: Int
  , prEdges        :: Int
  , prCommunities  :: Int
  , prReportPath   :: FilePath
  , prGraphPath    :: FilePath
  , prHtmlPath     :: Maybe FilePath
  , prNeo4jPath   :: Maybe FilePath
  } deriving (Eq, Show)

-- | Run the full pipeline
runPipeline :: AppEnv -> PipelineConfig -> IO (Either Text PipelineResult)
runPipeline appEnv config = catch (do
  let lp = loggingPort appEnv
      op = observabilityPort appEnv
      fsp = fileSystemPort appEnv
      ep = exportPort appEnv

  let configWithStreaming = case (cfgNeo4j config, cfgNeo4jPush config) of
        (True, Just uri) -> config { cfgNeo4jStreaming = Just Neo4jStreamingConfig
                                              { neo4jsUri = uri
                                              , neo4jsUser = T.pack (neo4jUser (gcNeo4j (cfgGraphosConfig config)))
                                              , neo4jsPassword = T.pack (neo4jPassword (gcNeo4j (cfgGraphosConfig config)))
                                              } }
        (True, Nothing) -> let n4cfg = gcNeo4j (cfgGraphosConfig config)
                            in config { cfgNeo4jStreaming = Just Neo4jStreamingConfig
                                              { neo4jsUri = T.pack (neo4jUri n4cfg)
                                              , neo4jsUser = T.pack (neo4jUser n4cfg)
                                              , neo4jsPassword = T.pack (neo4jPassword n4cfg)
                                              } }
        _ -> config

  mCheckpoint <- fspLoadCheckpoint fsp (cfgOutputDir configWithStreaming)
  case mCheckpoint of
    Just chk -> do
      lpLogInfo lp $ T.pack $ "Resuming from checkpoint: step " ++ show (chkCurrentStep chk)
                           ++ ", " ++ show (length (chkFilesExtracted chk)) ++ " files already extracted"
    Nothing -> lpLogInfo lp "No checkpoint found, starting fresh pipeline"

  lpLogInfo lp "Step 1: Detecting files..."
  detectStart <- getCurrentTime
  ignorePatterns <- fspLoadIgnorePatterns fsp (cfgInputPath configWithStreaming)
  let fec = gcFileExtensions (cfgGraphosConfig configWithStreaming)
      extMap = Map.fromList
        [ (CodeFiles, fecCode fec)
        , (DocFiles, fecDoc fec)
        , (PaperFiles, fecPaper fec)
        , (ImageFiles, fecImage fec)
        , (VideoFiles, fecVideo fec)
        , (OfficeFiles, fecOffice fec)
        ]
  detection <- detectFilesWithExtensionsAndIgnore' fsp (cfgInputPath configWithStreaming) extMap ignorePatterns
  detectEnd <- getCurrentTime
  opRecordHistogram op "graphos_pipeline_step_duration_seconds" (realToFrac (diffUTCTime detectEnd detectStart) :: Double)
  opIncCounter op "graphos_pipeline_steps_total" 1
  opDebugTraceSpan op "detect" (StartTime detectStart) (EndTime detectEnd) Map.empty
  if null (allFiles detection)
    then pure $ Left "No supported files found"
    else do
      lpLogInfo lp $ T.pack $ "  Found " ++ show (detectionTotalFiles detection) ++ " files"
      lpLogDebug lp $ T.pack $ "  File categories: " ++ show (Map.keys (detectionFiles detection))
      lpLogTrace lp $ T.pack $ "  Code files: " ++ show (Map.findWithDefault [] CodeFiles (detectionFiles detection))

      now <- getCurrentTime
      let pipelineId = T.pack $ show now
          checkpoint = PipelineCheckpoint
                        { chkPipelineId = pipelineId
                        , chkCurrentStep = StepExtract
                        , chkCompletedSteps = [StepDetect]
                        , chkFilesExtracted = []
                        , chkFilesPushedNeo4j = []
                        , chkStartedAt = T.pack $ show now
                        }
      fspSaveCheckpoint fsp (cfgOutputDir configWithStreaming) checkpoint

      lpLogInfo lp "Step 2: Extracting entities and relationships..."
      extractStart <- getCurrentTime
      extraction <- extractAll appEnv configWithStreaming detection
      extractEnd <- getCurrentTime
      opRecordHistogram op "graphos_extract_duration_seconds" (realToFrac (diffUTCTime extractEnd extractStart) :: Double)
      opIncCounter op "graphos_pipeline_steps_total" 1
      opSetGauge op "graphos_nodes_extracted" (fromIntegral $ Map.size (extractionNodes extraction))
      opSetGauge op "graphos_edges_extracted" (fromIntegral $ Map.size (extractionEdges extraction))
      opDebugTraceSpan op "extract" (StartTime extractStart) (EndTime extractEnd) (Map.fromList [("nodes", T.pack $ show $ Map.size (extractionNodes extraction)), ("edges", T.pack $ show $ Map.size (extractionEdges extraction))])
      lpLogInfo lp $ T.pack $ "  " ++ show (Map.size (extractionNodes extraction)) ++ " nodes, " ++
                  show (Map.size (extractionEdges extraction)) ++ " edges"
      lpLogDebug lp $ T.pack $ "  Nodes: " ++ show (Map.elems (extractionNodes extraction))

      when (cfgNeo4jStreaming configWithStreaming /= Nothing) $ do
        lpLogInfo lp "  [neo4j-stream] Running edge repair pass..."
        let graph = buildGraphFromExtractions (cfgDirected configWithStreaming) [extraction]
        (_msg, stmts, batches) <- epPushEdgeRepair ep graph
          (case cfgNeo4jStreaming configWithStreaming of
             Just s -> neo4jsUri s
             Nothing -> "http://localhost:7474")
          (case cfgNeo4jStreaming configWithStreaming of
             Just s -> neo4jsUser s
             Nothing -> "neo4j")
          (case cfgNeo4jStreaming configWithStreaming of
             Just s -> neo4jsPassword s
             Nothing -> "graphos_dev")
        lpLogInfo lp $ T.pack $ "  [neo4j-stream] Edge repair: " ++ show stmts ++ " statements in " ++ show batches ++ " batches"

      lpLogInfo lp "Step 3: Building graph..."
      buildStart <- getCurrentTime
      let graph = buildGraphFromExtractions (cfgDirected configWithStreaming) [extraction]
      _ <- evaluate (Map.size (gNodes graph) + Map.size (gEdges graph))
      graph `deepseq` pure ()
      buildEnd <- getCurrentTime
      opRecordHistogram op "graphos_build_duration_seconds" (realToFrac (diffUTCTime buildEnd buildStart) :: Double)
      opIncCounter op "graphos_pipeline_steps_total" 1
      opSetGauge op "graphos_graph_nodes" (fromIntegral $ Map.size (gNodes graph))
      opSetGauge op "graphos_graph_edges" (fromIntegral $ Map.size (gEdges graph))
      opDebugTraceSpan op "build" (StartTime buildStart) (EndTime buildEnd) (Map.fromList [("nodes", T.pack $ show $ Map.size (gNodes graph)), ("edges", T.pack $ show $ Map.size (gEdges graph))])
      lpLogInfo lp $ T.pack $ "  Graph: " ++ show (Map.size (gNodes graph)) ++ " nodes, " ++ show (Map.size (gEdges graph)) ++ " edges"

      let codeFiles = length $ Map.findWithDefault [] CodeFiles (detectionFiles detection)
          nonCodeFiles = detectionTotalFiles detection - codeFiles
          nodeCount = fromIntegral (Map.size (gNodes graph)) :: Double
          edgeCount = fromIntegral (Map.size (gEdges graph)) :: Double
          ratio = if nodeCount == 0 then 0 else edgeCount / nodeCount
      when (codeFiles > nonCodeFiles && nodeCount > 0 && ratio < edgeCollapseThreshold) $
        lpLogInfo lp $ T.pack $ "  WARNING: edge/node ratio (" ++ show ratio ++ ") is below threshold " ++ show edgeCollapseThreshold ++ "; edge extraction may have collapsed"

      createDirectoryIfMissing True (cfgOutputDir configWithStreaming)
      lpLogInfo lp $ T.pack $ "  Streaming graph data to " ++ cfgOutputDir configWithStreaming ++ "/graph.json"
      iw <- epOpenIncrementalWriter ep (cfgOutputDir configWithStreaming ++ "/graph.json")

      let checkpointPath = cfgOutputDir configWithStreaming ++ "/graph.checkpoint.json"
      epSaveCheckpoint ep graph checkpointPath
      lpLogInfo lp $ T.pack $ "  Checkpoint saved: " ++ checkpointPath

      performGC

      (enrichedGraph, finalCommMap, _finalCohesion, analysis, llmLabelsResult) <-
        if cfgNoCluster configWithStreaming
          then do
            lpLogInfo lp "Step 4: Skipping clustering (--no-cluster)"
            let emptyCommMap = Map.empty :: CommunityMap
                emptyCohesion = Map.empty :: CohesionMap
                noAnalysis = analyzeGraph graph emptyCommMap emptyCohesion
            epWriteCommunities ep iw emptyCommMap
            epWriteCohesion ep iw emptyCohesion
            epWriteGodNodes ep iw (analysisGodNodes noAnalysis)
            epWriteAnalysisTail ep iw Nothing
            epCloseWriter ep iw
            pure (graph, emptyCommMap, emptyCohesion, noAnalysis, Nothing :: Maybe (Map.Map CommunityId Text))
          else do
            lpLogInfo lp "Step 4: Detecting communities..."
            clusterStart <- getCurrentTime
            let res = Resolution { resGamma = cfgResolution configWithStreaming
                                 , resMinSize = cfgMinCommSize configWithStreaming
                                 , resMergeInto = MergeToNeighbor
                                 , resMaxIterations = cfgMaxLeidenIterations configWithStreaming
                                 }
                (commMap, cohesion) = clusterGraphWithResolution graph res
            _ <- evaluate (Map.size commMap + sum (map length (Map.elems commMap)))
            (commMap, cohesion) `deepseq` pure ()
            clusterEnd <- getCurrentTime
            opRecordHistogram op "graphos_cluster_duration_seconds" (realToFrac (diffUTCTime clusterEnd clusterStart) :: Double)
            opIncCounter op "graphos_pipeline_steps_total" 1
            opSetGauge op "graphos_communities" (fromIntegral $ Map.size commMap)
            opDebugTraceSpan op "cluster" (StartTime clusterStart) (EndTime clusterEnd) (Map.fromList [("communities", T.pack $ show $ Map.size commMap)])

            let allInferred = inferEdges (cfgEdgeDensity configWithStreaming) graph commMap
                enrichedGraph' = if null allInferred
                  then graph
                  else buildGraphFromExtractions (cfgDirected configWithStreaming)
                       [extractionFromLists (Map.elems (gNodes graph))
                                            (Map.elems (gEdges graph) ++ allInferred)]
            enrichedGraph' `deepseq` pure ()
            lpLogInfo lp $ T.pack $ "  Inferred " ++ show (length allInferred) ++ " additional edges (density: " ++ show (cfgEdgeDensity configWithStreaming) ++ ")"

            lpLogInfo lp "Step 5: Re-clustering and analyzing..."
            step5Start <- getCurrentTime
            let (finalComm, finalCohes) = clusterGraphWithResolution enrichedGraph' res
                anal = analyzeGraph enrichedGraph' finalComm finalCohes
            _ <- evaluate (Map.size finalComm + sum (map length (Map.elems finalComm)))
            _ <- evaluate (length (analysisGodNodes anal))
            (finalComm, finalCohes) `deepseq` pure ()
            step5End <- getCurrentTime
            opRecordHistogram op "graphos_cluster_step5_duration_seconds" (realToFrac (diffUTCTime step5End step5Start) :: Double)
            opDebugTraceSpan op "cluster_step5" (StartTime step5Start) (EndTime step5End) (Map.fromList [("communities", T.pack $ show $ Map.size finalComm)])
            lpLogInfo lp $ T.pack $ "  Re-cluster: " ++ show (Map.size finalComm) ++ " communities"

            let joinedGraph = joinCommunitiesToNodes enrichedGraph' finalComm

            epWriteNodes ep iw (Map.elems (gNodes joinedGraph))
            epWriteEdges ep iw (Map.elems (gEdges joinedGraph))
            epWriteCommunities ep iw finalComm
            epWriteCohesion ep iw finalCohes
            epWriteGodNodes ep iw (analysisGodNodes anal)

            llmLabels <- if cfgLabel configWithStreaming
              then do
                lpLogInfo lp "Step 5b: Labeling communities via LLM..."
                let lblCfg = gcLabeling (cfgGraphosConfig configWithStreaming)
                lpLogInfo lp $ T.pack $ "  Labeling config: provider=" ++ labelingProvider lblCfg
                                       ++ " model=" ++ labelingModel lblCfg
                                       ++ " baseUrl=" ++ labelingBaseUrl lblCfg
                                       ++ " batchSize=" ++ show (labelingBatchSize lblCfg)
                labelingStart <- getCurrentTime
                result <- labelCommunities appEnv enrichedGraph' finalComm finalCohes lblCfg
                labelingEnd <- getCurrentTime
                lpLogInfo lp $ T.pack $ "  Labeled " ++ show (Map.size (lrLabels result)) ++ " communities in "
                                       ++ show (diffUTCTime labelingEnd labelingStart) ++ "s"
                pure (Just (lrLabels result))
              else pure Nothing

            let artPoints = GAnalysis.articulationPoints enrichedGraph'
                aggregates = computeCommunityAggregates joinedGraph finalComm finalCohes artPoints llmLabels
            _ <- evaluate (length aggregates)
            epWriteCommunityAggregates ep iw aggregates

            epWriteAnalysisTail ep iw llmLabels
            epFlushWriter ep iw
            epCloseWriter ep iw
            lpLogDebug lp "  Final graph, communities, and cohesion written incrementally"
            pure (enrichedGraph', finalComm, finalCohes, anal, llmLabels)

      lpLogInfo lp "  graph.json written incrementally"

      performGC

      lpLogInfo lp "Step 6: Generating report..."
      let _report = generateReport enrichedGraph analysis configWithStreaming detection llmLabelsResult

      lpLogInfo lp "Step 7: Exporting outputs..."
      exportStart <- getCurrentTime
      createDirectoryIfMissing True (cfgOutputDir configWithStreaming)
      exports <- UEP.epExportAll ep enrichedGraph (cfgOutputDir configWithStreaming) analysis configWithStreaming detection llmLabelsResult
      exportEnd <- getCurrentTime
      opRecordHistogram op "graphos_export_duration_seconds" (realToFrac (diffUTCTime exportEnd exportStart) :: Double)
      opIncCounter op "graphos_pipeline_steps_total" 1

      when (cfgNeo4j configWithStreaming) $ do
        lpLogInfo lp "  Neo4j: Cypher export + push complete"

      when (cfgCommunityGraph configWithStreaming && not (cfgNoCluster configWithStreaming)) $ do
        lpLogInfo lp "Step 7b: Exporting community-level graph..."
        epExportCommunityGraph ep enrichedGraph finalCommMap (cfgOutputDir configWithStreaming ++ "/community_graph.json")
        lpLogInfo lp $ T.pack $ "  Community graph: " ++ cfgOutputDir configWithStreaming ++ "/community_graph.json"

      fspClearCheckpoint fsp (cfgOutputDir configWithStreaming)

      opShutdownObservability op

      let result = PipelineResult
            { prNodes       = Map.size (gNodes enrichedGraph)
            , prEdges       = Map.size (gEdges enrichedGraph)
            , prCommunities = Map.size finalCommMap
            , prReportPath  = UEP.erReport exports
            , prGraphPath   = UEP.erJSON exports
            , prHtmlPath    = UEP.erHTML exports
            , prNeo4jPath  = UEP.erNeo4j exports
            }
      lpLogInfo lp "Graph complete!"
      pure $ Right result
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Pipeline error: " ++ show e
  where
    allFiles d = concat (Map.elems (detectionFiles d))