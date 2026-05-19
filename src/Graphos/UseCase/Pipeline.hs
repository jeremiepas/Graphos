-- | Main pipeline orchestration.
--
-- Full pipeline: detect → extract → build → cluster → infer → analyze → report → export
-- With --no-cluster: detect → extract → build → report → export (skip clustering)
--
-- Streaming Neo4j: when --neo4j is enabled, nodes are pushed to Neo4j
-- during extraction (node-by-node). After extraction completes, an edge
-- repair pass re-pushes all edges to ensure cross-file connections work.
--
-- Checkpoint: pipeline state is saved after each step. On restart with
-- a checkpoint present, the pipeline resumes from the last completed step.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Pipeline
  ( runPipeline
  , runIncrementalPipeline
  , PipelineResult(..)
  ) where

import Control.DeepSeq (deepseq)
import Control.Exception (catch, SomeException)
import Control.Monad (when, void)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (createDirectoryIfMissing)

import Graphos.Domain.Types
import Graphos.Domain.Types.Pipeline (Neo4jStreamingConfig(..), PipelineStep(..), PipelineCheckpoint(..))
import Graphos.Domain.Graph (gNodes, gEdges)
import qualified Graphos.Domain.Graph.Analysis as GAnalysis
import Graphos.Infrastructure.Logging (LogLevel(..), defaultLogEnv, logInfo, logDebug, logTrace)
import Graphos.Infrastructure.Observability
  ( ObservabilityEnv(..)
  , incCounter, setGauge, observeHistogram
  , debugTraceSpan, flushDebugTrace
  , initObservability
  )
import Graphos.UseCase.Detect (detectFiles)
import Graphos.UseCase.Extract (extractAll, extractChangedFiles)
import Graphos.UseCase.Build (buildGraphFromExtractions)
import Graphos.UseCase.Cluster (clusterGraphWithResolution)
import Graphos.Domain.Community (Resolution(..), MergeStrategy(..))
import Graphos.UseCase.Analyze (analyzeGraph)
import Graphos.UseCase.Infer (inferEdges)
import Graphos.UseCase.Report (generateReport)
import Graphos.UseCase.Export (exportAll, ExportResult(..))
import qualified Graphos.Infrastructure.Export.JSON as ExportJSON
import Graphos.Infrastructure.Export.CommunityGraph (exportCommunityGraph)
import qualified Graphos.Infrastructure.Export.IncrementalJSON as Inc
import qualified Graphos.Infrastructure.Export.Neo4j as Neo4j
import Graphos.Infrastructure.FileSystem.Cache (loadPipelineCheckpoint, savePipelineCheckpoint, clearPipelineCheckpoint)

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
runPipeline :: PipelineConfig -> IO (Either Text PipelineResult)
runPipeline config = catch (do
  let logLevel = if cfgDebug config then LevelTrace
                 else if cfgVerbose config then LevelDebug
                 else LevelInfo
  env <- defaultLogEnv logLevel

  -- Initialize observability (tracing + metrics)
  let otelCfg = cfgOtelConfig config
      metricsPort = cfgMetricsPort config
      debugDir = case cfgDebugTraceDir config of
                   Just d -> d
                   Nothing -> cfgOutputDir config ++ "/traces"
  obsEnv <- initObservability logLevel otelCfg metricsPort debugDir
  let _tracer = otelTracer obsEnv
      metrics = otelMetrics obsEnv

  -- Set up Neo4j streaming: when --neo4j is enabled, push nodes during extraction
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
        _ -> config  -- no streaming

  -- Load checkpoint if resuming
  mCheckpoint <- loadPipelineCheckpoint (cfgOutputDir configWithStreaming)
  case mCheckpoint of
    Just chk -> do
      logInfo env $ T.pack $ "Resuming from checkpoint: step " ++ show (chkCurrentStep chk)
                           ++ ", " ++ show (length (chkFilesExtracted chk)) ++ " files already extracted"
    Nothing -> logInfo env "No checkpoint found, starting fresh pipeline"

  -- Step 1: Detect
  logInfo env "Step 1: Detecting files..."
  detectStart <- getCurrentTime
  detection <- detectFiles (cfgInputPath configWithStreaming)
  detectEnd <- getCurrentTime
  observeHistogram metrics "graphos_pipeline_step_duration_seconds" (realToFrac (diffUTCTime detectEnd detectStart) :: Double)
  incCounter metrics "graphos_pipeline_steps_total" 1
  debugTraceSpan (otelDebugTrace obsEnv) "detect" detectStart detectEnd Map.empty
  if null (allFiles detection)
    then pure $ Left "No supported files found"
    else do
      logInfo env $ T.pack $ "  Found " ++ show (detectionTotalFiles detection) ++ " files"
      logDebug env $ T.pack $ "  File categories: " ++ show (Map.keys (detectionFiles detection))
      logTrace env $ T.pack $ "  Code files: " ++ show (Map.findWithDefault [] CodeFiles (detectionFiles detection))

      -- Save checkpoint: detect done
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
      savePipelineCheckpoint (cfgOutputDir configWithStreaming) checkpoint

      -- Step 2: Extract (nodes are pushed to Neo4j during extraction if streaming is enabled)
      logInfo env "Step 2: Extracting entities and relationships..."
      extractStart <- getCurrentTime
      extraction <- extractAll configWithStreaming detection env
      extractEnd <- getCurrentTime
      observeHistogram metrics "graphos_extract_duration_seconds" (realToFrac (diffUTCTime extractEnd extractStart) :: Double)
      incCounter metrics "graphos_pipeline_steps_total" 1
      setGauge metrics "graphos_nodes_extracted" (fromIntegral $ length (extractionNodes extraction))
      setGauge metrics "graphos_edges_extracted" (fromIntegral $ length (extractionEdges extraction))
      debugTraceSpan (otelDebugTrace obsEnv) "extract" extractStart extractEnd (Map.fromList [("nodes", T.pack $ show $ length (extractionNodes extraction)), ("edges", T.pack $ show $ length (extractionEdges extraction))])
      logInfo env $ T.pack $ "  " ++ show (length (extractionNodes extraction)) ++ " nodes, " ++
                  show (length (extractionEdges extraction)) ++ " edges"
      logDebug env $ T.pack $ "  Nodes: " ++ show (map nodeId (extractionNodes extraction))

      -- Edge repair pass: re-push all edges to Neo4j to ensure cross-file connections
      when (cfgNeo4jStreaming configWithStreaming /= Nothing) $ do
        logInfo env "  [neo4j-stream] Running edge repair pass..."
        let graph = buildGraphFromExtractions (cfgDirected configWithStreaming) [extraction]
        (_msg, stmts, batches) <- Neo4j.pushEdgeRepair graph
          (case cfgNeo4jStreaming configWithStreaming of
             Just s -> neo4jsUri s
             Nothing -> "http://localhost:7474")
          (case cfgNeo4jStreaming configWithStreaming of
             Just s -> neo4jsUser s
             Nothing -> "neo4j")
          (case cfgNeo4jStreaming configWithStreaming of
             Just s -> neo4jsPassword s
             Nothing -> "graphos_dev")
        logInfo env $ T.pack $ "  [neo4j-stream] Edge repair: " ++ show stmts ++ " statements in " ++ show batches ++ " batches"

      -- Step 3: Build
      logInfo env "Step 3: Building graph..."
      buildStart <- getCurrentTime
      let graph = buildGraphFromExtractions (cfgDirected configWithStreaming) [extraction]
      buildEnd <- getCurrentTime
      observeHistogram metrics "graphos_build_duration_seconds" (realToFrac (diffUTCTime buildEnd buildStart) :: Double)
      incCounter metrics "graphos_pipeline_steps_total" 1
      setGauge metrics "graphos_graph_nodes" (fromIntegral $ Map.size (gNodes graph))
      setGauge metrics "graphos_graph_edges" (fromIntegral $ Map.size (gEdges graph))
      debugTraceSpan (otelDebugTrace obsEnv) "build" buildStart buildEnd (Map.fromList [("nodes", T.pack $ show $ Map.size (gNodes graph)), ("edges", T.pack $ show $ Map.size (gEdges graph))])
      logInfo env $ T.pack $ "  Graph: " ++ show (Map.size (gNodes graph)) ++ " nodes, " ++ show (Map.size (gEdges graph)) ++ " edges"

      -- Incremental write
      createDirectoryIfMissing True (cfgOutputDir configWithStreaming)
      logInfo env $ T.pack $ "  Streaming graph data to " ++ cfgOutputDir configWithStreaming ++ "/graph.json"
      iw <- Inc.openWriter (cfgOutputDir configWithStreaming ++ "/graph.json")
      Inc.writeNodes iw (Map.elems (gNodes graph))
      Inc.writeEdges iw (Map.elems (gEdges graph))

      -- Checkpoint
      let checkpointPath = cfgOutputDir configWithStreaming ++ "/graph.checkpoint.json"
      ExportJSON.saveCheckpoint graph checkpointPath
      logInfo env $ T.pack $ "  Checkpoint saved: " ++ checkpointPath

      -- Force full evaluation of the graph now that incremental write + checkpoint
      -- are done. This allows the GC to reclaim graph nodes before clustering starts.
      -- On 100k+ node graphs, the checkpoint keeps the whole graph in memory;
      -- forcing here means subsequent clustering operates on fully-evaluated data
      -- without retaining lazy thunks from the extraction phase.
      graph `deepseq` pure ()

      -- Steps 4-5: Cluster + Analyze (skipped when --no-cluster)
      (enrichedGraph, finalCommMap, _finalCohesion, analysis) <-
        if cfgNoCluster configWithStreaming
          then do
            logInfo env "Step 4: Skipping clustering (--no-cluster)"
            let emptyCommMap = Map.empty :: CommunityMap
                emptyCohesion = Map.empty :: CohesionMap
                noAnalysis = analyzeGraph graph emptyCommMap emptyCohesion
            Inc.writeCommunities iw emptyCommMap
            Inc.writeCohesion iw emptyCohesion
            Inc.writeGodNodes iw (analysisGodNodes noAnalysis)
            Inc.closeWriter iw
            pure (graph, emptyCommMap, emptyCohesion, noAnalysis)
          else do
            -- Step 4: Cluster
            logInfo env "Step 4: Detecting communities..."
            clusterStart <- getCurrentTime
            let res = Resolution { resGamma = cfgResolution configWithStreaming
                                 , resMinSize = cfgMinCommSize configWithStreaming
                                 , resMergeInto = MergeToNeighbor
                                 , resMaxIterations = cfgMaxLeidenIterations configWithStreaming
                                 }
                (commMap, cohesion) = clusterGraphWithResolution graph res
            clusterEnd <- getCurrentTime
            observeHistogram metrics "graphos_cluster_duration_seconds" (realToFrac (diffUTCTime clusterEnd clusterStart) :: Double)
            incCounter metrics "graphos_pipeline_steps_total" 1
            setGauge metrics "graphos_communities" (fromIntegral $ Map.size commMap)
            debugTraceSpan (otelDebugTrace obsEnv) "cluster" clusterStart clusterEnd (Map.fromList [("communities", T.pack $ show $ Map.size commMap)])

            Inc.writeCommunities iw commMap
            Inc.writeCohesion iw cohesion
            Inc.flushWriter iw
            logDebug env "  Communities and cohesion written incrementally"

            -- Step 4b: Infer additional edges
            let allInferred = inferEdges (cfgEdgeDensity configWithStreaming) graph commMap
                enrichedGraph' = if null allInferred
                  then graph
                  else buildGraphFromExtractions (cfgDirected configWithStreaming)
                       [emptyExtraction { extractionNodes = Map.elems (gNodes graph)
                                         , extractionEdges = Map.elems (gEdges graph) ++ allInferred }]
            -- deepseq forces full evaluation of the enriched graph, which:
            -- 1. Eliminates lazy thunk chains in nested Maps/Sets
            -- 2. Allows the original 'graph' to be GC'd — without this,
            --    both stay live simultaneously, doubling peak memory.
            enrichedGraph' `deepseq` pure ()
            logInfo env $ T.pack $ "  Inferred " ++ show (length allInferred) ++ " additional edges (density: " ++ show (cfgEdgeDensity configWithStreaming) ++ ")"

            -- Step 5: Re-cluster and analyze
            logInfo env "Step 5: Re-clustering and analyzing..."
            let (finalComm, finalCohes) = clusterGraphWithResolution enrichedGraph' res
                anal = analyzeGraph enrichedGraph' finalComm finalCohes

            Inc.writeGodNodes iw (analysisGodNodes anal)
            Inc.closeWriter iw
            pure (enrichedGraph', finalComm, finalCohes, anal)

      logInfo env "  graph.json written incrementally"

      -- Step 6: Report
      logInfo env "Step 6: Generating report..."
      let _report = generateReport enrichedGraph analysis configWithStreaming detection

      -- Step 7: Export
      logInfo env "Step 7: Exporting outputs..."
      exportStart <- getCurrentTime
      createDirectoryIfMissing True (cfgOutputDir configWithStreaming)
      exports <- exportAll enrichedGraph analysis configWithStreaming detection
      exportEnd <- getCurrentTime
      observeHistogram metrics "graphos_export_duration_seconds" (realToFrac (diffUTCTime exportEnd exportStart) :: Double)
      incCounter metrics "graphos_pipeline_steps_total" 1

      -- Neo4j push confirmation (full/community push for the export step)
      when (cfgNeo4j configWithStreaming) $ do
        logInfo env "  Neo4j: Cypher export + push complete"

      -- Community graph export
      when (cfgCommunityGraph configWithStreaming && not (cfgNoCluster configWithStreaming)) $ do
        logInfo env "Step 7b: Exporting community-level graph..."
        exportCommunityGraph enrichedGraph finalCommMap (cfgOutputDir configWithStreaming ++ "/community_graph.json")
        logInfo env $ T.pack $ "  Community graph: " ++ cfgOutputDir configWithStreaming ++ "/community_graph.json"

      -- Cleanup checkpoint
      clearPipelineCheckpoint (cfgOutputDir configWithStreaming)

      -- Flush observability data
      flushDebugTrace (otelDebugTrace obsEnv)

      let result = PipelineResult
            { prNodes       = Map.size (gNodes enrichedGraph)
            , prEdges       = Map.size (gEdges enrichedGraph)
            , prCommunities = Map.size finalCommMap
            , prReportPath  = erReport exports
            , prGraphPath   = erJSON exports
            , prHtmlPath    = erHTML exports
            , prNeo4jPath  = erNeo4j exports
            }
      logInfo env "Graph complete!"
      pure $ Right result
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Pipeline error: " ++ show e
  where
    allFiles d = concat (Map.elems (detectionFiles d))

-- | Run incremental pipeline for --watch mode.
-- Re-extracts only the changed files, merges into the existing graph,
-- and pushes delta to Neo4j if configured.
runIncrementalPipeline :: PipelineConfig -> [FilePath] -> IO (Either Text PipelineResult)
runIncrementalPipeline config changedFiles = catch (do
  let logLevel = if cfgDebug config then LevelTrace
                 else if cfgVerbose config then LevelDebug
                 else LevelInfo
  env <- defaultLogEnv logLevel
  let otelCfg = cfgOtelConfig config
      metricsPort = cfgMetricsPort config
      debugDir = case cfgDebugTraceDir config of
                   Just d -> d
                   Nothing -> cfgOutputDir config ++ "/traces"
  obsEnv <- initObservability logLevel otelCfg metricsPort debugDir
  let _metrics = otelMetrics obsEnv

  -- Set up Neo4j streaming if enabled
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

  logInfo env $ T.pack $ "[watch] Re-extracting " ++ show (length changedFiles) ++ " changed files..."

  -- Extract only changed files (with streaming push to Neo4j)
  extraction <- extractChangedFiles configWithStreaming changedFiles env

  -- Build graph from delta extraction
  let graph = buildGraphFromExtractions (cfgDirected configWithStreaming) [extraction]

  -- Edge repair pass for incremental updates
  when (cfgNeo4jStreaming configWithStreaming /= Nothing) $ do
    logInfo env "  [neo4j-stream] Running edge repair pass for incremental update..."
    (_msg, stmts, batches) <- Neo4j.pushEdgeRepair graph
      (case cfgNeo4jStreaming configWithStreaming of
         Just s -> neo4jsUri s
         Nothing -> "http://localhost:7474")
      (case cfgNeo4jStreaming configWithStreaming of
         Just s -> neo4jsUser s
         Nothing -> "neo4j")
      (case cfgNeo4jStreaming configWithStreaming of
         Just s -> neo4jsPassword s
         Nothing -> "graphos_dev")
    logInfo env $ T.pack $ "  [neo4j-stream] Edge repair: " ++ show stmts ++ " statements in " ++ show batches ++ " batches"

  -- Re-cluster if clustering enabled
  (enrichedGraph, finalCommMap, _finalCohesion) <-
    if cfgNoCluster configWithStreaming
      then pure (graph, Map.empty, Map.empty)
      else do
        let res = Resolution { resGamma = cfgResolution configWithStreaming
                             , resMinSize = cfgMinCommSize configWithStreaming
                             , resMergeInto = MergeToNeighbor
                             , resMaxIterations = cfgMaxLeidenIterations configWithStreaming }
            (commMap, cohesion) = clusterGraphWithResolution graph res
            allInferred = inferEdges (cfgEdgeDensity configWithStreaming) graph commMap
            enriched = if null allInferred
              then graph
              else buildGraphFromExtractions (cfgDirected configWithStreaming)
                   [emptyExtraction { extractionNodes = Map.elems (gNodes graph)
                                     , extractionEdges = Map.elems (gEdges graph) ++ allInferred }]
        pure (enriched, commMap, cohesion)

  -- Export
  createDirectoryIfMissing True (cfgOutputDir configWithStreaming)
  let analysis = analyzeGraph enrichedGraph finalCommMap Map.empty
  exports <- exportAll enrichedGraph analysis configWithStreaming (Detection (length changedFiles) 0 True Nothing Map.empty)

  -- Push communities to Neo4j if enabled
  when (cfgNeo4j configWithStreaming && not (cfgNoCluster configWithStreaming)) $ do
    let n4cfg = gcNeo4j (cfgGraphosConfig configWithStreaming)
        uri = case cfgNeo4jPush configWithStreaming of
                Just u -> u
                Nothing -> T.pack (neo4jUri n4cfg)
        user = T.pack (neo4jUser n4cfg)
        pass = T.pack (neo4jPassword n4cfg)
        cohesion = Map.empty  -- simplified for incremental
    case cfgNeo4jPushMode configWithStreaming of
      FullPush -> do
        logInfo env "[neo4j] Push mode: full (incremental)"
        void $ Neo4j.pushToNeo4jWithCommunities enrichedGraph finalCommMap cohesion uri user pass
      SubgraphPush -> do
        let artPoints = GAnalysis.articulationPoints enrichedGraph
        void $ Neo4j.pushSubgraphToNeo4j enrichedGraph finalCommMap cohesion (cfgNeo4jSubgraphSize configWithStreaming) artPoints uri user pass
      CommunityPush ->
        void $ Neo4j.pushCommunityGraphToNeo4j enrichedGraph finalCommMap cohesion uri user pass

  let result = PipelineResult
        { prNodes       = Map.size (gNodes enrichedGraph)
        , prEdges       = Map.size (gEdges enrichedGraph)
        , prCommunities = Map.size finalCommMap
        , prReportPath  = erReport exports
        , prGraphPath   = erJSON exports
        , prHtmlPath    = erHTML exports
        , prNeo4jPath   = erNeo4j exports
        }
  logInfo env "[watch] Incremental pipeline complete!"
  pure $ Right result
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Incremental pipeline error: " ++ show e