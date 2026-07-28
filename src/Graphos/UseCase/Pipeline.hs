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
  , runSingleFilePipeline
  , PipelineResult(..)
  , SingleFileResult(..)
  ) where

import Control.DeepSeq (deepseq)
import Control.Exception (catch, SomeException, evaluate)
import Control.Monad (when, void)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (createDirectoryIfMissing)
import System.Mem (performGC)

import Graphos.Domain.Types hiding (PushMode(..))
import Graphos.Domain.Types.Pipeline (Neo4jStreamingConfig(..), PipelineStep(..), PipelineCheckpoint(..), Neo4jPushMode(..))
import Graphos.Domain.Config (FileExtensionConfig(..))
import Graphos.Domain.Graph (gNodes, gEdges)
import qualified Graphos.Domain.Graph.Analysis as GAnalysis
import Graphos.Infrastructure.Logging (LogLevel(..), logInfo, logDebug, logTrace)
import Graphos.Infrastructure.Observability.SDK
  ( ObservabilityEnv(..)
  , incCounter, setGauge, observeHistogram
  , debugTraceSpan
  , initObservability
  , shutdownObservability
  )
import Graphos.UseCase.Detect (detectFilesWithExtensionsAndIgnore)
import Graphos.Infrastructure.FileSystem.Ignore (loadIgnorePatterns)
import Graphos.UseCase.Extract (extractAll, extractChangedFiles)
import Graphos.UseCase.Build (buildGraphFromExtractions)
import Graphos.UseCase.Cluster (clusterGraphWithResolution, clusterSingle)
import Graphos.Domain.Community (Resolution(..), MergeStrategy(..))
import Graphos.UseCase.Analyze (analyzeGraph)
import Graphos.UseCase.Infer (inferEdges)
import Graphos.UseCase.Report (generateReport)
import Graphos.UseCase.Export (exportAll, ExportResult(..))
import Graphos.UseCase.Ingest (ingestFile, FileIngestResult(..))
import Graphos.UseCase.IngestIndex (loadIndex, saveIndex, mergeIndices)
import Graphos.UseCase.Label (labelCommunities)
import Graphos.Domain.Labeling (LabelingResult(..))
import qualified Graphos.Infrastructure.Export.JSON as ExportJSON
import Graphos.Infrastructure.Export.CommunityGraph (exportCommunityGraph)
import qualified Graphos.Infrastructure.Export.IncrementalJSON as Inc
import Graphos.Infrastructure.FileSystem.Cache (loadPipelineCheckpoint, savePipelineCheckpoint, clearPipelineCheckpoint)
import Graphos.Infrastructure.Wiring (productionAppEnv)
import qualified Graphos.Infrastructure.Export.Neo4j as Neo4j

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
runPipeline :: PipelineConfig -> IO (Either Text PipelineResult)
runPipeline config = catch (do
  let logLevel = if cfgDebug config then LevelTrace
                 else if cfgVerbose config then LevelDebug
                 else LevelInfo
  -- Initialize observability (tracing + metrics + log shipping)
  let otelCfg = cfgOtelConfig config
      metricsPort = cfgMetricsPort config
      debugDir = case cfgDebugTraceDir config of
                   Just d -> d
                   Nothing -> cfgOutputDir config ++ "/traces"
  obsEnv <- initObservability logLevel otelCfg metricsPort debugDir
  let _tracer = otelTracer obsEnv
      metrics = otelMetrics obsEnv
      env = otelLogEnv obsEnv  -- Use the LogEnv from ObservabilityEnv (has OTLP shipping enabled)
      appEnv = productionAppEnv env obsEnv

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

  -- Step 1: Detect (using config-driven extensions and ignore patterns)
  logInfo env "Step 1: Detecting files..."
  detectStart <- getCurrentTime
  ignorePatterns <- loadIgnorePatterns (cfgInputPath configWithStreaming)
  let fec = gcFileExtensions (cfgGraphosConfig configWithStreaming)
      extMap = Map.fromList
        [ (CodeFiles, fecCode fec)
        , (DocFiles, fecDoc fec)
        , (PaperFiles, fecPaper fec)
        , (ImageFiles, fecImage fec)
        , (VideoFiles, fecVideo fec)
        , (OfficeFiles, fecOffice fec)
        ]
  detection <- detectFilesWithExtensionsAndIgnore (cfgInputPath configWithStreaming) extMap ignorePatterns
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
      extraction <- extractAll appEnv configWithStreaming detection
      extractEnd <- getCurrentTime
      observeHistogram metrics "graphos_extract_duration_seconds" (realToFrac (diffUTCTime extractEnd extractStart) :: Double)
      incCounter metrics "graphos_pipeline_steps_total" 1
      setGauge metrics "graphos_nodes_extracted" (fromIntegral $ Map.size (extractionNodes extraction))
      setGauge metrics "graphos_edges_extracted" (fromIntegral $ Map.size (extractionEdges extraction))
      debugTraceSpan (otelDebugTrace obsEnv) "extract" extractStart extractEnd (Map.fromList [("nodes", T.pack $ show $ Map.size (extractionNodes extraction)), ("edges", T.pack $ show $ Map.size (extractionEdges extraction))])
      logInfo env $ T.pack $ "  " ++ show (Map.size (extractionNodes extraction)) ++ " nodes, " ++
                  show (Map.size (extractionEdges extraction)) ++ " edges"
      logDebug env $ T.pack $ "  Nodes: " ++ show (Map.elems (extractionNodes extraction))

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
      -- Force evaluation inside the timed window so the recorded span duration
      -- reflects the real build work, not thunk creation.
      _ <- evaluate (Map.size (gNodes graph) + Map.size (gEdges graph))
      graph `deepseq` pure ()
      buildEnd <- getCurrentTime
      observeHistogram metrics "graphos_build_duration_seconds" (realToFrac (diffUTCTime buildEnd buildStart) :: Double)
      incCounter metrics "graphos_pipeline_steps_total" 1
      setGauge metrics "graphos_graph_nodes" (fromIntegral $ Map.size (gNodes graph))
      setGauge metrics "graphos_graph_edges" (fromIntegral $ Map.size (gEdges graph))
      debugTraceSpan (otelDebugTrace obsEnv) "build" buildStart buildEnd (Map.fromList [("nodes", T.pack $ show $ Map.size (gNodes graph)), ("edges", T.pack $ show $ Map.size (gEdges graph))])
      logInfo env $ T.pack $ "  Graph: " ++ show (Map.size (gNodes graph)) ++ " nodes, " ++ show (Map.size (gEdges graph)) ++ " edges"

      -- Sanity check: warn if a code-dominant input produced an implausibly
      -- sparse graph (e.g. edge extraction collapse).
      let codeFiles = length $ Map.findWithDefault [] CodeFiles (detectionFiles detection)
          nonCodeFiles = detectionTotalFiles detection - codeFiles
          nodeCount = fromIntegral (Map.size (gNodes graph)) :: Double
          edgeCount = fromIntegral (Map.size (gEdges graph)) :: Double
          ratio = if nodeCount == 0 then 0 else edgeCount / nodeCount
      when (codeFiles > nonCodeFiles && nodeCount > 0 && ratio < edgeCollapseThreshold) $
        logInfo env $ T.pack $ "  WARNING: edge/node ratio (" ++ show ratio ++ ") is below threshold " ++ show edgeCollapseThreshold ++ "; edge extraction may have collapsed"

      -- Incremental write: only open the writer here; node/edge/community sections
      -- are written from the enriched graph after clustering/inference so that
      -- graph.json and GRAPH_REPORT.md share the same source of truth.
      createDirectoryIfMissing True (cfgOutputDir configWithStreaming)
      logInfo env $ T.pack $ "  Streaming graph data to " ++ cfgOutputDir configWithStreaming ++ "/graph.json"
      iw <- Inc.openWriter (cfgOutputDir configWithStreaming ++ "/graph.json")

      -- Checkpoint (pre-inference state; retained for resume support)
      let checkpointPath = cfgOutputDir configWithStreaming ++ "/graph.checkpoint.json"
      ExportJSON.saveCheckpoint graph checkpointPath
      logInfo env $ T.pack $ "  Checkpoint saved: " ++ checkpointPath

      -- GC boundary to reclaim extraction Maps before clustering starts.
      -- On 100k+ node graphs, the extraction Maps can be 30-40% of peak memory.
      performGC

      -- Steps 4-5: Cluster + Analyze (skipped when --no-cluster)
      (enrichedGraph, finalCommMap, _finalCohesion, analysis, llmLabelsResult) <-
        if cfgNoCluster configWithStreaming
          then do
            logInfo env "Step 4: Skipping clustering (--no-cluster)"
            let emptyCommMap = Map.empty :: CommunityMap
                emptyCohesion = Map.empty :: CohesionMap
                noAnalysis = analyzeGraph graph emptyCommMap emptyCohesion
            Inc.writeCommunities iw emptyCommMap
            Inc.writeCohesion iw emptyCohesion
            Inc.writeGodNodes iw (analysisGodNodes noAnalysis)
            Inc.writeAnalysisTail iw Nothing
            Inc.closeWriter iw
            pure (graph, emptyCommMap, emptyCohesion, noAnalysis, Nothing :: Maybe (Map.Map CommunityId Text))
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
            -- Force the cluster result before recording the span end so the
            -- duration reflects real work, not thunk creation.
            _ <- evaluate (Map.size commMap + sum (map length (Map.elems commMap)))
            (commMap, cohesion) `deepseq` pure ()
            clusterEnd <- getCurrentTime
            observeHistogram metrics "graphos_cluster_duration_seconds" (realToFrac (diffUTCTime clusterEnd clusterStart) :: Double)
            incCounter metrics "graphos_pipeline_steps_total" 1
            setGauge metrics "graphos_communities" (fromIntegral $ Map.size commMap)
            debugTraceSpan (otelDebugTrace obsEnv) "cluster" clusterStart clusterEnd (Map.fromList [("communities", T.pack $ show $ Map.size commMap)])

            -- Step 4b: Infer additional edges
            let allInferred = inferEdges (cfgEdgeDensity configWithStreaming) graph commMap
                enrichedGraph' = if null allInferred
                  then graph
                  else buildGraphFromExtractions (cfgDirected configWithStreaming)
                       [extractionFromLists (Map.elems (gNodes graph))
                                            (Map.elems (gEdges graph) ++ allInferred)]
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

            -- Write the final graph state to graph.json. All sections are derived
            -- from the same (enrichedGraph', finalComm, anal) triple that feeds
            -- GRAPH_REPORT.md, ensuring report/export parity.
            Inc.writeNodes iw (Map.elems (gNodes enrichedGraph'))
            Inc.writeEdges iw (Map.elems (gEdges enrichedGraph'))
            Inc.writeCommunities iw finalComm
            Inc.writeCohesion iw finalCohes
            Inc.writeGodNodes iw (analysisGodNodes anal)

            -- Step 5b: LLM community labeling (optional, --label)
            llmLabels <- if cfgLabel configWithStreaming
              then do
                logInfo env "Step 5b: Labeling communities via LLM..."
                let lblCfg = gcLabeling (cfgGraphosConfig configWithStreaming)
                logInfo env $ T.pack $ "  Labeling config: provider=" ++ labelingProvider lblCfg
                                       ++ " model=" ++ labelingModel lblCfg
                                       ++ " baseUrl=" ++ labelingBaseUrl lblCfg
                                       ++ " batchSize=" ++ show (labelingBatchSize lblCfg)
                labelingStart <- getCurrentTime
                result <- labelCommunities enrichedGraph' finalComm finalCohes lblCfg
                labelingEnd <- getCurrentTime
                logInfo env $ T.pack $ "  Labeled " ++ show (Map.size (lrLabels result)) ++ " communities in "
                                       ++ show (diffUTCTime labelingEnd labelingStart) ++ "s"
                pure (Just (lrLabels result))
              else pure Nothing

            Inc.writeAnalysisTail iw llmLabels
            Inc.flushWriter iw
            Inc.closeWriter iw
            logDebug env "  Final graph, communities, and cohesion written incrementally"
            pure (enrichedGraph', finalComm, finalCohes, anal, llmLabels)

      logInfo env "  graph.json written incrementally"

      -- Release intermediate data structures before export.
      -- Clustering (LeidenState) and analysis (CachedFGL) are done;
      -- only the graph, community map, and analysis results are needed for export.
      -- performGC lets the runtime reclaim LeidenState vectors and FGL Patricia trees.
      performGC

      -- Step 6: Report
      logInfo env "Step 6: Generating report..."
      let _report = generateReport enrichedGraph analysis configWithStreaming detection llmLabelsResult

      -- Step 7: Export
      logInfo env "Step 7: Exporting outputs..."
      exportStart <- getCurrentTime
      createDirectoryIfMissing True (cfgOutputDir configWithStreaming)
      exports <- exportAll enrichedGraph analysis configWithStreaming detection llmLabelsResult
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

      -- Flush observability data (traces to OTLP, logs to Loki, debug traces)
      shutdownObservability obsEnv

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
  let otelCfg = cfgOtelConfig config
      metricsPort = cfgMetricsPort config
      debugDir = case cfgDebugTraceDir config of
                    Just d -> d
                    Nothing -> cfgOutputDir config ++ "/traces"
  obsEnv <- initObservability logLevel otelCfg metricsPort debugDir
  let _metrics = otelMetrics obsEnv
      env = otelLogEnv obsEnv  -- Use the LogEnv from ObservabilityEnv (has OTLP shipping enabled)
      appEnv = productionAppEnv env obsEnv

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
  extraction <- extractChangedFiles appEnv configWithStreaming changedFiles

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
                   [extractionFromLists (Map.elems (gNodes graph))
                                        (Map.elems (gEdges graph) ++ allInferred)]
        pure (enriched, commMap, cohesion)

  -- Export
  createDirectoryIfMissing True (cfgOutputDir configWithStreaming)
  let analysis = analyzeGraph enrichedGraph finalCommMap Map.empty
  exports <- exportAll enrichedGraph analysis configWithStreaming (Detection (length changedFiles) 0 True Nothing Map.empty) Nothing

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

-- ───────────────────────────────────────────────
-- Single-File Ingestion Pipeline
-- ───────────────────────────────────────────────

-- | Result of single-file ingestion pipeline
data SingleFileResult = SingleFileResult
  { sfrNodes       :: Int
  , sfrEdges       :: Int
  , sfrCommunities :: Int
  , sfrGraphPath   :: FilePath
  , sfrIndexPath   :: FilePath
  , sfrEmbeddingCount :: Int
  } deriving (Eq, Show)

-- | Run the single-file ingestion pipeline.
--
-- Flow: ingestFile → build subgraph → clusterSingle → update index → incremental export
--
-- This is the fast path for 'graphos ingest <file> [--embed]'.
-- It processes exactly one file without a directory scan, clusters only the
-- subgraph around the extracted nodes, and optionally generates embeddings.
--
-- The index is loaded from/merged with the existing index.json if present,
-- giving cumulative coverage across multiple ingest calls.
runSingleFilePipeline :: PipelineConfig -> FilePath -> IO (Either Text SingleFileResult)
runSingleFilePipeline config filePath = catch (do
  let logLevel = if cfgDebug config then LevelTrace
                 else if cfgVerbose config then LevelDebug
                 else LevelInfo
  let otelCfg = cfgOtelConfig config
      metricsPort = cfgMetricsPort config
      debugDir = case cfgDebugTraceDir config of
                   Just d -> d
                   Nothing -> cfgOutputDir config ++ "/traces"
  obsEnv <- initObservability logLevel otelCfg metricsPort debugDir
  let _metrics = otelMetrics obsEnv
      env = otelLogEnv obsEnv
      appEnv = productionAppEnv env obsEnv

  logInfo env $ T.pack $ "[ingest] Starting single-file pipeline for: " ++ filePath

  -- Step 1: Ingest file (detect + extract + optional embeddings)
  ingestResult <- ingestFile appEnv config filePath env
  case ingestResult of
    Left err -> pure $ Left err
    Right fir -> do
      -- Step 2: Build graph from the extraction
      let graph = buildGraphFromExtractions (cfgDirected config) [firExtraction fir]

      logInfo env $ T.pack $ "  Graph: " ++ show (Map.size (gNodes graph)) ++ " nodes, "
                                ++ show (Map.size (gEdges graph)) ++ " edges"

      -- Step 3: Cluster (fast subgraph clustering if not --no-cluster)
      (enrichedGraph, finalCommMap) <-
        if cfgNoCluster config
          then pure (graph, Map.empty)
          else do
            -- Use clusterSingle on the first extracted node as seed
            -- If no nodes, skip clustering
            let nodesMap = extractionNodes (firExtraction fir)
            case Map.elems nodesMap of
              (seedNode: _) -> do
                let res = Resolution { resGamma = cfgResolution config
                                     , resMinSize = cfgMinCommSize config
                                     , resMergeInto = MergeToNeighbor
                                     , resMaxIterations = cfgMaxLeidenIterations config
                                     }
                    (commMap, _cohesion) = clusterSingle graph (nodeId seedNode) 3 res
                    allInferred = inferEdges (cfgEdgeDensity config) graph commMap
                    enriched = if null allInferred
                      then graph
                      else buildGraphFromExtractions (cfgDirected config)
                           [extractionFromLists (Map.elems (gNodes graph))
                                                (Map.elems (gEdges graph) ++ allInferred)]
                logInfo env $ T.pack $ "  Clusters: " ++ show (Map.size commMap)
                pure (enriched, commMap)
              [] -> pure (graph, Map.empty)

      -- Step 4: Update index (merge with existing)
      createDirectoryIfMissing True (cfgOutputDir config)
      let indexPath = cfgOutputDir config ++ "/index.json"
      existingIndex <- loadIndex indexPath
      let mergedIndex = mergeIndices existingIndex (firIndex fir)
      saveIndex indexPath mergedIndex
      logInfo env $ T.pack $ "  Index: " ++ show (Map.size (iiNodes mergedIndex)) ++ " entries → " ++ indexPath

      -- Step 5: Export
      let analysis = analyzeGraph enrichedGraph finalCommMap Map.empty
          detection = Detection
            { detectionTotalFiles = 1
            , detectionTotalWords = 0
            , detectionNeedsGraph = True
            , detectionWarning = Nothing
            , detectionFiles = Map.empty
            }
      exports <- exportAll enrichedGraph analysis config detection Nothing

      -- Clean up
      clearPipelineCheckpoint (cfgOutputDir config)
      shutdownObservability obsEnv

      let embWithVectors = length $ filter (not . null . ieVector) (firEmbeddings fir)
      logInfo env "[ingest] Single-file pipeline complete!"

      pure $ Right SingleFileResult
        { sfrNodes       = Map.size (gNodes enrichedGraph)
        , sfrEdges       = Map.size (gEdges enrichedGraph)
        , sfrCommunities = Map.size finalCommMap
        , sfrGraphPath   = erJSON exports
        , sfrIndexPath   = indexPath
        , sfrEmbeddingCount = embWithVectors
        }
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Single-file pipeline error: " ++ show e