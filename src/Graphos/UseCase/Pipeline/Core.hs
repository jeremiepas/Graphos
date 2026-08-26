-- | Core pipeline orchestration — the full detect→extract→build→cluster→export flow.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Pipeline.Core
  ( runPipeline
  , PipelineResult(..)
  , edgeCollapseThreshold
  , generateGraphEmbeddings
  , writeEmbeddingsSidecar
  , logSemanticInference
  ) where

import Control.DeepSeq (deepseq)
import Control.Exception (catch, SomeException, evaluate)
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (toJSON, encode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (createDirectoryIfMissing)
import System.Mem (performGC)

import Graphos.Domain.Types hiding (PushMode(..))
import Graphos.Domain.Types.Pipeline (Neo4jStreamingConfig(..), PipelineStep(..), PipelineCheckpoint(..))
import Graphos.Domain.Config (FileExtensionConfig(..), SemanticEdgesConfig(..))
import Graphos.Domain.Graph (Graph, gNodes, gEdges, gCompositions, gEmbeddings, gEmbeddingsPath)
import Graphos.Domain.Community (computeCompositions, Resolution(..), MergeStrategy(..))
import qualified Graphos.Domain.Graph.Analysis as GAnalysis
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.LLMPort (LLMPort(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.UseCase.Port.ObservabilityPort (ObservabilityPort(..), StartTime(..), EndTime(..))
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..))
import qualified Graphos.UseCase.Port.ExportPort as UEP
import Graphos.UseCase.Port.ExportPort (ExportPort(..))
import Graphos.UseCase.Detect (detectFilesWithExtensionsAndIgnore')
import Graphos.UseCase.Extract (extractAll)
import Graphos.UseCase.Build (buildGraphFromExtractions)
import Graphos.UseCase.Cluster (clusterGraphWithResolution, joinCommunitiesToNodes, computeCommunityAggregates)
import Graphos.UseCase.Analyze (analyzeGraph)
import Graphos.UseCase.Infer (inferNonSemanticEdges, inferSemanticEdgesForMode, semanticMode, semanticModeName, SemanticMode(..))
import Graphos.UseCase.Report (generateReport)
import Graphos.UseCase.Label (labelCommunities)
import Graphos.Domain.Labeling (LabelingResult(..))

-- | Minimum ratio of edges to nodes for a code-dominant graph. Values below
-- this threshold after the build step indicate a likely edge-extraction
-- collapse and are logged as a prominent warning.
edgeCollapseThreshold :: Double
edgeCollapseThreshold = 0.05

-- | Generate embeddings for all nodes in a graph.
-- Nodes whose embedding call fails are omitted from the result.
generateGraphEmbeddings :: LLMPort -> EmbeddingConfig -> Graph -> IO (Map NodeId [Double])
generateGraphEmbeddings llm cfg graph = do
  let nodes = Map.elems (gNodes graph)
  results <- mapM genNodeEmbedding nodes
  let embs = Map.fromList [ (nodeId n, v) | (n, r) <- zip nodes results, Right v <- [r] ]
  pure embs
  where
    genNodeEmbedding n = do
      let inputText = nodeLabel n <> " " <> nodeSourceFile n
      lpGenerateEmbedding llm cfg inputText

-- | Write the embeddings map to a JSON sidecar file (object: node id -> vector).
writeEmbeddingsSidecar :: FilePath -> Map NodeId [Double] -> IO ()
writeEmbeddingsSidecar path embs = BSL.writeFile path (encode embs)

-- | Log the semantic edge inference decision (mode + inferred count) for the current run.
logSemanticInference :: LoggingPort -> SemanticEdgesConfig -> SemanticMode -> [Edge] -> IO ()
logSemanticInference lp se mode semanticEdges = do
  when (mode == SemanticFallback) (lpLogInfo lp "semantic inference capped at 10K code nodes, falling back to literal-name inference")
  when (mode == SemanticAutoSkip) (lpLogInfo lp "single-corpus graph detected, skipping semantic edge inference")
  lpLogInfo lp $ T.concat
    [ "semantic edges: inferred ", T.pack (show (length semanticEdges))
    , " (cap=", T.pack (show (seMaxFanOut se))
    , ", threshold=", T.pack (show (seThreshold se))
    , ", mode=", semanticModeName mode, ")"
    ]

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
      let excs = detectionExclusions detection
          totalExcluded = excRootAnchored excs + excDepthIndependent excs + excGitignore excs + excGraphosignore excs + excUnexplained excs
      when (totalExcluded > 0) $ do
        lpLogInfo lp $ T.pack $ "  Excluded " ++ show totalExcluded ++ " directories:"
        when (excRootAnchored excs > 0) $
          lpLogInfo lp $ T.pack $ "    root-anchored build output: " ++ show (excRootAnchored excs)
        when (excDepthIndependent excs > 0) $
          lpLogInfo lp $ T.pack $ "    depth-independent tooling: " ++ show (excDepthIndependent excs)
        when (excGitignore excs > 0) $
          lpLogInfo lp $ T.pack $ "    .gitignore: " ++ show (excGitignore excs)
        when (excGraphosignore excs > 0) $
          lpLogInfo lp $ T.pack $ "    .graphosignore: " ++ show (excGraphosignore excs)
        when (excUnexplained excs > 0) $
          lpLogInfo lp $ T.pack $ "    unexplained: " ++ show (excUnexplained excs)

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
      let builtGraph = buildGraphFromExtractions (cfgDirected configWithStreaming) [extraction]
      _ <- evaluate (Map.size (gNodes builtGraph) + Map.size (gEdges builtGraph))
      builtGraph `deepseq` pure ()
      buildEnd <- getCurrentTime
      opRecordHistogram op "graphos_build_duration_seconds" (realToFrac (diffUTCTime buildEnd buildStart) :: Double)
      opIncCounter op "graphos_pipeline_steps_total" 1
      opSetGauge op "graphos_graph_nodes" (fromIntegral $ Map.size (gNodes builtGraph))
      opSetGauge op "graphos_graph_edges" (fromIntegral $ Map.size (gEdges builtGraph))
      opDebugTraceSpan op "build" (StartTime buildStart) (EndTime buildEnd) (Map.fromList [("nodes", T.pack $ show $ Map.size (gNodes builtGraph)), ("edges", T.pack $ show $ Map.size (gEdges builtGraph))])
      lpLogInfo lp $ T.pack $ "  Graph: " ++ show (Map.size (gNodes builtGraph)) ++ " nodes, " ++ show (Map.size (gEdges builtGraph)) ++ " edges"

      let codeFiles = length $ Map.findWithDefault [] CodeFiles (detectionFiles detection)
          nonCodeFiles = detectionTotalFiles detection - codeFiles
          nodeCount = fromIntegral (Map.size (gNodes builtGraph)) :: Double
          edgeCount = fromIntegral (Map.size (gEdges builtGraph)) :: Double
          ratio = if nodeCount == 0 then 0 else edgeCount / nodeCount
      when (codeFiles > nonCodeFiles && nodeCount > 0 && ratio < edgeCollapseThreshold) $
        lpLogInfo lp $ T.pack $ "  WARNING: edge/node ratio (" ++ show ratio ++ ") is below threshold " ++ show edgeCollapseThreshold ++ "; edge extraction may have collapsed"

      createDirectoryIfMissing True (cfgOutputDir configWithStreaming)

      graph <- if cfgEmbed configWithStreaming
        then do
          let embCfg = gcEmbedding (cfgGraphosConfig configWithStreaming)
          lpLogInfo lp "  Generating node embeddings..."
          embs <- generateGraphEmbeddings (llmPort appEnv) embCfg builtGraph
          let sidecar = cfgOutputDir configWithStreaming ++ "/embeddings.json"
          writeEmbeddingsSidecar sidecar embs
          lpLogInfo lp $ T.pack $ "  Wrote " ++ show (Map.size embs) ++ " node embeddings to embeddings.json"
          pure (builtGraph { gEmbeddings = Just embs, gEmbeddingsPath = Just "embeddings.json" })
        else pure builtGraph

      lpLogInfo lp $ T.pack $ "  Streaming graph data to " ++ cfgOutputDir configWithStreaming ++ "/graph.json"
      iw <- epOpenIncrementalWriter ep (cfgOutputDir configWithStreaming ++ "/graph.json")

      let checkpointPath = cfgOutputDir configWithStreaming ++ "/graph.checkpoint.json"
      epSaveCheckpoint ep graph checkpointPath
      lpLogInfo lp $ T.pack $ "  Checkpoint saved: " ++ checkpointPath

      performGC

      (enrichedGraph, finalCommMap, _finalCohesion, analysis, llmLabelsResult, aggregatesResult) <-
        if cfgNoCluster configWithStreaming
          then do
            lpLogInfo lp "Step 4: Skipping clustering (--no-cluster)"
            let emptyCommMap = Map.empty :: CommunityMap
                emptyCohesion = Map.empty :: CohesionMap
                noAnalysis = analyzeGraph graph emptyCommMap emptyCohesion
            epWriteNodes ep iw (Map.elems (gNodes graph))
            epWriteEdges ep iw (Map.elems (gEdges graph))
            epWriteCommunities ep iw emptyCommMap
            epWriteCohesion ep iw emptyCohesion
            epWriteGodNodes ep iw (analysisGodNodes noAnalysis)
            epWriteCommunityAggregates ep iw []
            epWriteCompositions ep iw (gCompositions graph)
            epWriteEmbeddingsPath ep iw (fmap T.pack (gEmbeddingsPath graph))
            epWriteAnalysisTail ep iw Nothing
            epCloseWriter ep iw
            pure (graph, emptyCommMap, emptyCohesion, noAnalysis, Nothing :: Maybe (Map.Map CommunityId Text), [])
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

            let seCfg = (gcSemanticEdges (cfgGraphosConfig configWithStreaming)) { seEnabled = not (cfgNoSemanticEdges configWithStreaming) }
                force = cfgForceSemanticEdges configWithStreaming
                mode = semanticMode seCfg force graph
                semanticEdges = inferSemanticEdgesForMode mode seCfg graph
                allInferred = inferNonSemanticEdges (cfgEdgeDensity configWithStreaming) graph commMap ++ semanticEdges
                enrichedGraph' = (if null allInferred
                  then graph
                  else buildGraphFromExtractions (cfgDirected configWithStreaming)
                        [extractionFromLists (Map.elems (gNodes graph))
                                             (Map.elems (gEdges graph) ++ allInferred)])
                  { gEmbeddings = gEmbeddings graph
                  , gEmbeddingsPath = gEmbeddingsPath graph }
            enrichedGraph' `deepseq` pure ()
            logSemanticInference lp seCfg mode semanticEdges
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

            let compMap = computeCompositions enrichedGraph' finalComm
                graphWithComps = enrichedGraph' { gCompositions = Just (toJSON compMap) }
                joinedGraph = joinCommunitiesToNodes enrichedGraph' finalComm

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

            epWriteCompositions ep iw (gCompositions graphWithComps)
            epWriteEmbeddingsPath ep iw (fmap T.pack (gEmbeddingsPath graphWithComps))
            epWriteAnalysisTail ep iw llmLabels
            epFlushWriter ep iw
            epCloseWriter ep iw
            lpLogDebug lp "  Final graph, communities, and cohesion written incrementally"
            pure (graphWithComps, finalComm, finalCohes, anal, llmLabels, aggregates)

      lpLogInfo lp "  graph.json written incrementally"

      performGC

      lpLogInfo lp "Step 6: Generating report..."
      let _report = generateReport enrichedGraph analysis configWithStreaming detection llmLabelsResult

      lpLogInfo lp "Step 7: Exporting outputs..."
      exportStart <- getCurrentTime
      createDirectoryIfMissing True (cfgOutputDir configWithStreaming)
      exports <- UEP.epExportAll ep enrichedGraph (cfgOutputDir configWithStreaming) analysis configWithStreaming detection llmLabelsResult aggregatesResult
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