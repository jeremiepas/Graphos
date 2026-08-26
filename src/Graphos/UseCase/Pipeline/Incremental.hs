-- | Incremental pipeline — --watch mode and single-file ingestion.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Pipeline.Incremental
  ( runIncrementalPipeline
  , runSingleFilePipeline
  , SingleFileResult(..)
  ) where

import Control.Exception (catch, SomeException)
import Control.Monad (when, void)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)

import Graphos.Domain.Types hiding (PushMode(..))
import Graphos.Domain.Types.Pipeline (Neo4jStreamingConfig(..), Neo4jPushMode(..))
import Graphos.Domain.Config (SemanticEdgesConfig(..))
import Graphos.Domain.Graph (gNodes, gEdges)
import qualified Graphos.Domain.Graph.Analysis as GAnalysis
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.UseCase.Port.ObservabilityPort (ObservabilityPort(..))
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..))
import qualified Graphos.UseCase.Port.ExportPort as UEP
import Graphos.UseCase.Port.ExportPort (ExportPort(..))
import Graphos.UseCase.Extract (extractChangedFiles)
import Graphos.UseCase.Build (buildGraphFromExtractions)
import Graphos.UseCase.Cluster (clusterGraphWithResolution, clusterSingle)
import Graphos.Domain.Community (Resolution(..), MergeStrategy(..))
import Graphos.UseCase.Analyze (analyzeGraph)
import Graphos.UseCase.Infer (inferNonSemanticEdges, inferSemanticEdgesForMode, semanticMode)
import Graphos.UseCase.Ingest (ingestFile, FileIngestResult(..))
import Graphos.UseCase.Label (labelCommunities)
import Graphos.Domain.Labeling (LabelingResult(..))
import Graphos.UseCase.IngestIndex (loadIndex, saveIndex, mergeIndices)
import Graphos.UseCase.Pipeline.Core (PipelineResult(..), logSemanticInference)

-- | Run incremental pipeline for --watch mode.
runIncrementalPipeline :: AppEnv -> PipelineConfig -> [FilePath] -> IO (Either Text PipelineResult)
runIncrementalPipeline appEnv config changedFiles = catch (do
  let lp = loggingPort appEnv
      _op = observabilityPort appEnv
      _fsp = fileSystemPort appEnv
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

  lpLogInfo lp $ T.pack $ "[watch] Re-extracting " ++ show (length changedFiles) ++ " changed files..."

  extraction <- extractChangedFiles appEnv configWithStreaming changedFiles

  let graph = buildGraphFromExtractions (cfgDirected configWithStreaming) [extraction]

  when (cfgNeo4jStreaming configWithStreaming /= Nothing) $ do
    lpLogInfo lp "  [neo4j-stream] Running edge repair pass for incremental update..."
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

  (enrichedGraph, finalCommMap, _finalCohesion) <-
    if cfgNoCluster configWithStreaming
      then pure (graph, Map.empty, Map.empty)
      else do
        let res = Resolution { resGamma = cfgResolution configWithStreaming
                             , resMinSize = cfgMinCommSize configWithStreaming
                             , resMergeInto = MergeToNeighbor
                             , resMaxIterations = cfgMaxLeidenIterations configWithStreaming }
            (commMap, cohesion) = clusterGraphWithResolution graph res
            seCfg = (gcSemanticEdges (cfgGraphosConfig configWithStreaming)) { seEnabled = not (cfgNoSemanticEdges configWithStreaming) }
            force = cfgForceSemanticEdges configWithStreaming
            mode = semanticMode seCfg force graph
            semanticEdges = inferSemanticEdgesForMode mode seCfg graph
            allInferred = inferNonSemanticEdges (cfgEdgeDensity configWithStreaming) graph commMap ++ semanticEdges
            enriched = if null allInferred
              then graph
              else buildGraphFromExtractions (cfgDirected configWithStreaming)
                   [extractionFromLists (Map.elems (gNodes graph))
                                        (Map.elems (gEdges graph) ++ allInferred)]
        logSemanticInference lp seCfg mode semanticEdges
        pure (enriched, commMap, cohesion)

  createDirectoryIfMissing True (cfgOutputDir configWithStreaming)
  let analysis = analyzeGraph enrichedGraph finalCommMap Map.empty
  exports <- UEP.epExportAll ep enrichedGraph (cfgOutputDir configWithStreaming) analysis configWithStreaming (Detection (length changedFiles) 0 True Nothing Map.empty emptyExclusionCounts) Nothing []

  when (cfgNeo4j configWithStreaming && not (cfgNoCluster configWithStreaming)) $ do
    let n4cfg = gcNeo4j (cfgGraphosConfig configWithStreaming)
        uri = case cfgNeo4jPush configWithStreaming of
                Just u -> u
                Nothing -> T.pack (neo4jUri n4cfg)
        user = T.pack (neo4jUser n4cfg)
        pass = T.pack (neo4jPassword n4cfg)
        cohesion = Map.empty
    case cfgNeo4jPushMode configWithStreaming of
      FullPush -> do
        lpLogInfo lp "[neo4j] Push mode: full (incremental)"
        void $ epPushToNeo4jFull ep enrichedGraph finalCommMap cohesion uri user pass
      SubgraphPush -> do
        let artPoints = GAnalysis.articulationPoints enrichedGraph
        void $ epPushToNeo4jSubgraph ep enrichedGraph finalCommMap cohesion (cfgNeo4jSubgraphSize configWithStreaming) artPoints uri user pass
      CommunityPush ->
        void $ epPushToNeo4jCommunity ep enrichedGraph finalCommMap cohesion uri user pass

  let result = PipelineResult
        { prNodes       = Map.size (gNodes enrichedGraph)
        , prEdges       = Map.size (gEdges enrichedGraph)
        , prCommunities = Map.size finalCommMap
        , prReportPath  = UEP.erReport exports
        , prGraphPath   = UEP.erJSON exports
        , prHtmlPath    = UEP.erHTML exports
        , prNeo4jPath   = UEP.erNeo4j exports
        }
  lpLogInfo lp "[watch] Incremental pipeline complete!"
  pure $ Right result
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Incremental pipeline error: " ++ show e

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
runSingleFilePipeline :: AppEnv -> PipelineConfig -> FilePath -> IO (Either Text SingleFileResult)
runSingleFilePipeline appEnv config filePath = catch (do
  let lp = loggingPort appEnv
      op = observabilityPort appEnv
      fsp = fileSystemPort appEnv
      ep = exportPort appEnv

  lpLogInfo lp $ T.pack $ "[ingest] Starting single-file pipeline for: " ++ filePath

  ingestResult <- ingestFile appEnv config filePath
  case ingestResult of
    Left err -> pure $ Left err
    Right fir -> do
      let graph = buildGraphFromExtractions (cfgDirected config) [firExtraction fir]

      lpLogInfo lp $ T.pack $ "  Graph: " ++ show (Map.size (gNodes graph)) ++ " nodes, "
                                ++ show (Map.size (gEdges graph)) ++ " edges"

      (enrichedGraph, finalCommMap) <-
        if cfgNoCluster config
          then pure (graph, Map.empty)
          else do
            let nodesMap = extractionNodes (firExtraction fir)
            case Map.elems nodesMap of
              (seedNode: _) -> do
                let res = Resolution { resGamma = cfgResolution config
                                     , resMinSize = cfgMinCommSize config
                                     , resMergeInto = MergeToNeighbor
                                     , resMaxIterations = cfgMaxLeidenIterations config
                                     }
                    (commMap, _cohesion) = clusterSingle graph (nodeId seedNode) 3 res
                    seCfg = (gcSemanticEdges (cfgGraphosConfig config)) { seEnabled = not (cfgNoSemanticEdges config) }
                    force = cfgForceSemanticEdges config
                    mode = semanticMode seCfg force graph
                    semanticEdges = inferSemanticEdgesForMode mode seCfg graph
                    allInferred = inferNonSemanticEdges (cfgEdgeDensity config) graph commMap ++ semanticEdges
                    enriched = if null allInferred
                      then graph
                      else buildGraphFromExtractions (cfgDirected config)
                           [extractionFromLists (Map.elems (gNodes graph))
                                                (Map.elems (gEdges graph) ++ allInferred)]
                logSemanticInference lp seCfg mode semanticEdges
                lpLogInfo lp $ T.pack $ "  Clusters: " ++ show (Map.size commMap)
                pure (enriched, commMap)
              [] -> pure (graph, Map.empty)

      createDirectoryIfMissing True (cfgOutputDir config)
      let indexPath = cfgOutputDir config ++ "/index.json"
      existingIndex <- loadIndex indexPath
      let mergedIndex = mergeIndices existingIndex (firIndex fir)
      saveIndex indexPath mergedIndex
      lpLogInfo lp $ T.pack $ "  Index: " ++ show (Map.size (iiNodes mergedIndex)) ++ " entries → " ++ indexPath

      llmLabels <- if cfgLabel config
        then do
          let lblCfg = gcLabeling (cfgGraphosConfig config)
          result <- labelCommunities appEnv enrichedGraph finalCommMap Map.empty lblCfg
          pure $ if Map.null (lrLabels result) then Nothing else Just (lrLabels result)
        else pure Nothing

      let analysis = analyzeGraph enrichedGraph finalCommMap Map.empty
          detection = Detection
            { detectionTotalFiles = 1
            , detectionTotalWords = 0
            , detectionNeedsGraph = True
            , detectionWarning = Nothing
            , detectionFiles = Map.empty
            , detectionExclusions = emptyExclusionCounts
            }
      exports <- UEP.epExportAll ep enrichedGraph (cfgOutputDir config) analysis config detection llmLabels []

      fspClearCheckpoint fsp (cfgOutputDir config)
      opShutdownObservability op

      let embWithVectors = length $ filter (not . null . ieVector) (firEmbeddings fir)
      lpLogInfo lp "[ingest] Single-file pipeline complete!"

      pure $ Right SingleFileResult
        { sfrNodes       = Map.size (gNodes enrichedGraph)
        , sfrEdges       = Map.size (gEdges enrichedGraph)
        , sfrCommunities = Map.size finalCommMap
        , sfrGraphPath   = UEP.erJSON exports
        , sfrIndexPath   = indexPath
        , sfrEmbeddingCount = embWithVectors
        }
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Single-file pipeline error: " ++ show e