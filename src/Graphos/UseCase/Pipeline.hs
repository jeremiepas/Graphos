-- | Main pipeline orchestration.
--
-- Full pipeline: detect → extract → build → cluster → infer → analyze → report → export
-- With --no-cluster: detect → extract → build → report → export (skip clustering)
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Pipeline
  ( runPipeline
  , PipelineResult(..)
  ) where

import Control.Exception (catch, SomeException)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removeFile)

import Graphos.Domain.Types
import Graphos.Domain.Graph (gNodes, gEdges)
import Graphos.Infrastructure.Logging (LogLevel(..), defaultLogEnv, logInfo, logDebug, logTrace)
import Graphos.UseCase.Detect (detectFiles)
import Graphos.UseCase.Extract (extractAll)
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

  -- Step 1: Detect
  logInfo env "Step 1: Detecting files..."
  detection <- detectFiles (cfgInputPath config)
  if null (allFiles detection)
    then pure $ Left "No supported files found"
    else do
      logInfo env $ T.pack $ "  Found " ++ show (detectionTotalFiles detection) ++ " files"
      logDebug env $ T.pack $ "  File categories: " ++ show (Map.keys (detectionFiles detection))
      logTrace env $ T.pack $ "  Code files: " ++ show (Map.findWithDefault [] CodeFiles (detectionFiles detection))

      -- Step 2: Extract
      logInfo env "Step 2: Extracting entities and relationships..."
      extraction <- extractAll config detection env
      logInfo env $ T.pack $ "  " ++ show (length (extractionNodes extraction)) ++ " nodes, " ++
                  show (length (extractionEdges extraction)) ++ " edges"
      logDebug env $ T.pack $ "  Nodes: " ++ show (map nodeId (extractionNodes extraction))

      -- Step 3: Build
      logInfo env "Step 3: Building graph..."
      let graph = buildGraphFromExtractions (cfgDirected config) [extraction]
      logInfo env $ T.pack $ "  Graph: " ++ show (Map.size (gNodes graph)) ++ " nodes, " ++ show (Map.size (gEdges graph)) ++ " edges"

      -- Incremental write
      createDirectoryIfMissing True (cfgOutputDir config)
      logInfo env $ T.pack $ "  Streaming graph data to " ++ cfgOutputDir config ++ "/graph.json"
      iw <- Inc.openWriter (cfgOutputDir config ++ "/graph.json")
      Inc.writeNodes iw (Map.elems (gNodes graph))
      Inc.writeEdges iw (Map.elems (gEdges graph))

      -- Checkpoint
      let checkpointPath = cfgOutputDir config ++ "/graph.checkpoint.json"
      ExportJSON.saveCheckpoint graph checkpointPath
      logInfo env $ T.pack $ "  Checkpoint saved: " ++ checkpointPath

      -- Steps 4-5: Cluster + Analyze (skipped when --no-cluster)
      (enrichedGraph, finalCommMap, _finalCohesion, analysis) <-
        if cfgNoCluster config
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
            let res = Resolution { resGamma = cfgResolution config
                                 , resMinSize = cfgMinCommSize config
                                 , resMergeInto = MergeToNeighbor }
                (commMap, cohesion) = clusterGraphWithResolution graph res

            Inc.writeCommunities iw commMap
            Inc.writeCohesion iw cohesion
            Inc.flushWriter iw
            logDebug env "  Communities and cohesion written incrementally"

            -- Step 4b: Infer additional edges
            let allInferred = inferEdges (cfgEdgeDensity config) graph commMap
                enriched = if null allInferred
                  then graph
                  else buildGraphFromExtractions (cfgDirected config)
                       [emptyExtraction { extractionNodes = Map.elems (gNodes graph)
                                         , extractionEdges = Map.elems (gEdges graph) ++ allInferred }]
            logInfo env $ T.pack $ "  Inferred " ++ show (length allInferred) ++ " additional edges (density: " ++ show (cfgEdgeDensity config) ++ ")"

            -- Step 5: Re-cluster and analyze
            logInfo env "Step 5: Re-clustering and analyzing..."
            let (finalComm, finalCohes) = clusterGraphWithResolution enriched res
                anal = analyzeGraph enriched finalComm finalCohes

            Inc.writeGodNodes iw (analysisGodNodes anal)
            Inc.closeWriter iw
            pure (enriched, finalComm, finalCohes, anal)

      logInfo env "  graph.json written incrementally"

      -- Step 6: Report
      logInfo env "Step 6: Generating report..."
      let _report = generateReport enrichedGraph analysis config detection

      -- Step 7: Export
      logInfo env "Step 7: Exporting outputs..."
      createDirectoryIfMissing True (cfgOutputDir config)
      exports <- exportAll enrichedGraph analysis config detection

      -- Neo4j push confirmation
      when (cfgNeo4j config) $ do
        logInfo env "  Neo4j: Cypher export + push complete"

      -- Community graph export
      when (cfgCommunityGraph config && not (cfgNoCluster config)) $ do
        logInfo env "Step 7b: Exporting community-level graph..."
        exportCommunityGraph enrichedGraph finalCommMap (cfgOutputDir config ++ "/community_graph.json")
        logInfo env $ T.pack $ "  Community graph: " ++ cfgOutputDir config ++ "/community_graph.json"

      -- Cleanup checkpoint
      removeFile checkpointPath `catch` \(_ :: SomeException) -> pure ()

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