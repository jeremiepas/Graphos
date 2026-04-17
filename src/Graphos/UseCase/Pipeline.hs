-- | Main pipeline orchestration - detect → extract → build → cluster → analyze → report → export
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
import System.Directory (createDirectoryIfMissing)

import Graphos.Domain.Types
import Graphos.Domain.Graph (gNodes, gEdges, mergeExtractions, buildGraph)
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
import Graphos.Infrastructure.Export.CommunityGraph (exportCommunityGraph)

-- | Pipeline result
data PipelineResult = PipelineResult
  { prNodes        :: Int
  , prEdges        :: Int
  , prCommunities  :: Int
  , prReportPath   :: FilePath
  , prGraphPath    :: FilePath
  , prHtmlPath     :: Maybe FilePath
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

      -- Step 4: Cluster
      logInfo env "Step 4: Detecting communities..."
      let res = Resolution { resGamma = cfgResolution config
                           , resMinSize = cfgMinCommSize config
                           , resMergeInto = MergeToNeighbor }
          (commMap, _cohesion) = clusterGraphWithResolution graph res

      -- Step 4b: Infer additional edges based on density setting
      let allInferred = inferEdges (cfgEdgeDensity config) graph commMap
          enrichedGraph = if null allInferred
            then graph
            else buildGraphFromExtractions (cfgDirected config)
                 [emptyExtraction { extractionNodes = Map.elems (gNodes graph)
                                   , extractionEdges = Map.elems (gEdges graph) ++ allInferred }]
      logInfo env $ T.pack $ "  Inferred " ++ show (length allInferred) ++ " additional edges (density: " ++ show (cfgEdgeDensity config) ++ ")"

      -- Step 5: Re-cluster with enriched graph and analyze
      logInfo env "Step 5: Re-clustering and analyzing..."
      let (finalCommMap, finalCohesion) = clusterGraphWithResolution enrichedGraph res
      let analysis = analyzeGraph enrichedGraph finalCommMap finalCohesion

      -- Step 6: Report
      logInfo env "Step 6: Generating report..."
      let _report = generateReport enrichedGraph analysis config detection

      -- Step 7: Export
      logInfo env "Step 7: Exporting outputs..."
      createDirectoryIfMissing True (cfgOutputDir config)
      exports <- exportAll enrichedGraph analysis config detection

      -- Step 7b: Community graph export (if enabled)
      when (cfgCommunityGraph config) $ do
        logInfo env "Step 7b: Exporting community-level graph..."
        exportCommunityGraph enrichedGraph finalCommMap (cfgOutputDir config ++ "/community_graph.json")
        logInfo env $ T.pack $ "  Community graph: " ++ cfgOutputDir config ++ "/community_graph.json"

      let result = PipelineResult
            { prNodes       = Map.size (gNodes enrichedGraph)
            , prEdges       = Map.size (gEdges enrichedGraph)
            , prCommunities = Map.size finalCommMap
            , prReportPath  = erReport exports
            , prGraphPath   = erJSON exports
            , prHtmlPath    = erHTML exports
            }
      logInfo env "Graph complete!"
      pure $ Right result
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Pipeline error: " ++ show e
  where
    allFiles d = concat (Map.elems (detectionFiles d))