-- | Export orchestration - all output formats
{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.Export
  ( ExportResult(..)
  , exportAll
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map

import qualified Graphos.Infrastructure.Export.HTML as ExportHTML
import qualified Graphos.Infrastructure.Export.Obsidian as ExportObsidian
import qualified Graphos.Infrastructure.Export.Report as ExportReport
import qualified Graphos.Infrastructure.Export.Neo4j as ExportNeo4j
import qualified Graphos.Infrastructure.Export.Memgraph as ExportMemgraph
import qualified Graphos.UseCase.Report as Report
import Graphos.Domain.Types hiding (PushMode(..))
import Graphos.Domain.Types.Pipeline (Neo4jPushMode(..), MemgraphPushMode(..))
import Graphos.Domain.Graph (Graph, gNodes)
import Graphos.Domain.Graph.Analysis (articulationPoints)

-- | Result of all export operations
data ExportResult = ExportResult
  { erReport    :: FilePath
  , erJSON      :: FilePath
  , erHTML      :: Maybe FilePath
  , erObsidian  :: Maybe FilePath
  , erNeo4j     :: Maybe FilePath
  , erMemgraph  :: Maybe FilePath
  } deriving (Eq, Show)

-- | Export all output formats.
-- Community data is passed to Neo4j push for Community nodes + BELONGS_TO edges.
-- Note: graph.json is written incrementally by the Pipeline's IncrementalJSON writer,
--       so we don't rewrite it here. This avoids holding the full graph in memory
--       as a JSON AST during export, reducing peak memory by ~1× graph size.
exportAll :: Graph -> Analysis -> PipelineConfig -> Detection -> Maybe (Map.Map CommunityId T.Text) -> IO ExportResult
exportAll g analysis config detection mLabels = do
  -- graph.json is already written incrementally by the Pipeline's IncrementalJSON writer.
  -- No need to rewrite it here — that would double memory usage during export.
  let jsonPath = cfgOutputDir config ++ "/graph.json"

  let reportPath = cfgOutputDir config ++ "/GRAPH_REPORT.md"
  let reportContent = Report.generateReport g analysis config detection mLabels
  ExportReport.exportReport reportContent reportPath

  htmlPath <- if cfgNoViz config
    then pure Nothing
    else do
      let htmlPath = cfgOutputDir config ++ "/graph.html"
      Just htmlPath <$ ExportHTML.exportHTML g analysis htmlPath

  obsidianPath <- if cfgObsidian config
    then do
      let obsDir = maybe (cfgOutputDir config ++ "/obsidian") id (cfgObsidianDir config)
      Just obsDir <$ ExportObsidian.exportObsidian g analysis obsDir
    else pure Nothing

  -- Neo4j export: Cypher file + push (with communities)
  let neo4jCfg = gcNeo4j (cfgGraphosConfig config)
  neo4jPath <- if cfgNeo4j config
    then do
      let cypherPath = cfgOutputDir config ++ "/graph.cypher"
      ExportNeo4j.exportCypher g cypherPath

      -- Push to Neo4j based on push mode
      let pushUri = case cfgNeo4jPush config of
            Just uri -> T.unpack uri
            Nothing  -> neo4jUri neo4jCfg
          commMap = analysisCommunities analysis
          cohesionMap = analysisCohesion analysis
          pushMode = cfgNeo4jPushMode config
          topN = cfgNeo4jSubgraphSize config

      (msg, _stmts, _batches) <- case pushMode of
        FullPush -> do
          TIO.putStrLn $ "[neo4j] Push mode: full (all nodes + edges + communities)"
          ExportNeo4j.pushToNeo4jWithCommunities
            g commMap cohesionMap
            (T.pack pushUri)
            (T.pack (neo4jUser neo4jCfg))
            (T.pack (neo4jPassword neo4jCfg))

        SubgraphPush -> do
          let artPoints = articulationPoints g
              totalNodes = Map.size (gNodes g)
          TIO.putStrLn $ "[neo4j] Push mode: subgraph (communities + " <> T.pack (show topN) <> " representatives/community, " <> T.pack (show (length artPoints)) <> " bridge nodes)"
          TIO.putStrLn $ "[neo4j] Full graph: " <> T.pack (show totalNodes) <> " nodes → subgraph: ~" <> T.pack (show (topN * Map.size commMap + length artPoints)) <> " representative nodes"
          ExportNeo4j.pushSubgraphToNeo4j
            g commMap cohesionMap topN artPoints
            (T.pack pushUri)
            (T.pack (neo4jUser neo4jCfg))
            (T.pack (neo4jPassword neo4jCfg))

        CommunityPush -> do
          TIO.putStrLn $ "[neo4j] Push mode: community-only (communities + inter-community edges)"
          ExportNeo4j.pushCommunityGraphToNeo4j
            g commMap cohesionMap
            (T.pack pushUri)
            (T.pack (neo4jUser neo4jCfg))
            (T.pack (neo4jPassword neo4jCfg))

      TIO.putStrLn $ "[neo4j] " <> msg
      pure (Just cypherPath)
    else pure Nothing

  -- Memgraph export: Cypher file + push (with communities)
  let memgraphCfg = gcMemgraph (cfgGraphosConfig config)
  memgraphPath <- if cfgMemgraph config
    then do
      let cypherPath = cfgOutputDir config ++ "/memgraph.cypher"
      ExportMemgraph.exportMemgraphCypher g cypherPath

      let pushUri = case cfgMemgraphPush config of
            Just uri -> T.unpack uri
            Nothing  -> mgUri memgraphCfg
          commMap = analysisCommunities analysis
          cohesionMap = analysisCohesion analysis
          pushMode = cfgMemgraphPushMode config
          topN = cfgMemgraphSubgraphSize config

      (msg, _stmts, _batches) <- case pushMode of
        MemgraphFull -> do
          TIO.putStrLn $ "[memgraph] Push mode: full (all nodes + edges + communities)"
          ExportMemgraph.pushToMemgraphWithCommunities
            g commMap cohesionMap
            (T.pack pushUri)
            (T.pack (mgUser memgraphCfg))
            (T.pack (mgPassword memgraphCfg))

        MemgraphSubgraph -> do
          let artPoints = articulationPoints g
              totalNodes = Map.size (gNodes g)
          TIO.putStrLn $ "[memgraph] Push mode: subgraph (communities + " <> T.pack (show topN) <> " representatives/community, " <> T.pack (show (length artPoints)) <> " bridge nodes)"
          TIO.putStrLn $ "[memgraph] Full graph: " <> T.pack (show totalNodes) <> " nodes → subgraph: ~" <> T.pack (show (topN * Map.size commMap + length artPoints)) <> " representative nodes"
          ExportMemgraph.pushSubgraphToMemgraph
            g commMap cohesionMap topN artPoints
            (T.pack pushUri)
            (T.pack (mgUser memgraphCfg))
            (T.pack (mgPassword memgraphCfg))

        MemgraphCommunity -> do
          TIO.putStrLn $ "[memgraph] Push mode: community-only (communities + inter-community edges)"
          ExportMemgraph.pushCommunityGraphToMemgraph
            g commMap cohesionMap
            (T.pack pushUri)
            (T.pack (mgUser memgraphCfg))
            (T.pack (mgPassword memgraphCfg))

      TIO.putStrLn $ "[memgraph] " <> msg
      pure (Just cypherPath)
    else pure Nothing

  pure ExportResult
    { erReport    = reportPath
    , erJSON      = jsonPath
    , erHTML      = htmlPath
    , erObsidian  = obsidianPath
    , erNeo4j     = neo4jPath
    , erMemgraph  = memgraphPath
    }