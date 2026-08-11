-- | Export orchestration - all output formats via ExportPort.
{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.Export
  ( exportAll
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map

import Graphos.UseCase.Report (generateReport)
import Graphos.Domain.Types hiding (PushMode(..))
import Graphos.Domain.Types.Pipeline (Neo4jPushMode(..), MemgraphPushMode(..))
import Graphos.Domain.Graph (Graph, gNodes)
import Graphos.Domain.Graph.Analysis (articulationPoints)
import Graphos.UseCase.Port.ExportPort (ExportPort(..), ExportResult(..))

-- | Export all output formats via the ExportPort.
-- Community data is passed to Neo4j push for Community nodes + BELONGS_TO edges.
-- Note: graph.json is written incrementally by the Pipeline's IncrementalJSON writer,
--       so we don't rewrite it here. This avoids holding the full graph in memory
--       as a JSON AST during export, reducing peak memory by ~1x graph size.
exportAll :: ExportPort -> Graph -> Analysis -> PipelineConfig -> Detection -> Maybe (Map.Map CommunityId T.Text) -> IO ExportResult
exportAll ep g analysis config detection mLabels = do
  -- graph.json is already written incrementally by the Pipeline's IncrementalJSON writer.
  -- No need to rewrite it here — that would double memory usage during export.
  let jsonPath = cfgOutputDir config ++ "/graph.json"

  let reportPath = cfgOutputDir config ++ "/GRAPH_REPORT.md"
  let reportContent = generateReport g analysis config detection mLabels
  epExportReport ep reportContent reportPath

  htmlPath <- if cfgNoViz config
    then pure Nothing
    else do
      let hPath = cfgOutputDir config ++ "/graph.html"
      Just hPath <$ epExportHTML ep g analysis mLabels hPath

  obsidianPath <- if cfgObsidian config
    then do
      let obsDir = maybe (cfgOutputDir config ++ "/obsidian") id (cfgObsidianDir config)
      Just obsDir <$ epExportObsidian ep g analysis obsDir
    else pure Nothing

  -- Neo4j export: Cypher file + push (with communities)
  let neo4jCfg = gcNeo4j (cfgGraphosConfig config)
  neo4jPath <- if cfgNeo4j config
    then do
      let cypherPath = cfgOutputDir config ++ "/graph.cypher"
      epExportCypher ep g cypherPath

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
          epPushToNeo4jFull ep g commMap cohesionMap
            (T.pack pushUri)
            (T.pack (neo4jUser neo4jCfg))
            (T.pack (neo4jPassword neo4jCfg))

        SubgraphPush -> do
          let artPoints = articulationPoints g
              totalNodes = Map.size (gNodes g)
          TIO.putStrLn $ "[neo4j] Push mode: subgraph (communities + " <> T.pack (show topN) <> " representatives/community, " <> T.pack (show (length artPoints)) <> " bridge nodes)"
          TIO.putStrLn $ "[neo4j] Full graph: " <> T.pack (show totalNodes) <> " nodes → subgraph: ~" <> T.pack (show (topN * Map.size commMap + length artPoints)) <> " representative nodes"
          epPushToNeo4jSubgraph ep g commMap cohesionMap topN artPoints
            (T.pack pushUri)
            (T.pack (neo4jUser neo4jCfg))
            (T.pack (neo4jPassword neo4jCfg))

        CommunityPush -> do
          TIO.putStrLn $ "[neo4j] Push mode: community-only (communities + inter-community edges)"
          epPushToNeo4jCommunity ep g commMap cohesionMap
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
      epExportMemgraphCypher ep g cypherPath

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
          epPushToMemgraphFull ep g commMap cohesionMap
            (T.pack pushUri)
            (T.pack (mgUser memgraphCfg))
            (T.pack (mgPassword memgraphCfg))

        MemgraphSubgraph -> do
          let artPoints = articulationPoints g
              totalNodes = Map.size (gNodes g)
          TIO.putStrLn $ "[memgraph] Push mode: subgraph (communities + " <> T.pack (show topN) <> " representatives/community, " <> T.pack (show (length artPoints)) <> " bridge nodes)"
          TIO.putStrLn $ "[memgraph] Full graph: " <> T.pack (show totalNodes) <> " nodes → subgraph: ~" <> T.pack (show (topN * Map.size commMap + length artPoints)) <> " representative nodes"
          epPushToMemgraphSubgraph ep g commMap cohesionMap topN artPoints
            (T.pack pushUri)
            (T.pack (mgUser memgraphCfg))
            (T.pack (mgPassword memgraphCfg))

        MemgraphCommunity -> do
          TIO.putStrLn $ "[memgraph] Push mode: community-only (communities + inter-community edges)"
          epPushToMemgraphCommunity ep g commMap cohesionMap
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