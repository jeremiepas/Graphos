-- | Export orchestration - all output formats
{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.Export
  ( ExportResult(..)
  , exportAll
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Graphos.Infrastructure.Export.JSON as ExportJSON
import qualified Graphos.Infrastructure.Export.HTML as ExportHTML
import qualified Graphos.Infrastructure.Export.Obsidian as ExportObsidian
import qualified Graphos.Infrastructure.Export.Report as ExportReport
import qualified Graphos.Infrastructure.Export.Neo4j as ExportNeo4j
import qualified Graphos.UseCase.Report as Report
import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph)

-- | Result of all export operations
data ExportResult = ExportResult
  { erReport   :: FilePath
  , erJSON     :: FilePath
  , erHTML     :: Maybe FilePath
  , erObsidian :: Maybe FilePath
  , erNeo4j   :: Maybe FilePath
  } deriving (Eq, Show)

-- | Export all output formats.
-- Community data is passed to Neo4j push for Community nodes + BELONGS_TO edges.
exportAll :: Graph -> Analysis -> PipelineConfig -> Detection -> IO ExportResult
exportAll g analysis config detection = do
  let jsonPath = cfgOutputDir config ++ "/graph.json"
  ExportJSON.exportGraph g analysis jsonPath

  let reportPath = cfgOutputDir config ++ "/GRAPH_REPORT.md"
  let reportContent = Report.generateReport g analysis config detection
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

      -- Push to Neo4j with community structure
      let pushUri = case cfgNeo4jPush config of
            Just uri -> T.unpack uri
            Nothing  -> neo4jUri neo4jCfg
          commMap = analysisCommunities analysis
          cohesionMap = analysisCohesion analysis
      (msg, _stmts, _batches) <- ExportNeo4j.pushToNeo4jWithCommunities
        g commMap cohesionMap
        (T.pack pushUri)
        (T.pack (neo4jUser neo4jCfg))
        (T.pack (neo4jPassword neo4jCfg))
      TIO.putStrLn $ "[neo4j] " <> msg
      pure (Just cypherPath)
    else pure Nothing

  pure ExportResult
    { erReport   = reportPath
    , erJSON     = jsonPath
    , erHTML     = htmlPath
    , erObsidian = obsidianPath
    , erNeo4j   = neo4jPath
    }