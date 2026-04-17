-- | Export orchestration - all output formats
module Graphos.UseCase.Export
  ( ExportResult(..)
  , exportAll
  ) where

import qualified Graphos.Infrastructure.Export.JSON as ExportJSON
import qualified Graphos.Infrastructure.Export.HTML as ExportHTML
import qualified Graphos.Infrastructure.Export.Obsidian as ExportObsidian
import qualified Graphos.Infrastructure.Export.Report as ExportReport
import qualified Graphos.UseCase.Report as Report
import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph)

-- | Result of all export operations
data ExportResult = ExportResult
  { erReport   :: FilePath
  , erJSON     :: FilePath
  , erHTML     :: Maybe FilePath
  , erObsidian :: Maybe FilePath
  } deriving (Eq, Show)

-- | Export all output formats
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

  pure ExportResult
    { erReport   = reportPath
    , erJSON     = jsonPath
    , erHTML     = htmlPath
    , erObsidian = obsidianPath
    }