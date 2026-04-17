-- | Report export - writes GRAPH_REPORT.md to disk
module Graphos.Infrastructure.Export.Report
  ( exportReport
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Write report to file
exportReport :: Text -> FilePath -> IO ()
exportReport reportContent path = writeFile path (T.unpack reportContent)