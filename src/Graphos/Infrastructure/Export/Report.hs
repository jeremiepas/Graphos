-- | Report export - writes GRAPH_REPORT.md to disk
module Graphos.Infrastructure.Export.Report
  ( exportReport
  ) where

import Data.Text (Text)

import Graphos.Infrastructure.FileSystem.AtomicWrite (writeTextFileAtomic)

-- | Write report to file
exportReport :: Text -> FilePath -> IO ()
exportReport reportContent path = writeTextFileAtomic path reportContent