-- | Report export - writes GRAPH_REPORT.md to disk
module Graphos.Infrastructure.Export.Report
  ( exportReport
  ) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Graphos.Infrastructure.FileSystem.AtomicWrite (writeFileAtomic)

-- | Write the markdown report to disk atomically (temp + rename), so an
-- interrupted write never leaves a truncated GRAPH_REPORT.md behind.
exportReport :: Text -> FilePath -> IO ()
exportReport reportContent path =
  writeFileAtomic path (encodeUtf8 reportContent)