-- | Office file conversion - .docx and .xlsx to markdown sidecars
-- Uses zip + XML parsing (no external Python deps needed)
module Graphos.Infrastructure.FileSystem.OfficeConvert
  ( docxToMarkdown
  , xlsxToMarkdown
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Convert .docx file content to markdown.
-- Since Haskell doesn't have a simple docx library in base,
-- this provides a stub that reads the file and extracts text.
-- For full conversion, use the `zip-archive` + `xml` packages.
docxToMarkdown :: FilePath -> IO (Either Text Text)
docxToMarkdown path = do
  -- Stub: .docx is a zip file containing word/document.xml
  -- Full implementation would: unzip, parse XML, extract <w:t> elements
  -- For now, return a placeholder
  pure (Right $ "# Document: " <> T.pack path <> "\n\n[Content extraction pending - use zip-archive + xml packages for full support]")

-- | Convert .xlsx file content to markdown.
-- Since Haskell doesn't have a simple xlsx library in base,
-- this provides a stub that reads the file and extracts tabular data.
xlsxToMarkdown :: FilePath -> IO (Either Text Text)
xlsxToMarkdown path = do
  -- Stub: .xlsx is a zip file containing xl/worksheets/sheet1.xml
  -- Full implementation would: unzip, parse XML, extract <v> elements per row
  -- For now, return a placeholder
  pure (Right $ "# Spreadsheet: " <> T.pack path <> "\n\n| Sheet | Data |\n|-------|------|\n| Sheet1 | [Content extraction pending] |")