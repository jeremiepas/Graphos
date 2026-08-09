-- | Extraction orchestration (re-export hub) — parallel extraction for all files.
-- Routes each file to its configured extractor (LSP, tree-sitter, or stub)
-- based on the graphos.yaml config.
--
-- Implementation lives in focused sub-modules:
--   - UseCase.Extract.Core       — orchestration (extractAll, extractChangedFiles)
--   - UseCase.Extract.LSP        — LSP workflow (connect, extract symbols)
--   - UseCase.Extract.TreeSitter  — tree-sitter FFI workflow
--   - UseCase.Extract.Haskell     — Haskell stub fallback
--   - UseCase.Extract.Image       — image/vision extraction
--   - UseCase.Extract.Office      — office document extraction
--   - UseCase.Extract.Markdown    — markdown/doc extraction
module Graphos.UseCase.Extract
  ( extractAll
  , extractChangedFiles
  , extractFromFile
  , extractViaTreeSitterFFI
  , extractorForExt
  , resolveGranularity
  , granularityForFile
  , pushExtractionStreaming
  , isStubExtraction
  ) where

import Graphos.UseCase.Extract.Core
  ( extractAll
  , extractChangedFiles
  , pushExtractionStreaming
  , extractorForExt
  , resolveGranularity
  , granularityForFile
  , isStubExtraction
  )
import Graphos.UseCase.Extract.LSP (extractFromFile)
import Graphos.UseCase.Extract.TreeSitter (extractViaTreeSitterFFI)