-- | PDF extraction via pdftotext CLI.
--
-- Calls the external `pdftotext` command (from poppler-utils) to extract
-- text from PDF files, then parses the text through the pure Domain.PdfStructure
-- module to build an Extraction with hierarchical section nodes and Contains edges.
--
-- When pdftotext is not available, logs a warning and returns a stub node
-- (matching the pattern of other extractors on failure).
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Extract.Pdf
  ( extractPdfFile
  ) where

import Control.Exception (SomeException, catch)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Bits ((.|.))
import Data.Text.Short (fromText, toText)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Graphos.Domain.Config.Extraction (Granularity(..), PdfExtractionMode(..))
import Graphos.Domain.Config.Core (GraphosConfig(..), gcPdfExtraction)
import Graphos.Domain.PdfStructure (parsePdfStructure, pdfStructureToExtraction)
import Graphos.Domain.Types (Extraction, extractionFromLists, extractionNodes, extractionEdges, Node(..), FileType(..), bitNodeLineStart, bitNodeKind)
import Graphos.Domain.Types.Pipeline (PipelineConfig(..), cfgGraphosConfig)
import Graphos.Infrastructure.Logging (LogEnv, logDebug, logInfo, logWarn)

-- | Extract entities from a PDF file.
--
-- Calls `pdftotext <filePath> -` to extract text, then parses the structure
-- through Domain.PdfStructure and converts to an Extraction.
--
-- If pdftotext is not found or fails, logs a warning and returns a stub node.
extractPdfFile :: LogEnv -> PipelineConfig -> FilePath -> IO Extraction
extractPdfFile logEnv config filePath = do
  exists <- doesFileExist filePath
  case exists of
    False -> do
      logWarn logEnv $ T.pack $ "[pdf] File not found: " ++ filePath
      logDebug logEnv $ T.pack $ "[pdf] " ++ filePath ++ " -> stub (1 node, 0 edges)"
      pure (extractionFromLists [pdfStubNode filePath] [])
    True -> do
      let pdfMode = resolvePdfExtractionMode config
          granularity = pdfModeToGranularity pdfMode
      logInfo logEnv $ T.pack $ "[pdf] Extracting: " ++ filePath ++ " (mode: " ++ showPdfMode pdfMode ++ ", granularity: " ++ showGranularity granularity ++ ")"
      catch
        (do (exitCode, stdout, stderr) <- readProcessWithExitCode "pdftotext" [filePath, "-"] ""
            case exitCode of
              ExitSuccess -> do
                let text = T.pack stdout
                if T.null (T.strip text)
                  then do
                    logWarn logEnv $ T.pack $ "[pdf] Empty text from: " ++ filePath
                    logDebug logEnv $ T.pack $ "[pdf] " ++ filePath ++ " -> stub (1 node, 0 edges)"
                    pure (extractionFromLists [pdfStubNode filePath] [])
                  else do
                    let struct = parsePdfStructure granularity text
                        extraction = pdfStructureToExtraction filePath struct
                        nNodes = Map.size (extractionNodes extraction)
                        nEdges = Map.size (extractionEdges extraction)
                    logDebug logEnv $ T.pack $ "[pdf] " ++ filePath ++ " -> " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
                    pure extraction
              ExitFailure code -> do
                logWarn logEnv $ T.pack $ "[pdf] pdftotext failed (exit " ++ show code ++ ") for " ++ filePath ++ ": " ++ take 200 stderr
                logDebug logEnv $ T.pack $ "[pdf] " ++ filePath ++ " -> stub (1 node, 0 edges)"
                pure (extractionFromLists [pdfStubNode filePath] [])
        )
        (\(e :: SomeException) -> do
          logWarn logEnv $ T.pack $ "[pdf] Exception extracting " ++ filePath ++ ": " ++ show e
          logDebug logEnv $ T.pack $ "[pdf] " ++ filePath ++ " -> stub (1 node, 0 edges)"
          pure (extractionFromLists [pdfStubNode filePath] [])
        )

-- | Resolve PDF extraction mode from config.
-- Priority: global gcPdfExtraction setting.
resolvePdfExtractionMode :: PipelineConfig -> PdfExtractionMode
resolvePdfExtractionMode config =
  gcPdfExtraction (cfgGraphosConfig config)

-- | Convert PdfExtractionMode to Granularity for PDF parsing.
--   * PdfSmall  → GranularityFile  (top-level titles only, no paragraphs)
--   * PdfMedium → GranularityFunction (titles + sections, no paragraphs)
--   * PdfLarge  → GranularityFine  (all levels + paragraphs)
pdfModeToGranularity :: PdfExtractionMode -> Granularity
pdfModeToGranularity PdfSmall  = GranularityFile
pdfModeToGranularity PdfMedium = GranularityFunction
pdfModeToGranularity PdfLarge  = GranularityFine

-- | Human-readable PDF mode name for logging.
showPdfMode :: PdfExtractionMode -> String
showPdfMode PdfSmall  = "small"
showPdfMode PdfMedium = "medium"
showPdfMode PdfLarge  = "large"

-- | Human-readable granularity name for logging.
showGranularity :: Granularity -> String
showGranularity GranularityFine     = "fine"
showGranularity GranularityFunction = "function"
showGranularity GranularityFile     = "file"

-- | Create a stub node for a PDF file (used when extraction fails).
pdfStubNode :: FilePath -> Node
pdfStubNode filePath =
  let name = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      nid = T.pack (show dirHash) <> "_paper_" <> name
   in Node
        { nodeId           = nid
        , nodeLabel        = fromText name
        , nodeFileType     = PaperFile
        , nodeSourceFile   = fromText (T.pack filePath)
        , nodeLineStart    = Just 1
        , nodeLineEnd      = Nothing
        , nodeSignature    = Nothing
        , nodeCommunityId  = Nothing
        , nodeDegree       = Nothing
        , nodeIsBridge     = Nothing
        , nodeExtra        = Nothing
        , nodeKind         = Just (fromText "File")
        , nodePresentBits  = bitNodeLineStart .|. bitNodeKind
        }