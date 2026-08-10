-- | Tree-sitter extraction workflow (FFI via port).
module Graphos.UseCase.Extract.TreeSitter
  ( extractViaTreeSitterFFI
  , grammarForFile
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.FilePath (takeExtension)

import Graphos.Domain.Types (PipelineConfig(..), Extraction(..), extractionFromLists, GraphosConfig(..), gcExtractors, ExtractorConfig(..), ecGrammar, Granularity)
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.ExtractionPort (ExtractionPort(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.Domain.Graph (makeStubNode)

-- | Get the tree-sitter grammar name for a file from config.
grammarForFile :: PipelineConfig -> FilePath -> String
grammarForFile config fp =
  case Map.lookup (takeExtension fp) (gcExtractors (cfgGraphosConfig config)) of
    Just ec -> case ecGrammar ec of
      Just g  -> g
      Nothing -> drop 1 (takeExtension fp)
    Nothing -> drop 1 (takeExtension fp)

-- | Extract from a single file using tree-sitter FFI bindings via port.
extractViaTreeSitterFFI :: AppEnv -> Granularity -> String -> FilePath -> IO Extraction
extractViaTreeSitterFFI appEnv _ "markdown" filePath = epExtractDocFile (extractionPort appEnv) filePath
extractViaTreeSitterFFI appEnv _ "haskell"  filePath = epExtractHaskellStub (extractionPort appEnv) filePath
extractViaTreeSitterFFI appEnv _gran grammar filePath = do
  let ep = extractionPort appEnv
      lp = loggingPort appEnv
  content <- BS.readFile filePath
  result <- epParseWithGrammar ep grammar filePath content
  case result of
    Nothing -> do
      lpLogWarn lp $ T.pack $ "  [tree-sitter] No grammar for " ++ grammar ++ " or parse failed for " ++ filePath
      pure (extractionFromLists [makeStubNode filePath] [])
    Just extraction -> do
      let nNodes = Map.size (extractionNodes extraction)
          nEdges = Map.size (extractionEdges extraction)
      lpLogDebug lp $ T.pack $ "  [tree-sitter] " ++ filePath ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
      pure extraction