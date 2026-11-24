-- | Tree-sitter extraction workflow (FFI via port).
module Graphos.UseCase.Extract.TreeSitter
  ( extractViaTreeSitterFFI
  , GrammarSelection(..)
  , grammarSelectionForFile
  , grammarForFile
  , hasRuntimeGrammar
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.FilePath (takeExtension)

import Graphos.Domain.Types (PipelineConfig(..), Extraction(..), extractionFromLists, GraphosConfig(..), gcExtractors, ExtractorConfig(..), ecGrammar, ecGrammarPath, ecQueries, Granularity)
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.ExtractionPort (ExtractionPort(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.Domain.Graph (makeStubNode)

-- | Everything the extractor needs to select a tree-sitter grammar for one
-- file: the grammar name (for the built-in binding table), an optional runtime
-- shared library, and an optional directory of runtime .scm queries.
data GrammarSelection = GrammarSelection
  { gsGrammarName :: String
  , gsGrammarPath :: Maybe FilePath
  , gsQueriesPath :: Maybe FilePath
  } deriving (Eq, Show)

-- | Resolve the grammar selection for a file from per-extension config.
grammarSelectionForFile :: PipelineConfig -> FilePath -> GrammarSelection
grammarSelectionForFile config fp =
  case Map.lookup (takeExtension fp) (gcExtractors (cfgGraphosConfig config)) of
    Just ec -> GrammarSelection
      { gsGrammarName = maybe (drop 1 (takeExtension fp)) id (ecGrammar ec)
      , gsGrammarPath = ecGrammarPath ec
      , gsQueriesPath = ecQueries ec
      }
    Nothing -> GrammarSelection
      { gsGrammarName = drop 1 (takeExtension fp)
      , gsGrammarPath = Nothing
      , gsQueriesPath = Nothing
      }

-- | Get the tree-sitter grammar name for a file from config.
grammarForFile :: PipelineConfig -> FilePath -> String
grammarForFile config fp = gsGrammarName (grammarSelectionForFile config fp)

-- | True when the file is configured with a runtime grammar library, so the
-- grammar is available even when its name has no compiled-in binding.
hasRuntimeGrammar :: PipelineConfig -> FilePath -> Bool
hasRuntimeGrammar config fp = case gsGrammarPath (grammarSelectionForFile config fp) of
  Just _  -> True
  Nothing -> False

-- | Extract from a single file using tree-sitter FFI bindings via port.
extractViaTreeSitterFFI :: AppEnv -> Granularity -> GrammarSelection -> FilePath -> IO Extraction
extractViaTreeSitterFFI appEnv _ selection filePath
  | gsGrammarName selection == "markdown" =
      epExtractDocFile (extractionPort appEnv) filePath
  | gsGrammarName selection == "haskell" =
      epExtractHaskellStub (extractionPort appEnv) filePath
extractViaTreeSitterFFI appEnv gran selection filePath = do
  let ep = extractionPort appEnv
      lp = loggingPort appEnv
      grammar = gsGrammarName selection
  content <- BS.readFile filePath
  result <- epParseWithGrammar ep gran grammar (gsGrammarPath selection) (gsQueriesPath selection) filePath content
  case result of
    Nothing -> do
      lpLogWarn lp $ T.pack $ "  [tree-sitter] No grammar for " ++ grammar ++ " or parse failed for " ++ filePath
      pure (extractionFromLists [makeStubNode filePath] [])
    Just extraction -> do
      let nNodes = Map.size (extractionNodes extraction)
          nEdges = Map.size (extractionEdges extraction)
      lpLogDebug lp $ T.pack $ "  [tree-sitter] " ++ filePath ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
      pure extraction
