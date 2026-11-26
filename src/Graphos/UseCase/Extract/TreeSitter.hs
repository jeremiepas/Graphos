-- | Tree-sitter extraction workflow (FFI via port).
module Graphos.UseCase.Extract.TreeSitter
  ( extractViaTreeSitterFFI
  , GrammarSelection(..)
  , grammarSelectionForFile
  , grammarForFile
  , hasRuntimeGrammar
  ) where

import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, tryJust)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import System.FilePath (takeExtension)

import Graphos.Domain.Types (PipelineConfig(..), Extraction(..), ExtractionError(..), extractionFromLists, GraphosConfig(..), gcExtractors, ExtractorConfig(..), ecGrammar, ecGrammarPath, ecQueries, Granularity)
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
      -- Per-file failure isolation (custom-extractors 7.1): a read or parse
      -- failure yields a stub carrying an 'ExtractionError', never an exception
      -- that would abort the whole extraction. Async cancellation is left to
      -- propagate.
      failed msg = do
        lpLogWarn lp $ T.pack $ "  [tree-sitter] " ++ filePath ++ ": " ++ T.unpack msg
        pure ((extractionFromLists [makeStubNode filePath] [])
                { extractionErrors = [ExtractionError filePath (T.pack grammar) msg] })
  readResult <- tryJust synchronousFailure (BS.readFile filePath)
  case readResult of
    Left msg -> failed ("could not read file: " <> msg)
    Right content -> do
      parseResult <- tryJust synchronousFailure $
        epParseWithGrammar ep gran grammar (gsGrammarPath selection) (gsQueriesPath selection) filePath content
      case parseResult of
        Left msg -> failed ("parse failed: " <> msg)
        Right Nothing ->
          failed ("no grammar for '" <> T.pack grammar <> "' or parse failed")
        Right (Just extraction) -> do
          let nNodes = Map.size (extractionNodes extraction)
              nEdges = Map.size (extractionEdges extraction)
          lpLogDebug lp $ T.pack $ "  [tree-sitter] " ++ filePath ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
          pure extraction

-- | The text of a synchronous failure, or 'Nothing' for thread cancellation,
-- which must keep propagating so 'mapConcurrently' can unwind its siblings.
synchronousFailure :: SomeException -> Maybe Text
synchronousFailure e = case fromException e :: Maybe SomeAsyncException of
  Just _  -> Nothing
  Nothing -> Just (T.pack (displayException e))
