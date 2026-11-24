-- | Port interface for extraction operations.
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Infrastructure.Wiring provides the concrete implementations.
module Graphos.UseCase.Port.ExtractionPort
  ( -- * Extraction port
    ExtractionPort(..)
    -- * External command extractor gate
  , CommandGate(..)
    -- * Opaque LSP handle
  , LSPHandle(..)
  , SymbolResult(..)
  ) where

import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Graphos.Domain.Types (Extraction, Node, Edge)
import Graphos.Domain.Config.Extraction (ExtractorConfig, Granularity)
import Graphos.Domain.Extraction (CommandOutcome)
import Graphos.Domain.Types.Pipeline (PipelineConfig)
import Graphos.Infrastructure.FileSystem.Ignore (AnnotatedPattern)

-- | Opaque handle to an LSP server connection.
-- Internally stores the real Infrastructure.LSP.Client as Dynamic.
-- UseCase only passes this around — never inspects it.
data LSPHandle = LSPHandle
  { lhHandle :: !Dynamic
  , lhCommand :: !String
  , lhArgs :: ![String]
  , lhRootUri :: !String
  }

-- | Result of workspace symbol extraction.
data SymbolResult = SymbolResult
  { srNodes :: [Node]
  , srEdges :: [Edge]
  }

-- | Gate applied before any external command extractor is spawned
-- (custom-extractors 3.4). Carrying it as one record keeps the "disabled",
-- "sensitive" and "ignored" decisions in a single place that is evaluated
-- before 'System.Process.proc'.
data CommandGate = CommandGate
  { cgEnabled  :: Bool               -- ^ @allow_command_extractors@ (CLI default true, library default false)
  , cgRoot     :: FilePath           -- ^ scan root the ignore patterns are relative to
  , cgPatterns :: [AnnotatedPattern] -- ^ ignore patterns that also gate command extraction
  } deriving (Eq, Show)

-- | Record-of-functions port for extraction operations.
-- Provides fine-grained primitives that UseCase.Extract uses
-- to orchestrate the extraction workflow.
data ExtractionPort = ExtractionPort
  { -- LSP lifecycle
    epFindLSPServer        :: String -> IO (Maybe (String, [String]))
  , epConnectLSP           :: String -> [String] -> String -> IO (Either Text LSPHandle)
  , epDisconnectLSP        :: LSPHandle -> IO ()
  , epIsServerConnected    :: LSPHandle -> IO Bool
    -- LSP extraction
  , epExtractViaLSP        :: LSPHandle -> FilePath -> IO Extraction
  , epHasWorkspaceSymbols  :: LSPHandle -> IO Bool
  , epExtractWorkspaceSymbols :: LSPHandle -> IO (Either Text (Map FilePath SymbolResult))
    -- TreeSitter extraction
  , epParseWithGrammar     :: Granularity -> String -> FilePath -> ByteString -> IO (Maybe Extraction)
    -- File-level extraction (delegates to sub-modules)
  , epExtractDocFile       :: FilePath -> IO Extraction
  , epExtractOfficeFile    :: PipelineConfig -> FilePath -> IO Extraction
  , epExtractPdfFile       :: PipelineConfig -> FilePath -> IO Extraction
  , epExtractHaskellStub   :: FilePath -> IO Extraction
  , epExtractImageFile     :: PipelineConfig -> FilePath -> IO Extraction
  , epExtractImageFromBytes :: PipelineConfig -> FilePath -> ByteString -> IO Extraction
    -- Office media extraction
  , epExtractMediaFile     :: FilePath -> FilePath -> IO (Either Text ByteString)
  , epDocxMediaPaths       :: FilePath -> IO [FilePath]
  , epPptxMediaPaths       :: FilePath -> IO [FilePath]
    -- Neo4j streaming
  , epPushExtractionStreaming :: PipelineConfig -> Extraction -> IO ()
    -- Config lookups
  , epLanguageServerCommands :: Map String (String, [String])
    -- | Check whether a tree-sitter grammar is available for the given language.
    -- Returns True if the port can parse this language via tree-sitter.
  , epHasTreeSitterGrammar :: String -> Bool
    -- | External command extractor (custom-extractors 3.1–3.4): run the
    -- declared argv for every file, honouring @batch@, and report one outcome
    -- per file. Never throws; gated files are reported as skipped.
  , epExtractCommandFiles :: CommandGate -> ExtractorConfig -> [FilePath] -> IO (Map FilePath CommandOutcome)
  }
