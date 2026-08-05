-- | Port interface for extraction operations.
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Infrastructure.Wiring provides the concrete implementations.
module Graphos.UseCase.Port.ExtractionPort
  ( -- * Extraction port
    ExtractionPort(..)
    -- * Opaque LSP handle
  , LSPHandle(..)
  , SymbolResult(..)
  ) where

import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Graphos.Domain.Types (Extraction, Node, Edge)
import Graphos.Domain.Types.Pipeline (PipelineConfig)

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
  , epParseWithGrammar     :: String -> FilePath -> ByteString -> IO (Maybe Extraction)
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
  }