-- | Production wiring — creates AppEnv with real Infrastructure implementations.
-- This is the composition root where Infrastructure is wired into UseCase ports.
-- Main.hs calls 'productionAppEnv' and passes the result to UseCase functions.
module Graphos.Infrastructure.Wiring
  ( -- * Production wiring
    productionAppEnv
  , productionLoggingPort
  , productionObservabilityPort
  , productionFileSystemPort
  , productionExtractionPort
  , productionExportPort
  , productionLLMPort
  ) where

import Control.Monad (when)
import Data.Dynamic (toDyn)
import qualified Data.Map.Strict as Map
import Foreign.Ptr (Ptr)
import Unsafe.Coerce (unsafeCoerce)

import Graphos.Domain.Types (Extraction(..))
import Graphos.Domain.Types.Pipeline (PipelineConfig(..), Neo4jStreamingConfig(..))
import Graphos.Domain.Config.Extraction (Granularity(..))
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.ExtractionPort (ExtractionPort(..), LSPHandle(..), SymbolResult(..))
import Graphos.UseCase.Port.ExportPort (ExportPort(..))
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.UseCase.Port.ObservabilityPort (ObservabilityPort(..))
import Graphos.UseCase.Port.LLMPort (LLMPort(..), ImageAnalysis(..), ImageKind(..), Entity(..))

-- Infrastructure imports (only Wiring imports Infrastructure directly)
import qualified Graphos.Infrastructure.LSP.Client as LSP
import qualified Graphos.Infrastructure.LSP.Protocol as LSPProtocol
import Graphos.Infrastructure.Extract.TreeSitter.Core (parseWithGrammar)
import Graphos.Infrastructure.Extract.TreeSitter.Convert (tsNodesToExtraction)
import qualified Graphos.Infrastructure.Export.Neo4j as Neo4j
import Graphos.Infrastructure.FileSystem.Cache
  ( loadPipelineCheckpoint, savePipelineCheckpoint, clearPipelineCheckpoint )
import Graphos.Infrastructure.FileSystem.Ignore (loadIgnorePatterns)
import Graphos.Infrastructure.FileSystem.OfficeConvert
  ( docxExtractMediaPaths, pptxExtractMediaPaths, extractMediaFile )
import Graphos.Infrastructure.Logging
  ( LogEnv, logInfo, logDebug, logTrace, logWarn, logError )
import Graphos.Infrastructure.Observability.SDK
  ( ObservabilityEnv(..)
  , shutdownObservability, incCounter, setGauge
  , debugTraceEvent
  )
import qualified Graphos.Infrastructure.LLM.OpenAI as OpenAI
import qualified Graphos.Infrastructure.LLM.Embedding as Emb
import qualified Graphos.Infrastructure.LLM.Vision as Vision
import Graphos.Infrastructure.Security (validateUrl)

import qualified TreeSitter.TypeScript as TSTypeScript
import qualified TreeSitter.Python as TSPython
import qualified TreeSitter.JSON as TSJSON
import qualified TreeSitter.Go as TSGo
import qualified TreeSitter.Rust as TSRust
import qualified TreeSitter.Haskell as TSHaskell
import qualified TreeSitter.Language as TS_LANG

-- UseCase imports (for delegation to sub-modules)
import Graphos.UseCase.Extract.Markdown (extractDocFile)
import Graphos.UseCase.Extract.Office (extractOfficeFile)
import Graphos.UseCase.Extract.Haskell (extractHaskellStub)
import Graphos.UseCase.Extract.Image (extractImageFile, extractImageFromBytes)

-- | Create a production AppEnv from a LogEnv and ObservabilityEnv.
-- Called once at startup in Main.hs.
productionAppEnv :: LogEnv -> ObservabilityEnv -> AppEnv
productionAppEnv logEnv obsEnv = AppEnv
  { extractionPort      = productionExtractionPort logEnv
  , exportPort          = productionExportPort logEnv obsEnv
  , fileSystemPort      = productionFileSystemPort
  , loggingPort         = productionLoggingPort logEnv
  , observabilityPort   = productionObservabilityPort obsEnv
  , llmPort              = productionLLMPort
  }

-- | Production logging port — delegates to Infrastructure.Logging.
productionLoggingPort :: LogEnv -> LoggingPort
productionLoggingPort env = LoggingPort
  { lpLogTrace = logTrace env
  , lpLogDebug = logDebug env
  , lpLogInfo  = logInfo env
  , lpLogWarn  = logWarn env
  , lpLogError = logError env
  }

-- | Production observability port — delegates to Infrastructure.Observability.SDK.
productionObservabilityPort :: ObservabilityEnv -> ObservabilityPort
productionObservabilityPort obsEnv = ObservabilityPort
  { opInitObservability = \_ _ _ -> pure ()  -- already initialized by Main
  , opShutdownObservability = shutdownObservability obsEnv
  , opIncCounter = \name delta -> incCounter (otelMetrics obsEnv) name delta
  , opSetGauge = \name val -> setGauge (otelMetrics obsEnv) name val
  , opTraceEvent = \name attrs -> debugTraceEvent (otelDebugTrace obsEnv) name (Map.fromList attrs)
  }

-- | Production file system port — delegates to Infrastructure.FileSystem.
productionFileSystemPort :: FileSystemPort
productionFileSystemPort = FileSystemPort
  { fspLoadCheckpoint     = loadPipelineCheckpoint
  , fspSaveCheckpoint     = savePipelineCheckpoint
  , fspClearCheckpoint    = clearPipelineCheckpoint
  , fspLoadIgnorePatterns = loadIgnorePatterns
  }

-- | Production extraction port — delegates to Infrastructure.LSP, TreeSitter, etc.
productionExtractionPort :: LogEnv -> ExtractionPort
productionExtractionPort logEnv = ExtractionPort
  { epFindLSPServer = LSP.findLSPServer
  , epConnectLSP = \cmd args rootUri -> do
      let config = LSP.LSPClientConfig
            { LSP.lspCommand = cmd
            , LSP.lspArgs    = args
            , LSP.lspRootUri = rootUri
            , LSP.lspTimeout  = 300
            }
      result <- LSP.connectToLSP config
      case result of
        Right client -> pure $ Right $ LSPHandle
          { lhHandle = toDyn client
          , lhCommand = cmd
          , lhArgs = args
          , lhRootUri = rootUri
          }
        Left err -> pure $ Left err
  , epDisconnectLSP = \(LSPHandle dynHandle _ _ _) ->
      LSP.disconnectLSP (unsafeCoerce dynHandle :: LSP.LSPClient)
  , epIsServerConnected = \(LSPHandle dynHandle _ _ _) ->
      LSP.isServerConnected (unsafeCoerce dynHandle :: LSP.LSPClient)
  , epExtractViaLSP = \(LSPHandle dynHandle _ _ _) fp ->
      LSP.extractViaLSP (unsafeCoerce dynHandle :: LSP.LSPClient) fp
  , epHasWorkspaceSymbols = \(LSPHandle dynHandle _ _ _) ->
      pure $ LSPProtocol.scpWorkspaceSymbolProvider (LSP.lspServerCaps (unsafeCoerce dynHandle :: LSP.LSPClient))
  , epExtractWorkspaceSymbols = \(LSPHandle dynHandle _ _ _) -> do
      let client = unsafeCoerce dynHandle :: LSP.LSPClient
      result <- LSP.extractWorkspaceSymbols client
      case result of
        Right syms -> do
          let fileMap = LSP.workspaceSymbolsToDocumentSymbols syms
          pure $ Right $ Map.map
            (\symbols -> SymbolResult
              { srNodes = LSP.symbolToNodes "<workspace>" symbols
              , srEdges = LSP.symbolTreeToEdges "<workspace>" symbols
              }
            ) fileMap
        Left err -> pure $ Left err
  , epParseWithGrammar = \grammar filePath content ->
      case getGrammarPtr grammar of
        Nothing -> pure Nothing
        Just lang -> do
          result <- parseWithGrammar lang content
          case result of
            Nothing -> pure Nothing
            Just nodes -> pure $ Just $ tsNodesToExtraction GranularityFunction filePath nodes
  , epExtractDocFile = \filePath -> extractDocFile logEnv filePath
  , epExtractOfficeFile = \config fp -> extractOfficeFile config logEnv fp
  , epExtractHaskellStub = extractHaskellStub
  , epExtractImageFile = \config fp -> extractImageFile config logEnv fp
  , epExtractImageFromBytes = \config fp bytes -> extractImageFromBytes config logEnv fp bytes
  , epExtractMediaFile = extractMediaFile
  , epDocxMediaPaths = docxExtractMediaPaths
  , epPptxMediaPaths = pptxExtractMediaPaths
  , epPushExtractionStreaming = \config extraction ->
      case cfgNeo4jStreaming config of
        Nothing -> pure ()
        Just n4cfg -> do
          let nNodes = Map.size (extractionNodes extraction)
              nEdges = Map.size (extractionEdges extraction)
          when (nNodes > 0 || nEdges > 0) $ do
            (_msg, _stmts, _batches) <- Neo4j.pushFileExtraction extraction
              (neo4jsUri n4cfg) (neo4jsUser n4cfg) (neo4jsPassword n4cfg)
            pure ()
  , epLanguageServerCommands = LSP.languageServerCommands
  }

-- | Production export port — delegates to Infrastructure.Export.* and UseCase.Export.
productionExportPort :: LogEnv -> ObservabilityEnv -> ExportPort
productionExportPort _logEnv _obsEnv = ExportPort
  { epExportAll = \_outputDir _analysis _config _detection _mLabels ->
      -- TODO: Will be wired to UseCase.Export.exportAll once UseCase.Export is refactored
      error "productionExportPort: not yet wired — call UseCase.Export.exportAll directly for now"
  }

-- | Production LLM port — delegates to Infrastructure.LLM.*.
productionLLMPort :: LLMPort
productionLLMPort = LLMPort
  { lpCallLLM = \lCfg prompt -> OpenAI.callLLM lCfg prompt
  , lpParseLabelsFromResponse = OpenAI.parseLabelsFromResponse
  , lpGenerateEmbedding = \eCfg text -> Emb.generateEmbedding eCfg text
  , lpAnalyzeImage = \vCfg lCfg fp -> do
      result <- Vision.analyzeImage vCfg lCfg fp
      case result of
        Right ia -> pure $ Right $ ImageAnalysis
          { iaDescription = Vision.iaDescription ia
          , iaEntities = map convertEntity (Vision.iaEntities ia)
          , iaKind = convertImageKind (Vision.iaKind ia)
          }
        Left err -> pure $ Left err
  , lpValidateUrl = validateUrl
  }

-- | Convert Infrastructure ImageKind to Port ImageKind.
convertImageKind :: Vision.ImageKind -> ImageKind
convertImageKind Vision.Photo      = Photo
convertImageKind Vision.Diagram    = Diagram
convertImageKind Vision.Screenshot = Screenshot
convertImageKind Vision.Resume     = Resume
convertImageKind Vision.Chart      = Chart
convertImageKind Vision.OtherKind  = OtherKind

-- | Convert Infrastructure Entity to Port Entity.
convertEntity :: Vision.Entity -> Entity
convertEntity e = Entity
  { entityLabel = Vision.entityLabel e
  , entityType  = Vision.entityType e
  }

-- | Get the tree-sitter language pointer for a grammar name.
getGrammarPtr :: String -> Maybe (Ptr TS_LANG.Language)
getGrammarPtr "typescript"   = Just TSTypeScript.tree_sitter_typescript
getGrammarPtr "tsx"          = Just TSTypeScript.tree_sitter_typescript
getGrammarPtr "javascript"   = Just TSTypeScript.tree_sitter_typescript
getGrammarPtr "python"       = Just TSPython.tree_sitter_python
getGrammarPtr "json"         = Just TSJSON.tree_sitter_json
getGrammarPtr "go"           = Just TSGo.tree_sitter_go
getGrammarPtr "rust"         = Just TSRust.tree_sitter_rust
getGrammarPtr "haskell"      = Just TSHaskell.tree_sitter_haskell
getGrammarPtr _              = Nothing