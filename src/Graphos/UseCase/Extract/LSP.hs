-- | LSP extraction workflow — connect to a server, extract symbols per file.
module Graphos.UseCase.Extract.LSP
  ( FileGroup
  , groupByLSPServer
  , extractGroup
  , doExtractWithSharedLSP
  , extractionFromPortSymbols
  , extractFilesWithLSP
  , extractFromFile
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension)

import Graphos.Domain.Types (PipelineConfig(..), Extraction(..), extractionFromLists)
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.ExtractionPort (ExtractionPort(..), LSPHandle(..), SymbolResult(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.Domain.Graph (makeStubNode)

-- | A group of files sharing the same LSP server command
type FileGroup = (String, [FilePath])

-- | Group files by their LSP server command
groupByLSPServer :: Map.Map String (String, [String]) -> [FilePath] -> [FileGroup]
groupByLSPServer langServerCmds files =
  let fileWithServer = [(serverCmd langServerCmds f, f) | f <- files]
      grouped = Map.toList $ Map.fromListWith (++) [(cmd, [fp]) | (cmd, fp) <- fileWithServer]
  in grouped
  where
    serverCmd cmds fp = case Map.lookup (takeExtension fp) cmds of
      Just (cmd, _) -> cmd
      Nothing       -> "stub"

-- | Extract all files in a group using a single shared LSP connection
extractGroup :: AppEnv -> FilePath -> PipelineConfig -> FileGroup -> IO [Extraction]
extractGroup appEnv absRoot _config (serverCmd, files) =
  let logDebug = lpLogDebug (loggingPort appEnv)
  in if serverCmd == "stub"
    then mapM (\fp -> do
      logDebug $ T.pack $ "  [stub] " ++ fp
      pure (extractionFromLists [makeStubNode fp] [])
    ) files
    else doExtractWithSharedLSP appEnv absRoot serverCmd files

-- | Connect to an LSP server once and extract all files for it.
doExtractWithSharedLSP :: AppEnv -> FilePath -> String -> [FilePath] -> IO [Extraction]
doExtractWithSharedLSP appEnv absRoot serverCmd files = do
  let ep = extractionPort appEnv
      lp = loggingPort appEnv
      logInfo  = lpLogInfo lp
      logDebug = lpLogDebug lp
      logWarn  = lpLogWarn lp

  mbLSPOpts <- epFindLSPServer ep (takeExtension (case files of (f:_) -> f; [] -> ""))
  case mbLSPOpts of
    Nothing -> mapM (\fp -> do
      logWarn $ T.pack $ "  LSP " ++ serverCmd ++ " disappeared for " ++ fp
      pure (extractionFromLists [makeStubNode fp] [])
      ) files
    Just (cmd, args) -> do
      logDebug $ T.pack $ "  [lsp] Connecting to " ++ cmd ++ " for " ++ show (length files) ++ " files"
      result <- epConnectLSP ep cmd args absRoot
      case result of
        Left err -> do
          logWarn $ T.pack $ "  [lsp] Connection failed: " ++ T.unpack err
          mapM (\fp -> pure (extractionFromLists [makeStubNode fp] [])) files
        Right handle -> do
          hasWsSym <- epHasWorkspaceSymbols ep handle
          extractions <- if hasWsSym
            then do
              logInfo $ T.pack $ "  [lsp] Server supports workspace/symbol — using project-level extraction"
              wsResult <- epExtractWorkspaceSymbols ep handle
              case wsResult of
                Right fileMap
                  | not (Map.null fileMap) -> do
                    logInfo $ T.pack $ "  [lsp] workspace/symbol returned symbols across " ++ show (Map.size fileMap) ++ " files"
                    pure [extractionFromPortSymbols fp symResult | fp <- files
                          , let symResult = Map.findWithDefault (SymbolResult [] []) fp fileMap]
                  | otherwise -> do
                    logDebug $ T.pack $ "  [lsp] workspace/symbol returned empty — falling back to per-file extraction"
                    extractFilesWithLSP appEnv handle files
                Left err -> do
                  logWarn $ T.pack $ "  [lsp] workspace/symbol failed: " ++ T.unpack err ++ " — falling back to per-file extraction"
                  extractFilesWithLSP appEnv handle files
            else do
              logDebug $ T.pack $ "  [lsp] Server does not support workspace/symbol — using per-file extraction"
              extractFilesWithLSP appEnv handle files
          mapM_ (\(fp, ext) -> do
            let nNodes = Map.size (extractionNodes ext)
                nEdges = Map.size (extractionEdges ext)
            logDebug $ T.pack $ "  [lsp] " ++ fp ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
            ) (zip files extractions)
          enriched <- mapM (\(fp, ext) ->
            if Map.null (extractionNodes ext) && takeExtension fp `elem` [".hs", ".lhs"]
              then do
                logDebug $ T.pack $ "  [haskell-stub] LSP gave 0 symbols for " ++ fp ++ ", using parser fallback"
                epExtractHaskellStub ep fp
              else pure ext
            ) (zip files extractions)
          epDisconnectLSP ep handle
          pure enriched

-- | Build an Extraction from a port SymbolResult.
extractionFromPortSymbols :: FilePath -> SymbolResult -> Extraction
extractionFromPortSymbols _filePath symResult =
  let nodes = srNodes symResult
      edges = srEdges symResult
  in extractionFromLists nodes edges

-- | Extract files via LSP, short-circuiting when the server dies.
extractFilesWithLSP :: AppEnv -> LSPHandle -> [FilePath] -> IO [Extraction]
extractFilesWithLSP _appEnv _ [] = pure []
extractFilesWithLSP appEnv handle (fp:fps) = do
  let ep = extractionPort appEnv
  alive <- epIsServerConnected ep handle
  if not alive
    then pure [extractionFromLists [makeStubNode f] [] | f <- fp:fps]
    else do
      ext <- epExtractViaLSP ep handle fp
      rest <- extractFilesWithLSP appEnv handle fps
      pure (ext : rest)

-- | Extract from a single file using LSP (standalone)
extractFromFile :: AppEnv -> FilePath -> IO Extraction
extractFromFile appEnv filePath = do
  let ep = extractionPort appEnv
      lp = loggingPort appEnv
      logTrace = lpLogTrace lp
      logDebug = lpLogDebug lp
      logWarn  = lpLogWarn lp
      ext = takeExtension filePath
  logTrace $ T.pack $ "  Extracting: " ++ filePath ++ " (extension: " ++ ext ++ ")"
  absRoot <- canonicalizePath "."
  mbLSPOpts <- epFindLSPServer ep ext
  case mbLSPOpts of
    Nothing -> do
      logDebug $ T.pack $ "  [stub] " ++ filePath ++ " - no LSP for " ++ ext
      pure (extractionFromLists [makeStubNode filePath] [])
    Just (cmd, args) -> do
      logDebug $ T.pack $ "  [lsp] " ++ filePath ++ " → " ++ cmd ++ " " ++ unwords args
      result <- epConnectLSP ep cmd args absRoot
      case result of
        Left err -> do
          logWarn $ T.pack $ "  [lsp] Connection failed for " ++ filePath ++ ": " ++ T.unpack err
          pure (extractionFromLists [makeStubNode filePath] [])
        Right handle -> do
          extraction <- epExtractViaLSP ep handle filePath
          epDisconnectLSP ep handle
          let nNodes = Map.size (extractionNodes extraction)
              nEdges = Map.size (extractionEdges extraction)
          logDebug $ T.pack $ "  [lsp] " ++ filePath ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
          pure extraction