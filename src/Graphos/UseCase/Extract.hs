-- | Extraction orchestration (re-export hub) — parallel LSP extraction for all files.
-- Delegates to Haskell, Markdown, and LSP sub-modules.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Extract
  ( extractAll
  , extractFromFile
  ) where

import Control.Concurrent (newQSemN, waitQSemN, signalQSemN)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket_)
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension)

import Graphos.Domain.Types
import Graphos.Domain.Graph (mergeExtractions)
import Graphos.Infrastructure.LSP.Client (LSPClient(..), extractViaLSP, findLSPServer, LSPClientConfig(..), connectToLSP, disconnectLSP, languageServerCommands, extractWorkspaceSymbols, workspaceSymbolsToDocumentSymbols, symbolToNodes, symbolTreeToEdges)
import Graphos.Infrastructure.LSP.Protocol (scpWorkspaceSymbolProvider, DocumentSymbolResult(..))
import Graphos.Infrastructure.Logging (LogEnv, logInfo, logDebug, logTrace, logWarn)
import Graphos.UseCase.Extract.Haskell (makeStubNode, extractHaskellStub)
import Graphos.UseCase.Extract.Markdown (extractDocFile)

-- | Extract entities from all detected files.
extractAll :: PipelineConfig -> Detection -> LogEnv -> IO Extraction
extractAll config detection env = do
  let codeFiles = Map.findWithDefault [] CodeFiles (detectionFiles detection)
      docFiles  = Map.findWithDefault [] DocFiles  (detectionFiles detection)
      numThreads = max 1 (cfgThreads config)

  absRoot <- canonicalizePath (cfgInputPath config)

  logInfo env $ T.pack $ "  Processing " ++ show (length codeFiles) ++ " code files, " ++ show (length docFiles) ++ " doc files"

  let exts = nub (sort (map takeExtension codeFiles))
  logDebug env $ T.pack $ "  File extensions: " ++ show exts

  mapM_ (\ext -> do
    mbLSP <- findLSPServer ext
    case mbLSP of
      Just (cmd, args) -> logDebug env $ T.pack $ "  LSP for " ++ ext ++ ": " ++ cmd ++ " " ++ unwords args
      Nothing          -> logWarn env $ T.pack $ "  No LSP for " ++ ext ++ " - using stub extraction"
    ) exts

  let fileGroups = groupByLSPServer codeFiles
      numGroups = length fileGroups

  logInfo env $ T.pack $ "  LSP server groups: " ++ show numGroups ++ " (threads: " ++ show numThreads ++ ")"

  codeExtractions <- if numThreads <= 1
    then concatMapM (extractGroup env absRoot) fileGroups
    else if numGroups <= numThreads
      then do
        results <- mapConcurrently (extractGroup env absRoot) fileGroups
        pure (concat results)
      else do
        sem <- newQSemN numThreads
        results <- mapConcurrently (\grp -> bracket_
          (waitQSemN sem 1)
          (signalQSemN sem 1)
          (extractGroup env absRoot grp)) fileGroups
        pure (concat results)

  docExtractions <- mapM (extractDocFile env) docFiles
  let docExtraction = foldr mergeExtractions emptyExtraction docExtractions

  let merged = foldr mergeExtractions docExtraction codeExtractions
  logInfo env $ T.pack $ "  Extracted " ++ show (length (extractionNodes merged)) ++ " nodes, " ++ show (length (extractionEdges merged)) ++ " edges"
  pure merged

-- | A group of files sharing the same LSP server command
type FileGroup = (String, [FilePath])

-- | Group files by their LSP server command
groupByLSPServer :: [FilePath] -> [FileGroup]
groupByLSPServer files =
  let fileWithServer = [(serverCmd f, f) | f <- files]
      grouped = Map.toList $ Map.fromListWith (++) [(cmd, [fp]) | (cmd, fp) <- fileWithServer]
  in grouped
  where
    serverCmd fp = case Map.lookup (takeExtension fp) languageServerCommands of
      Just (cmd, _) -> cmd
      Nothing       -> "stub"

-- | Extract all files in a group using a single shared LSP connection
extractGroup :: LogEnv -> FilePath -> FileGroup -> IO [Extraction]
extractGroup env absRoot (serverCmd, files) =
  if serverCmd == "stub"
    then mapM (\fp -> do
      logDebug env $ T.pack $ "  [stub] " ++ fp
      pure emptyExtraction { extractionNodes = [makeStubNode fp] }
    ) files
    else doExtractWithSharedLSP env absRoot serverCmd files

-- | Connect to an LSP server once and extract all files for it.
doExtractWithSharedLSP :: LogEnv -> FilePath -> String -> [FilePath] -> IO [Extraction]
doExtractWithSharedLSP env absRoot serverCmd files = do
  mbLSPOpts <- findLSPServer (takeExtension (case files of (f:_) -> f; [] -> ""))
  case mbLSPOpts of
    Nothing -> mapM (\fp -> do
      logWarn env $ T.pack $ "  LSP " ++ serverCmd ++ " disappeared for " ++ fp
      pure emptyExtraction { extractionNodes = [makeStubNode fp] }
      ) files
    Just (cmd, args) -> do
      logDebug env $ T.pack $ "  [lsp] Connecting to " ++ cmd ++ " for " ++ show (length files) ++ " files"
      let config = LSPClientConfig
            { lspCommand = cmd
            , lspArgs    = args
            , lspRootUri = absRoot
            , lspTimeout  = 300
            }
      result <- connectToLSP config
      case result of
        Left err -> do
          logWarn env $ T.pack $ "  [lsp] Connection failed: " ++ T.unpack err
          mapM (\fp -> pure emptyExtraction { extractionNodes = [makeStubNode fp] }) files
        Right client -> do
          let hasWsSym = scpWorkspaceSymbolProvider (lspServerCaps client)
          extractions <- if hasWsSym
            then do
              logInfo env $ T.pack $ "  [lsp] Server supports workspace/symbol — using project-level extraction"
              wsResult <- extractWorkspaceSymbols client
              case wsResult of
                Right syms
                  | not (null syms) -> do
                    let fileSymbols = workspaceSymbolsToDocumentSymbols syms
                    logInfo env $ T.pack $ "  [lsp] workspace/symbol returned " ++ show (length syms) ++ " symbols across " ++ show (Map.size fileSymbols) ++ " files"
                    pure [extractionFromSymbols fp (Map.findWithDefault [] fp fileSymbols) | fp <- files]
                  | otherwise -> do
                    logDebug env $ T.pack $ "  [lsp] workspace/symbol returned empty — falling back to per-file extraction"
                    mapM (extractViaLSP client) files
                Left err -> do
                  logWarn env $ T.pack $ "  [lsp] workspace/symbol failed: " ++ T.unpack err ++ " — falling back to per-file extraction"
                  mapM (extractViaLSP client) files
            else do
              logDebug env $ T.pack $ "  [lsp] Server does not support workspace/symbol — using per-file extraction"
              mapM (extractViaLSP client) files
          mapM_ (\(fp, ext) -> do
            let nNodes = length (extractionNodes ext)
                nEdges = length (extractionEdges ext)
            logDebug env $ T.pack $ "  [lsp] " ++ fp ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
            ) (zip files extractions)
          enriched <- mapM (\(fp, ext) ->
            if null (extractionNodes ext) && takeExtension fp `elem` [".hs", ".lhs"]
              then do
                logDebug env $ T.pack $ "  [haskell-stub] LSP gave 0 symbols for " ++ fp ++ ", using parser fallback"
                extractHaskellStub fp
              else pure ext
            ) (zip files extractions)
          disconnectLSP client
          pure enriched

-- | Extract from a single file using LSP (standalone)
extractFromFile :: LogEnv -> FilePath -> IO Extraction
extractFromFile env filePath = do
  let ext = takeExtension filePath
  logTrace env $ T.pack $ "  Extracting: " ++ filePath ++ " (extension: " ++ ext ++ ")"
  absRoot <- canonicalizePath "."
  mbLSPOpts <- findLSPServer ext
  case mbLSPOpts of
    Nothing -> do
      logDebug env $ T.pack $ "  [stub] " ++ filePath ++ " - no LSP for " ++ ext
      pure emptyExtraction
           { extractionNodes = [makeStubNode filePath]
           , extractionEdges = []
           }
    Just (cmd, args) -> do
      logDebug env $ T.pack $ "  [lsp] " ++ filePath ++ " → " ++ cmd ++ " " ++ unwords args
      let config = LSPClientConfig
            { lspCommand = cmd
            , lspArgs    = args
            , lspRootUri = absRoot
            , lspTimeout  = 300
            }
      result <- connectToLSP config
      case result of
        Left err -> do
          logWarn env $ T.pack $ "  [lsp] Connection failed for " ++ filePath ++ ": " ++ T.unpack err
          pure emptyExtraction
              { extractionNodes = [makeStubNode filePath]
              }
        Right client -> do
          extraction <- extractViaLSP client filePath
          disconnectLSP client
          let nNodes = length (extractionNodes extraction)
              nEdges = length (extractionEdges extraction)
          logDebug env $ T.pack $ "  [lsp] " ++ filePath ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
          pure extraction

-- | Sequential concatMapM
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- | Build an Extraction from a file's DocumentSymbolResults.
extractionFromSymbols :: FilePath -> [DocumentSymbolResult] -> Extraction
extractionFromSymbols filePath symbols =
  let nodes = symbolToNodes filePath symbols
      edges = symbolTreeToEdges filePath symbols
  in emptyExtraction
    { extractionNodes = nodes
    , extractionEdges = edges
    }