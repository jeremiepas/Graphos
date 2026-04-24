-- | Extraction orchestration (re-export hub) — parallel extraction for all files.
-- Routes each file to its configured extractor (LSP, tree-sitter, or stub)
-- based on the graphos.yaml config.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Extract
  ( extractAll
  , extractFromFile
  , extractViaTreeSitterFFI
  , extractorForExt
  ) where

import Control.Concurrent (newQSemN, waitQSemN, signalQSemN)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Exception (bracket_, catch, SomeException(..))
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Foreign.Ptr (Ptr)
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension)

import Graphos.Domain.Types (PipelineConfig(..), Extraction(..), emptyExtraction, Detection(..), FileCategory(..), ExtractorMode(..), ExtractorConfig(..), ecMode, GraphosConfig(..), gcExtractors)
import Graphos.Domain.Graph (mergeExtractions)
import Graphos.Infrastructure.LSP.Client (LSPClient(..), extractViaLSP, findLSPServer, LSPClientConfig(..), connectToLSP, disconnectLSP, languageServerCommands, extractWorkspaceSymbols, workspaceSymbolsToDocumentSymbols, symbolToNodes, symbolTreeToEdges, isServerConnected)
import Graphos.Infrastructure.LSP.Protocol (scpWorkspaceSymbolProvider, DocumentSymbolResult(..))
import Graphos.Infrastructure.Extract.TreeSitter.Core (parseWithGrammar)
import Graphos.Infrastructure.Extract.TreeSitter.Convert (tsNodesToExtraction)
import qualified TreeSitter.TypeScript as TSTypeScript
import qualified TreeSitter.Python as TSPython
import qualified TreeSitter.JSON as TSJSON
import qualified TreeSitter.Go as TSGo
import qualified TreeSitter.Rust as TSRust
import qualified TreeSitter.Haskell as TSHaskell
import qualified TreeSitter.Language as TS_LANG
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

  -- Split code files by extractor mode
  let (treeSitterFiles, lspFiles, stubFiles) = partitionByExtractor config codeFiles

  -- Log extractor routing
  unless (null treeSitterFiles) $
    logInfo env $ T.pack $ "  tree-sitter: " ++ show (length treeSitterFiles) ++ " files"
  unless (null lspFiles) $
    logInfo env $ T.pack $ "  LSP: " ++ show (length lspFiles) ++ " files"
  unless (null stubFiles) $
    logDebug env $ T.pack $ "  stub: " ++ show (length stubFiles) ++ " files"

  -- Run code extraction and doc extraction concurrently
  -- (docs don't depend on code extraction results)
  let codeAction :: IO [Extraction]
      codeAction = do
        -- Tree-sitter extraction (parallel when -j > 1)
        tsExtractions <- if numThreads <= 1
          then mapM (\fp -> extractViaTreeSitterFFI env (grammarForFile config fp) fp) treeSitterFiles
          else do
            sem <- newQSemN numThreads
            mapConcurrently (\fp -> bracket_
              (waitQSemN sem 1)
              (signalQSemN sem 1)
              (extractViaTreeSitterFFI env (grammarForFile config fp) fp)) treeSitterFiles

        -- LSP extraction
        let fileGroups = groupByLSPServer lspFiles
            numGroups = length fileGroups
        logInfo env $ T.pack $ "  LSP server groups: " ++ show numGroups ++ " (threads: " ++ show numThreads ++ ")"
        lspExtractions <- if numThreads <= 1
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

        -- Stub extraction
        stubExtractions <- mapM (\fp -> do
          logDebug env $ T.pack $ "  [stub] " ++ fp
          pure emptyExtraction { extractionNodes = [makeStubNode fp] }
          ) stubFiles

        logDebug env $ T.pack $ "  [code] ts=" ++ show (length tsExtractions) ++ " lsp=" ++ show (length lspExtractions) ++ " stub=" ++ show (length stubExtractions)
        pure (tsExtractions ++ lspExtractions ++ stubExtractions)

      docAction :: IO [Extraction]
      docAction = do
        logDebug env $ T.pack $ "  [doc] Starting extraction for " ++ show (length docFiles) ++ " doc files"
        docSem <- newQSemN 32
        results <- mapConcurrently (\fp -> bracket_
          (waitQSemN docSem 1)
          (signalQSemN docSem 1)
          (extractDocFile env fp)) docFiles
        logDebug env $ T.pack $ "  [doc] Extraction complete: " ++ show (length results) ++ " results"
        pure results

  -- Run both actions concurrently and merge results
  logDebug env "  [extract] Waiting for code + doc extraction to complete..."
  (codeResults, docExtractions) <- concurrently codeAction docAction
  logDebug env "  [extract] Code + doc extraction complete, merging..."

  let codeExtraction = foldr mergeExtractions emptyExtraction codeResults
      docExtraction  = foldr mergeExtractions emptyExtraction docExtractions
      merged         = foldr mergeExtractions docExtraction [codeExtraction]

  logInfo env $ T.pack $ "  Extracted " ++ show (length (extractionNodes merged)) ++ " nodes, " ++ show (length (extractionEdges merged)) ++ " edges"
  pure merged

-- | Partition code files by their configured extractor mode.
partitionByExtractor :: PipelineConfig -> [FilePath] -> ([FilePath], [FilePath], [FilePath])
partitionByExtractor config files = foldr go ([], [], []) files
  where
    go fp (ts, lsp, stub) = case extractorForExt config (takeExtension fp) of
      ExtractTreeSitter -> (fp:ts, lsp, stub)
      ExtractLSP       -> (ts, fp:lsp, stub)
      ExtractStub      -> (ts, lsp, fp:stub)

-- | Get the tree-sitter grammar name for a file from config.
grammarForFile :: PipelineConfig -> FilePath -> String
grammarForFile config fp =
  case Map.lookup (takeExtension fp) (gcExtractors (cfgGraphosConfig config)) of
    Just ec -> case ecGrammar ec of
      Just g  -> g
      Nothing -> drop 1 (takeExtension fp)  -- fallback: use extension without dot
    Nothing -> drop 1 (takeExtension fp)

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
                    extractFilesWithLSP client files
                Left err -> do
                  logWarn env $ T.pack $ "  [lsp] workspace/symbol failed: " ++ T.unpack err ++ " — falling back to per-file extraction"
                  extractFilesWithLSP client files
            else do
              logDebug env $ T.pack $ "  [lsp] Server does not support workspace/symbol — using per-file extraction"
              extractFilesWithLSP client files
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

-- | Extract files via LSP, short-circuiting when the server dies.
-- Once the server disconnects, remaining files get stubs immediately
-- instead of repeatedly hitting the dead connection.
extractFilesWithLSP :: LSPClient -> [FilePath] -> IO [Extraction]
extractFilesWithLSP _ [] = pure []
extractFilesWithLSP client (fp:fps) = do
  alive <- isServerConnected client
  if not alive
    then do
      -- Server is dead — return stubs for this and all remaining files
      let stubs = [emptyExtraction { extractionNodes = [makeStubNode f], extractionEdges = [] } | f <- fp:fps]
      pure stubs
    else do
      ext <- extractViaLSP client fp
      rest <- extractFilesWithLSP client fps
      pure (ext : rest)

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

-- ───────────────────────────────────────────────
-- Tree-sitter extraction
-- ───────────────────────────────────────────────

-- | Extract from a single file using tree-sitter FFI bindings.
-- For "markdown" grammar, delegates to the built-in Markdown parser
-- (no tree-sitter-markdown C grammar available on Hackage).
extractViaTreeSitterFFI :: LogEnv -> String -> FilePath -> IO Extraction
extractViaTreeSitterFFI env "markdown" filePath = extractDocFile env filePath
extractViaTreeSitterFFI env grammar filePath =
  case getGrammarPtr grammar of
    Nothing -> do
      logWarn env $ T.pack $ "  [tree-sitter] No grammar for " ++ grammar ++ " — using stub"
      pure emptyExtraction { extractionNodes = [makeStubNode filePath] }
    Just lang -> catch (do
      content <- BS.readFile filePath
      result <- parseWithGrammar lang content
      case result of
        Nothing -> do
          logWarn env $ T.pack $ "  [tree-sitter] Parse failed for " ++ filePath
          pure emptyExtraction { extractionNodes = [makeStubNode filePath] }
        Just nodes -> do
          let extraction = tsNodesToExtraction filePath nodes
              nNodes = length (extractionNodes extraction)
              nEdges = length (extractionEdges extraction)
          logDebug env $ T.pack $ "  [tree-sitter] " ++ filePath ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
          pure extraction
      ) $ \(e :: SomeException) -> do
        logWarn env $ T.pack $ "  [tree-sitter] Error for " ++ filePath ++ ": " ++ show e
        pure emptyExtraction { extractionNodes = [makeStubNode filePath] }

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

-- ───────────────────────────────────────────────
-- Extractor routing
-- ───────────────────────────────────────────────

-- | Get the extractor mode for a file extension from the config.
extractorForExt :: PipelineConfig -> String -> ExtractorMode
extractorForExt config ext =
  case Map.lookup ext (gcExtractors (cfgGraphosConfig config)) of
    Just ec -> ecMode ec
    Nothing -> ExtractStub  -- unknown extensions get stubs, not LSP

-- | Build an Extraction from a file's DocumentSymbolResults.
extractionFromSymbols :: FilePath -> [DocumentSymbolResult] -> Extraction
extractionFromSymbols filePath symbols =
  let nodes = symbolToNodes filePath symbols
      edges = symbolTreeToEdges filePath symbols
  in emptyExtraction
    { extractionNodes = nodes
    , extractionEdges = edges
    }