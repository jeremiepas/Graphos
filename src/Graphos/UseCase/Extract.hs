-- | Extraction orchestration (re-export hub) — parallel extraction for all files.
-- Routes each file to its configured extractor (LSP, tree-sitter, or stub)
-- based on the graphos.yaml config.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Extract
  ( extractAll
  , extractChangedFiles
  , extractFromFile
  , extractViaTreeSitterFFI
  , extractorForExt
  , pushExtractionStreaming
  ) where

import Control.Concurrent (newQSemN, waitQSemN, signalQSemN)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Exception (bracket_, catch, SomeException(..))
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.List as List (foldl')
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Text as T
import Foreign.Ptr (Ptr)
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension)

import Graphos.Domain.Types (PipelineConfig(..), Extraction(..), emptyExtraction, Detection(..), FileCategory(..), ExtractorMode(..), ExtractorConfig(..), ecMode, GraphosConfig(..), gcExtractors, NodeId, Node(..), Edge)
import Graphos.Domain.Types.Pipeline (Neo4jStreamingConfig(..))
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
import qualified Graphos.Infrastructure.Export.Neo4j as Neo4j
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

  -- Cap parallelism for large doc corpora to reduce peak memory.
  -- For 10k+ doc files, each holding ~10-50KB in memory, full 32-way
  -- parallelism can OOM. Use min(numThreads, 8) for docs.
  let docThreads = min 8 (max 1 numThreads)

  -- Run code extraction and doc extraction concurrently
  -- (docs don't depend on code extraction results)
  --
  -- Memory-efficient accumulators: use Maps for nodes (O(log n) per key)
  -- and DList-style accumulation for edges (O(1) append, single flatten at end).
  -- For a 12k-file codebase, the old list-append approach (edges ++) caused
  -- O(n²) allocation and GC pressure. DList pattern avoids this entirely.
  codeNodeMapRef <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  codeEdgeAccRef  <- newIORef id :: IO (IORef ([Edge] -> [Edge]))
  docNodeMapRef  <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  docEdgeAccRef   <- newIORef id :: IO (IORef ([Edge] -> [Edge]))

  let -- Merge a single file's extraction into the accumulator.
      -- Nodes: Map insertWith for O(log n) per key (dedup by id).
      -- Edges: DList-style prepend — O(1) append, deferred flatten.
      accumulateNodes :: IORef (Map.Map NodeId Node) -> [Node] -> IO ()
      accumulateNodes ref nodes = modifyIORef' ref $ \acc ->
        List.foldl' (\m n -> Map.insertWith (\_old new -> new) (nodeId n) n m) acc nodes

      accumulateEdges :: IORef ([Edge] -> [Edge]) -> [Edge] -> IO ()
      accumulateEdges ref edges = modifyIORef' ref $ \acc -> acc . (edges ++)

      accumulate :: IORef (Map.Map NodeId Node) -> IORef ([Edge] -> [Edge]) -> Extraction -> IO ()
      accumulate nodeRef edgeRef ext = do
        accumulateNodes nodeRef (extractionNodes ext)
        accumulateEdges edgeRef (extractionEdges ext)

  void $ concurrently
    -- Code extraction: merge each result into accumulator immediately
    (do
      -- Tree-sitter extraction (parallel when -j > 1)
      if numThreads <= 1
        then mapM_ (\fp -> do
          ext <- extractViaTreeSitterFFI env (grammarForFile config fp) fp
          pushExtractionStreaming config env ext
          accumulate codeNodeMapRef codeEdgeAccRef ext
          ) treeSitterFiles
        else do
          sem <- newQSemN numThreads
          mapM_ (\fp -> bracket_
            (waitQSemN sem 1)
            (signalQSemN sem 1)
            (do ext <- extractViaTreeSitterFFI env (grammarForFile config fp) fp
                pushExtractionStreaming config env ext
                accumulate codeNodeMapRef codeEdgeAccRef ext
            )) treeSitterFiles

      -- LSP extraction
      let fileGroups = groupByLSPServer lspFiles
          numGroups = length fileGroups
      logInfo env $ T.pack $ "  LSP server groups: " ++ show numGroups ++ " (threads: " ++ show numThreads ++ ")"
      if numThreads <= 1
        then mapM_ (\grp -> do
          exts <- extractGroup env absRoot config grp
          mapM_ (\ext -> pushExtractionStreaming config env ext >> accumulate codeNodeMapRef codeEdgeAccRef ext) exts
          ) fileGroups
        else if numGroups <= numThreads
          then do
            results <- mapConcurrently (extractGroup env absRoot config) fileGroups
            mapM_ (\ext -> pushExtractionStreaming config env ext >> accumulate codeNodeMapRef codeEdgeAccRef ext) (concat results)
          else do
            sem <- newQSemN numThreads
            results <- mapConcurrently (\grp -> bracket_
              (waitQSemN sem 1)
              (signalQSemN sem 1)
              (extractGroup env absRoot config grp)) fileGroups
            mapM_ (\ext -> pushExtractionStreaming config env ext >> accumulate codeNodeMapRef codeEdgeAccRef ext) (concat results)

      -- Stub extraction
      mapM_ (\fp -> do
        logDebug env $ T.pack $ "  [stub] " ++ fp
        let ext = emptyExtraction { extractionNodes = [makeStubNode fp] }
        pushExtractionStreaming config env ext
        accumulate codeNodeMapRef codeEdgeAccRef ext
        ) stubFiles
    )
    -- Doc extraction: merge each result into accumulator immediately
    (do
      logDebug env $ T.pack $ "  [doc] Starting extraction for " ++ show (length docFiles) ++ " doc files (threads: " ++ show docThreads ++ ")"
      if docThreads <= 1
        then mapM_ (\fp -> do
          ext <- extractDocFile env fp
          pushExtractionStreaming config env ext
          accumulate docNodeMapRef docEdgeAccRef ext
          ) docFiles
        else do
          sem <- newQSemN docThreads
          let chunks = chunkList 500 docFiles
          mapM_ (\chunk -> do
            results <- mapConcurrently (\fp -> bracket_
              (waitQSemN sem 1)
              (signalQSemN sem 1)
              (extractDocFile env fp)) chunk
            mapM_ (\ext -> pushExtractionStreaming config env ext >> accumulate docNodeMapRef docEdgeAccRef ext) results
            ) chunks
      logDebug env "  [doc] Extraction complete"
    )

  logDebug env "  [extract] Code + doc extraction complete"

  -- Build final Extraction from Map accumulators + DList flattening
  -- DList flatten is O(n) — just chains the appends without thunk nesting.
  codeNodeMap <- readIORef codeNodeMapRef
  codeEdgeAcc <- readIORef codeEdgeAccRef
  docNodeMap <- readIORef docNodeMapRef
  docEdgeAcc <- readIORef docEdgeAccRef
  let mergedNodeMap = codeNodeMap `Map.union` docNodeMap  -- code wins on dupes
      mergedEdgeList = codeEdgeAcc (docEdgeAcc [])  -- flatten DList: O(n)
      merged = emptyExtraction
        { extractionNodes = Map.elems mergedNodeMap
        , extractionEdges = mergedEdgeList
        }

  logInfo env $ T.pack $ "  Extracted " ++ show (length (extractionNodes merged)) ++ " nodes, " ++ show (length (extractionEdges merged)) ++ " edges"
  pure merged

-- | Push a single extraction to Neo4j if streaming is configured.
-- This is a no-op when --neo4j-stream is not set.
pushExtractionStreaming :: PipelineConfig -> LogEnv -> Extraction -> IO ()
pushExtractionStreaming config env extraction =
  case cfgNeo4jStreaming config of
    Nothing -> pure ()
    Just n4cfg -> do
      let nNodes = length (extractionNodes extraction)
          nEdges = length (extractionEdges extraction)
      when (nNodes > 0 || nEdges > 0) $ do
        logDebug env $ T.pack $ "  [neo4j-stream] Pushing " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
        (msg, stmts, _batches) <- Neo4j.pushFileExtraction extraction
          (neo4jsUri n4cfg) (neo4jsUser n4cfg) (neo4jsPassword n4cfg)
        logTrace env $ T.pack $ "  [neo4j-stream] " ++ T.unpack msg ++ " (" ++ show stmts ++ " statements)"

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
extractGroup :: LogEnv -> FilePath -> PipelineConfig -> FileGroup -> IO [Extraction]
extractGroup env absRoot _config (serverCmd, files) =
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

-- | Split a list into chunks of given size.
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)

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

-- ───────────────────────────────────────────────
-- Incremental extraction for watch mode
-- ───────────────────────────────────────────────

-- | Extract only a list of changed files (for --watch mode).
--
-- Routes each changed file to its extractor and pushes to Neo4j if streaming is configured.
-- Returns the merged extraction of all changed files.
extractChangedFiles :: PipelineConfig -> [FilePath] -> LogEnv -> IO Extraction
extractChangedFiles config changedFiles env = do
  absRoot <- canonicalizePath (cfgInputPath config)
  let (tsFiles, lspFiles, stubFiles) = partitionByExtractor config changedFiles

  -- Tree-sitter files
  tsExtractions <- mapM (\fp -> extractViaTreeSitterFFI env (grammarForFile config fp) fp) tsFiles
  mapM_ (\ext -> pushExtractionStreaming config env ext) tsExtractions

  -- LSP files (grouped by server)
  let fileGroups = groupByLSPServer lspFiles
  lspExtractions <- concatMapM (extractGroup env absRoot config) fileGroups
  mapM_ (\ext -> pushExtractionStreaming config env ext) lspExtractions

  -- Stub files
  stubExtractions <- mapM (\fp -> do
    logDebug env $ T.pack $ "  [stub] " ++ fp
    pure emptyExtraction { extractionNodes = [makeStubNode fp] }
    ) stubFiles
  mapM_ (\ext -> pushExtractionStreaming config env ext) stubExtractions

  let merged = List.foldl' mergeExtractions emptyExtraction
                 (tsExtractions ++ lspExtractions ++ stubExtractions)
  logInfo env $ T.pack $ "  [watch] Extracted " ++ show (length (extractionNodes merged)) ++ " nodes, " ++ show (length (extractionEdges merged)) ++ " edges from " ++ show (length changedFiles) ++ " changed files"
  pure merged