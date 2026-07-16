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
import Control.Exception (bracket_, catch, evaluate, SomeException(..))
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.List as List (foldl')
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Text as T
import Foreign.Ptr (Ptr)
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension, takeFileName)
import Data.Char (toLower)
import System.Mem (performGC)

import Graphos.Domain.Types (PipelineConfig(..), Extraction(..), emptyExtraction, extractionFromLists, Detection(..), FileCategory(..), ExtractorMode(..), ExtractorConfig(..), ecMode, GraphosConfig(..), gcExtractors, gcVision, VisionConfig(..), NodeId, Node(..), Edge(..), FileType(..))
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
import Graphos.UseCase.Extract.Office (extractOfficeFile)
import Graphos.UseCase.Extract.Image (extractImageFile, extractImageFromBytes)
import Graphos.Infrastructure.FileSystem.OfficeConvert
  ( docxExtractMediaPaths
  , pptxExtractMediaPaths
  , extractMediaFile
  )

-- | Extract entities from all detected files.
extractAll :: PipelineConfig -> Detection -> LogEnv -> IO Extraction
extractAll config detection env = do
  let codeFiles = Map.findWithDefault [] CodeFiles (detectionFiles detection)
      docFiles  = Map.findWithDefault [] DocFiles  (detectionFiles detection)
      officeFiles = Map.findWithDefault [] OfficeFiles (detectionFiles detection)
      imageFiles = Map.findWithDefault [] ImageFiles (detectionFiles detection)
      numThreads = max 1 (cfgThreads config)
      vCfg = gcVision (cfgGraphosConfig config)

  absRoot <- canonicalizePath (cfgInputPath config)

  logInfo env $ T.pack $ "  Processing " ++ show (length codeFiles) ++ " code files, " ++ show (length docFiles) ++ " doc files, " ++ show (length officeFiles) ++ " office files, " ++ show (length imageFiles) ++ " image files"

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
  officeNodeMapRef <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  officeEdgeAccRef  <- newIORef id :: IO (IORef ([Edge] -> [Edge]))
  imageNodeMapRef <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  imageEdgeAccRef  <- newIORef id :: IO (IORef ([Edge] -> [Edge]))

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
        accumulateNodes nodeRef (Map.elems (extractionNodes ext))
        accumulateEdges edgeRef (Map.elems (extractionEdges ext))

  -- Process office files alongside doc files
  let officeThreadCount = max 1 (min 4 numThreads)
  unless (null officeFiles) $
    logInfo env $ T.pack $ "  office: " ++ show (length officeFiles) ++ " files"
  unless (null imageFiles) $
    logInfo env $ T.pack $ "  image: " ++ show (length imageFiles) ++ " files" ++ (if vcEnabled vCfg then "" else " (vision disabled)")

  -- Image extraction: batch size from config, with GC between batches.
  -- When vision is disabled, each image gets a stub node (no LLM call).
  let imageBatchSize = max 1 (vcBatchSize vCfg)

  -- Collect embedded image paths from PPTX/DOCX office files.
  -- These are analyzed alongside standalone images.
  embeddedImages <- if not (null officeFiles) && vcEnabled vCfg
    then concat <$> mapM (\fp -> collectEmbeddedImages env fp) officeFiles
    else pure []

  unless (null embeddedImages) $
    logInfo env $ T.pack $ "  image: " ++ show (length embeddedImages) ++ " embedded images from office files"

  -- All image sources: standalone image files + embedded images from office docs
  let allImageSources = map StandaloneImage imageFiles ++ map (uncurry EmbeddedImage) embeddedImages

  void $ concurrently
    -- Code + office extraction (concurrently with doc + image extraction)
    (void $ concurrently
      -- Code extraction: merge each result into accumulator immediately
      (do
        -- Tree-sitter extraction: process in chunks with GC between batches
        -- to release intermediate Extraction values and reduce peak memory.
        let tsChunks = chunkList 500 treeSitterFiles
        mapM_ (\chunk -> do
          if numThreads <= 1
            then mapM_ (\fp -> do
              ext <- extractViaTreeSitterFFI env (grammarForFile config fp) fp
              pushExtractionStreaming config env ext
              accumulate codeNodeMapRef codeEdgeAccRef ext
              ) chunk
            else do
              sem <- newQSemN numThreads
              mapM_ (\fp -> bracket_
                (waitQSemN sem 1)
                (signalQSemN sem 1)
                (do ext <- extractViaTreeSitterFFI env (grammarForFile config fp) fp
                    pushExtractionStreaming config env ext
                    accumulate codeNodeMapRef codeEdgeAccRef ext
                )) chunk
          -- Force evaluation of accumulator and GC after each chunk
          -- so intermediate Extraction values can be reclaimed.
          n <- readIORef codeNodeMapRef >>= evaluate . Map.size
          _ <- evaluate n
          performGC
          ) tsChunks

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
        -- GC after LSP extraction to release connection buffers
        performGC

        -- Stub extraction
        mapM_ (\fp -> do
          logDebug env $ T.pack $ "  [stub] " ++ fp
          let ext = extractionFromLists [makeStubNode fp] []
          pushExtractionStreaming config env ext
          accumulate codeNodeMapRef codeEdgeAccRef ext
          ) stubFiles
      )
      -- Office extraction: process office files concurrently with code extraction
      (do
        unless (null officeFiles) $ do
          logDebug env $ T.pack $ "  [office] Starting extraction for " ++ show (length officeFiles) ++ " office files"
          if officeThreadCount <= 1
            then mapM_ (\fp -> do
              ext <- extractOfficeFile config env fp
              pushExtractionStreaming config env ext
              accumulate officeNodeMapRef officeEdgeAccRef ext
              ) officeFiles
            else do
              sem <- newQSemN officeThreadCount
              let chunks = chunkList 100 officeFiles
              mapM_ (\chunk -> do
                results <- mapConcurrently (\fp -> bracket_
                  (waitQSemN sem 1)
                  (signalQSemN sem 1)
                  (extractOfficeFile config env fp)) chunk
                mapM_ (\ext -> pushExtractionStreaming config env ext >> accumulate officeNodeMapRef officeEdgeAccRef ext) results
                n <- readIORef officeNodeMapRef >>= evaluate . Map.size
                _ <- evaluate n
                performGC
                ) chunks
          logDebug env "  [office] Extraction complete"
      )
    )
    -- Doc + Image extraction: run concurrently with code+office
    (void $ concurrently
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
              -- Force evaluation and GC after each chunk to release
              -- intermediate Extraction values.
              n <- readIORef docNodeMapRef >>= evaluate . Map.size
              _ <- evaluate n
              performGC
              ) chunks
        logDebug env "  [doc] Extraction complete"
      )
      -- Image extraction: batch processing with GC between batches
      -- Standalone images use extractImageFile; embedded images use extractImageFromBytes.
      -- When vision is disabled, extractImageFile produces stub nodes.
      (do
        unless (null allImageSources) $ do
          logDebug env $ T.pack $ "  [image] Starting extraction for " ++ show (length imageFiles) ++ " standalone + " ++ show (length embeddedImages) ++ " embedded images (batch: " ++ show imageBatchSize ++ ")"
          let imageChunks = chunkList imageBatchSize allImageSources
          mapM_ (\chunk -> do
            -- Process each image source in the batch
            results <- mapM (extractImageSource config env) chunk
            mapM_ (\ext -> pushExtractionStreaming config env ext >> accumulate imageNodeMapRef imageEdgeAccRef ext) results
            -- Force evaluation and GC between batches to release base64 data
            -- and LLM response buffers — this is critical for memory efficiency
            -- since each image analysis produces ~1-5MB of base64 data.
            n <- readIORef imageNodeMapRef >>= evaluate . Map.size
            _ <- evaluate n
            performGC
            ) imageChunks
          logDebug env "  [image] Extraction complete"
        unless (null allImageSources) $ do
          n <- readIORef imageNodeMapRef >>= evaluate . Map.size
          logInfo env $ T.pack $ "  [image] Produced " ++ show n ++ " image nodes"
      )
    )

  logDebug env "  [extract] Code + doc + office + image extraction complete"

  -- Build final Extraction from Map accumulators + DList flattening
  -- DList flatten is O(n) — just chains the appends without thunk nesting.
  codeNodeMap <- readIORef codeNodeMapRef
  codeEdgeAcc <- readIORef codeEdgeAccRef
  docNodeMap <- readIORef docNodeMapRef
  docEdgeAcc <- readIORef docEdgeAccRef
  officeNodeMap <- readIORef officeNodeMapRef
  officeEdgeAcc <- readIORef officeEdgeAccRef
  imageNodeMap <- readIORef imageNodeMapRef
  imageEdgeAcc <- readIORef imageEdgeAccRef
  let mergedNodeMap = codeNodeMap `Map.union` docNodeMap `Map.union` officeNodeMap `Map.union` imageNodeMap  -- code wins on dupes
      mergedEdgeList = codeEdgeAcc (docEdgeAcc (officeEdgeAcc (imageEdgeAcc [])))  -- flatten DList: O(n)
      merged = Extraction
        { extractionNodes = mergedNodeMap
        , extractionEdges = Map.fromList [(edgeId e, e) | e <- mergedEdgeList]
        }

  logInfo env $ T.pack $ "  Extracted " ++ show (Map.size (extractionNodes merged)) ++ " nodes, " ++ show (Map.size (extractionEdges merged)) ++ " edges"
  pure merged

-- | Push a single extraction to Neo4j if streaming is configured.
-- This is a no-op when --neo4j-stream is not set.
pushExtractionStreaming :: PipelineConfig -> LogEnv -> Extraction -> IO ()
pushExtractionStreaming config env extraction =
  case cfgNeo4jStreaming config of
    Nothing -> pure ()
    Just n4cfg -> do
      let nNodes = Map.size (extractionNodes extraction)
          nEdges = Map.size (extractionEdges extraction)
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
      pure (extractionFromLists [makeStubNode fp] [])
    ) files
    else doExtractWithSharedLSP env absRoot serverCmd files

-- | Connect to an LSP server once and extract all files for it.
doExtractWithSharedLSP :: LogEnv -> FilePath -> String -> [FilePath] -> IO [Extraction]
doExtractWithSharedLSP env absRoot serverCmd files = do
  mbLSPOpts <- findLSPServer (takeExtension (case files of (f:_) -> f; [] -> ""))
  case mbLSPOpts of
    Nothing -> mapM (\fp -> do
      logWarn env $ T.pack $ "  LSP " ++ serverCmd ++ " disappeared for " ++ fp
      pure (extractionFromLists [makeStubNode fp] [])
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
          mapM (\fp -> pure (extractionFromLists [makeStubNode fp] [])) files
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
            let nNodes = Map.size (extractionNodes ext)
                nEdges = Map.size (extractionEdges ext)
            logDebug env $ T.pack $ "  [lsp] " ++ fp ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
            ) (zip files extractions)
          enriched <- mapM (\(fp, ext) ->
            if Map.null (extractionNodes ext) && takeExtension fp `elem` [".hs", ".lhs"]
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
      let stubs = [extractionFromLists [makeStubNode f] [] | f <- fp:fps]
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
      pure (extractionFromLists [makeStubNode filePath] [])
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
          pure (extractionFromLists [makeStubNode filePath] [])
        Right client -> do
          extraction <- extractViaLSP client filePath
          disconnectLSP client
          let nNodes = Map.size (extractionNodes extraction)
              nEdges = Map.size (extractionEdges extraction)
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
-- Image extraction helpers
-- ───────────────────────────────────────────────

-- | An image source: either a standalone file path or an embedded image
-- from an office document (archive path, media path within archive).
data ImageSource
  = StandaloneImage FilePath
  | EmbeddedImage FilePath FilePath  -- ^ (archive path, media path within archive)
  deriving (Eq, Show)

-- | Extract an image from either a standalone file or embedded source.
-- Standalone images use extractImageFile; embedded images use extractImageFromBytes.
extractImageSource :: PipelineConfig -> LogEnv -> ImageSource -> IO Extraction
extractImageSource config env (StandaloneImage fp) =
  extractImageFile config env fp
extractImageSource config env (EmbeddedImage archivePath mediaPath) = do
  -- Extract the embedded image bytes from the office archive
  mediaResult <- extractMediaFile archivePath mediaPath
  case mediaResult of
    Left err -> do
      logWarn env $ T.pack $ "  [vision] Error extracting media " ++ mediaPath ++ " from " ++ archivePath ++ ": " ++ T.unpack err
      pure (extractionFromLists [imageStubNode mediaPath] [])
    Right bytes -> do
      let displayName = archivePath ++ "/" ++ takeFileName mediaPath
      extractImageFromBytes config env displayName bytes
  where
    imageStubNode :: FilePath -> Node
    imageStubNode fp = Node
      { nodeId = T.pack fp
      , nodeLabel = T.pack (takeFileName fp)
      , nodeFileType = ImageFile
      , nodeSourceFile = T.pack fp
      , nodeLineStart = Nothing
      , nodeLineEnd = Nothing
      , nodeSignature = Nothing
      , nodeCommunityId = Nothing
      , nodeKind = Just "Image"
      , nodeDegree = Nothing
      , nodeIsBridge = Nothing
      , nodeExtra = Nothing
      , nodeSourceLocation = Nothing
      , nodeSourceUrl = Nothing
      , nodeCapturedAt = Nothing
      , nodeAuthor = Nothing
      , nodeContributor = Nothing
      }

-- | Collect embedded image paths from PPTX and DOCX office files.
-- Returns a list of (archivePath, mediaPath) pairs for each embedded image.
collectEmbeddedImages :: LogEnv -> FilePath -> IO [(FilePath, FilePath)]
collectEmbeddedImages _env fp = do
  let ext = map toLower (takeExtension fp)
  case ext of
    ".docx" -> do
      paths <- docxExtractMediaPaths fp
      pure [(fp, p) | p <- paths]
    ".pptx" -> do
      paths <- pptxExtractMediaPaths fp
      pure [(fp, p) | p <- paths]
    _ -> pure []

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
      pure (extractionFromLists [makeStubNode filePath] [])
    Just lang -> catch (do
      content <- BS.readFile filePath
      result <- parseWithGrammar lang content
      case result of
        Nothing -> do
          logWarn env $ T.pack $ "  [tree-sitter] Parse failed for " ++ filePath
          pure (extractionFromLists [makeStubNode filePath] [])
        Just nodes -> do
          let extraction = tsNodesToExtraction filePath nodes
              nNodes = Map.size (extractionNodes extraction)
              nEdges = Map.size (extractionEdges extraction)
          logDebug env $ T.pack $ "  [tree-sitter] " ++ filePath ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
          pure extraction
      ) $ \(e :: SomeException) -> do
        logWarn env $ T.pack $ "  [tree-sitter] Error for " ++ filePath ++ ": " ++ show e
        pure (extractionFromLists [makeStubNode filePath] [])

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
  in extractionFromLists nodes edges

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
    pure (extractionFromLists [makeStubNode fp] [])
    ) stubFiles
  mapM_ (\ext -> pushExtractionStreaming config env ext) stubExtractions

  let merged = List.foldl' mergeExtractions emptyExtraction
                 (tsExtractions ++ lspExtractions ++ stubExtractions)
  logInfo env $ T.pack $ "  [watch] Extracted " ++ show (Map.size (extractionNodes merged)) ++ " nodes, " ++ show (Map.size (extractionEdges merged)) ++ " edges from " ++ show (length changedFiles) ++ " changed files"
  pure merged