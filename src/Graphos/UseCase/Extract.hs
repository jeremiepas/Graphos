-- | Extraction orchestration (re-export hub) — parallel extraction for all files.
-- Routes each file to its configured extractor (LSP, tree-sitter, or stub)
-- based on the graphos.yaml config.
--
-- CHANGED: Functions now take AppEnv instead of LogEnv/Infrastructure imports.
-- All Infrastructure calls go through UseCase.Port.* interfaces.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Extract
  ( extractAll
  , extractChangedFiles
  , extractFromFile
  , extractViaTreeSitterFFI
  , extractorForExt
  , resolveGranularity
  , granularityForFile
  , pushExtractionStreaming
  ) where

import Control.Concurrent (newQSemN, waitQSemN, signalQSemN)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Exception (bracket_, evaluate)
import Control.Monad (unless, void)
import qualified Data.ByteString as BS
import qualified Data.List as List (foldl')
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension, takeFileName)
import Data.Char (toLower)
import System.Mem (performGC)

import Graphos.Domain.Types (PipelineConfig(..), Extraction(..), emptyExtraction, extractionFromLists, Detection(..), FileCategory(..), ExtractorMode(..), ExtractorConfig(..), ecMode, GraphosConfig(..), gcExtractors, gcVision, Granularity(..), VisionConfig(..), NodeId, Node(..), Edge(..), FileType(..))
import Graphos.Domain.Graph (mergeExtractions)
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.ExtractionPort (ExtractionPort(..), LSPHandle(..), SymbolResult(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.UseCase.Extract.Haskell (makeStubNode)

-- | Extract entities from all detected files.
extractAll :: AppEnv -> PipelineConfig -> Detection -> IO Extraction
extractAll appEnv config detection = do
  let ep = extractionPort appEnv
      lp = loggingPort appEnv
      logInfo  = lpLogInfo lp
      logDebug = lpLogDebug lp

  let codeFiles = Map.findWithDefault [] CodeFiles (detectionFiles detection)
      docFiles  = Map.findWithDefault [] DocFiles  (detectionFiles detection)
      officeFiles = Map.findWithDefault [] OfficeFiles (detectionFiles detection)
      imageFiles = Map.findWithDefault [] ImageFiles (detectionFiles detection)
      paperFiles = Map.findWithDefault [] PaperFiles (detectionFiles detection)
      numThreads = max 1 (cfgThreads config)
      vCfg = gcVision (cfgGraphosConfig config)

  absRoot <- canonicalizePath (cfgInputPath config)

  logInfo $ T.pack $ "  Processing " ++ show (length codeFiles) ++ " code files, " ++ show (length docFiles) ++ " doc files, " ++ show (length officeFiles) ++ " office files, " ++ show (length imageFiles) ++ " image files, " ++ show (length paperFiles) ++ " paper files"
  logInfo $ T.pack $ "  Granularity: " ++ granularityName (resolveGranularity (cfgGranularity config) (cfgGraphosConfig config) "") ++ case cfgGranularity config of
    Just _  -> " (CLI override)"
    Nothing -> ""

  -- Split code files by extractor mode
  let (treeSitterFiles, lspFiles, stubFiles) = partitionByExtractor config codeFiles

  -- Log extractor routing
  unless (null treeSitterFiles) $
    logInfo $ T.pack $ "  tree-sitter: " ++ show (length treeSitterFiles) ++ " files"
  unless (null lspFiles) $
    logInfo $ T.pack $ "  LSP: " ++ show (length lspFiles) ++ " files"
  unless (null stubFiles) $
    logDebug $ T.pack $ "  stub: " ++ show (length stubFiles) ++ " files"

  -- Cap parallelism for large doc corpora to reduce peak memory.
  let docThreads = min 8 (max 1 numThreads)

  -- Memory-efficient accumulators
  codeNodeMapRef <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  codeEdgeAccRef  <- newIORef id :: IO (IORef ([Edge] -> [Edge]))
  docNodeMapRef  <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  docEdgeAccRef   <- newIORef id :: IO (IORef ([Edge] -> [Edge]))
  officeNodeMapRef <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  officeEdgeAccRef  <- newIORef id :: IO (IORef ([Edge] -> [Edge]))
  imageNodeMapRef <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  imageEdgeAccRef  <- newIORef id :: IO (IORef ([Edge] -> [Edge]))
  paperNodeMapRef <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  paperEdgeAccRef  <- newIORef id :: IO (IORef ([Edge] -> [Edge]))

  let -- Merge a single file's extraction into the accumulator.
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
    logInfo $ T.pack $ "  office: " ++ show (length officeFiles) ++ " files"
  unless (null imageFiles) $
    logInfo $ T.pack $ "  image: " ++ show (length imageFiles) ++ " files" ++ (if vcEnabled vCfg then "" else " (vision disabled)")

  let imageBatchSize = max 1 (vcBatchSize vCfg)

  -- Collect embedded image paths from PPTX/DOCX office files via port
  embeddedImagesList <- if not (null officeFiles) && vcEnabled vCfg
    then concat <$> mapM (collectEmbeddedImages ep) officeFiles
    else pure []

  unless (null embeddedImagesList) $
    logInfo $ T.pack $ "  image: " ++ show (length embeddedImagesList) ++ " embedded images from office files"

  let allImageSources = map StandaloneImage imageFiles ++ map (uncurry EmbeddedImage) embeddedImagesList

  void $ concurrently
    (void $ concurrently
      (do
        -- Tree-sitter extraction
        let tsChunks = chunkList 500 treeSitterFiles
        mapM_ (\chunk -> do
          if numThreads <= 1
            then mapM_ (\fp -> do
              ext <- extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarForFile config fp) fp
              epPushExtractionStreaming ep config ext
              accumulate codeNodeMapRef codeEdgeAccRef ext
              ) chunk
            else do
              sem <- newQSemN numThreads
              mapM_ (\fp -> bracket_
                (waitQSemN sem 1)
                (signalQSemN sem 1)
                (do ext <- extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarForFile config fp) fp
                    epPushExtractionStreaming ep config ext
                    accumulate codeNodeMapRef codeEdgeAccRef ext
                )) chunk
          n <- readIORef codeNodeMapRef >>= evaluate . Map.size
          _ <- evaluate n
          performGC
          ) tsChunks

        -- LSP extraction
        let fileGroups = groupByLSPServer (epLanguageServerCommands ep) lspFiles
            numGroups = length fileGroups
        logInfo $ T.pack $ "  LSP server groups: " ++ show numGroups ++ " (threads: " ++ show numThreads ++ ")"
        if numThreads <= 1
          then mapM_ (\grp -> do
            exts <- extractGroup appEnv absRoot config grp
            mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate codeNodeMapRef codeEdgeAccRef ext) exts
            ) fileGroups
          else if numGroups <= numThreads
            then do
              results <- mapConcurrently (extractGroup appEnv absRoot config) fileGroups
              mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate codeNodeMapRef codeEdgeAccRef ext) (concat results)
            else do
              sem <- newQSemN numThreads
              results <- mapConcurrently (\grp -> bracket_
                (waitQSemN sem 1)
                (signalQSemN sem 1)
                (extractGroup appEnv absRoot config grp)) fileGroups
              mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate codeNodeMapRef codeEdgeAccRef ext) (concat results)
        performGC

        -- Stub extraction
        mapM_ (\fp -> do
          logDebug $ T.pack $ "  [stub] " ++ fp
          let ext = extractionFromLists [makeStubNode fp] []
          epPushExtractionStreaming ep config ext
          accumulate codeNodeMapRef codeEdgeAccRef ext
          ) stubFiles
      )
      -- Office extraction
      (do
        unless (null officeFiles) $ do
          logDebug $ T.pack $ "  [office] Starting extraction for " ++ show (length officeFiles) ++ " office files"
          if officeThreadCount <= 1
            then mapM_ (\fp -> do
              ext <- epExtractOfficeFile ep config fp
              epPushExtractionStreaming ep config ext
              accumulate officeNodeMapRef officeEdgeAccRef ext
              ) officeFiles
            else do
              sem <- newQSemN officeThreadCount
              let chunks = chunkList 100 officeFiles
              mapM_ (\chunk -> do
                results <- mapConcurrently (\fp -> bracket_
                  (waitQSemN sem 1)
                  (signalQSemN sem 1)
                  (epExtractOfficeFile ep config fp)) chunk
                mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate officeNodeMapRef officeEdgeAccRef ext) results
                n <- readIORef officeNodeMapRef >>= evaluate . Map.size
                _ <- evaluate n
                performGC
                ) chunks
          logDebug "  [office] Extraction complete"
       )
     )
     (void $ concurrently
       -- Doc extraction
       (do
         logDebug $ T.pack $ "  [doc] Starting extraction for " ++ show (length docFiles) ++ " doc files (threads: " ++ show docThreads ++ ")"
         if docThreads <= 1
           then mapM_ (\fp -> do
             ext <- epExtractDocFile ep fp
             epPushExtractionStreaming ep config ext
             accumulate docNodeMapRef docEdgeAccRef ext
             ) docFiles
           else do
             sem <- newQSemN docThreads
             let chunks = chunkList 500 docFiles
             mapM_ (\chunk -> do
               results <- mapConcurrently (\fp -> bracket_
                 (waitQSemN sem 1)
                 (signalQSemN sem 1)
                 (epExtractDocFile ep fp)) chunk
               mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate docNodeMapRef docEdgeAccRef ext) results
               n <- readIORef docNodeMapRef >>= evaluate . Map.size
               _ <- evaluate n
               performGC
               ) chunks
         logDebug "  [doc] Extraction complete"
       )
       (void $ concurrently
         -- Image extraction
         (do
           unless (null allImageSources) $ do
             logDebug $ T.pack $ "  [image] Starting extraction for " ++ show (length imageFiles) ++ " standalone + " ++ show (length embeddedImagesList) ++ " embedded images (batch: " ++ show imageBatchSize ++ ")"
             let imageChunks = chunkList imageBatchSize allImageSources
             mapM_ (\chunk -> do
               results <- mapM (extractImageSource appEnv config) chunk
               mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate imageNodeMapRef imageEdgeAccRef ext) results
               n <- readIORef imageNodeMapRef >>= evaluate . Map.size
               _ <- evaluate n
               performGC
               ) imageChunks
             logDebug "  [image] Extraction complete"
           unless (null allImageSources) $ do
             n <- readIORef imageNodeMapRef >>= evaluate . Map.size
             logInfo $ T.pack $ "  [image] Produced " ++ show n ++ " image nodes"
         )
         -- Paper (PDF) extraction
         (do
           unless (null paperFiles) $ do
             logInfo $ T.pack $ "  [paper] Starting extraction for " ++ show (length paperFiles) ++ " paper files"
             let paperThreadCount = max 1 (min 4 numThreads)
             if paperThreadCount <= 1
               then mapM_ (\fp -> do
                 ext <- epExtractPdfFile ep config fp
                 epPushExtractionStreaming ep config ext
                 accumulate paperNodeMapRef paperEdgeAccRef ext
                 ) paperFiles
               else do
                 sem <- newQSemN paperThreadCount
                 let chunks = chunkList 50 paperFiles
                 mapM_ (\chunk -> do
                   results <- mapConcurrently (\fp -> bracket_
                     (waitQSemN sem 1)
                     (signalQSemN sem 1)
                     (epExtractPdfFile ep config fp)) chunk
                   mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate paperNodeMapRef paperEdgeAccRef ext) results
                   n <- readIORef paperNodeMapRef >>= evaluate . Map.size
                   _ <- evaluate n
                   performGC
                   ) chunks
           logDebug "  [paper] Extraction complete"
         )
       )
     )

  logDebug "  [extract] Code + doc + office + image + paper extraction complete"

  -- Build final Extraction from accumulators
  codeNodeMap <- readIORef codeNodeMapRef
  codeEdgeAcc <- readIORef codeEdgeAccRef
  docNodeMap <- readIORef docNodeMapRef
  docEdgeAcc <- readIORef docEdgeAccRef
  officeNodeMap <- readIORef officeNodeMapRef
  officeEdgeAcc <- readIORef officeEdgeAccRef
  imageNodeMap <- readIORef imageNodeMapRef
  imageEdgeAcc <- readIORef imageEdgeAccRef
  paperNodeMap <- readIORef paperNodeMapRef
  paperEdgeAcc <- readIORef paperEdgeAccRef
  let mergedNodeMap = codeNodeMap `Map.union` docNodeMap `Map.union` officeNodeMap `Map.union` imageNodeMap `Map.union` paperNodeMap
      mergedEdgeList = codeEdgeAcc (docEdgeAcc (officeEdgeAcc (imageEdgeAcc (paperEdgeAcc []))))
      merged = Extraction
        { extractionNodes = mergedNodeMap
        , extractionEdges = Map.fromList [(edgeId e, e) | e <- mergedEdgeList]
        }

  logInfo $ T.pack $ "  Extracted " ++ show (Map.size (extractionNodes merged)) ++ " nodes, " ++ show (Map.size (extractionEdges merged)) ++ " edges"
  pure merged

-- | Push a single extraction to Neo4j if streaming is configured.
pushExtractionStreaming :: ExtractionPort -> PipelineConfig -> Extraction -> IO ()
pushExtractionStreaming ep config extraction =
  epPushExtractionStreaming ep config extraction

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
      Nothing -> drop 1 (takeExtension fp)
    Nothing -> drop 1 (takeExtension fp)

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

  let ext = takeExtension filePath
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
data ImageSource
  = StandaloneImage FilePath
  | EmbeddedImage FilePath FilePath  -- ^ (archive path, media path within archive)
  deriving (Eq, Show)

-- | Extract an image from either a standalone file or embedded source.
extractImageSource :: AppEnv -> PipelineConfig -> ImageSource -> IO Extraction
extractImageSource appEnv config (StandaloneImage fp) =
  epExtractImageFile (extractionPort appEnv) config fp
extractImageSource appEnv config (EmbeddedImage archivePath mediaPath) = do
  let ep = extractionPort appEnv
      lp = loggingPort appEnv
  mediaResult <- epExtractMediaFile ep archivePath mediaPath
  case mediaResult of
    Left err -> do
      lpLogWarn lp $ T.pack $ "  [vision] Error extracting media " ++ mediaPath ++ " from " ++ archivePath ++ ": " ++ T.unpack err
      pure (extractionFromLists [imageStubNode mediaPath] [])
    Right bytes -> do
      let displayName = archivePath ++ "/" ++ takeFileName mediaPath
      epExtractImageFromBytes ep config displayName bytes
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
      }

-- | Collect embedded image paths from PPTX and DOCX office files via port.
collectEmbeddedImages :: ExtractionPort -> FilePath -> IO [(FilePath, FilePath)]
collectEmbeddedImages ep fp = do
  let ext = map toLower (takeExtension fp)
  case ext of
    ".docx" -> do
      paths <- epDocxMediaPaths ep fp
      pure [(fp, p) | p <- paths]
    ".pptx" -> do
      paths <- epPptxMediaPaths ep fp
      pure [(fp, p) | p <- paths]
    _ -> pure []

-- ───────────────────────────────────────────────
-- Tree-sitter extraction
-- ───────────────────────────────────────────────

-- | Extract from a single file using tree-sitter FFI bindings via port.
extractViaTreeSitterFFI :: AppEnv -> Granularity -> String -> FilePath -> IO Extraction
extractViaTreeSitterFFI appEnv _ "markdown" filePath = epExtractDocFile (extractionPort appEnv) filePath
extractViaTreeSitterFFI appEnv _gran grammar filePath = do
  let ep = extractionPort appEnv
      lp = loggingPort appEnv
  content <- BS.readFile filePath
  result <- epParseWithGrammar ep grammar filePath content
  case result of
    Nothing -> do
      lpLogWarn lp $ T.pack $ "  [tree-sitter] No grammar for " ++ grammar ++ " or parse failed for " ++ filePath
      pure (extractionFromLists [makeStubNode filePath] [])
    Just extraction -> do
      let nNodes = Map.size (extractionNodes extraction)
          nEdges = Map.size (extractionEdges extraction)
      lpLogDebug lp $ T.pack $ "  [tree-sitter] " ++ filePath ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
      pure extraction

-- ───────────────────────────────────────────────
-- Extractor routing
-- ───────────────────────────────────────────────

-- | Get the extractor mode for a file extension from the config.
extractorForExt :: PipelineConfig -> String -> ExtractorMode
extractorForExt config ext =
  case Map.lookup ext (gcExtractors (cfgGraphosConfig config)) of
    Just ec -> ecMode ec
    Nothing -> ExtractStub  -- unknown extensions get stubs, not LSP

-- | Resolve the effective granularity for a file extension.
resolveGranularity :: Maybe Granularity -> GraphosConfig -> String -> Granularity
resolveGranularity cliOverride gcfg ext =
  case cliOverride of
    Just g  -> g
    Nothing ->
      case Map.lookup ext (gcExtractors gcfg) >>= ecGranularity of
        Just g  -> g
        Nothing -> gcGranularity gcfg

-- | Resolve the effective granularity for a concrete file path.
granularityForFile :: PipelineConfig -> FilePath -> Granularity
granularityForFile config fp =
  resolveGranularity (cfgGranularity config) (cfgGraphosConfig config) (takeExtension fp)

-- | Human-readable granularity name for logs.
granularityName :: Granularity -> String
granularityName GranularityFine     = "fine"
granularityName GranularityFunction = "function"
granularityName GranularityFile     = "file"

-- ───────────────────────────────────────────────
-- Incremental extraction for watch mode
-- ───────────────────────────────────────────────

-- | Extract only a list of changed files (for --watch mode).
extractChangedFiles :: AppEnv -> PipelineConfig -> [FilePath] -> IO Extraction
extractChangedFiles appEnv config changedFiles = do
  let ep = extractionPort appEnv
      lp = loggingPort appEnv
      logInfo  = lpLogInfo lp
      logDebug = lpLogDebug lp

  absRoot <- canonicalizePath (cfgInputPath config)
  let (tsFiles, lspFiles, stubFiles) = partitionByExtractor config changedFiles

  -- Tree-sitter files
  tsExtractions <- mapM (\fp -> extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarForFile config fp) fp) tsFiles
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) tsExtractions

  -- LSP files (grouped by server)
  let fileGroups = groupByLSPServer (epLanguageServerCommands ep) lspFiles
  lspExtractions <- concatMapM (extractGroup appEnv absRoot config) fileGroups
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) lspExtractions

  -- Stub files
  stubExtractions <- mapM (\fp -> do
    logDebug $ T.pack $ "  [stub] " ++ fp
    pure (extractionFromLists [makeStubNode fp] [])
    ) stubFiles
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) stubExtractions

  let merged = List.foldl' mergeExtractions emptyExtraction
                 (tsExtractions ++ lspExtractions ++ stubExtractions)
  logInfo $ T.pack $ "  [watch] Extracted " ++ show (Map.size (extractionNodes merged)) ++ " nodes, " ++ show (Map.size (extractionEdges merged)) ++ " edges from " ++ show (length changedFiles) ++ " changed files"
  pure merged