-- | Core extraction orchestration — parallel extraction routing files to extractors.
module Graphos.UseCase.Extract.Core
  ( extractAll
  , extractChangedFiles
  , pushExtractionStreaming
  , partitionByExtractor
  , extractorForExt
  , resolveGranularity
  , granularityForFile
  , granularityName
  , isStubExtraction
  , concatMapM
  , chunkList
  , ImageSource(..)
  , extractImageSource
  , collectEmbeddedImages
  ) where

import Control.Concurrent (newQSemN, waitQSemN, signalQSemN)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Exception (bracket_, evaluate)
import Control.Monad (unless, void, when)
import Data.List (nubBy, sortBy)
import Data.Ord (comparing)
import qualified Data.List as List (foldl')
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', atomicModifyIORef')
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension, takeFileName)
import Data.Char (toLower)
import System.Mem (performGC)

import Graphos.Domain.Types (PipelineConfig(..), Extraction(..), emptyExtraction, extractionFromLists, Detection(..), FileCategory(..), ExtractorMode(..), ExtractorConfig(..), ecMode, GraphosConfig(..), gcExtractors, gcGranularity, gcVision, Granularity(..), VisionConfig(..), NodeId, Node(..), Edge(..), FileType(..))
import Graphos.Domain.Graph (mergeExtractions)
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.ExtractionPort (ExtractionPort(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.Domain.Graph (makeStubNode)
import Graphos.UseCase.Extract.LSP (groupByLSPServer, extractGroup)
import Graphos.UseCase.Extract.TreeSitter (extractViaTreeSitterFFI, grammarForFile)

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

  let (treeSitterFiles, lspFiles, stubFiles) = partitionByExtractor config codeFiles

  let hasSpecialHandler g = g == "markdown" || g == "haskell"
      grammarAvailable g = hasSpecialHandler g || epHasTreeSitterGrammar ep g
      missingGrammars = nubBy (\(g1, _) (g2, _) -> g1 == g2)
        $ sortBy (comparing fst)
        $ filter (\(g, _) -> not (grammarAvailable g))
        $ fmap (\fp -> (grammarForFile config fp, takeExtension fp)) treeSitterFiles
  unless (null missingGrammars) $
    lpLogWarn lp $ T.pack $ "  [extract] WARNING: No tree-sitter grammar binding for: "
      ++ unwords (map (\(g, ext) -> g ++ " (" ++ ext ++ ")") missingGrammars)
      ++ ". Files will use stub extraction."

  unless (null treeSitterFiles) $
    logInfo $ T.pack $ "  tree-sitter: " ++ show (length treeSitterFiles) ++ " files"
  unless (null lspFiles) $
    logInfo $ T.pack $ "  LSP: " ++ show (length lspFiles) ++ " files"
  unless (null stubFiles) $
    logDebug $ T.pack $ "  stub: " ++ show (length stubFiles) ++ " files"

  let docThreads = min 8 (max 1 numThreads)

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
  runningRef <- newIORef emptyExtraction :: IO (IORef Extraction)

  let totalFiles = length codeFiles + length docFiles + length officeFiles + length imageFiles + length paperFiles
  progressRef <- newIORef 0 :: IO (IORef Int)
  let logProgress :: IO ()
      logProgress = do
        n <- atomicModifyIORef' progressRef $ \c -> (c + 1, c)
        let count = n + 1
        when (totalFiles > 0 && count `mod` 50 == 0) $
          let pct = (count * 100) `div` totalFiles :: Int
          in logInfo $ T.pack $ "  [extract] Processed " ++ show count ++ "/" ++ show totalFiles ++ " files (" ++ show pct ++ "%)"

  let accumulateNodes :: IORef (Map.Map NodeId Node) -> [Node] -> IO ()
      accumulateNodes ref nodes = modifyIORef' ref $ \acc ->
        List.foldl' (\m n -> Map.insertWith (\_old new -> new) (nodeId n) n m) acc nodes

      accumulateEdges :: IORef ([Edge] -> [Edge]) -> [Edge] -> IO ()
      accumulateEdges ref edges = modifyIORef' ref $ \acc -> acc . (edges ++)

      accumulate :: IORef (Map.Map NodeId Node) -> IORef ([Edge] -> [Edge]) -> Extraction -> IO ()
      accumulate nodeRef edgeRef ext = do
        accumulateNodes nodeRef (Map.elems (extractionNodes ext))
        accumulateEdges edgeRef (Map.elems (extractionEdges ext))

      mergeIntoRunning :: Extraction -> IO ()
      mergeIntoRunning ext = modifyIORef' runningRef $ \running -> mergeExtractions running ext

  let officeThreadCount = max 1 (min 4 numThreads)
  unless (null officeFiles) $
    logInfo $ T.pack $ "  office: " ++ show (length officeFiles) ++ " files"
  unless (null imageFiles) $
    logInfo $ T.pack $ "  image: " ++ show (length imageFiles) ++ " files" ++ (if vcEnabled vCfg then "" else " (vision disabled)")

  let imageBatchSize = max 1 (vcBatchSize vCfg)

  embeddedImagesList <- if not (null officeFiles) && vcEnabled vCfg
    then concat <$> mapM (collectEmbeddedImages ep) officeFiles
    else pure []

  unless (null embeddedImagesList) $
    logInfo $ T.pack $ "  image: " ++ show (length embeddedImagesList) ++ " embedded images from office files"

  let allImageSources = map StandaloneImage imageFiles ++ map (uncurry EmbeddedImage) embeddedImagesList

  void $ concurrently
    (void $ concurrently
      (do
        let tsChunks = chunkList 500 treeSitterFiles
        mapM_ (\chunk -> do
          if numThreads <= 1
            then mapM_ (\fp -> do
              ext <- extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarForFile config fp) fp
              epPushExtractionStreaming ep config ext
              accumulate codeNodeMapRef codeEdgeAccRef ext
              mergeIntoRunning ext
              logProgress
              ) chunk
            else do
              sem <- newQSemN numThreads
              mapM_ (\fp -> bracket_
                (waitQSemN sem 1)
                (signalQSemN sem 1)
                (do ext <- extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarForFile config fp) fp
                    epPushExtractionStreaming ep config ext
                    accumulate codeNodeMapRef codeEdgeAccRef ext
                    mergeIntoRunning ext
                    logProgress
                )) chunk
          n <- readIORef codeNodeMapRef >>= evaluate . Map.size
          _ <- evaluate n
          performGC
          ) tsChunks

        let fileGroups = groupByLSPServer (epLanguageServerCommands ep) lspFiles
            numGroups = length fileGroups
            lspConcurrency = cfgLspConcurrency config
        logInfo $ T.pack $ "  LSP server groups: " ++ show numGroups ++ " (lsp-concurrency: " ++ show lspConcurrency ++ ")"
        if numThreads <= 1
          then mapM_ (\grp -> do
            exts <- extractGroup appEnv absRoot config grp
            mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate codeNodeMapRef codeEdgeAccRef ext >> mergeIntoRunning ext) exts
            mapM_ (\_ -> logProgress) grp
            ) fileGroups
          else do
            sem <- newQSemN lspConcurrency
            results <- mapConcurrently (\grp -> bracket_
              (waitQSemN sem 1)
              (signalQSemN sem 1)
              (extractGroup appEnv absRoot config grp)) fileGroups
            mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate codeNodeMapRef codeEdgeAccRef ext >> mergeIntoRunning ext) (concat results)
            mapM_ (\grp -> mapM_ (\_ -> logProgress) grp) fileGroups
        performGC

        mapM_ (\fp -> do
          logDebug $ T.pack $ "  [stub] " ++ fp
          let ext = extractionFromLists [makeStubNode fp] []
          epPushExtractionStreaming ep config ext
          accumulate codeNodeMapRef codeEdgeAccRef ext
          mergeIntoRunning ext
          logProgress
          ) stubFiles
      )
      (do
        unless (null officeFiles) $ do
          logDebug $ T.pack $ "  [office] Starting extraction for " ++ show (length officeFiles) ++ " office files"
          if officeThreadCount <= 1
            then mapM_ (\fp -> do
              ext <- epExtractOfficeFile ep config fp
              epPushExtractionStreaming ep config ext
              accumulate officeNodeMapRef officeEdgeAccRef ext
              logProgress
              ) officeFiles
            else do
              sem <- newQSemN officeThreadCount
              let chunks = chunkList 100 officeFiles
              mapM_ (\chunk -> do
                results <- mapConcurrently (\fp -> bracket_
                  (waitQSemN sem 1)
                  (signalQSemN sem 1)
                  (epExtractOfficeFile ep config fp)) chunk
                mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate officeNodeMapRef officeEdgeAccRef ext >> mergeIntoRunning ext) results
                mapM_ (\_ -> logProgress) chunk
                n <- readIORef officeNodeMapRef >>= evaluate . Map.size
                _ <- evaluate n
                performGC
                ) chunks
          logDebug "  [office] Extraction complete"
       )
     )
      (void $ concurrently
        (do
          logDebug $ T.pack $ "  [doc] Starting extraction for " ++ show (length docFiles) ++ " doc files (threads: " ++ show docThreads ++ ")"
          if docThreads <= 1
            then mapM_ (\fp -> do
              ext <- epExtractDocFile ep fp
              epPushExtractionStreaming ep config ext
              accumulate docNodeMapRef docEdgeAccRef ext
              logProgress
              ) docFiles
            else do
              sem <- newQSemN docThreads
              let chunks = chunkList 500 docFiles
              mapM_ (\chunk -> do
                results <- mapConcurrently (\fp -> bracket_
                  (waitQSemN sem 1)
                  (signalQSemN sem 1)
                  (epExtractDocFile ep fp)) chunk
                mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate docNodeMapRef docEdgeAccRef ext >> mergeIntoRunning ext) results
                n <- readIORef docNodeMapRef >>= evaluate . Map.size
                _ <- evaluate n
                performGC
                ) chunks
          logDebug "  [doc] Extraction complete"
        )
       (void $ concurrently
         (do
           unless (null allImageSources) $ do
             logDebug $ T.pack $ "  [image] Starting extraction for " ++ show (length imageFiles) ++ " standalone + " ++ show (length embeddedImagesList) ++ " embedded images (batch: " ++ show imageBatchSize ++ ")"
             let imageChunks = chunkList imageBatchSize allImageSources
             mapM_ (\chunk -> do
               results <- mapM (extractImageSource appEnv config) chunk
               mapM_ (\ext -> do
                 epPushExtractionStreaming ep config ext
                 accumulate imageNodeMapRef imageEdgeAccRef ext
                 mergeIntoRunning ext) results
               n <- readIORef imageNodeMapRef >>= evaluate . Map.size
               _ <- evaluate n
               performGC
               ) imageChunks
             logDebug "  [image] Extraction complete"
           unless (null allImageSources) $ do
             n <- readIORef imageNodeMapRef >>= evaluate . Map.size
             logInfo $ T.pack $ "  [image] Produced " ++ show n ++ " image nodes"
         )
          (do
            if null paperFiles
              then logDebug "  [paper] Extraction complete"
              else do
                logInfo $ T.pack $ "  [paper] Starting extraction for " ++ show (length paperFiles) ++ " paper files"
                let paperThreadCount = max 1 (min 4 numThreads)
                paperSuccessRef <- newIORef 0 :: IO (IORef Int)
                paperStubRef    <- newIORef 0 :: IO (IORef Int)
                let recordResult ext = do
                      if isStubExtraction ext
                        then modifyIORef' paperStubRef (+ 1)
                        else modifyIORef' paperSuccessRef (+ 1)
                if paperThreadCount <= 1
                  then mapM_ (\fp -> do
                    ext <- epExtractPdfFile ep config fp
                    epPushExtractionStreaming ep config ext
                    accumulate paperNodeMapRef paperEdgeAccRef ext
                    recordResult ext
                    ) paperFiles
                   else do
                    sem <- newQSemN paperThreadCount
                    let chunks = chunkList 50 paperFiles
                    mapM_ (\chunk -> do
                      results <- mapConcurrently (\fp -> bracket_
                        (waitQSemN sem 1)
                        (signalQSemN sem 1)
                         (do ext <- epExtractPdfFile ep config fp
                             recordResult ext
                             pure ext)) chunk
                      mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate paperNodeMapRef paperEdgeAccRef ext >> mergeIntoRunning ext) results
                      n <- readIORef paperNodeMapRef >>= evaluate . Map.size
                      _ <- evaluate n
                      performGC
                      ) chunks
                    _ <- readIORef paperSuccessRef
                    _ <- readIORef paperStubRef
                    pure ()
                successCount <- readIORef paperSuccessRef
                stubCount <- readIORef paperStubRef
                logInfo $ T.pack $ "  [paper] Extraction complete: " ++ show (length paperFiles) ++ " files, " ++ show successCount ++ " successful, " ++ show stubCount ++ " stubbed"
          )
       )
     )

  logDebug "  [extract] Code + doc + office + image + paper extraction complete"

  running <- readIORef runningRef
  let merged = running

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

-- | Sequential concatMapM
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- | Split a list into chunks of given size.
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)

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

-- | Get the extractor mode for a file extension from the config.
extractorForExt :: PipelineConfig -> String -> ExtractorMode
extractorForExt config ext =
  case Map.lookup ext (gcExtractors (cfgGraphosConfig config)) of
    Just ec -> ecMode ec
    Nothing -> ExtractStub

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

-- | Classify whether an Extraction represents a stub (single file node, no edges).
isStubExtraction :: Extraction -> Bool
isStubExtraction ext =
  let nodes = extractionNodes ext
      edges = extractionEdges ext
  in Map.size nodes == 1
     && Map.null edges
     && case Map.lookupMin nodes of
          Just (_, node) -> nodeKind node == Just "File"
          Nothing        -> False

-- | Extract only a list of changed files (for --watch mode).
extractChangedFiles :: AppEnv -> PipelineConfig -> [FilePath] -> IO Extraction
extractChangedFiles appEnv config changedFiles = do
  let ep = extractionPort appEnv
      lp = loggingPort appEnv
      logInfo  = lpLogInfo lp
      logDebug = lpLogDebug lp

  absRoot <- canonicalizePath (cfgInputPath config)
  let (tsFiles, lspFiles, stubFiles) = partitionByExtractor config changedFiles

  tsExtractions <- mapM (\fp -> extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarForFile config fp) fp) tsFiles
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) tsExtractions

  let fileGroups = groupByLSPServer (epLanguageServerCommands ep) lspFiles
  lspExtractions <- concatMapM (extractGroup appEnv absRoot config) fileGroups
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) lspExtractions

  stubExtractions <- mapM (\fp -> do
    logDebug $ T.pack $ "  [stub] " ++ fp
    pure (extractionFromLists [makeStubNode fp] [])
    ) stubFiles
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) stubExtractions

  let merged = List.foldl' mergeExtractions emptyExtraction
                 (tsExtractions ++ lspExtractions ++ stubExtractions)
  logInfo $ T.pack $ "  [watch] Extracted " ++ show (Map.size (extractionNodes merged)) ++ " nodes, " ++ show (Map.size (extractionEdges merged)) ++ " edges from " ++ show (length changedFiles) ++ " changed files"
  pure merged