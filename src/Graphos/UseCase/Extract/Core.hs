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
  , extractionConfigFingerprint
  , isStubExtraction
  , concatMapM
  , chunkList
  , ImageSource(..)
  , extractImageSource
  , collectEmbeddedImages
  , collapseDetectedFiles
  , applyTaggingSources
  ) where

import Control.Concurrent (newQSemN, waitQSemN, signalQSemN)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Exception (bracket_, evaluate)
import Control.Monad (forM, unless, void, when)
import Data.List (nubBy, sortBy)
import Data.Ord (comparing)
import Data.Bits ((.|.))
import qualified Data.List as List (foldl')
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', atomicModifyIORef')
import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)
import Data.Text.Short (fromText, toText)
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension, takeFileName)
import Data.Char (toLower)
import System.Mem (performGC)

import Graphos.Domain.Config (PdfExtractionMode(..))
import Graphos.Domain.Types (PipelineConfig(..), Extraction(..), emptyExtraction, extractionFromLists, Detection(..), FileCategory(..), FileClass(..), isSourceClass, ExtractorMode(..), ExtractorConfig(..), ecMode, GraphosConfig(..), gcExtractors, gcGranularity, gcVision, gcPdfExtraction, Granularity(..), VisionConfig(..)  , NodeId, Node(..), Edge(..), EdgeId(..), FileType(..)  , bitNodeKind, bitNodeExtra, bitNodeSource, relationToText, setFieldPresent)
import Graphos.Domain.Graph (mergeExtractions)
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..))
import Graphos.UseCase.Port.ExtractionPort (ExtractionPort(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.Domain.Graph (makeStubNode)
import Graphos.Infrastructure.Extract.TreeSitter.Convert (makeNodeId)
import Graphos.UseCase.Extract.LSP (groupByLSPServer, extractGroup)
import Graphos.UseCase.Extract.TreeSitter (extractViaTreeSitterFFI, grammarForFile)
import Data.Aeson (object, (.=))

-- | Extract entities from all detected files.
--
-- When @mProvenance@ is 'Just', this applies a multi-source tagging post-pass
-- over the merged extraction: each node whose source file was attributed to a
-- source is recomputed under that source's qualified path, tagged with its
-- source name on 'nodeSource'/'nodeSourceFile', and edges are remapped so
-- their endpoints (and ids) follow. When @mProvenance@ is 'Nothing' (the
-- single-source case) the extraction is returned unchanged, preserving prior
-- behavior byte-for-byte.
extractAll :: AppEnv -> PipelineConfig -> Detection -> Maybe (Map Text (Text, Text)) -> IO Extraction
extractAll appEnv config detection mProvenance = do
  let ep = extractionPort appEnv
      lp = loggingPort appEnv
      fsp = fileSystemPort appEnv
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

  -- Extraction cache (wire-incremental-update 4.2, D1): code files consult the
  -- persistent content+config cache before any extractor runs. `--fresh`
  -- bypasses the consult (cold rebuild); misses and fresh runs still write
  -- through (cache writes remain permitted under --fresh per the spec).
  -- LSP extraction is a live-server query (not per-file deterministic across
  -- runs), so only the tree-sitter and stub paths participate for now; their
  -- results are the deterministic parse tree products the cache soundness
  -- theorem covers.
  let cacheEnabled = not (cfgFresh config)
      fingerprint  = extractionConfigFingerprint config
  cachedTS <- if cacheEnabled
    then do
      results <- forM treeSitterFiles $ \fp -> do
        mExt <- fspLoadCachedExtraction fsp fingerprint fp (cfgOutputDir config)
        pure (fp, mExt)
      let hits = [(fp, ext) | (fp, Just ext) <- results]
          misses = [fp | (fp, Nothing) <- results]
      pure (Just (hits, misses))
    else pure Nothing
  let (cachedTSHits, treeSitterFiles') = case cachedTS of
        Just (hits, misses) -> (hits, misses)
        Nothing             -> ([], treeSitterFiles)
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
  codeEdgeAccRef  <- newIORef Map.empty :: IO (IORef (Map.Map EdgeId Edge))
  docNodeMapRef  <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  docEdgeAccRef   <- newIORef Map.empty :: IO (IORef (Map.Map EdgeId Edge))
  officeNodeMapRef <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  officeEdgeAccRef  <- newIORef Map.empty :: IO (IORef (Map.Map EdgeId Edge))
  imageNodeMapRef <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  imageEdgeAccRef  <- newIORef Map.empty :: IO (IORef (Map.Map EdgeId Edge))
  paperNodeMapRef <- newIORef Map.empty :: IO (IORef (Map.Map NodeId Node))
  paperEdgeAccRef  <- newIORef Map.empty :: IO (IORef (Map.Map EdgeId Edge))
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

      accumulateEdges :: IORef (Map.Map EdgeId Edge) -> [Edge] -> IO ()
      accumulateEdges ref edges = modifyIORef' ref $ \acc -> Map.union (Map.fromList [(edgeId e, e) | e <- edges]) acc

      accumulate :: IORef (Map.Map NodeId Node) -> IORef (Map.Map EdgeId Edge) -> Extraction -> IO ()
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

  let
    -- Accumulate, push, and persist one extraction (shared by cache hits and
    -- fresh extractions). Write-through happens for fresh extractions and for
    -- `--fresh` cold runs alike.
    produce ext = do
      epPushExtractionStreaming ep config ext
      accumulate codeNodeMapRef codeEdgeAccRef ext
      mergeIntoRunning ext

    extractTS :: FilePath -> IO ()
    extractTS fp = do
      ext <- extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarForFile config fp) fp
      fspSaveCachedExtraction fsp fingerprint fp ext (cfgOutputDir config)
      produce ext

  void $ concurrently
    (void $ concurrently
      (do
        -- Serve cached extractions first (wire-incremental-update 4.2): no
        -- parse is invoked for them.
        mapM_ (\ext -> produce ext >> logProgress) (map snd cachedTSHits)

        let tsChunks = chunkList 500 treeSitterFiles'
        mapM_ (\chunk -> do
          if numThreads <= 1
            then mapM_ (\fp -> do
              extractTS fp
              logProgress
              ) chunk
            else do
              sem <- newQSemN numThreads
              mapM_ (\fp -> bracket_
                (waitQSemN sem 1)
                (signalQSemN sem 1)
                (do extractTS fp
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
          fspSaveCachedExtraction fsp fingerprint fp ext (cfgOutputDir config)
          produce ext
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

  -- Cache hit/miss report (wire-incremental-update 4.3): how much of the code
  -- stage was served from the persistent extraction cache.
  let cacheHits = length cachedTSHits
      cacheMisses = length treeSitterFiles'
  when cacheEnabled $ unless (cacheHits == 0 && cacheMisses == 0) $
    logInfo $ T.pack $ "  [cache] reused " ++ show cacheHits ++ " file extraction(s), re-extracted "
                       ++ show cacheMisses

  logInfo $ T.pack $ "  Extracted " ++ show (Map.size (extractionNodes merged)) ++ " nodes, " ++ show (Map.size (extractionEdges merged)) ++ " edges"
  pure (applyTaggingSources mProvenance merged)

-- | Tag an extraction with multi-source provenance.
--
-- For every node whose 'nodeSourceFile' is a key in @prov@ (a real path ->
-- (sourceName, qualifiedPath) map), recompute its nodeId from the qualified
-- path so files with identical relative paths in different sources get
-- distinct identifiers, record the source on 'nodeSource'/'nodeSourceFile',
-- and rebuild edges so their endpoints and ids follow the resulting remap.
-- Nodes absent from @prov@ are left untouched. When @mProv@ is 'Nothing' the
-- extraction is returned unchanged, preserving single-source behavior.
applyTaggingSources
  :: Maybe (Map Text (Text, Text))
  -> Extraction
  -> Extraction
applyTaggingSources Nothing ext = ext
applyTaggingSources (Just prov) ext =
    let pairs  = map tagNode (Map.elems (extractionNodes ext))
        remap  = Map.fromList [(oldId, nodeId node) | (oldId, node) <- pairs, oldId /= nodeId node]
        nodes' = map snd pairs
        edges' = map (remapEdge remap) (Map.elems (extractionEdges ext))
    in ext { extractionNodes = Map.fromList (map (\n -> (nodeId n, n)) nodes')
           , extractionEdges = Map.fromList (map (\e -> (edgeId e, e)) edges') }
  where
    tagNode n = case Map.lookup (toText (nodeSourceFile n)) prov of
      Just (srcName, qPath) ->
        let newNid  = makeNodeId (T.unpack qPath) (toText (nodeLabel n))
            tagged  = n { nodeId           = newNid
                      , nodeSource       = Just (fromText srcName)
                      , nodeSourceFile   = fromText qPath
                      , nodePresentBits  = setFieldPresent bitNodeSource (nodePresentBits n) }
        in (nodeId n, tagged)
      Nothing -> (nodeId n, n)
    remapEdge remap e =
      let s = Map.findWithDefault (edgeSource e) (edgeSource e) remap
          t = Map.findWithDefault (edgeTarget e) (edgeTarget e) remap
      in e { edgeSource = s, edgeTarget = t, edgeId = EdgeId (s <> "->" <> t <> ":" <> relationToText (edgeRelation e)) }

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
      , nodeLabel = fromText (T.pack (takeFileName fp))
      , nodeFileType = ImageFile
      , nodeSourceFile = fromText (T.pack fp)
      , nodeSource = Nothing
      , nodeLineStart = Nothing
      , nodeLineEnd = Nothing
      , nodeSignature = Nothing
      , nodeCommunityId = Nothing
      , nodeKind = Just (fromText "Image")
      , nodeDegree = Nothing
      , nodeIsBridge = Nothing
      , nodeExtra = Nothing
      , nodePresentBits = bitNodeKind
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

-- | Canonical fingerprint text of every extraction-affecting configuration
-- value: the effective per-extension granularity under the CLI override, the
-- extractor mode per extension, and the pdf extraction level. The key-sorting
-- makes the serialization canonical so identical configs always fingerprint
-- identically.
--
-- INV-CACHE-SOUND (AVI-521 §1.2): any future extraction-affecting config value
-- MUST be added here — an unfingerprinted config change would serve stale
-- cached extractions. Documented in the cache module header contract.
extractionConfigFingerprint :: PipelineConfig -> Text
extractionConfigFingerprint config = T.pack . unwords $
  concat
    [ [ "granularity=" ++ show (gcGranularity gcfg)
      , "cliGranularity=" ++ maybe "none" show (cfgGranularity config)
      , "pdf=" ++ pdfLevelName (gcPdfExtraction gcfg)
      ]
    , [ "extractor." ++ ext ++ "=" ++ show (ecMode ec)
            ++ ";granularity=" ++ maybe "auto" show (ecGranularity ec)
      | (ext, ec) <- Map.toAscList (gcExtractors gcfg)
      ]
    ]
  where
    gcfg = cfgGraphosConfig config
    pdfLevelName PdfSmall  = "small"
    pdfLevelName PdfMedium = "medium"
    pdfLevelName PdfLarge  = "large"

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
--
-- Wire-incremental-update 5.1: every changed file's extraction is written
-- through to the persistent extraction cache under the fingerprinted key, so
-- a subsequent @--update@ run over the same content is a cache hit.
extractChangedFiles :: AppEnv -> PipelineConfig -> [FilePath] -> IO Extraction
extractChangedFiles appEnv config changedFiles = do
  let ep = extractionPort appEnv
      lp = loggingPort appEnv
      fsp = fileSystemPort appEnv
      logInfo  = lpLogInfo lp
      logDebug = lpLogDebug lp

  absRoot <- canonicalizePath (cfgInputPath config)
  let fingerprint = extractionConfigFingerprint config
      (tsFiles, lspFiles, stubFiles) = partitionByExtractor config changedFiles

  tsExtractions <- mapM (\fp -> do
    ext <- extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarForFile config fp) fp
    fspSaveCachedExtraction fsp fingerprint fp ext (cfgOutputDir config)
    pure ext) tsFiles
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) tsExtractions

  let fileGroups = groupByLSPServer (epLanguageServerCommands ep) lspFiles
  lspExtractions <- concatMapM (extractGroup appEnv absRoot config) fileGroups
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) lspExtractions

  stubExtractions <- mapM (\fp -> do
    logDebug $ T.pack $ "  [stub] " ++ fp
    let ext = extractionFromLists [makeStubNode fp] []
    fspSaveCachedExtraction fsp fingerprint fp ext (cfgOutputDir config)
    pure ext
    ) stubFiles
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) stubExtractions

  let merged = List.foldl' mergeExtractions emptyExtraction
                  (tsExtractions ++ lspExtractions ++ stubExtractions)
  logInfo $ T.pack $ "  [watch] Extracted " ++ show (Map.size (extractionNodes merged)) ++ " nodes, " ++ show (Map.size (extractionEdges merged)) ++ " edges from " ++ show (length changedFiles) ++ " changed files"
  pure merged

-- | In collapse mode, produce one representative node per detected
-- (non-'Source') file. Each node carries 'childCount' = the number of nodes
-- that file would have produced if extracted normally. Detected files are not
-- added to the graph as their full node set; they are represented compactly so
-- a huge generated/vendored/minified file occupies a single node.
collapseDetectedFiles
  :: AppEnv
  -> PipelineConfig
  -> Map.Map FileClass [FilePath] -- ^ detectionClassification (all files by class)
  -> IO [Node]
collapseDetectedFiles appEnv config classification = do
  let detectedFiles = [ f | (c, fs) <- Map.toList classification, not (isSourceClass c), f <- fs ]
  if null detectedFiles
    then pure []
    else mapM (\fp -> do
                  count <- countExtractedNodes appEnv config fp
                  pure (collapsedNode fp count)
                ) detectedFiles

-- | Count the nodes a single file would produce if extracted normally.
-- Mirrors the routing in 'extractAll' (tree-sitter / LSP / stub) but processes
-- exactly one file, then keeps only nodes whose source is this file so imported
-- and external nodes (which belong to other files) are not double-counted.
countExtractedNodes
  :: AppEnv -> PipelineConfig -> FilePath -> IO Int
countExtractedNodes appEnv config fp = do
   let (tsFiles, _, stubFiles) = partitionByExtractor config [fp]
   ext <- if not (null tsFiles)
         then extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarForFile config fp) fp
       else if not (null stubFiles)
         then pure (extractionFromLists [makeStubNode fp] [])
        else do
          absRoot <- canonicalizePath (cfgInputPath config)
          let groups = groupByLSPServer (epLanguageServerCommands (extractionPort appEnv)) [fp]
          extractions <- mapM (extractGroup appEnv absRoot config) groups
          pure (List.foldl' mergeExtractions emptyExtraction (concat extractions))
   pure $ length [ () | n <- Map.elems (extractionNodes ext), toText (nodeSourceFile n) == T.pack fp ]

-- | Build a single representative node for a detected file, tagging it with the
-- detection class and a childCount attribute recording how many nodes the file
-- would have produced. Kept pure so it is trivially testable.
collapsedNode :: FilePath -> Int -> Node
collapsedNode fp count = Node
  { nodeId           = T.pack fp
  , nodeLabel        = fromText (T.pack (takeFileName fp))
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText (T.pack fp)
  , nodeSource       = Nothing
  , nodeLineStart    = Nothing
  , nodeLineEnd      = Nothing
  , nodeSignature    = Nothing
  , nodeCommunityId  = Nothing
  , nodeKind         = Just (fromText "Collapsed")
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Just collapsedNodeExtra
  , nodePresentBits  = bitNodeKind .|. bitNodeExtra
  }
  where
    collapsedNodeExtra = object
      [ "childCount" .= count
      , "sourceFile" .= T.pack fp
      ]