-- | Core extraction orchestration — parallel extraction routing files to extractors.
module Graphos.UseCase.Extract.Core
  ( extractAll
  , extractChangedFiles
  , pushExtractionStreaming
  , partitionByExtractor
  , extractorForExt
  , commandGateFor
  , resolveGranularity
  , granularityForFile
  , granularityName
  , extractionConfigFingerprint
  , isStubExtraction
  , StubCoverage(..)
  , stubCoverage
  , stubDominanceThreshold
  , dominantStubExt
  , stubCoverageWarning
  , concatMapM
  , chunkList
  , summariseExtractionErrors
  , extractionErrorSummaryLines
  , isolateFileExtraction
  , ImageSource(..)
  , extractImageSource
  , collectEmbeddedImages
  , collapseDetectedFiles
  , applyTaggingSources
  ) where

import Control.Concurrent (newQSemN, waitQSemN, signalQSemN)
import Control.Concurrent.Async (concurrently, mapConcurrently, mapConcurrently_)
import Control.Exception (SomeAsyncException, SomeException, bracket_, displayException, evaluate, fromException, tryJust)
import Control.Monad (forM, forM_, unless, void, when)
import Data.List (nub, nubBy, sortBy, intercalate, maximumBy)
import Data.Ord (comparing)
import Data.Bits ((.|.))
import qualified Data.List as List (foldl')
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)
import Data.Text.Short (fromText, toText)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (takeExtension, takeFileName, (</>))
import Data.Char (toLower)
import System.Mem (performGC)
import System.Posix.Files (getFileStatus, fileSize, modificationTime)

import Graphos.Domain.Config (PdfExtractionMode(..), ResolveConfig(..))
import Graphos.Domain.Types (PipelineConfig(..), Extraction(..), ExtractionError(..), emptyExtraction, extractionFromLists, Detection(..), FileCategory(..), FileClass(..), isSourceClass, ExtractorMode(..), ExtractorConfig(..), ecMode, GraphosConfig(..), gcExtractors, gcGranularity, gcVision, gcPdfExtraction, gcResolve, Granularity(..), VisionConfig(..)  , NodeId, Node(..), Edge(..), EdgeId(..), FileType(..)  , bitNodeKind, bitNodeExtra, bitNodeSource, relationToText, setFieldPresent)
import Graphos.Domain.Graph (mergeExtractions)
import Graphos.Domain.Extraction (CommandOutcome(..))
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..), AnnotatedPattern)
import Graphos.UseCase.Port.ExtractionPort (ExtractionPort(..), CommandGate(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.Domain.Graph (makeStubNode)
import Graphos.Infrastructure.Extract.TreeSitter.Convert (makeNodeId)
import Graphos.Infrastructure.Extract.Resolve
  ( resolveExtraction, resolveOverrideWarnings, resolveTotal
  , ResolveStats(..) )
import Graphos.Infrastructure.Extract.TreeSitter.Core (mkSourceFile)
import Graphos.Infrastructure.Extract.TreeSitter.RuntimeGrammar
  ( QueryKind(..), queryKindFile, resolveRuntimePath )
import Graphos.UseCase.Extract.LSP (FileGroup, groupByLSPServer, extractGroup)
import Graphos.UseCase.Extract.TreeSitter
  ( extractViaTreeSitterFFI, grammarForFile
  , grammarSelectionForFile, hasRuntimeGrammar )
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

  let (treeSitterFiles, lspFiles, commandFiles, stubFiles) = partitionByExtractor config codeFiles

  -- Extraction cache (wire-incremental-update 4.2, D1; custom-extractors 5.4):
  -- code files consult the persistent content+config cache before any extractor
  -- runs. `--fresh` bypasses the consult (cold rebuild); misses and fresh runs
  -- still write through (cache writes remain permitted under --fresh per the
  -- spec). LSP extraction is a live-server query (not per-file deterministic
  -- across runs), so only the tree-sitter, command and stub paths participate;
  -- their results are the deterministic products the cache soundness theorem
  -- covers. A command file only reaches the cache after a /successful/
  -- invocation — failures and skips are stub-extracted but not persisted, so a
  -- transient failure cannot pin a stub into the cache.
  let cacheEnabled = not (cfgFresh config)
  fingerprint <- extractionConfigFingerprint config
  let consultCache :: [FilePath] -> IO (Maybe ([(FilePath, Extraction)], [FilePath]))
      consultCache files
        | not cacheEnabled = pure Nothing
        | otherwise = do
            results <- forM files $ \fp -> do
              mExt <- fspLoadCachedExtraction fsp fingerprint fp (cfgOutputDir config)
              pure (fp, mExt)
            let hits = [(fp, ext) | (fp, Just ext) <- results]
                misses = [fp | (fp, Nothing) <- results]
            pure (Just (hits, misses))
  cachedTS  <- consultCache treeSitterFiles
  cachedCmd <- consultCache commandFiles
  let (cachedTSHits, treeSitterFiles') = case cachedTS of
        Just (hits, misses) -> (hits, misses)
        Nothing             -> ([], treeSitterFiles)
      (cachedCmdHits, commandFiles') = case cachedCmd of
        Just (hits, misses) -> (hits, misses)
        Nothing             -> ([], commandFiles)
  let hasSpecialHandler g = g == "markdown" || g == "haskell"
      grammarAvailable g fp = hasSpecialHandler g || hasRuntimeGrammar config fp
        || epHasTreeSitterGrammar ep g
      missingGrammars = nubBy (\(g1, _) (g2, _) -> g1 == g2)
        $ sortBy (comparing fst)
        $ filter (\(g, fp) -> not (grammarAvailable g fp))
        $ fmap (\fp -> (grammarForFile config fp, takeExtension fp)) treeSitterFiles
  unless (null missingGrammars) $
    lpLogWarn lp $ T.pack $ "  [extract] WARNING: No tree-sitter grammar binding for: "
      ++ unwords (map (\(g, ext) -> g ++ " (" ++ ext ++ ")") missingGrammars)
      ++ ". Files will use stub extraction."

  unless (null treeSitterFiles) $
    logInfo $ T.pack $ "  tree-sitter: " ++ show (length treeSitterFiles) ++ " files"
  unless (null lspFiles) $
    logInfo $ T.pack $ "  LSP: " ++ show (length lspFiles) ++ " files"
  unless (null commandFiles) $
    logInfo $ T.pack $ "  command: " ++ show (length commandFiles) ++ " files"
  mapM_ (\msg -> logInfo $ T.pack $ "  [extract] WARNING: " ++ msg)
    (stubCoverageWarning (stubCoverage (treeSitterFiles ++ lspFiles ++ commandFiles) stubFiles))

  -- Command extractors are gated before any process is spawned (custom-extractors
  -- 3.4): the scan-root ignore patterns are loaded once, only when a command file
  -- survives partitioning.
  commandIgnorePatterns <- if null commandFiles'
    then pure []
    else fspLoadIgnorePatterns fsp absRoot
  let commandGate = commandGateFor config absRoot commandIgnorePatterns
      commandGroups = commandFileGroups config commandFiles'

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

  -- Extraction threads (tree-sitter pool, doc, office, image, paper) update
  -- these refs concurrently: a plain read-then-write ('modifyIORef'') loses
  -- one thread's update when two interleave, dropping whole files from the
  -- merged extraction (pc26: up to 787 files differed between two runs).
  let atomicModify :: IORef a -> (a -> a) -> IO ()
      atomicModify ref f = atomicModifyIORef' ref (\x -> (f x, ()))

      accumulateNodes :: IORef (Map.Map NodeId Node) -> [Node] -> IO ()
      accumulateNodes ref nodes = atomicModify ref $ \acc ->
        List.foldl' (\m n -> Map.insertWith (\_old new -> new) (nodeId n) n m) acc nodes

      accumulateEdges :: IORef (Map.Map EdgeId Edge) -> [Edge] -> IO ()
      accumulateEdges ref edges = atomicModify ref $ \acc -> Map.union (Map.fromList [(edgeId e, e) | e <- edges]) acc

      accumulate :: IORef (Map.Map NodeId Node) -> IORef (Map.Map EdgeId Edge) -> Extraction -> IO ()
      accumulate nodeRef edgeRef ext = do
        accumulateNodes nodeRef (Map.elems (extractionNodes ext))
        accumulateEdges edgeRef (Map.elems (extractionEdges ext))

      mergeIntoRunning :: Extraction -> IO ()
      mergeIntoRunning ext = atomicModify runningRef $ \running -> mergeExtractions running ext

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
      ext <- isolateFileExtraction lp fp (T.pack (grammarForFile config fp)) $
               extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarSelectionForFile config fp) fp
      -- A failed extraction is never written to the cache: a transient error
      -- must not pin a stub for the content (parity with the command path).
      when (null (extractionErrors ext)) $
        fspSaveCachedExtraction fsp fingerprint fp ext (cfgOutputDir config)
      produce ext

    -- One command-mode extension at a time (a batch invocation spans only the
    -- files of one declared command). Successful outputs are cached; failures
    -- and gate skips fall back to a stub node, record the reason where it is an
    -- error, and are deliberately not written to the cache.
    runCommandGroup :: (ExtractorConfig, [FilePath]) -> IO ()
    runCommandGroup (ec, fps) = do
      outcomes <- epExtractCommandFiles ep commandGate ec fps
      forM_ fps $ \fp -> do
        handleCommandOutcome fp $ case Map.lookup fp outcomes of
          Just outcome -> outcome
          Nothing      -> CommandFailed (ExtractionError fp "command"
                            "the extractor reported no outcome for this file")
        logProgress

    handleCommandOutcome :: FilePath -> CommandOutcome -> IO ()
    handleCommandOutcome fp (CommandProduced ext) = do
      fspSaveCachedExtraction fsp fingerprint fp ext (cfgOutputDir config)
      produce ext
    handleCommandOutcome fp (CommandFailed err) = do
      lpLogWarn lp $ T.pack $ "  [command] " ++ fp ++ ": " ++ T.unpack (eeMessage err)
      produce (commandStub fp [err])
    handleCommandOutcome fp (CommandSkipped reason) = do
      logDebug $ T.pack $ "  [command] " ++ fp ++ ": skipped (" ++ T.unpack reason ++ ")"
      produce (commandStub fp [])

    commandStub fp errs =
      (extractionFromLists [makeStubNode fp] []) { extractionErrors = errs }

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

        -- Cached command extractions are served without spawning anything.
        mapM_ (\ext -> produce ext >> logProgress) (map snd cachedCmdHits)

        let commandConcurrency = max 1 (min 4 numThreads)
        if commandConcurrency <= 1
          then mapM_ runCommandGroup commandGroups
          else do
            sem <- newQSemN commandConcurrency
            mapConcurrently_ (\grp -> bracket_
              (waitQSemN sem 1)
              (signalQSemN sem 1)
              (runCommandGroup grp)) commandGroups
        performGC

        let fileGroups = groupByLSPServer (epLanguageServerCommands ep) lspFiles
            numGroups = length fileGroups
            lspConcurrency = cfgLspConcurrency config
        logInfo $ T.pack $ "  LSP server groups: " ++ show numGroups ++ " (lsp-concurrency: " ++ show lspConcurrency ++ ")"
        if numThreads <= 1
          then mapM_ (\grp -> do
            exts <- extractGroupIsolated appEnv absRoot config grp
            mapM_ (\ext -> epPushExtractionStreaming ep config ext >> accumulate codeNodeMapRef codeEdgeAccRef ext >> mergeIntoRunning ext) exts
            mapM_ (\_ -> logProgress) grp
            ) fileGroups
          else do
            sem <- newQSemN lspConcurrency
            results <- mapConcurrently (\grp -> bracket_
              (waitQSemN sem 1)
              (signalQSemN sem 1)
              (extractGroupIsolated appEnv absRoot config grp)) fileGroups
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
              ext <- isolateFileExtraction lp fp "office" (epExtractOfficeFile ep config fp)
              epPushExtractionStreaming ep config ext
              accumulate officeNodeMapRef officeEdgeAccRef ext
              mergeIntoRunning ext
              logProgress
              ) officeFiles
            else do
              sem <- newQSemN officeThreadCount
              let chunks = chunkList 100 officeFiles
              mapM_ (\chunk -> do
                results <- mapConcurrently (\fp -> bracket_
                  (waitQSemN sem 1)
                  (signalQSemN sem 1)
                  (isolateFileExtraction lp fp "office" (epExtractOfficeFile ep config fp))) chunk
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
              ext <- isolateFileExtraction lp fp "doc" (epExtractDocFile ep fp)
              epPushExtractionStreaming ep config ext
              accumulate docNodeMapRef docEdgeAccRef ext
              mergeIntoRunning ext
              logProgress
              ) docFiles
            else do
              sem <- newQSemN docThreads
              let chunks = chunkList 500 docFiles
              mapM_ (\chunk -> do
                results <- mapConcurrently (\fp -> bracket_
                  (waitQSemN sem 1)
                  (signalQSemN sem 1)
                  (isolateFileExtraction lp fp "doc" (epExtractDocFile ep fp))) chunk
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
               results <- mapM (\src -> isolateFileExtraction lp (imageSourcePath src) "image" (extractImageSource appEnv config src)) chunk
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
                        then atomicModify paperStubRef (+ 1)
                        else atomicModify paperSuccessRef (+ 1)
                if paperThreadCount <= 1
                  then mapM_ (\fp -> do
                    ext <- isolateFileExtraction lp fp "pdf" (epExtractPdfFile ep config fp)
                    epPushExtractionStreaming ep config ext
                    accumulate paperNodeMapRef paperEdgeAccRef ext
                    mergeIntoRunning ext
                    recordResult ext
                    ) paperFiles
                   else do
                    sem <- newQSemN paperThreadCount
                    let chunks = chunkList 50 paperFiles
                    mapM_ (\chunk -> do
                      results <- mapConcurrently (\fp -> bracket_
                        (waitQSemN sem 1)
                        (signalQSemN sem 1)
                         (do ext <- isolateFileExtraction lp fp "pdf" (epExtractPdfFile ep config fp)
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

  -- Declarative import resolution (custom-extractors 6.1–6.4): a config
  -- `resolve:` entry (or the built-in registration it overrides) rewrites the
  -- external placeholder targets of the merged corpus to real module nodes.
  -- Runs before multi-source tagging so the rules see the on-disk paths.
  let resolveCfg = gcResolve (cfgGraphosConfig config)
  mapM_ (\msg -> lpLogWarn lp (T.pack ("  [resolve] WARNING: " ++ msg)))
        (resolveOverrideWarnings resolveCfg)
  (resolved, resolveStats) <- resolveExtraction absRoot resolveCfg merged
  when (resolveTotal resolveStats > 0) $
    logInfo $ T.pack $ "  [resolve] " ++ show (resolveTotal resolveStats)
      ++ " import target(s) resolved (package " ++ show (rsPackage resolveStats)
      ++ ", psr4 " ++ show (rsPSR4 resolveStats)
      ++ ", module-path " ++ show (rsModulePath resolveStats)
      ++ ", path-prefix " ++ show (rsPathPrefix resolveStats) ++ "; "
      ++ show (rsDropped resolveStats) ++ " placeholder node(s) dropped)"

  -- Cache hit/miss report (wire-incremental-update 4.3): how much of the code
  -- stage was served from the persistent extraction cache.
  let cacheHits = length cachedTSHits + length cachedCmdHits
      cacheMisses = length treeSitterFiles' + length commandFiles'
  when cacheEnabled $ unless (cacheHits == 0 && cacheMisses == 0) $
    logInfo $ T.pack $ "  [cache] reused " ++ show cacheHits ++ " file extraction(s), re-extracted "
                       ++ show cacheMisses

  logInfo $ T.pack $ "  Extracted " ++ show (Map.size (extractionNodes resolved)) ++ " nodes, " ++ show (Map.size (extractionEdges resolved)) ++ " edges"
  mapM_ (lpLogWarn lp) (extractionErrorSummaryLines (extractionErrors resolved))
  pure (applyTaggingSources mProvenance resolved)

-- | Group per-file extraction errors by extractor name, most frequent first
-- (custom-extractors 7.2). Ties are broken alphabetically so the summary is
-- stable across runs.
summariseExtractionErrors :: [ExtractionError] -> [(Text, Int)]
summariseExtractionErrors errs =
  sortBy compareCount $ Map.toList $ Map.fromListWith (+)
    [ (eeExtractor e, 1 :: Int) | e <- errs ]
  where
    compareCount (n1, c1) (n2, c2) =
      compare c2 c1 <> compare n1 n2

-- | Render the per-extractor error roll-up for the log and @GRAPH_REPORT.md@
-- (custom-extractors 7.2): one summary line naming each extractor and its
-- failure count, followed by at most five representative failures. An empty
-- list means a clean extraction, so nothing is rendered.
extractionErrorSummaryLines :: [ExtractionError] -> [Text]
extractionErrorSummaryLines [] = []
extractionErrorSummaryLines errs =
  [ "  [extract] " <> T.pack (show (length errs)) <> " file(s) recorded extraction errors ("
      <> T.intercalate ", " [ name <> ": " <> T.pack (show n)
                             | (name, n) <- summariseExtractionErrors errs ]
      <> ")"
  ] ++ map detail (take 5 errs)
    ++ [ "    … and " <> T.pack (show (length errs - 5)) <> " more"
       | length errs > 5 ]
  where
    detail e = "    " <> eeExtractor e <> " " <> T.pack (eeFile e) <> ": " <> eeMessage e

-- | Run one file's extraction in isolation: a synchronous failure becomes a
-- stub plus an 'ExtractionError' for that file, so one failing extractor can
-- never abort the build (custom-extractors 7.1, design D8). Thread
-- cancellation ('SomeAsyncException') is deliberately not caught.
isolateFileExtraction :: LoggingPort -> FilePath -> Text -> IO Extraction -> IO Extraction
isolateFileExtraction lp fp extractorName action = do
  result <- tryJust synchronousFailureText action
  case result of
    Right ext -> pure ext
    Left msg -> do
      lpLogWarn lp $ T.pack $ "  [" ++ T.unpack extractorName ++ "] " ++ fp ++ ": " ++ T.unpack msg
      pure (extractionFailure fp extractorName msg)

-- | The text of a synchronous failure, or 'Nothing' for thread cancellation.
synchronousFailureText :: SomeException -> Maybe Text
synchronousFailureText e = case fromException e :: Maybe SomeAsyncException of
  Just _  -> Nothing
  Nothing -> Just (T.pack (displayException e))

-- | A stub extraction recording one per-file failure (custom-extractors 7.1).
extractionFailure :: FilePath -> Text -> Text -> Extraction
extractionFailure fp extractorName msg =
  (extractionFromLists [makeStubNode fp] [])
    { extractionErrors = [ExtractionError fp extractorName msg] }

-- | Extract one LSP server group in isolation. A crash while serving a group
-- becomes a per-file stub with a recorded error for every file in that group,
-- so one misbehaving server cannot abort the build (custom-extractors 7.1).
extractGroupIsolated :: AppEnv -> FilePath -> PipelineConfig -> FileGroup -> IO [Extraction]
extractGroupIsolated appEnv absRoot config grp@(serverCmd, files) = do
  result <- tryJust synchronousFailureText (extractGroup appEnv absRoot config grp)
  case result of
    Right exts -> pure exts
    Left msg   -> pure [ extractionFailure fp (T.pack serverCmd) msg | fp <- files ]

-- | Path an 'ImageSource' names, used for per-file error reporting.
imageSourcePath :: ImageSource -> FilePath
imageSourcePath (StandaloneImage fp) = fp
imageSourcePath (EmbeddedImage archivePath mediaPath) = archivePath ++ "/" ++ mediaPath

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
        let kindText = case nodeKind n of
              Just k  -> toText k
              Nothing -> ""
            newNid = makeNodeId (mkSourceFile (T.unpack qPath) "") kindText (toText (nodeLabel n))
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

-- | Partition code files by their configured extractor mode, in declaration
-- order: @(tree-sitter, LSP, command, stub)@.
--
-- A tree-sitter entry carrying @grammar_path@ stays on the tree-sitter route
-- (custom-extractors 5.2): dispatch, fingerprinting and caching treat it as
-- tree-sitter, and only the loader selected inside
-- 'Graphos.UseCase.Extract.TreeSitter.extractViaTreeSitterFFI' differs.
--
-- Command entries get their own route (5.1): they are no longer stubbed, and
-- 'Extract/Core.hs' runs the declared argv through the command port.
partitionByExtractor :: PipelineConfig -> [FilePath] -> ([FilePath], [FilePath], [FilePath], [FilePath])
partitionByExtractor config files = foldr go ([], [], [], []) files
  where
    go fp (ts, lsp, cmd, stub) = case extractorForExt config (takeExtension fp) of
      ExtractTreeSitter -> (fp:ts, lsp, cmd, stub)
      ExtractLSP        -> (ts, fp:lsp, cmd, stub)
      ExtractCommand    -> (ts, lsp, fp:cmd, stub)
      ExtractStub       -> (ts, lsp, cmd, fp:stub)

-- | Group command-mode files by extension so each declared command gets its own
-- invocation plan: @batch@ spans the files of one extension only, and the
-- runner receives that extension's 'ExtractorConfig'.
commandFileGroups :: PipelineConfig -> [FilePath] -> [(ExtractorConfig, [FilePath])]
commandFileGroups config files =
  [ (ec, fps)
  | ext <- nub [ takeExtension fp | fp <- files ]
  , Just ec <- [ Map.lookup ext (gcExtractors (cfgGraphosConfig config)) ]
  , let fps = [ fp | fp <- files, takeExtension fp == ext ]
  ]

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

-- | Gate for external command extractors (custom-extractors 3.4).
--
-- Combines the @allow_command_extractors@ switch (CLI default 'True', library
-- default 'False') with the ignore patterns that must also stop a command from
-- running: the patterns loaded for the scan root plus the CLI @--ignore@
-- patterns. The runner consults it /before/ spawning any process.
commandGateFor :: PipelineConfig -> FilePath -> [AnnotatedPattern] -> CommandGate
commandGateFor config root patterns = CommandGate
  { cgEnabled  = cfgAllowCommandExtractors config
  , cgRoot     = root
  , cgPatterns = patterns ++ cfgIgnorePatterns config
  }

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

-- | Coverage summary for a 'partitionByExtractor' split of code files: how
-- many were routed to a real extractor (tree-sitter or LSP) versus fell
-- through to 'ExtractStub' because no 'defaultExtractors'/config entry
-- matched their extension, broken down by extension for the stubbed half.
data StubCoverage = StubCoverage
  { scDetected     :: Int
  , scStructural   :: Int
  , scStubbedByExt :: [(String, Int)]
  } deriving (Eq, Show)

-- | Build a 'StubCoverage' from the structural (tree-sitter ++ LSP) and
-- 'ExtractStub' file lists 'partitionByExtractor' already produces.
stubCoverage :: [FilePath] -> [FilePath] -> StubCoverage
stubCoverage structuralFiles stubFiles = StubCoverage
  { scDetected     = length structuralFiles + length stubFiles
  , scStructural   = length structuralFiles
  , scStubbedByExt = Map.toAscList $ Map.fromListWith (+)
      [ (takeExtension fp, 1 :: Int) | fp <- stubFiles ]
  }

-- | Fraction of stubbed code files above which 'stubCoverageWarning' states
-- that the graph likely under-represents the corpus.
stubDominanceThreshold :: Double
stubDominanceThreshold = 0.5

-- | The extension responsible for the most stubbed files, if any were.
dominantStubExt :: StubCoverage -> Maybe String
dominantStubExt sc = case scStubbedByExt sc of
  []  -> Nothing
  xs  -> Just . fst $ maximumBy (comparing snd) xs

-- | Render the INFO-level stub-coverage warning (extraction-quality:
-- "Stub extraction is a visible, reported outcome"), or 'Nothing' when no
-- code file was stubbed. Appends a dominance clause when the stubbed share
-- exceeds 'stubDominanceThreshold'.
stubCoverageWarning :: StubCoverage -> Maybe String
stubCoverageWarning sc
  | null (scStubbedByExt sc) = Nothing
  | otherwise = Just $
      show totalStubbed ++ " code files produced stub nodes only (no structure extracted): "
        ++ intercalate ", " [ ext ++ " (" ++ show n ++ ")" | (ext, n) <- scStubbedByExt sc ]
        ++ dominanceSuffix
  where
    totalStubbed = sum (map snd (scStubbedByExt sc))
    dominanceSuffix
      | scDetected sc > 0
      , fromIntegral totalStubbed / fromIntegral (scDetected sc) > stubDominanceThreshold
      , Just dom <- dominantStubExt sc
      = " -- the graph likely under-represents this corpus; dominant stubbed type: " ++ dom
      | otherwise = ""

-- | Canonical fingerprint text of every extraction-affecting configuration
-- value: the effective per-extension granularity under the CLI override, the
-- extractor mode per extension, the pdf extraction level, the command argv /
-- batch / timeout of every command extractor, the tree-sitter grammar name of
-- built-in entries, the runtime grammar library and query fingerprints, and
-- the declarative @resolve:@ entries. The key-sorting
-- makes the serialization canonical so identical configs always fingerprint
-- identically.
--
-- INV-CACHE-SOUND (AVI-521 §1.2): any future extraction-affecting config value
-- MUST be added here — an unfingerprinted config change would serve stale
-- cached extractions. Documented in the cache module header contract.
--
-- Runtime inputs (grammar shared library, @.scm@ queries, @source_of_truth@
-- index) are fingerprinted by their on-disk size and mtime, so replacing or
-- editing one invalidates every cached extraction that used it
-- (custom-extractors 5.3 / runtime-tree-sitter-grammar: "Grammar change
-- invalidates cache").
extractionConfigFingerprint :: PipelineConfig -> IO Text
extractionConfigFingerprint config = do
  extractorEntries <- mapM extractorEntry (Map.toAscList (gcExtractors gcfg))
  resolveEntries   <- mapM resolveEntry   (Map.toAscList (gcResolve gcfg))
  pure . T.pack . unwords $
    concat
      [ [ "granularity=" ++ show (gcGranularity gcfg)
        , "cliGranularity=" ++ maybe "none" show (cfgGranularity config)
        , "pdf=" ++ pdfLevelName (gcPdfExtraction gcfg)
          -- bump when tree-sitter extraction output changes for unchanged files
        , "treesitter-imports=go,rust,ruby:1"
        , "jsxGrammar=tsx-aa950f5"  -- .tsx/.js/.jsx/.mjs/.cjs parsed with the vendored TSX grammar
          -- bump when tree-sitter node ids/labels/sig scheme changes (AVI-1452)
        , "nodeIdentity=av1452-bounded-ids"
        , "idScheme=3"  -- 3: nameless definitions keyed `anonymous`, nameless const/var lists not emitted
          -- command extractors change the output of an unchanged file when the
          -- gate or their invocation plan changes (custom-extractors 5.3)
        , "allowCommand=" ++ show (cfgAllowCommandExtractors config)
        , "commandExtract=1"  -- bump when command id-bucketing / decoding changes
        ]
      , extractorEntries
      , resolveEntries
      ]
  where
    gcfg = cfgGraphosConfig config
    pdfLevelName PdfSmall  = "small"
    pdfLevelName PdfMedium = "medium"
    pdfLevelName PdfLarge  = "large"

    extractorEntry (ext, ec) = do
      extra <- modeExtra ec
      pure $ "extractor." ++ ext ++ "=" ++ show (ecMode ec)
        ++ ";granularity=" ++ maybe "auto" show (ecGranularity ec)
        ++ extra

    -- Mode-specific fields that change extraction output for unchanged bytes.
    modeExtra ec = case ecMode ec of
      ExtractCommand -> pure $
        ";command=" ++ show (ecCommand ec)
          ++ ";batch=" ++ maybe "false" show (ecBatch ec)
          ++ ";timeout=" ++ maybe "default" show (ecTimeoutSeconds ec)
      ExtractTreeSitter -> do
        grammarStamp <- maybe (pure "none") fileStamp (ecGrammarPath ec)
        queriesStamp <- maybe (pure "none") queryDirStamp (ecQueries ec)
        pure $ ";grammar=" ++ builtinGrammar ec
          ++ ";grammarPath=" ++ maybe "none" id (ecGrammarPath ec)
          ++ ";grammarStamp=" ++ grammarStamp
          ++ ";queries=" ++ maybe "none" id (ecQueries ec)
          ++ ";queriesStamp=" ++ queriesStamp
      _ -> pure ""

    -- The declared @grammar@ name selects the compiled-in binding when no
    -- runtime library is configured ('grammarForFile' → 'getGrammarPtr'), so
    -- changing it changes the parse of unchanged bytes and must invalidate the
    -- cache. With a @grammar_path@ the name is bypassed entirely
    -- ('parseWithConfiguredGrammar' loads the runtime library instead), so it
    -- is deliberately not fingerprinted there — renaming a runtime grammar's
    -- label is not an extraction-affecting change.
    builtinGrammar ec = case ecGrammarPath ec of
      Just _  -> "runtime"
      Nothing -> maybe "none" id (ecGrammar ec)

    -- One stamp per recognised query file, in a fixed order, so adding,
    -- deleting or editing any of them changes the fingerprint.
    queryDirStamp dir = do
      stamps <- mapM (\kind -> fileStamp (dir </> queryKindFile kind))
                     [ QueryDefinitions, QueryImports, QueryCalls ]
      pure (intercalate "," stamps)

    resolveEntry (ext, rc) = do
      sotStamp <- maybe (pure "none") fileStamp (rcSourceOfTruth rc)
      pure $ "resolve." ++ ext ++ "=" ++ show (rcKind rc)
        ++ ";roots=" ++ intercalate "," (rcRoots rc)
        ++ ";sourceOfTruth=" ++ maybe "none" id (rcSourceOfTruth rc)
        ++ ";sourceOfTruthStamp=" ++ sotStamp

-- | Size+mtime stamp of a runtime input file (grammar library, @.scm@ query,
-- resolve source-of-truth index). A missing file fingerprints as @missing@, so
-- creating or deleting it always changes the extraction fingerprint.
fileStamp :: FilePath -> IO String
fileStamp raw = do
  path <- resolveRuntimePath raw
  exists <- doesFileExist path
  if not exists
    then pure "missing"
    else do
      st <- getFileStatus path
      pure $ show (fileSize st) ++ "@"
        ++ show (realToFrac (modificationTime st) :: Double)

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
  fingerprint <- extractionConfigFingerprint config
  let (tsFiles, lspFiles, commandFiles, stubFiles) = partitionByExtractor config changedFiles

  tsExtractions <- mapM (\fp -> do
    ext <- isolateFileExtraction lp fp (T.pack (grammarForFile config fp)) $
             extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarSelectionForFile config fp) fp
    when (null (extractionErrors ext)) $
      fspSaveCachedExtraction fsp fingerprint fp ext (cfgOutputDir config)
    pure ext) tsFiles
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) tsExtractions

  let fileGroups = groupByLSPServer (epLanguageServerCommands ep) lspFiles
  lspExtractions <- concatMapM (extractGroupIsolated appEnv absRoot config) fileGroups
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) lspExtractions

  -- Command route (custom-extractors 5.1): same gate, same cache write-through
  -- for successful outputs as the full extraction path.
  commandIgnorePatterns <- if null commandFiles
    then pure []
    else fspLoadIgnorePatterns fsp absRoot
  let commandGate = commandGateFor config absRoot commandIgnorePatterns
  commandExtractions <- fmap concat $ forM (commandFileGroups config commandFiles) $ \(ec, fps) -> do
    outcomes <- epExtractCommandFiles ep commandGate ec fps
    forM fps $ \fp -> case Map.lookup fp outcomes of
      Just (CommandProduced ext) -> do
        fspSaveCachedExtraction fsp fingerprint fp ext (cfgOutputDir config)
        pure ext
      Just (CommandFailed err) -> do
        lpLogWarn lp $ T.pack $ "  [command] " ++ fp ++ ": " ++ T.unpack (eeMessage err)
        pure ((extractionFromLists [makeStubNode fp] []) { extractionErrors = [err] })
      Just (CommandSkipped reason) -> do
        logDebug $ T.pack $ "  [command] " ++ fp ++ ": skipped (" ++ T.unpack reason ++ ")"
        pure (extractionFromLists [makeStubNode fp] [])
      Nothing ->
        pure ((extractionFromLists [makeStubNode fp] [])
               { extractionErrors = [ ExtractionError fp "command"
                   "the extractor reported no outcome for this file" ] })
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) commandExtractions

  stubExtractions <- mapM (\fp -> do
    logDebug $ T.pack $ "  [stub] " ++ fp
    let ext = extractionFromLists [makeStubNode fp] []
    fspSaveCachedExtraction fsp fingerprint fp ext (cfgOutputDir config)
    pure ext
    ) stubFiles
  mapM_ (\ext -> epPushExtractionStreaming ep config ext) stubExtractions

  let merged = List.foldl' mergeExtractions emptyExtraction
                  (tsExtractions ++ lspExtractions ++ commandExtractions ++ stubExtractions)
  -- Declarative import resolution over the batch of changed files (6.4).
  -- The precedence warning is emitted here too (6.3), so a watch/--update run
  -- surfaces the same "config overrides built-in resolver" notice as a full
  -- build instead of silently using a different resolver than it reports.
  let resolveCfg = gcResolve (cfgGraphosConfig config)
  mapM_ (\msg -> lpLogWarn lp (T.pack ("  [resolve] WARNING: " ++ msg)))
        (resolveOverrideWarnings resolveCfg)
  (resolved, resolveStats) <- resolveExtraction absRoot resolveCfg merged
  when (resolveTotal resolveStats > 0) $
    logInfo $ T.pack $ "  [resolve] " ++ show (resolveTotal resolveStats)
      ++ " import target(s) resolved in the changed-file batch"
      ++ " (package " ++ show (rsPackage resolveStats)
      ++ ", psr4 " ++ show (rsPSR4 resolveStats)
      ++ ", module-path " ++ show (rsModulePath resolveStats)
      ++ ", path-prefix " ++ show (rsPathPrefix resolveStats) ++ "; "
      ++ show (rsDropped resolveStats) ++ " placeholder node(s) dropped)"
  logInfo $ T.pack $ "  [watch] Extracted " ++ show (Map.size (extractionNodes resolved)) ++ " nodes, " ++ show (Map.size (extractionEdges resolved)) ++ " edges from " ++ show (length changedFiles) ++ " changed files"
  mapM_ (lpLogWarn lp) (extractionErrorSummaryLines (extractionErrors resolved))
  pure resolved

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
   let (tsFiles, _, commandFiles, stubFiles) = partitionByExtractor config [fp]
   ext <- if not (null tsFiles)
         then extractViaTreeSitterFFI appEnv (granularityForFile config fp) (grammarSelectionForFile config fp) fp
       else if not (null commandFiles)
         -- A collapsed command-mode file is represented by its stub node:
         -- counting does not spawn the external extractor a second time.
         then pure (extractionFromLists [makeStubNode fp] [])
       else if not (null stubFiles)
         then pure (extractionFromLists [makeStubNode fp] [])
        else do
          absRoot <- canonicalizePath (cfgInputPath config)
          let groups = groupByLSPServer (epLanguageServerCommands (extractionPort appEnv)) [fp]
          extractions <- mapM (extractGroupIsolated appEnv absRoot config) groups
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