-- | Core pipeline orchestration — the full detect→extract→build→cluster→export flow.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Pipeline.Core
  ( runPipeline
  , runClusterOnlyPipeline
  , PipelineResult(..)
  , ClusterOutput(..)
  , clusterGraph
  , edgeCollapseThreshold
  , generateGraphEmbeddings
  , writeEmbeddingsSidecar
  , logSemanticInference
  ) where

import Control.Concurrent.STM
  ( atomically
  , newTBQueueIO, readTBQueue, writeTBQueue
  )
import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (deepseq)
import Control.Exception (catch, SomeException, evaluate, bracket)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (toJSON, encode)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text.Short (toText)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (createDirectoryIfMissing)
import System.Mem (performGC)
import qualified Data.ByteString.Lazy as BSL
import System.IO (Handle, hFlush)

import Graphos.Domain.Types hiding (PushMode(..))
import Graphos.Domain.Types.Pipeline (Neo4jStreamingConfig(..), PipelineStep(..), PipelineCheckpoint(..))
import Graphos.Domain.Config (FileExtensionConfig(..), SemanticEdgesConfig(..))
import Graphos.Domain.Config.Detection (DetectionConfig(..), DetectionMode(..), applyDetectionOverrides)
import Graphos.Domain.Embedding (prepare)
import Graphos.Domain.Graph (Graph, gNodes, gEdges, gCompositions, gEmbeddings, gEmbeddingsPath, addEdges)
import Graphos.Domain.Community (computeCompositions, Resolution(..), MergeStrategy(..))
import qualified Graphos.Domain.Graph.Analysis as GAnalysis
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.LLMPort (LLMPort(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.UseCase.Port.ObservabilityPort (ObservabilityPort(..), StartTime(..), EndTime(..))
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..))
import Graphos.Infrastructure.FileSystem.Ignore (apPriority)
import Graphos.Infrastructure.FileSystem.AtomicWrite
  ( writeFileAtomic
  , commitAtomicHandle
  , discardAtomicHandle
  , openAtomicHandle
  )
import qualified Data.Aeson.Key as AKey (fromText)
import Graphos.Infrastructure.LLM.EmbeddingCache (loadVector, saveVector)
import Graphos.UseCase.Pipeline.Staging
  ( withStagedOutput
  , relocateStagedPath
  )
import qualified Graphos.UseCase.Port.ExportPort as UEP
import Graphos.UseCase.Port.ExportPort (ExportPort(..))
import Graphos.UseCase.Detect (detectFilesWithExtensionsAndIgnore')
import Graphos.UseCase.Extract (extractAll, collapseDetectedFiles)
import Graphos.UseCase.Build (buildGraphFromExtractions)
import Graphos.UseCase.Cluster (clusterGraphWithResolution, joinCommunitiesToNodes, computeCommunityAggregates)
import Graphos.UseCase.Analyze (analyzeGraph)
import Graphos.UseCase.Infer (inferNonSemanticEdgesWith, inferSemanticEdgesForMode, semanticMode, semanticModeName, SemanticMode(..))
import Graphos.UseCase.Report (generateReport)
import Graphos.UseCase.Label (labelCommunities)
import Graphos.UseCase.Load (validateGraphFile, corruptGraphMessage)
import Graphos.Domain.Labeling (LabelingResult(..))

-- | Minimum ratio of edges to nodes for a code-dominant graph. Values below
-- this threshold after the build step indicate a likely edge-extraction
-- collapse and are logged as a prominent warning.
edgeCollapseThreshold :: Double
edgeCollapseThreshold = 0.05

-- | Generate embeddings for all nodes in a graph.
--
-- Text per node is @nodeLabel <> " " <> nodeSourceFile@ (unchanged from the
-- sequential loop). Embedded texts are deduplicated and served through the
-- content-addressed disk cache ('loadVector', keyed over the prepared text):
-- only cache misses go on the wire, chunked by 'embBatchSize'; only fresh
-- (miss) vectors are written back to the cache — hits are read-only (D5).
-- A failing chunk degrades per-input (D2 bisection): its own text gets no
-- vector, siblings keep theirs. The cache is a pure optimization: with a
-- sound cache the assignment is baseline-equal (@runCached_fst@ in the Lean
-- artifact).
--
-- Sidecar write (D9): with 'embStreaming' (default True) the sidecar at
-- @sidecarPath@ is built by appending each completed batch's entries to a
-- staged file (created via 'openAtomicHandle') which is atomically renamed
-- into place when the pass completes; a caught abort discards the staged
-- file without renaming, leaving the prior sidecar intact (and a process
-- kill leaves a temp file, never a partial final path — the
-- atomic-output-writes convention). With 'embStreaming == False' the
-- sidecar is assembled in memory and written once at the end
-- ('writeEmbeddingsSidecar', today's behavior). Both paths produce the same
-- /content/ (a JSON object node-id → vector); batch completion order may
-- differ from write-at-end key order, which is immaterial for JSON objects.
generateGraphEmbeddings :: LLMPort -> EmbeddingConfig -> Graph -> FilePath -> FilePath -> IO (Map NodeId [Double])
generateGraphEmbeddings llm cfg graph cacheRoot sidecarPath = do
  let nodes = Map.elems (gNodes graph)
      -- Assignment text per node (unchanged from the sequential loop).
      nodeTexts = [ (nodeId n, toText (nodeLabel n) <> " " <> toText (nodeSourceFile n))
                  | n <- nodes ]
      -- Dedup: embed each unique text once (Set.toList of Set.fromList keeps
      -- ascending order; the assignment is order-independent per
      -- pipeline_keys_irrelevant).
      uniqueTexts = Set.toList (Set.fromList (map snd nodeTexts))
  -- Cache lookup for every unique text — over the *prepared* text (D4):
  -- the key hashes (model, docPrefix, prepared), so two raw texts that
  -- prepare identically converge on one entry.
  cachedVecs <- mapM (loadVector cacheRoot (T.pack (embModel cfg)) (embDocPrefix cfg) . prepare cfg) uniqueTexts
  let cachedTable = Map.fromList [ (t, v) | (t, Just v) <- zip uniqueTexts cachedVecs ]
      misses = [ t | (t, Nothing) <- zip uniqueTexts cachedVecs ]
      chunks = chunkBy (max 1 (embBatchSize cfg)) misses
  lpLogDebug (loggingPortOf llm) $ T.pack $
    "  Embedding " ++ show (length nodeTexts) ++ " nodes -> "
      ++ show (length uniqueTexts) ++ " unique texts ("
      ++ show (Map.size cachedTable) ++ " cached, "
      ++ show (length misses) ++ " to embed) in "
      ++ show (length chunks) ++ " batch(es) of <= " ++ show (embBatchSize cfg)
  -- Nodes served straight from the cache (no API call): their entries join
  -- the sidecar up front in both paths.
  let cachedEntries =
        [ (nid, v)
        | (nid, t) <- nodeTexts
        , Just v <- [Map.lookup t cachedTable]
        ]
      -- Nodes whose text is a miss, grouped by chunk index so each batch's
      -- completion maps back to its nodes.
      missNodesByChunk =
        [ [ (nid, t)
          | (nid, t) <- nodeTexts
          , t `Set.member` Set.fromList c
          ]
        | c <- chunks
        ]
  if embStreaming cfg
    then
      -- Streaming path (D9): staged append per batch + atomic rename. The
      -- bracket's cleanup discards the staged temp file; by the time cleanup
      -- runs on the success path the file has already been renamed away
      -- (discard's removeFile is best-effort and ignores the missing temp).
      bracket
        (openAtomicHandle sidecarPath)
        (\(tmpPath, h) -> discardAtomicHandle tmpPath h)
        (\(tmpPath, h) -> do
            BSL.hPut h "{"
            seenRef <- newIORef False
            -- Cache-hit entries append first (they are already in hand).
            writeEntries seenRef h cachedEntries
            -- Embed only the misses; each completed batch appends its own
            -- entries. A failed batch contributes nothing for its texts.
            chunkResults <- pooledMapConcurrently'
                              (max 1 (embConcurrency cfg))
                              (\c nodes' -> appendChunk seenRef h c nodes' llm cfg)
                              (zip chunks missNodesByChunk)
            let freshTable = Map.fromList (concat chunkResults) :: Map Text [Double]
                vecTable = Map.union freshTable cachedTable
            writeBackFresh cfg cacheRoot freshTable
            -- Close the JSON object and flush; commitSidecar fsyncs + renames.
            BSL.hPut h "}"
            hFlush h
            let assignment = Map.fromList
                  [ (nid, v)
                  | (nid, t) <- nodeTexts
                  , Just v <- [Map.lookup t vecTable]
                  ]
            -- Atomic rename into place (D9): only reached on the success
            -- path — an exception inside the body skips it entirely, so the
            -- prior sidecar is untouched and the temp file is discarded.
            commitSidecar tmpPath sidecarPath h
            pure assignment
        )
    else do
      -- Write-at-end path (today's behavior): assemble, then write once.
      chunkResults <- pooledMapConcurrently (max 1 (embConcurrency cfg))
                                            (embedChunk llm cfg) chunks
      let freshTable = Map.fromList (concat chunkResults) :: Map Text [Double]
          vecTable = Map.union freshTable cachedTable
          assignment = Map.fromList
            [ (nid, v)
            | (nid, t) <- nodeTexts
            , Just v <- [Map.lookup t vecTable]
            ]
      writeEmbeddingsSidecar sidecarPath assignment
      writeBackFresh cfg cacheRoot freshTable
      pure assignment

-- | Append @(node, vector)@ pairs to a handle as comma-separated JSON
-- object members, using an IORef "seen a member yet?" flag shared by every
-- append into the same staged object (cache-hit entries first, then each
-- completed batch). This keeps @{@ ++ members-with-comma-between ++ @}@
-- syntactically valid regardless of how many batches append, in what order,
-- or how many succeed.
writeEntries :: IORef Bool -> Handle -> [(NodeId, [Double])] -> IO ()
writeEntries seenRef h entries =
  mapM_ one entries
  where
    one (nid, v) = do
      seen <- readIORef seenRef
      BSL.hPut h (commaPrefix seen <> encode (AKey.fromText nid) <> ":" <> encode v)
      writeIORef seenRef True
      where commaPrefix seen = if seen then BSL.singleton 44 else BSL.empty

-- | Embed one chunk (its unique texts) with D2 bisection and append the
-- chunk's nodes' entries to the staged sidecar handle as the batch completes
-- (streaming, D9). Returns the batch's fresh @(text, vector)@ pairs for
-- cache write-back.
appendChunk :: IORef Bool -> Handle -> [Text] -> [(NodeId, Text)] -> LLMPort -> EmbeddingConfig -> IO [(Text, [Double])]
appendChunk seenRef h chunkTexts chunkNodes llm cfg = do
  fresh <- embedChunk llm cfg chunkTexts
  writeEntries seenRef h [ (nid, v) | (nid, v) <- distribute chunkNodes fresh ]
  pure fresh
  where
    distribute nodes fresh =
      [ (nid, v)
      | (nid, t) <- nodes
      , Just v <- [lookup t fresh]
      ]

-- | Rename the staged sidecar into place after fsync (the @}@ is already
-- written and flushed by the caller).
commitSidecar :: FilePath -> FilePath -> Handle -> IO ()
commitSidecar = commitAtomicHandle

-- | Write back only the fresh (miss) vectors — hits are read-only (D5),
-- so a warm re-run performs zero cache writes.
writeBackFresh :: EmbeddingConfig -> FilePath -> Map Text [Double] -> IO ()
writeBackFresh cfg cacheRoot freshTable =
  mapM_ (\(t, v) -> saveVector cacheRoot (T.pack (embModel cfg)) (embDocPrefix cfg) (prepare cfg t) v)
        (Map.toList freshTable)

-- | Embed one chunk of unique texts with per-input failure isolation
-- (design D2): on a whole-request failure with more than one text, the batch
-- is bisected and the halves retried independently (recursion depth
-- ≤ log2(batchSize)); at batch size 1 the failing text is logged (first 200
-- chars) with its error and contributes no vector — its siblings' vectors
-- survive regardless of where the culprit sits in the batch.
embedChunk :: LLMPort -> EmbeddingConfig -> [Text] -> IO [(Text, [Double])]
embedChunk llm cfg chunk = case chunk of
  []  -> pure []
  [t] -> do
    r <- lpGenerateEmbeddings llm cfg [t]
    case r of
      Left err -> do
        -- Per-input failure, reported (never silently dropped): the text's
        -- truncated form (first 200 chars) and the error go to the log.
        lpLogWarn (loggingPortOf llm) $ T.pack $
          "embedding failed for input (batch-size 1, no vector): "
            ++ take 200 (T.unpack t) ++ " — " ++ T.unpack err
        pure []
      Right vecs -> pure (zip chunk vecs)
  _ -> do
    r <- lpGenerateEmbeddings llm cfg chunk
    case r of
      Right vecs -> pure (zip chunk vecs)
      Left _ -> do
        let (half1, half2) = splitAt (length chunk `div` 2) chunk
        (<>) <$> embedChunk llm cfg half1
             <*> embedChunk llm cfg half2

-- | Split a list into consecutive chunks of at most @n@ elements
-- (list-level analogue of @T.chunksOf@; @n >= 1@).
chunkBy :: Int -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy n xs = let (c, rest) = splitAt n xs in c : chunkBy n rest

-- | Map an IO action over a list with at most @limit@ actions in flight
-- (a bounded worker pool over the input items, each producing a list of
-- results). @limit 1@ runs sequentially in input order. An exception in one
-- worker is caught and degrades that task to an empty contribution, so one
-- failed batch never cancels its siblings.
pooledMapConcurrently :: Int -> (a -> IO [(Text, [Double])]) -> [a] -> IO [[(Text, [Double])]]
pooledMapConcurrently limit action items
  | limit <= 1 = mapM safeAction items
  | otherwise = do
      queue <- newTBQueueIO (fromIntegral (max 1 (length items)) + fromIntegral limit)
      atomically $ mapM_ (writeTBQueue queue . Just) items
      -- One sentinel per worker: every worker drains until it sees its own
      -- Nothing, so none can block forever on an empty queue.
      atomically $ mapM_ (writeTBQueue queue) (replicate limit Nothing)
      concat <$> mapConcurrently (\_ -> worker queue) [1 .. limit]
      where
        safeAction x = action x `catch` \(_ :: SomeException) -> pure []
        worker queue = do
          mItem <- atomically (readTBQueue queue)
          case mItem of
            Nothing -> pure []
            Just x  -> do
              r <- safeAction x
              rest <- worker queue
              pure (r : rest)

-- | Generalized worker pool over paired inputs (streaming variant): items
-- carry their own payload, the action receives @(item, context)@ and returns
-- a list of results; an exception in one worker degrades that task to an
-- empty contribution. @limit 1@ runs sequentially in input order.
pooledMapConcurrently' :: Int -> (a -> b -> IO [c]) -> [(a, b)] -> IO [[c]]
pooledMapConcurrently' limit action items
  | limit <= 1 = mapM safeAction items
  | otherwise = do
      queue <- newTBQueueIO (fromIntegral (max 1 (length items)) + fromIntegral limit)
      atomically $ mapM_ (writeTBQueue queue . Just) items
      atomically $ mapM_ (writeTBQueue queue) (replicate limit Nothing)
      concat <$> mapConcurrently (\_ -> worker queue) [1 .. limit]
      where
        safeAction (x, y) = action x y `catch` \(_ :: SomeException) -> pure []
        worker queue = do
          mItem <- atomically (readTBQueue queue)
          case mItem of
            Nothing -> pure []
            Just x  -> do
              r <- safeAction x
              rest <- worker queue
              pure (r : rest)

-- | Placeholder accessor: LLMPort carries no logging port; debug noise is
-- routed through the module-level no-op unless wired otherwise.
loggingPortOf :: LLMPort -> LoggingPort
loggingPortOf _ = LoggingPort
  { lpLogTrace = \_ -> pure ()
  , lpLogDebug = \_ -> pure ()
  , lpLogInfo  = \_ -> pure ()
  , lpLogWarn  = \_ -> pure ()
  , lpLogError = \_ -> pure ()
  }

-- | Write the embeddings map to a JSON sidecar file (object: node id -> vector).
writeEmbeddingsSidecar :: FilePath -> Map NodeId [Double] -> IO ()
writeEmbeddingsSidecar path embs = writeFileAtomic path (encode embs)

-- | Log the semantic edge inference decision (mode + inferred count) for the current run.
logSemanticInference :: LoggingPort -> SemanticEdgesConfig -> SemanticMode -> [Edge] -> IO ()
logSemanticInference lp se mode semanticEdges = do
  when (mode == SemanticFallback) (lpLogInfo lp "semantic inference capped at 10K code nodes, falling back to literal-name inference")
  when (mode == SemanticAutoSkip) (lpLogInfo lp "single-corpus graph detected, skipping semantic edge inference")
  lpLogInfo lp $ T.concat
    [ "semantic edges: inferred ", T.pack (show (length semanticEdges))
    , " (cap=", T.pack (show (seMaxFanOut se))
    , ", threshold=", T.pack (show (seThreshold se))
    , ", mode=", semanticModeName mode, ")"
    ]

-- | Pipeline result
data PipelineResult = PipelineResult
  { prNodes        :: Int
  , prEdges        :: Int
  , prCommunities  :: Int
  , prReportPath   :: FilePath
  , prGraphPath    :: FilePath
  , prHtmlPath     :: Maybe FilePath
  , prNeo4jPath   :: Maybe FilePath
  } deriving (Eq, Show)

-- | Result of the shared clustering / analysis stage. Both the full pipeline
-- and the cluster-only entry point use this to avoid duplicating Steps 4-5.
data ClusterOutput = ClusterOutput
  { coEnrichedGraph :: Graph  -- ^ Graph ready for report + export (communities/analysis baked in)
  , coGraphToWrite  :: Graph  -- ^ Graph whose nodes+edges are written incrementally to graph.json
  , coCommunities   :: CommunityMap
  , coCohesion      :: CohesionMap
  , coAnalysis      :: Analysis
  , coLabels        :: Maybe (Map CommunityId Text)
  , coAggregates    :: [CommunityAggregate]
  } deriving (Eq, Show)

-- | Run the full pipeline.
--
-- The build/export phase writes into a staging directory which is swapped
-- into the real output directory only on success, so an interrupted or
-- failed rebuild never destroys the previous output.
runPipeline :: AppEnv -> PipelineConfig -> IO (Either Text PipelineResult)
runPipeline appEnv config =
  let finalOutputDir = cfgOutputDir config
      graphPath = finalOutputDir ++ "/graph.json"
  in validateStartupGraph (cfgStrictGraph config) graphPath >>= \startup ->
     case startup of
       Left err -> pure (Left (corruptGraphMessage graphPath err))
       Right () -> withStagedOutput finalOutputDir $ \stagingDir -> do
         let stagedConfig = config { cfgOutputDir = stagingDir }
         result <- runPipelineBody appEnv stagedConfig
         pure $ fmap (relocateResult finalOutputDir stagingDir) result
   where
     relocateResult final staging pr = pr
       { prReportPath = relocateStagedPath staging final (prReportPath pr)
       , prGraphPath  = relocateStagedPath staging final (prGraphPath pr)
       , prHtmlPath   = relocateStagedPath staging final <$> prHtmlPath pr
       , prNeo4jPath  = relocateStagedPath staging final <$> prNeo4jPath pr
       }

-- | Validate an existing graph.json at startup before any staging or
-- destructive work begins. Skipped when strict mode is disabled
-- (--no-strict-graph); otherwise fails with a clear message naming the path
-- and a recovery hint when the file is corrupt.
validateStartupGraph :: Bool -> FilePath -> IO (Either Text ())
validateStartupGraph strict graphPath =
  if strict
    then validateGraphFile graphPath
    else pure (Right ())

-- | The pipeline body, writing all artifacts under @cfgOutputDir@ (which the
-- caller points at a staging directory during a staged rebuild).
runPipelineBody :: AppEnv -> PipelineConfig -> IO (Either Text PipelineResult)
runPipelineBody appEnv config = catch (do
  let lp = loggingPort appEnv
      op = observabilityPort appEnv
      fsp = fileSystemPort appEnv
      ep = exportPort appEnv

  let configWithStreaming = case (cfgNeo4j config, cfgNeo4jPush config) of
        (True, Just uri) -> config { cfgNeo4jStreaming = Just Neo4jStreamingConfig
                                              { neo4jsUri = uri
                                              , neo4jsUser = T.pack (neo4jUser (gcNeo4j (cfgGraphosConfig config)))
                                              , neo4jsPassword = T.pack (neo4jPassword (gcNeo4j (cfgGraphosConfig config)))
                                              } }
        (True, Nothing) -> let n4cfg = gcNeo4j (cfgGraphosConfig config)
                            in config { cfgNeo4jStreaming = Just Neo4jStreamingConfig
                                              { neo4jsUri = T.pack (neo4jUri n4cfg)
                                              , neo4jsUser = T.pack (neo4jUser n4cfg)
                                              , neo4jsPassword = T.pack (neo4jPassword n4cfg)
                                              } }
        _ -> config

  let isFresh = cfgFresh configWithStreaming
  when isFresh (lpLogInfo lp "Checkpoint recovery disabled (--fresh); starting fresh")
  when (not isFresh) (do
    mCheckpoint <- fspLoadCheckpoint fsp (cfgOutputDir configWithStreaming)
    case mCheckpoint of
      Just chk -> lpLogInfo lp $ T.pack $ "Resuming from checkpoint: step " ++ show (chkCurrentStep chk)
                                    ++ ", " ++ show (length (chkFilesExtracted chk)) ++ " files already extracted"
      Nothing -> lpLogInfo lp "No checkpoint found, starting fresh pipeline")

  lpLogInfo lp "Step 1: Detecting files..."
  detectStart <- getCurrentTime
  ignorePatterns <- fspLoadIgnorePatterns fsp (cfgInputPath configWithStreaming)
  let cliPatterns = cfgIgnorePatterns configWithStreaming
      allIgnorePatterns = ignorePatterns ++ cliPatterns
      inputRoot = cfgInputPath configWithStreaming
      gitignoreCount = length (filter (\ap -> apPriority ap == 1) ignorePatterns)
      graphosignoreCount = length (filter (\ap -> apPriority ap == 2) ignorePatterns)
      cliCount = length cliPatterns
  lpLogInfo lp $ T.pack $ "Loaded " ++ show gitignoreCount ++ " ignore patterns from " ++ inputRoot ++ "/.gitignore"
  lpLogInfo lp $ T.pack $ "Loaded " ++ show graphosignoreCount ++ " ignore patterns from " ++ inputRoot ++ "/.graphosignore"
  lpLogInfo lp $ T.pack $ "Loaded " ++ show cliCount ++ " ignore patterns from --ignore"
  let fec = gcFileExtensions (cfgGraphosConfig configWithStreaming)
      extMap = Map.fromList
        [ (CodeFiles, fecCode fec)
        , (DocFiles, fecDoc fec)
        , (PaperFiles, fecPaper fec)
        , (ImageFiles, fecImage fec)
        , (VideoFiles, fecVideo fec)
        , (OfficeFiles, fecOffice fec)
        ]
  let baseDetection    = gcDetection (cfgGraphosConfig configWithStreaming)
      effectiveMode    = if cfgDetectDisabled configWithStreaming
                           then Off
                           else maybe (dcMode baseDetection) id (cfgDetectionMode configWithStreaming)
      effectiveDetection =
        case applyDetectionOverrides baseDetection effectiveMode (cfgMinifiedThreshold configWithStreaming) of
          Right ok -> ok
          Left err -> error $ "graphos: invalid detection config: " ++ err
  detection <- detectFilesWithExtensionsAndIgnore' fsp effectiveDetection (cfgInputPath configWithStreaming) extMap allIgnorePatterns (lpLogDebug lp)
  detectEnd <- getCurrentTime
  opRecordHistogram op "graphos_pipeline_step_duration_seconds" (realToFrac (diffUTCTime detectEnd detectStart) :: Double)
  opIncCounter op "graphos_pipeline_steps_total" 1
  opDebugTraceSpan op "detect" (StartTime detectStart) (EndTime detectEnd) Map.empty
  if null (allFiles detection)
    then pure $ Left "No supported files found"
    else do
      let excs = detectionExclusions detection
          totalExcluded = excRootAnchored excs + excDepthIndependent excs + excGitignore excs + excGraphosignore excs + excUnexplained excs
          ignoredFiles = excIgnoredFiles excs
      lpLogInfo lp $ T.pack $ "Ignored " ++ show ignoredFiles ++ " files"
      lpLogInfo lp $ T.pack $ "  Found " ++ show (detectionTotalFiles detection) ++ " files"
      lpLogInfo lp $ T.pack $ "  Detected (classification): " ++
        intercalate ", " [show (length fs) ++ " " ++ show c
         | (c, fs) <- Map.toList (detectionClassification detection), not (null fs)]
      lpLogDebug lp $ T.pack $ "  File categories: " ++ show (Map.keys (detectionFiles detection))
      lpLogTrace lp $ T.pack $ "  Code files: " ++ show (Map.findWithDefault [] CodeFiles (detectionFiles detection))
      when (totalExcluded > 0) $ do
        lpLogInfo lp $ T.pack $ "  Excluded " ++ show totalExcluded ++ " directories:"
        when (excRootAnchored excs > 0) $
          lpLogInfo lp $ T.pack $ "    root-anchored build output: " ++ show (excRootAnchored excs)
        when (excDepthIndependent excs > 0) $
          lpLogInfo lp $ T.pack $ "    depth-independent tooling: " ++ show (excDepthIndependent excs)
        when (excGitignore excs > 0) $
          lpLogInfo lp $ T.pack $ "    .gitignore: " ++ show (excGitignore excs)
        when (excGraphosignore excs > 0) $
          lpLogInfo lp $ T.pack $ "    .graphosignore: " ++ show (excGraphosignore excs)
        when (excUnexplained excs > 0) $
          lpLogInfo lp $ T.pack $ "    unexplained: " ++ show (excUnexplained excs)

      now <- getCurrentTime
      let pipelineId = T.pack $ show now
          checkpoint = PipelineCheckpoint
                        { chkPipelineId = pipelineId
                        , chkCurrentStep = StepExtract
                        , chkCompletedSteps = [StepDetect]
                        , chkFilesExtracted = []
                        , chkFilesPushedNeo4j = []
                        , chkStartedAt = T.pack $ show now
                        }
      fspSaveCheckpoint fsp (cfgOutputDir configWithStreaming) checkpoint

      lpLogInfo lp "Step 2: Extracting entities and relationships..."
      extractStart <- getCurrentTime
      extraction <- extractAll appEnv configWithStreaming detection
      extractEnd <- getCurrentTime
      collapsedNodes <- if dcMode effectiveDetection == Collapse
            then collapseDetectedFiles appEnv configWithStreaming (detectionClassification detection)
            else pure []
      lpLogInfo lp $ T.pack $ "  Collapsed " ++ show (length collapsedNodes) ++ " detected file(s) into single nodes"
      let collapsedExtraction = extraction
            { extractionNodes = Map.union (Map.fromList [(nodeId n, n) | n <- collapsedNodes]) (extractionNodes extraction) }
      opRecordHistogram op "graphos_extract_duration_seconds" (realToFrac (diffUTCTime extractEnd extractStart) :: Double)
      opIncCounter op "graphos_pipeline_steps_total" 1
      opSetGauge op "graphos_nodes_extracted" (fromIntegral $ Map.size (extractionNodes extraction))
      opSetGauge op "graphos_edges_extracted" (fromIntegral $ Map.size (extractionEdges extraction))
      opDebugTraceSpan op "extract" (StartTime extractStart) (EndTime extractEnd) (Map.fromList [("nodes", T.pack $ show $ Map.size (extractionNodes extraction)), ("edges", T.pack $ show $ Map.size (extractionEdges extraction))])
      lpLogInfo lp $ T.pack $ "  " ++ show (Map.size (extractionNodes extraction)) ++ " nodes, " ++
                  show (Map.size (extractionEdges extraction)) ++ " edges"
      lpLogDebug lp $ T.pack $ "  Nodes: " ++ show (Map.elems (extractionNodes extraction))

      when (cfgNeo4jStreaming configWithStreaming /= Nothing) $ do
        lpLogInfo lp "  [neo4j-stream] Running edge repair pass..."
        let graph = buildGraphFromExtractions (cfgDirected configWithStreaming) [collapsedExtraction]
        (_msg, stmts, batches) <- epPushEdgeRepair ep graph
          (case cfgNeo4jStreaming configWithStreaming of
             Just s -> neo4jsUri s
             Nothing -> "http://localhost:7474")
          (case cfgNeo4jStreaming configWithStreaming of
             Just s -> neo4jsUser s
             Nothing -> "neo4j")
          (case cfgNeo4jStreaming configWithStreaming of
             Just s -> neo4jsPassword s
             Nothing -> "graphos_dev")
        lpLogInfo lp $ T.pack $ "  [neo4j-stream] Edge repair: " ++ show stmts ++ " statements in " ++ show batches ++ " batches"

      lpLogInfo lp "Step 3: Building graph..."
      buildStart <- getCurrentTime
      let builtGraph = buildGraphFromExtractions (cfgDirected configWithStreaming) [collapsedExtraction]
      _ <- evaluate (Map.size (gNodes builtGraph) + Map.size (gEdges builtGraph))
      builtGraph `deepseq` pure ()
      buildEnd <- getCurrentTime
      opRecordHistogram op "graphos_build_duration_seconds" (realToFrac (diffUTCTime buildEnd buildStart) :: Double)
      opIncCounter op "graphos_pipeline_steps_total" 1
      opSetGauge op "graphos_graph_nodes" (fromIntegral $ Map.size (gNodes builtGraph))
      opSetGauge op "graphos_graph_edges" (fromIntegral $ Map.size (gEdges builtGraph))
      opDebugTraceSpan op "build" (StartTime buildStart) (EndTime buildEnd) (Map.fromList [("nodes", T.pack $ show $ Map.size (gNodes builtGraph)), ("edges", T.pack $ show $ Map.size (gEdges builtGraph))])
      lpLogInfo lp $ T.pack $ "  Graph: " ++ show (Map.size (gNodes builtGraph)) ++ " nodes, " ++ show (Map.size (gEdges builtGraph)) ++ " edges"

      let codeFiles = length $ Map.findWithDefault [] CodeFiles (detectionFiles detection)
          nonCodeFiles = detectionTotalFiles detection - codeFiles
          nodeCount = fromIntegral (Map.size (gNodes builtGraph)) :: Double
          edgeCount = fromIntegral (Map.size (gEdges builtGraph)) :: Double
          ratio = if nodeCount == 0 then 0 else edgeCount / nodeCount
      when (codeFiles > nonCodeFiles && nodeCount > 0 && ratio < edgeCollapseThreshold) $
        lpLogInfo lp $ T.pack $ "  WARNING: edge/node ratio (" ++ show ratio ++ ") is below threshold " ++ show edgeCollapseThreshold ++ "; edge extraction may have collapsed"

      createDirectoryIfMissing True (cfgOutputDir configWithStreaming)

      graph <- if cfgEmbed configWithStreaming
        then do
          let embCfg = gcEmbedding (cfgGraphosConfig configWithStreaming)
              embCacheRoot = cfgOutputDir configWithStreaming ++ "/cache"
              sidecar = cfgOutputDir configWithStreaming ++ "/embeddings.json"
          lpLogInfo lp "  Generating node embeddings..."
          -- generateGraphEmbeddings writes the sidecar itself (streaming
          -- staged path when embStreaming, write-at-end otherwise, D9).
          embs <- generateGraphEmbeddings (llmPort appEnv) embCfg builtGraph embCacheRoot sidecar
          lpLogInfo lp $ T.pack $ "  Wrote " ++ show (Map.size embs) ++ " node embeddings to embeddings.json"
          pure (builtGraph { gEmbeddings = Just embs, gEmbeddingsPath = Just "embeddings.json" })
        else pure builtGraph

      lpLogInfo lp $ T.pack $ "  Streaming graph data to " ++ cfgOutputDir configWithStreaming ++ "/graph.json"
      iw <- epOpenIncrementalWriter ep (cfgOutputDir configWithStreaming ++ "/graph.json")

      let checkpointPath = cfgOutputDir configWithStreaming ++ "/graph.checkpoint.json"
      epSaveCheckpoint ep graph checkpointPath (T.pack (cfgInputPath configWithStreaming))
      lpLogInfo lp $ T.pack $ "  Checkpoint saved: " ++ checkpointPath

      performGC

      -- Step 4/5: clustering + analysis (shared with --cluster-only).
      clusterOut <- clusterGraph appEnv graph configWithStreaming
      let enrichedGraph     = coEnrichedGraph clusterOut
          finalCommMap      = coCommunities clusterOut
          analysis          = coAnalysis clusterOut
          llmLabelsResult   = coLabels clusterOut
          aggregatesResult  = coAggregates clusterOut

      lpLogInfo lp "  graph.json written incrementally"
      epWriteNodes ep iw (Map.elems (gNodes (coGraphToWrite clusterOut)))
      epWriteEdges ep iw (Map.elems (gEdges (coGraphToWrite clusterOut)))
      epWriteCommunities ep iw finalCommMap
      epWriteCohesion ep iw (coCohesion clusterOut)
      epWriteGodNodes ep iw (analysisGodNodes analysis)
      epWriteCommunityAggregates ep iw (coAggregates clusterOut)
      epWriteCompositions ep iw (gCompositions enrichedGraph)
      epWriteEmbeddingsPath ep iw (fmap T.pack (gEmbeddingsPath enrichedGraph))
      epWriteAnalysisTail ep iw llmLabelsResult
      epFlushWriter ep iw
      epCloseWriter ep iw
      lpLogDebug lp "  Final graph, communities, and cohesion written incrementally"

      performGC

      lpLogInfo lp "Step 6: Generating report..."
      let _report = generateReport enrichedGraph analysis configWithStreaming detection llmLabelsResult

      lpLogInfo lp "Step 7: Exporting outputs..."
      exportStart <- getCurrentTime
      createDirectoryIfMissing True (cfgOutputDir configWithStreaming)
      exports <- UEP.epExportAll ep enrichedGraph (cfgOutputDir configWithStreaming) analysis configWithStreaming detection llmLabelsResult aggregatesResult
      exportEnd <- getCurrentTime
      opRecordHistogram op "graphos_export_duration_seconds" (realToFrac (diffUTCTime exportEnd exportStart) :: Double)
      opIncCounter op "graphos_pipeline_steps_total" 1

      when (cfgNeo4j configWithStreaming) $ do
        lpLogInfo lp "  Neo4j: Cypher export + push complete"

      when (cfgCommunityGraph configWithStreaming && not (cfgNoCluster configWithStreaming)) $ do
        lpLogInfo lp "Step 7b: Exporting community-level graph..."
        epExportCommunityGraph ep enrichedGraph finalCommMap (cfgOutputDir configWithStreaming ++ "/community_graph.json")
        lpLogInfo lp $ T.pack $ "  Community graph: " ++ cfgOutputDir configWithStreaming ++ "/community_graph.json"

      fspClearCheckpoint fsp (cfgOutputDir configWithStreaming)

      opShutdownObservability op

      let result = PipelineResult
            { prNodes       = Map.size (gNodes enrichedGraph)
            , prEdges       = Map.size (gEdges enrichedGraph)
            , prCommunities = Map.size finalCommMap
            , prReportPath  = UEP.erReport exports
            , prGraphPath   = UEP.erJSON exports
            , prHtmlPath    = UEP.erHTML exports
            , prNeo4jPath  = UEP.erNeo4j exports
            }
      lpLogInfo lp "Graph complete!"
      pure $ Right result
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Pipeline error: " ++ show e
  where
    allFiles d = concat (Map.elems (detectionFiles d))

-- | Shared clustering + analysis stage (Steps 4-5). Used by both the full
-- pipeline and --cluster-only so neither duplicates community detection, edge
-- inference, re-clustering, analysis, labeling, or aggregation logic.
clusterGraph :: AppEnv -> Graph -> PipelineConfig -> IO ClusterOutput
clusterGraph appEnv graph config = do
  let lp = loggingPort appEnv
      op = observabilityPort appEnv
  if cfgNoCluster config
    then do
      lpLogInfo lp "Step 4: Skipping clustering (--no-cluster)"
      let emptyCommMap = Map.empty :: CommunityMap
          emptyCohesion = Map.empty :: CohesionMap
          noAnalysis = analyzeGraph graph emptyCommMap emptyCohesion
      pure ClusterOutput
        { coEnrichedGraph = graph
        , coGraphToWrite = graph
        , coCommunities = emptyCommMap
        , coCohesion = emptyCohesion
        , coAnalysis = noAnalysis
        , coLabels = Nothing
        , coAggregates = []
        }
    else do
      lpLogInfo lp "Step 4: Detecting communities..."
      clusterStart <- getCurrentTime
      let res = Resolution { resGamma = cfgResolution config
                           , resMinSize = cfgMinCommSize config
                           , resMergeInto = MergeToNeighbor
                           , resMaxIterations = cfgMaxLeidenIterations config }
          (commMap, cohesion) = clusterGraphWithResolution graph res
      _ <- evaluate (Map.size commMap + sum (map length (Map.elems commMap)))
      (commMap, cohesion) `deepseq` pure ()
      clusterEnd <- getCurrentTime
      opRecordHistogram op "graphos_cluster_duration_seconds" (realToFrac (diffUTCTime clusterEnd clusterStart) :: Double)
      opIncCounter op "graphos_pipeline_steps_total" 1
      opSetGauge op "graphos_communities" (fromIntegral $ Map.size commMap)
      opDebugTraceSpan op "cluster" (StartTime clusterStart) (EndTime clusterEnd) (Map.fromList [("communities", T.pack $ show $ Map.size commMap)])

      let seCfg = (gcSemanticEdges (cfgGraphosConfig config)) { seEnabled = not (cfgNoSemanticEdges config) }
          force = cfgForceSemanticEdges config
          mode = semanticMode seCfg force graph
          semanticEdges = inferSemanticEdgesForMode mode seCfg graph
          allInferred = inferNonSemanticEdgesWith (cfgEdgeDensity config) seCfg Map.empty graph commMap ++ semanticEdges
          enrichedGraph' = (if null allInferred
            then graph
            else addEdges graph allInferred)
            { gEmbeddings = gEmbeddings graph
            , gEmbeddingsPath = gEmbeddingsPath graph }
      enrichedGraph' `deepseq` pure ()
      logSemanticInference lp seCfg mode semanticEdges
      lpLogInfo lp $ T.pack $ "  Inferred " ++ show (length allInferred) ++ " additional edges (density: " ++ show (cfgEdgeDensity config) ++ ")"

      lpLogInfo lp "Step 5: Re-clustering and analyzing..."
      step5Start <- getCurrentTime
      let (finalComm, finalCohes) = clusterGraphWithResolution enrichedGraph' res
          anal = analyzeGraph enrichedGraph' finalComm finalCohes
      _ <- evaluate (Map.size finalComm + sum (map length (Map.elems finalComm)))
      _ <- evaluate (length (analysisGodNodes anal))
      (finalComm, finalCohes) `deepseq` pure ()
      step5End <- getCurrentTime
      opRecordHistogram op "graphos_cluster_step5_duration_seconds" (realToFrac (diffUTCTime step5End step5Start) :: Double)
      opDebugTraceSpan op "cluster_step5" (StartTime step5Start) (EndTime step5End) (Map.fromList [("communities", T.pack $ show $ Map.size finalComm)])
      lpLogInfo lp $ T.pack $ "  Re-cluster: " ++ show (Map.size finalComm) ++ " communities"

      let compMap = computeCompositions enrichedGraph' finalComm
          graphWithComps = enrichedGraph' { gCompositions = Just (toJSON compMap) }
          joinedGraph = joinCommunitiesToNodes enrichedGraph' finalComm

      let lblCfg = gcLabeling (cfgGraphosConfig config)
      llmLabels <- if cfgLabel config
        then do
          lpLogInfo lp "Step 5b: Labeling communities via LLM..."
          lpLogInfo lp $ T.pack $ "  Labeling config: provider=" ++ labelingProvider lblCfg
                                 ++ " model=" ++ labelingModel lblCfg
                                 ++ " baseUrl=" ++ labelingBaseUrl lblCfg
                                 ++ " batchSize=" ++ show (labelingBatchSize lblCfg)
          labelingStart <- getCurrentTime
          result <- labelCommunities appEnv enrichedGraph' finalComm finalCohes lblCfg
          labelingEnd <- getCurrentTime
          lpLogInfo lp $ T.pack $ "  Labeled " ++ show (Map.size (lrLabels result)) ++ " communities in "
                                 ++ show (diffUTCTime labelingEnd labelingStart) ++ "s"
          pure (Just (lrLabels result))
        else pure Nothing

      let artPoints = GAnalysis.articulationPoints enrichedGraph'
          aggregates = computeCommunityAggregates joinedGraph finalComm finalCohes artPoints llmLabels
      _ <- evaluate (length aggregates)

      pure ClusterOutput
        { coEnrichedGraph = graphWithComps
        , coGraphToWrite = joinedGraph
        , coCommunities = finalComm
        , coCohesion = finalCohes
        , coAnalysis = anal
        , coLabels = llmLabels
        , coAggregates = aggregates
        }

-- | Cluster-only entry point: load the last checkpoint, reconstruct the graph,
-- run only clustering + analysis, then exit. Enables incremental rebuilds and
-- debugging of large pipelines without re-running detect/extract/build.
runClusterOnlyPipeline :: AppEnv -> PipelineConfig -> IO (Either Text Int)
runClusterOnlyPipeline appEnv config = catch (do
  let lp = loggingPort appEnv
      ep = exportPort appEnv
      inputRoot = cfgInputPath config
      checkpointPath = cfgOutputDir config ++ "/graph.checkpoint.json"
  lpLogInfo lp "Step 1: Loading checkpoint..."
  mLoaded <- UEP.epLoadCheckpoint ep checkpointPath
  case mLoaded of
    Left err -> pure $ Left $ T.concat ["Failed to load checkpoint: ", err]
    Right loaded -> do
      let graph = UEP.lcGraph loaded
          mSrc  = UEP.lcInputSource loaded
      when (isJust mSrc && fromJust mSrc /= T.pack inputRoot) $
        lpLogInfo lp $ T.pack $ "  Warning: checkpoint was created from input " ++ show (fromJust mSrc)
                       ++ " but --cluster-only input is " ++ show inputRoot
      lpLogInfo lp $ T.pack $ "  Loaded checkpoint: " ++ show (Map.size (gNodes graph)) ++ " nodes, "
                       ++ show (Map.size (gEdges graph)) ++ " edges"
      lpLogInfo lp "Step 2: Clustering..."
      clusterOut <- clusterGraph appEnv graph config
      let commCount = Map.size (coCommunities clusterOut)
      lpLogInfo lp $ T.pack $ "  Clustered into " ++ show commCount ++ " communities. Exiting (--cluster-only)."
      pure $ Right commCount
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Cluster-only pipeline error: " ++ show e