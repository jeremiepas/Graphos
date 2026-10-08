-- | Graphos CLI - main entry point
module Main where

import Options.Applicative
import System.Exit (exitWith, ExitCode(..), exitSuccess)
import Data.Text.Short (toText)
import qualified Data.Text as T
import Control.Concurrent.MVar (newMVar)
import Control.Monad (forM_, when)
import Data.Maybe (isJust)
import Data.Char (toLower)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import System.IO (stdout, BufferMode(..), hSetBuffering, hPutStrLn, hFlush)
import qualified Data.Text.IO as TIO
import System.IO (stderr)
import System.Process (createProcess, proc, waitForProcess)
import qualified System.Process as Process
import System.Environment (getArgs, getExecutablePath, withArgs, lookupEnv, getEnvironment)
import Text.Read (readMaybe)
import System.FilePath ((</>))
import qualified Data.Time.Clock as TCC (getCurrentTime)
import qualified Data.Time.Format as TTF (formatTime, defaultTimeLocale)
import Graphos.Infrastructure.Export.HTML (renderResearchHtml)

import Graphos.CLI.Parser
import Graphos.Domain.Types (PipelineConfig(..), Node(..), Edge(..), relationToText, edgeConfidence, Detection(..), emptyExclusionCounts, defaultConfig, Analysis(..), NullModel(..))
import qualified Graphos.Domain.Types.Graph as LG (LabeledGraph(..))
import Graphos.UseCase.Subgraph (extractSubgraph, SubgraphConfig(..))
import Graphos.Infrastructure.Export.JSON (exportSubgraphJSON, exportGraphWithLabels)
import Graphos.Domain.Types.Pipeline (Neo4jPushMode(..), MemgraphPushMode(..))
import Graphos.UseCase.Pipeline (runPipeline, runClusterOnlyPipeline, runIncrementalPipeline, runSingleFilePipeline, PipelineResult(..), SingleFileResult(..))
import Graphos.Infrastructure.Wiring (productionAppEnv)
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Load (loadGraphFromFile, loadGraphFromFileStrict, LoadResult(..), validateGraphFile, corruptGraphMessage)
import Graphos.UseCase.SpecCheck (runSpecCheck', renderSpecReport, reportGates, parseCheckName, filterReport, spofDecisions, srCandidates)
import Graphos.Infrastructure.SpecParse (parseSpecCorpus)
import qualified Graphos.UseCase.SpecAdjudicate as SpecAdjudicate
import Graphos.Infrastructure.Wiring (productionLLMPort)
import Graphos.Domain.Config (defaultLabelingConfig)
import Graphos.UseCase.Query (queryGraphWithIndexScoredScoped, pathQueryWithIndex, explainNodeWithIndex, symbolLookup, neighborhoodExpansion, resolveNodeArg, NodeResolution(..), QueryResponse(..))
import Graphos.Domain.Query.Cypher.Parser (parseStatement)
import Graphos.Domain.Query.Cypher.AST (CypherStatement(..))
import Graphos.Domain.Query.Cypher.Eval (evaluateStatement)
import qualified Graphos.Domain.Query.Cypher.Eval as MutEval (mrGraph)
import Graphos.UseCase.Query.Research (buildResearchViewIO)
import Graphos.Domain.Community (computeCompositions)
import Graphos.UseCase.Merge (mergeGraphsAndAnalyze, MergeResult(..))
import qualified Graphos.UseCase.Merge as Merge (mrGraph)
import Graphos.Domain.Graph (Graph, gNodes, gEdges, gAdjFwd, gAdjBack, neighbors, degree)
import Graphos.Domain.Graph.Analysis (articulationPoints, toCachedFGL)
import Graphos.Domain.Graph.Index (communityOfNode)
import Graphos.UseCase.Query.Refine (RefineConfig(..), refineResponse)
import Graphos.UseCase.Query.Render (CommonQueryOpts(..), renderQueryResponseText, renderQueryResponseJSON, renderSymbolResultText, renderSymbolResultJSON, renderNeighborsResultText, renderNeighborsResultJSON, renderPathResultJSON, renderExplainResultJSON, renderAmbiguousText, renderAmbiguousJSON, renderNotFoundText, renderNotFoundJSON, renderMutationResultText, renderMutationResultJSON)
import Graphos.Infrastructure.Export.PersistMutation (persistMutatedGraph)
import Graphos.Domain.Community (detectCommunities, scoreAllCohesion, Resolution(..), MergeStrategy(..))
import Graphos.Infrastructure.LSP.Capabilities (LanguageServerInfo(..), discoverLanguageServers)
import Graphos.Infrastructure.Logging (LogLevel(..), defaultLogEnv, LogEnv(..), logInfo, logDebug, logError)
import Graphos.Infrastructure.Export.Neo4j (pushSubgraphToNeo4j, pushCommunityGraphToNeo4j, pushToNeo4jWithCommunities)
import Graphos.Infrastructure.Export.Memgraph (pushToMemgraphWithCommunities, pushSubgraphToMemgraph, pushCommunityGraphToMemgraph)
import Graphos.Infrastructure.Observability.SDK
  ( initObservability
  , shutdownObservability
  , ObservabilityEnv(..)
  , OtelConfig(..)
  , defaultOtelConfig
  )
import Graphos.Domain.Config (defaultGraphosConfig, ObservabilityConfig(..), gcObservability, VisionConfig(..), vcEnabled, gcVision, gcIngest, icEmbed, gcSemanticEdges, gcMemory, MemoryConfig(..), MemoryBudgetSetting(..), MemInfo(..), deriveBudget, memorySafetyReserve, formatBytes, effectiveOutputDir)
import Graphos.Infrastructure.Config (loadConfig, loadConfigSilent, generateDefaultConfig, writeConfigYaml, configDocComments)
import Graphos.Infrastructure.System.Memory (readMemInfo)
import Graphos.Infrastructure.Server.Static (startServeServer)
import Graphos.Infrastructure.Server.MCP (startMCPServerFromFile)
import Graphos.Infrastructure.FileSystem.Watcher (watchDirectory, defaultGraphosWatchConfig)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Timeout (timeout)

import qualified Graphos.UseCase.Export as Export
import Graphos.UseCase.Port.ExportPort (ExportResult(..))
import Graphos.Domain.Scaffold (parseTarget, ScaffoldRequest(..))
import Graphos.UseCase.Scaffold (selectTargets, planScaffold, CommandReference(..))
import Graphos.Infrastructure.Scaffold.Writer (writeScaffold, gatherDetectionFacts, runInstallSkill)


-- ───────────────────────────────────────────────
-- CLI argument parsing (imported from Graphos.CLI.Parser)
-- ───────────────────────────────────────────────

-- All parsers (pipelineOpts, queryOpts, commandOpts, etc.)
-- and the Command type are defined in Graphos.CLI.Parser

loadGraphOpt :: Bool -> FilePath -> IO (Either T.Text LoadResult)
loadGraphOpt strict path =
  if strict then loadGraphFromFileStrict path else loadGraphFromFile path

-- | Empty analysis payload for 'exportGraphWithLabels' (migrate-graph):
-- the loaded analysis sections are carried by 'LoadResult' fields that the
-- JGF writer takes from the graph itself; only the community labels are
-- preserved explicitly.
emptyAnalysisForMigrate :: LoadResult -> Analysis
emptyAnalysisForMigrate lr = Analysis
  { analysisCommunities   = lrCommunities lr
  , analysisNullModel     = DefaultNullModel
  , analysisCohesion      = lrCohesion lr
  , analysisGodNodes      = lrGodNodes lr
  , analysisSurprises     = []
  , analysisQuestions     = []
  , analysisArticulation  = []
  , analysisBccCount      = 0
  }

parseHeapSize :: String -> Maybe Int
parseHeapSize s = case reads s of
  [(n, "")] -> case () of
    _ | 'g' `elem` lower || 'G' `elem` s -> Just (round (n * 1024 :: Double))
    _ | 'm' `elem` lower || 'M' `elem` s -> Just (round n)
    _ -> Just (round n)
  _ -> Nothing
  where lower = map toLower s

stripRTSFlags :: [String] -> ([String], Bool, Maybe String)
stripRTSFlags args = go args False Nothing
  where
    go :: [String] -> Bool -> Maybe String -> ([String], Bool, Maybe String)
    go [] profile heap = ( [], profile, heap )
    go (a:as) profile heap = case a of
      "--rts-profile" -> go as True heap
      "--max-heap" -> case as of
        h:rest -> go rest profile (Just h)
        _      -> go as profile heap
      other -> let (rest, p, h) = go as profile heap
               in (other : rest, p, h)

-- | Environment marker set on the re-exec'd child process: its presence means
-- budget establishment already happened in the parent (never derive or
-- re-exec again); its value carries the active heap budget in bytes (empty
-- when the parent re-exec'd for profiling only, with no budget).
reexecEnvVar :: String
reexecEnvVar = "GRAPHOS_MEMORY_BUDGET"

-- | Whether an explicit @-o/--output@ flag was passed on the command line.
-- Comparing against the parser default cannot distinguish an omitted flag from
-- a literal @-o graphos-out@, so the decision factors through the raw argv.
-- Covers every optparse-applicative spelling: @--output VALUE@, @--output=VALUE@,
-- @-o VALUE@ and @-oVALUE@.
cliOutputFlagGiven :: IO Bool
cliOutputFlagGiven = do
  args <- getArgs
  pure (go args)
  where
    go [] = False
    go (a : rest)
      | a == "--output"          = True
      | a == "-o"                = True
      | isPrefixOf "--output=" a = True
      | isPrefixOf "-o" a        = True
      | otherwise                = go rest
    isPrefixOf []      _      = True
    isPrefixOf _       []     = False
    isPrefixOf (x:xs) (y:ys)  = x == y && isPrefixOf xs ys

-- | Effective output directory for the Run branch (multi-source-graphs 2.2):
-- CLI @-o@ wins; otherwise the graphos.yaml @output:@ key; otherwise the
-- canonical 'defaultOutputDirName'.
effectiveRunOutputDir :: PipelineConfig -> GraphosConfig -> IO FilePath
effectiveRunOutputDir config graphosCfg = do
  given <- cliOutputFlagGiven
  pure $ if given
    then cfgOutputDir config
    else effectiveOutputDir graphosCfg

-- | Establish the active heap budget before the pipeline starts (Workflow 01,
-- memory-budget-guard): an explicit @--max-heap@ (or fixed graphos.yaml
-- @memory.budget@) wins; otherwise a default is derived from MemAvailable
-- minus the safety reserve; otherwise the run is uncapped with a WARN.
-- Re-executes the binary under the corresponding RTS @-M@ cap — in that case
-- this never returns (the parent waits for the child and exits with its
-- code). Returns the active budget when execution continues in-process.
establishMemoryBudget :: PipelineConfig -> IO (Maybe Integer)
establishMemoryBudget config = do
  let profile = cfgRtsProfile config
      cliBytes = fmap (\mb -> fromIntegral mb * 1024 * 1024) (cfgMaxHeap config) :: Maybe Integer
  yamlSetting <- if isJust cliBytes
    then pure BudgetAuto  -- the CLI flag decides; skip the config read
    else memBudget . gcMemory <$> loadConfigSilent
  let explicitBytes = case (cliBytes, yamlSetting) of
        (Just b, _)             -> Just b
        (Nothing, BudgetFixed b) -> Just b
        _                        -> Nothing
      optOut = cfgNoMemoryBudget config || yamlSetting == BudgetOff
      uncapped reason = do
        hPutStrLn stderr $ "[memory] WARNING: no budget applied (" ++ reason ++ ") — the run is uncapped"
        if profile then reexecWithRTS profile Nothing else pure Nothing
  case explicitBytes of
    Just b -> do
      hPutStrLn stderr $ "[memory] budget: " ++ formatBytes b ++ " (explicit)"
      reexecWithRTS profile (Just b)
    Nothing
      | optOut -> uncapped (if cfgNoMemoryBudget config
                              then "--no-memory-budget"
                              else "memory.budget: off")
      | otherwise -> do
          mInfo <- readMemInfo
          case (deriveBudget mInfo Nothing memorySafetyReserve, mInfo) of
            (Just derived, Just mi) -> do
              hPutStrLn stderr $ "[memory] budget: " ++ formatBytes derived
                ++ " (derived from available " ++ formatBytes (miMemAvailable mi) ++ ")"
              reexecWithRTS profile (Just derived)
            _ -> uncapped "memory info unavailable"

-- | Re-execute the binary with RTS flags (@-M@ heap cap and/or profiling),
-- marking the child via 'reexecEnvVar', then wait and exit with the child's
-- exit code — so heap-exhaustion (2) and pre-flight (1) exits survive the
-- re-exec unchanged.
reexecWithRTS :: Bool -> Maybe Integer -> IO a
reexecWithRTS profile mBudgetBytes = do
  originalArgs <- getArgs
  exePath <- getExecutablePath
  envs <- getEnvironment
  let rtsFlags = concat
        [ if profile then ["-s", "-hT"] else []
        , maybe [] (\b -> ["-M" ++ show b]) mBudgetBytes
        ]
  hPutStrLn stderr $ "[graphos] Re-executing with RTS flags: +RTS " ++ unwords rtsFlags
  hFlush stderr
  let (cleanArgs, _, _) = stripRTSFlags originalArgs
      finalArgs = ["+RTS"] ++ rtsFlags ++ ["--"] ++ cleanArgs
      childEnv = (reexecEnvVar, maybe "" show mBudgetBytes)
                   : filter ((/= reexecEnvVar) . fst) envs
      spec = (proc exePath finalArgs) { Process.env = Just childEnv }
  (_, _, _, ph) <- createProcess spec
  code <- waitForProcess ph
  exitWith code

runClusterOnlyMode :: AppEnv -> LogEnv -> ObservabilityEnv -> PipelineConfig -> IO ()
runClusterOnlyMode appEnv env obsEnv config' = do
  logInfo env "Running in --cluster-only mode (clustering from checkpoint)..."
  clusterResult <- case cfgTimeout config' of
    Nothing -> runClusterOnlyPipeline appEnv config'
    Just secs -> do
      logInfo env $ "[pipeline] Running with " <> T.pack (show secs ++ "s timeout")
      let timeoutMicros = fromIntegral (secs * 1000000)
      timed <- timeout timeoutMicros (runClusterOnlyPipeline appEnv config')
      case timed of
        Nothing -> do
          logError env $ "[pipeline] TIMEOUT: cluster-only exceeded " <> T.pack (show secs ++ "s limit")
          exitWith (ExitFailure 1)
        Just r -> pure r
  case clusterResult of
    Left err -> do
      logError env $ "Cluster-only failed: " <> err
      exitWith (ExitFailure 1)
    Right commCount -> do
      logInfo env "Cluster-only complete!"
      logInfo env $ T.pack $ "  Communities: " ++ show commCount
      let shutdownMicros = cfgOtelShutdownTimeout config' * 1000000
      _ <- timeout shutdownMicros (shutdownObservability obsEnv)
      exitSuccess

main :: IO ()
main = do
  rawArgs <- getArgs
  -- Strip RTS flags passed by parent process (+RTS ... --) but preserve CLI flags
  let args = case break (== "--") rawArgs of
        (_, []) -> rawArgs  -- No "--" found, use all args
        (_, _:rest) -> rest  -- Drop RTS flags and "--", keep rest
  cmd <- withArgs args (execParser opts)
  case cmd of
    Run config -> do
      -- Budget establishment (memory-budget-guard): the parent derives (or
      -- takes the explicit) heap budget and re-execs under RTS -M; the child
      -- recognizes the env marker and reads the active budget from it.
      reexecMarker <- lookupEnv reexecEnvVar
      activeBudget <- case reexecMarker of
        Just v  -> pure (readMaybe v :: Maybe Integer)
        Nothing -> establishMemoryBudget config
      -- Load graphos.yaml config and merge with CLI defaults
      graphosCfg <- loadConfig
      -- Effective output directory (multi-source-graphs 2.2): the CLI -o
      -- always wins; otherwise the graphos.yaml `output:` key; otherwise the
      -- canonical "graphos-out".
      outputDir <- effectiveRunOutputDir config graphosCfg
      let obsCfg = gcObservability graphosCfg
          -- Merge config file + CLI flags: CLI flags override config file values
          -- SDK reads OTEL_* env vars; we set them from CLI flags
          -- When --no-observability is set, force-disable all observability
          otelCfg = defaultOtelConfig
            { otelEnabled        = (not (cfgNoObservability config)) && (obsEnabled obsCfg || cfgOtelEnabled config || isJust (cfgMetricsPort config))
            , otelEndpoint       = obsEndpoint obsCfg
            , otelServiceName    = obsServiceName obsCfg
            , otelLogsEndpoint   = obsEndpoint obsCfg ++ "/v1/logs"
            }
          metricsPort = if cfgNoObservability config
                           then Nothing
                           else case cfgMetricsPort config of
                                    Just p  -> Just p
                                    Nothing -> if obsMetricsPort obsCfg > 0 then Just (obsMetricsPort obsCfg) else Nothing
          debugDir = case cfgDebugTraceDir config of
                       Just d  -> d
                       Nothing -> if null (obsDebugTraceDir obsCfg)
                                    then outputDir ++ "/traces"
                                    else obsDebugTraceDir obsCfg
          config' = config { cfgOutputDir    = outputDir
                           , cfgGraphosConfig = graphosCfg { gcVision = (gcVision graphosCfg) { vcEnabled = cfgVision config || vcEnabled (gcVision graphosCfg) } }
                            , cfgOtelConfig     = otelCfg
                            , cfgMetricsPort    = metricsPort
                            , cfgDebugTraceDir  = Just debugDir
                            , cfgActiveBudgetBytes = activeBudget
                            }
      -- Fail-fast on a corrupt existing graph.json before doing any work.
      -- Strict by default; pass --no-strict-graph for tolerant loading.
      when (cfgStrictGraph config') $ do
        let graphFile = cfgOutputDir config' </> "graph.json"
        validateGraphFile graphFile >>= \case
          Left err -> do
            hPutStrLn stderr $ "[graphos] " ++ T.unpack (corruptGraphMessage graphFile err)
            exitWith (ExitFailure 1)
          Right () -> pure ()
      -- Initialize observability (tracing, metrics, debug trace)
      let logLevel = if cfgDebug config || obsDebug obsCfg then LogTrace
                      else if cfgVerbose config then LogDebug
                      else LogInfo
      obsEnv <- initObservability logLevel otelCfg metricsPort debugDir
      let _tracer = otelTracer obsEnv
          _metrics = otelMetrics obsEnv
          env = otelLogEnv obsEnv
          appEnv = productionAppEnv env obsEnv
      when (cfgClusterOnly config') $ runClusterOnlyMode appEnv env obsEnv config'
      -- MCP mode: start MCP server and exit
      case cfgMCP config' of
         Just graphPath -> do
           putStrLn $ "[graphos] Starting MCP server with " ++ graphPath
           startMCPServerFromFile graphPath
         Nothing ->
           -- Watch mode: run initial pipeline, then watch for changes
           if cfgWatch config'
             then do
               logInfo env "Starting initial pipeline (watch mode)..."
               result <- runPipeline appEnv config'
               case result of
                 Left err -> do
                   logError env $ "Initial pipeline failed: " <> err
                   exitWith (ExitFailure 1)
                 Right res -> do
                   logInfo env "Initial pipeline complete! Watching for changes..."
                   logInfo env $ T.pack $ "  Nodes: " ++ show (prNodes res)
                   logInfo env $ T.pack $ "  Edges: " ++ show (prEdges res)
                   logInfo env $ T.pack $ "  Communities: " ++ show (prCommunities res)
                   -- Start watcher
                   shutdownVar <- newMVar ()
                   watchDirectory (cfgInputPath config') (\changedFiles -> do
                     let filesList = T.splitOn ", " (T.pack changedFiles)
                     logInfo env $ T.pack $ "[watch] Files changed: " ++ show (length filesList) ++ " files"
                     incResult <- runIncrementalPipeline appEnv config' (map T.unpack filesList)
                     case incResult of
                       Left err' -> logError env $ T.pack $ "[watch] Incremental pipeline failed: " ++ T.unpack err'
                       Right _ -> logInfo env "[watch] Incremental update complete"
                     ) defaultGraphosWatchConfig shutdownVar
              else do
                -- Normal mode: run once and exit
                logInfo env "Starting pipeline..."
                logDebug env $ "Config: " <> T.pack (show config')
                result <- case cfgTimeout config' of
                  Nothing -> runPipeline appEnv config'
                  Just secs -> do
                    logInfo env $ "[pipeline] Running with " <> T.pack (show secs ++ "s timeout")
                    let timeoutMicros = fromIntegral (secs * 1000000)
                    timeoutedResult <- timeout timeoutMicros (runPipeline appEnv config')
                    case timeoutedResult of
                      Nothing -> do
                        logError env $ "[pipeline] TIMEOUT: Pipeline exceeded " <> T.pack (show secs ++ "s limit")
                        exitWith (ExitFailure 1)
                      Just res -> return res
                let shutdownMicros = cfgOtelShutdownTimeout config' * 1000000
                shutdownResult <- timeout shutdownMicros (shutdownObservability obsEnv)
                case shutdownResult of
                  Nothing -> hPutStrLn stderr $ "[graphos] WARNING: Observability shutdown timed out after " ++ show (cfgOtelShutdownTimeout config') ++ "s"
                  Just () -> pure ()
                case result of
                 Left err -> do
                   logError env $ "Pipeline failed: " <> err
                   exitWith (ExitFailure 1)
                 Right res -> do
                   logInfo env "Graph complete!"
                   logInfo env $ T.pack $ "  Nodes: " ++ show (prNodes res)
                   logInfo env $ T.pack $ "  Edges: " ++ show (prEdges res)
                   logInfo env $ T.pack $ "  Communities: " ++ show (prCommunities res)
                   logInfo env $ T.pack $ "  Report: " ++ prReportPath res
                   logInfo env $ T.pack $ "  Graph: " ++ prGraphPath res
                   case prHtmlPath res of
                     Just html -> do
                       logInfo env $ T.pack $ "  HTML: " ++ html
                       logInfo env $ T.pack $ "  View: graphos serve --dir " ++ cfgOutputDir config' ++ " --port 8080"
                     Nothing  -> pure ()
                   case prNeo4jPath res of
                     Just cypher -> logInfo env $ T.pack $ "  Neo4j: " ++ cypher
                     Nothing     -> pure ()

    QueryCmd question mode qopts -> do
      -- In JSON mode raise the threshold so INFO/DEBUG never reach stdout
      -- (defaultLogEnv routes non-error levels to stdout), keeping the JSON a
      -- single clean document; errors still go to stderr.
      env <- defaultLogEnv (if cqoJson qopts then LogError else LogInfo)
      let graphPath = cqoGraphPath qopts
          budget    = cqoBudget qopts
      logInfo env $ "Query: " <> question <> " (" <> mode <> ", budget=" <> T.pack (show budget) <> ")"
      loadResult <- loadGraphOpt (cqoStrictGraph qopts) graphPath
      case loadResult of
        Left err -> (if cqoJson qopts then hPutStrLn stderr else putStrLn) $ "Error: " ++ T.unpack err
        Right loaded -> do
           let g = lrGraph loaded
               idx = lrIndex loaded
               scoredResp0 = queryGraphWithIndexScoredScoped g idx (toCachedFGL g) question mode budget (cqoPath qopts)
               scoredResp = case cqoMaxNodes qopts of
                 Just n | n > 0 -> scoredResp0 { qrespNodes = take n (qrespNodes scoredResp0) }
                 _ -> scoredResp0
               labelWidth = case cqoMaxLabelChars qopts of
                 Just n | n > 0 -> n
                 _ -> cqoLabelWidth qopts
               refineCfg = RefineConfig { rcEdgeMode = cqoEdges qopts, rcLabelWidth = labelWidth }
               refinedResp = refineResponse refineCfg (gNodes g) scoredResp
           if cqoJson qopts
            then putStrLn $ T.unpack $ renderQueryResponseJSON refinedResp
            else putStrLn $ T.unpack $ renderQueryResponseText budget refinedResp

    CypherCmd queryText allowWrite copts -> do
      env <- defaultLogEnv (if cqoJson copts then LogError else LogInfo)
      let graphPath = cqoGraphPath copts
          budget    = cqoBudget copts
      logInfo env $ "Cypher: " <> queryText
      loadResult <- loadGraphOpt (cqoStrictGraph copts) graphPath
      case loadResult of
        Left err -> (if cqoJson copts then hPutStrLn stderr else putStrLn) $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              idx = lrIndex loaded
          case parseStatement queryText of
            Left err -> (if cqoJson copts then hPutStrLn stderr else putStrLn) $ "Cypher error: " ++ T.unpack err
            Right st -> case st of
              MutStatement _ | not allowWrite -> do
                let msg = "Write statements require --write (or cypher_mutate MCP / POST /api/cypher/mutate); this surface is read-only"
                (if cqoJson copts then hPutStrLn stderr else putStrLn) $ "Cypher error: " ++ msg
              _ -> case evaluateStatement budget st g idx of
                Left err -> (if cqoJson copts then hPutStrLn stderr else putStrLn) $ "Cypher error: " ++ T.unpack err
                Right mr -> do
                  if cqoJson copts
                    then putStrLn $ T.unpack $ renderMutationResultJSON mr
                    else putStrLn $ T.unpack $ renderMutationResultText budget mr
                  when allowWrite $ case st of
                    MutStatement _ -> do
                      res <- persistMutatedGraph graphPath loaded (MutEval.mrGraph mr)
                      case res of
                        Left err -> hPutStrLn stderr $ "Persist error: " ++ T.unpack err
                        Right backup -> do
                          putStrLn $ "Persisted to " ++ graphPath ++ " (backup: " ++ backup ++ ")"
                          putStrLn "Note: the next extraction run overwrites graph.json and discards mutations."
                    _ -> pure ()

    PathCmd from to popts -> do
      env <- defaultLogEnv (if cqoJson popts then LogError else LogInfo)
      let graphPath = cqoGraphPath popts
      logInfo env $ "Path: " <> from <> " -> " <> to
      logDebug env "Loading graph from disk..."
      loadResult <- loadGraphOpt (cqoStrictGraph popts) graphPath
      case loadResult of
        Left err -> (if cqoJson popts then hPutStrLn stderr else putStrLn) $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              idx = lrIndex loaded
              mpath = pathQueryWithIndex g idx from to
          if cqoJson popts then putStrLn $ T.unpack $ renderPathResultJSON mpath else
           case mpath of
            Nothing -> putStrLn $ "No path found between '" ++ T.unpack from ++ "' and '" ++ T.unpack to ++ "'"
            Just path -> do
              let hops = length path - 1
              putStrLn $ "Shortest path (" ++ show hops ++ " hops):"
              let go []     = pure ()
                  go (nid:ns) = do
                    let mNext = case ns of
                          (n':_) -> Just n'
                          []     -> Nothing
                        mEdge = maybe Nothing (\nxt -> Map.lookup (nid, nxt) (gEdges g)) mNext
                    case Map.lookup nid (gNodes g) of
                      Just n -> do
                        let relLabel = maybe "references" (T.unpack . relationToText . edgeRelation) mEdge
                            confLabel = maybe "" (\e -> " [" ++ show (edgeConfidence e) ++ "]") mEdge
                        putStrLn $ "  " ++ T.unpack (toText (nodeLabel n)) ++ " --" ++ relLabel ++ "-->" ++ confLabel
                      Nothing -> pure ()
                    go ns
              go path

    ExplainCmd node eopts -> do
      let graphPath = cqoGraphPath eopts
      (if cqoJson eopts then hPutStrLn stderr else putStrLn) $ "[graphos] Explain: " ++ T.unpack node
      loadResult <- loadGraphOpt (cqoStrictGraph eopts) graphPath
      case loadResult of
        Left err -> (if cqoJson eopts then hPutStrLn stderr else putStrLn) $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              idx = lrIndex loaded
              mnode = explainNodeWithIndex g idx node
          if cqoJson eopts then putStrLn $ T.unpack $ renderExplainResultJSON mnode else
           case mnode of
            Nothing -> putStrLn $ "Node not found: " ++ T.unpack node
            Just n -> do
              putStrLn $ "NODE: " ++ T.unpack (toText (nodeLabel n))
              putStrLn $ "  ID: " ++ T.unpack (nodeId n)
              putStrLn $ "  Source: " ++ T.unpack (toText (nodeSourceFile n))
              case (nodeLineStart n, nodeLineEnd n) of
                (Just start, Just end) | start /= end -> putStrLn $ "  Location: L" ++ show start ++ "-" ++ show end
                (Just start, _)                        -> putStrLn $ "  Location: L" ++ show start
                _                                      -> pure ()
              putStrLn $ "  Type: " ++ show (nodeFileType n)
              putStrLn $ "  Degree: " ++ show (degree g (nodeId n))
              -- Show community (O(log N) via index instead of O(C×M) scan)
              case communityOfNode (nodeId n) idx of
                Just cid -> putStrLn $ "  Community: " ++ show cid
                Nothing  -> pure ()
              -- Show neighbors
              putStrLn ""
              putStrLn "CONNECTIONS:"
              let nbs = Set.toList (neighbors g (nodeId n))
              forM_ nbs $ \nbId -> do
                let mNb  = Map.lookup nbId (gNodes g)
                    mEdge = asum [Map.lookup (nodeId n, nbId) (gEdges g)
                                 ,Map.lookup (nbId, nodeId n) (gEdges g)]
                case mNb of
                  Just nb -> do
                    let relLabel = maybe "related" (T.unpack . relationToText . edgeRelation) mEdge
                        confLabel = maybe "" (\e -> " [" ++ show (edgeConfidence e) ++ "]") mEdge
                    putStrLn $ "  --" ++ relLabel ++ "--> " ++ T.unpack (toText (nodeLabel nb)) ++ confLabel
                  Nothing -> pure ()

    SymbolsCmd name symOpts -> do
      loadResult <- loadGraphOpt (cqoStrictGraph symOpts) (cqoGraphPath symOpts)
      case loadResult of
        Left err -> putStrLn $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              idx = lrIndex loaded
              result = symbolLookup name g idx
          if cqoJson symOpts
            then putStrLn $ T.unpack $ renderSymbolResultJSON result
            else putStrLn $ T.unpack $ renderSymbolResultText (cqoBudget symOpts) result

    NeighborsCmd nodeArg depth nbrOpts -> do
      loadResult <- loadGraphOpt (cqoStrictGraph nbrOpts) (cqoGraphPath nbrOpts)
      case loadResult of
        Left err -> (if cqoJson nbrOpts then hPutStrLn stderr else putStrLn) $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              idx = lrIndex loaded
          case resolveNodeArg nodeArg g idx of
            ResolvedSingle nid -> do
              let result = neighborhoodExpansion nid depth g idx
              if cqoJson nbrOpts
                then putStrLn $ T.unpack $ renderNeighborsResultJSON result
                else putStrLn $ T.unpack $ renderNeighborsResultText (cqoBudget nbrOpts) result
            Ambiguous cands ->
              if cqoJson nbrOpts
                then putStrLn $ T.unpack $ renderAmbiguousJSON cands
                else putStrLn $ T.unpack $ renderAmbiguousText cands
            NotFound -> do
              if cqoJson nbrOpts
                then putStrLn $ T.unpack $ renderNotFoundJSON nodeArg
                else putStrLn $ T.unpack $ renderNotFoundText nodeArg
              exitWith (ExitFailure 1)

    ResearchCmd termsArg seedsArg graphPath _doHtml doJson termsFileArg labelArg _researchMode commonOpts -> do
      hSetBuffering stdout NoBuffering
      graphosCfg <- loadConfigSilent
      loadResult <- loadGraphOpt (cqoStrictGraph commonOpts) graphPath
      case loadResult of
        Left err -> hPutStrLn stderr $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              idx = lrIndex loaded
              commMap = lrCommunities loaded
              comps = computeCompositions g commMap
              edgeMode = Just (cqoEdges commonOpts)
          termsFileTerms <- case termsFileArg of
            Nothing -> pure []
            Just path -> do
              exists <- doesFileExist path
              if exists
                then do
                  content <- TIO.readFile path
                  pure $ filter (not . T.null) (T.lines content)
                else do
                  hPutStrLn stderr $ "Error: terms file not found: " ++ path
                  exitWith (ExitFailure 1)
          let terms = termsArg <> termsFileTerms
              dedupedTerms = go mempty terms
               where
                  go _ [] = []
                  go seen (t:rest)
                    | Map.member t seen = go seen rest
                    | otherwise = t : go (Map.insert t () seen) rest
          rv <- buildResearchViewIO g idx commMap comps dedupedTerms seedsArg edgeMode
          ts <- TTF.formatTime TTF.defaultTimeLocale "%Y%m%dT%H%M%S" <$> TCC.getCurrentTime
          -- Effective output directory (multi-source-graphs 2.2): research
          -- HTML lands next to the graph it was built from (config `output:`
          -- honored; CLI research has no -o, so the config value always wins).
          let outDir = effectiveOutputDir graphosCfg
              lbl = maybe ts id labelArg
              base = "research-" ++ lbl
              htmlPath = outDir </> (base ++ ".html")
          createDirectoryIfMissing True outDir
          writeFile htmlPath (T.unpack (renderResearchHtml rv))
          hPutStrLn stderr $ "Wrote research HTML to " ++ htmlPath
          when doJson $ do
            BL.putStr (encode rv)

    PushCmd graphPath uri user password pushMode topN -> do
      putStrLn $ "[graphos] Push: loading " ++ graphPath
      loadResult <- loadGraphFromFile graphPath
      case loadResult of
        Left err -> putStrLn $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              totalNodes = Map.size (gNodes g)
              totalEdges = Map.size (gEdges g)
          -- If communities are empty, compute them now
          (commMap, cohesionMap) <- if Map.null (lrCommunities loaded)
            then do
              putStrLn $ "[graphos] No communities found in graph.json — computing communities..."
              let commMap' = detectCommunities g
                  cohesionMap' = scoreAllCohesion g commMap'
              putStrLn $ "[graphos] Computed " ++ show (Map.size commMap') ++ " communities"
              pure (commMap', cohesionMap')
            else pure (lrCommunities loaded, lrCohesion loaded)
          let numCommunities = Map.size commMap
          putStrLn $ "[graphos] Graph loaded: " ++ show totalNodes ++ " nodes, " ++ show totalEdges ++ " edges, " ++ show numCommunities ++ " communities"
          env <- defaultLogEnv LogInfo
          (msg, _stmts, _batches) <- case pushMode of
            FullPush -> do
              logInfo env $ T.pack $ "[neo4j] Push mode: full (all nodes + edges + communities)"
              pushToNeo4jWithCommunities g commMap cohesionMap (T.pack uri) (T.pack user) (T.pack password)
            SubgraphPush -> do
              let artPoints = articulationPoints g
              logInfo env $ T.pack $ "[neo4j] Push mode: subgraph (communities + " ++ show topN ++ " representatives/community, " ++ show (length artPoints) ++ " bridge nodes)"
              logInfo env $ T.pack $ "[neo4j] Full graph: " ++ show totalNodes ++ " nodes → subgraph: ~" ++ show (topN * numCommunities + length artPoints) ++ " representative nodes"
              pushSubgraphToNeo4j g commMap cohesionMap topN artPoints (T.pack uri) (T.pack user) (T.pack password)
            CommunityPush -> do
              logInfo env $ T.pack $ "[neo4j] Push mode: community-only (communities + inter-community edges)"
              pushCommunityGraphToNeo4j g commMap cohesionMap (T.pack uri) (T.pack user) (T.pack password)
          logInfo env $ "[neo4j] " <> msg

    PushMemgraphCmd graphPath uri user password pushMode topN -> do
      putStrLn $ "[graphos] Push to Memgraph: loading " ++ graphPath
      loadResult <- loadGraphFromFile graphPath
      case loadResult of
        Left err -> putStrLn $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              totalNodes = Map.size (gNodes g)
              totalEdges = Map.size (gEdges g)
          (commMap, cohesionMap) <- if Map.null (lrCommunities loaded)
            then do
              putStrLn $ "[graphos] No communities found in graph.json — computing communities..."
              let commMap' = detectCommunities g
                  cohesionMap' = scoreAllCohesion g commMap'
              putStrLn $ "[graphos] Computed " ++ show (Map.size commMap') ++ " communities"
              pure (commMap', cohesionMap')
            else pure (lrCommunities loaded, lrCohesion loaded)
          let numCommunities = Map.size commMap
          putStrLn $ "[graphos] Graph loaded: " ++ show totalNodes ++ " nodes, " ++ show totalEdges ++ " edges, " ++ show numCommunities ++ " communities"
          env <- defaultLogEnv LogInfo
          (msg, _stmts, _batches) <- case pushMode of
            MemgraphFull -> do
              logInfo env $ "[memgraph] Push mode: full (all nodes + edges + communities)"
              pushToMemgraphWithCommunities g commMap cohesionMap (T.pack uri) (T.pack user) (T.pack password)
            MemgraphSubgraph -> do
              let artPoints = articulationPoints g
              logInfo env $ T.pack $ "[memgraph] Push mode: subgraph (communities + " ++ show topN ++ " representatives/community, " ++ show (length artPoints) ++ " bridge nodes)"
              pushSubgraphToMemgraph g commMap cohesionMap topN artPoints (T.pack uri) (T.pack user) (T.pack password)
            MemgraphCommunity -> do
              logInfo env $ "[memgraph] Push mode: community-only (communities + inter-community edges)"
              pushCommunityGraphToMemgraph g commMap cohesionMap (T.pack uri) (T.pack user) (T.pack password)
          logInfo env $ "[memgraph] " <> msg

    MergeCmd pathA pathB outputDir density resolution minCommSize maxLeidenIterations noViz verbose -> do
      let logLevel = if verbose then LogDebug else LogInfo
      env <- defaultLogEnv logLevel
      obsEnv <- initObservability logLevel defaultOtelConfig (Just 9464) (outputDir ++ "/traces")
      let appEnv = productionAppEnv (otelLogEnv obsEnv) obsEnv
      logInfo env $ "[merge] Loading graph A: " <> T.pack pathA
      resultA <- loadGraphFromFile pathA
      case resultA of
        Left err -> do
          logError env $ "[merge] Failed to load graph A: " <> err
          exitWith (ExitFailure 1)
        Right graphA -> do
          logInfo env $ "[merge] Loading graph B: " <> T.pack pathB
          resultB <- loadGraphFromFile pathB
          case resultB of
            Left err -> do
              logError env $ "[merge] Failed to load graph B: " <> err
              exitWith (ExitFailure 1)
            Right graphB -> do
              logInfo env $ T.pack $ "[merge] Graph A: " ++ show (Map.size (gNodes (lrGraph graphA))) ++ " nodes, " ++ show (Map.size (gEdges (lrGraph graphA))) ++ " edges"
              logInfo env $ T.pack $ "[merge] Graph B: " ++ show (Map.size (gNodes (lrGraph graphB))) ++ " nodes, " ++ show (Map.size (gEdges (lrGraph graphB))) ++ " edges"
              logInfo env "[merge] Merging graphs..."
              let res = Resolution { resGamma = resolution
                                   , resMinSize = minCommSize
                                   , resMergeInto = MergeToNeighbor
                                   , resMaxIterations = maxLeidenIterations }
                  mergeResult = mergeGraphsAndAnalyze (lrGraph graphA) (lrGraph graphB) density res (gcSemanticEdges defaultGraphosConfig) False
                  mergedGraph = Merge.mrGraph mergeResult
                  commMap = mrCommunities mergeResult
              logInfo env $ T.pack $ "[merge] Merged graph: " ++ show (Map.size (gNodes mergedGraph)) ++ " nodes, " ++ show (Map.size (gEdges mergedGraph)) ++ " edges"
              logInfo env $ T.pack $ "[merge] Communities: " ++ show (Map.size commMap)
              -- Export
              createDirectoryIfMissing True outputDir
              let analysis = mrAnalysis mergeResult
                  graphosCfg = defaultGraphosConfig
                  config = defaultConfig
                        { cfgOutputDir = outputDir
                        , cfgNoViz = noViz
                        , cfgEdgeDensity = density
                        , cfgResolution = resolution
                        , cfgMinCommSize = minCommSize
                        , cfgMaxLeidenIterations = maxLeidenIterations
                        , cfgGraphosConfig = graphosCfg
                        }
                  detection = Detection
                        { detectionTotalFiles = 0
                        , detectionTotalWords = 0
                        , detectionNeedsGraph = True
                        , detectionWarning = Nothing
                        , detectionFiles = Map.empty
                        , detectionClassification = Map.empty
                        , detectionExclusions = emptyExclusionCounts
                        }
              logInfo env "[merge] Exporting..."
              exports <- Export.exportAll (exportPort appEnv) mergedGraph analysis config detection Nothing []
              logInfo env "[merge] Merge complete!"
              logInfo env $ T.pack $ "  Nodes: " ++ show (Map.size (gNodes mergedGraph))
              logInfo env $ T.pack $ "  Edges: " ++ show (Map.size (gEdges mergedGraph))
              logInfo env $ T.pack $ "  Communities: " ++ show (Map.size commMap)
              logInfo env $ T.pack $ "  Report: " ++ erReport exports
              logInfo env $ T.pack $ "  Graph: " ++ erJSON exports
              case erHTML exports of
                Just html -> logInfo env $ T.pack $ "  HTML: " ++ html
                Nothing   -> pure ()

    IngestCmd filePath embedOverride outputDir labelFlag -> do
      -- Load graphos.yaml config
      graphosCfg <- loadConfig
      let logLevel = LogInfo
          ingestCfg = gcIngest graphosCfg
          effectiveEmbed = maybe (icEmbed ingestCfg) id embedOverride
          config = defaultConfig
                { cfgOutputDir = outputDir
                , cfgEmbed = effectiveEmbed
                , cfgGraphosConfig = graphosCfg
                , cfgLabel = labelFlag
                }
      env <- defaultLogEnv logLevel
      obsEnv <- initObservability logLevel (cfgOtelConfig config) (cfgMetricsPort config) (cfgOutputDir config ++ "/traces")
      let appEnv = productionAppEnv env obsEnv
      logInfo env $ T.pack $ "[ingest] Ingesting file: " ++ filePath ++ (if effectiveEmbed then " (embeddings enabled)" else "")
      result <- Graphos.UseCase.Pipeline.runSingleFilePipeline appEnv config filePath
      case result of
        Left err -> do
          logError env $ "[ingest] Failed: " <> err
          exitWith (ExitFailure 1)
        Right res -> do
          logInfo env "[ingest] Ingest complete!"
          logInfo env $ T.pack $ "  Nodes: " ++ show (sfrNodes res)
          logInfo env $ T.pack $ "  Edges: " ++ show (sfrEdges res)
          logInfo env $ T.pack $ "  Communities: " ++ show (sfrCommunities res)
          logInfo env $ T.pack $ "  Graph: " ++ sfrGraphPath res
          logInfo env $ T.pack $ "  Index: " ++ sfrIndexPath res
          when (sfrEmbeddingCount res > 0) $
            logInfo env $ T.pack $ "  Embeddings: " ++ show (sfrEmbeddingCount res) ++ " vectors"

    SpeccheckCmd specsDir asJson strictCov checkNames strictDups mGraph adjudicate -> do
      parsed <- case mGraph of
        Just graphPath -> do
          loadResult <- loadGraphFromFile graphPath
          pure $ case loadResult of
            Left err -> Left err
            Right loaded -> Right
              ( Map.elems (gNodes (lrGraph loaded))
              , Map.elems (gEdges (lrGraph loaded))
              )
        Nothing -> parseSpecCorpus specsDir
      case parsed of
        Left err -> do
          hPutStrLn stderr $ "[graphos] speccheck: " ++ T.unpack err
          exitWith (ExitFailure 1)
        Right (specNodes, specEdges) -> do
          let checks = [ c | Just c <- map parseCheckName checkNames ]
              -- Duplication candidates need node embeddings; the structural
              -- corpus parse carries none, so they only appear when a graph
              -- with an embeddings sidecar is loaded (future: --embeddings).
              dups = []
              spofs = spofDecisions specNodes specEdges
          madj <- if adjudicate
            then do
              let report0 = runSpecCheck'
                              specNodes specEdges strictCov strictDups dups spofs Nothing
              adjns <- SpecAdjudicate.adjudicateCandidates
                         productionLLMPort defaultLabelingConfig specNodes
                         (srCandidates report0) 2
              pure (Just
                [ (SpecAdjudicate.adjPair a, SpecAdjudicate.adjVerdict a, SpecAdjudicate.adjRationale a)
                | a <- adjns ])
            else pure Nothing
          let report = runSpecCheck'
                         specNodes specEdges strictCov strictDups dups spofs madj
          if asJson
            then BL.putStr (encode (filterReport checks report)) >> putStrLn ""
            else TIO.putStrLn (renderSpecReport (filterReport checks report))
          if reportGates report then exitWith (ExitFailure 1) else exitSuccess

    SubgraphCmd graphPath mConfigPath outPath boundaryHops noDerive -> do
      case mConfigPath of
        Nothing -> do
          hPutStrLn stderr "[graphos] subgraph: --config is required (JSON: named subsystems with path patterns)"
          exitWith (ExitFailure 1)
        Just configPath -> do
          putStrLn $ "[graphos] Subgraph: loading " ++ graphPath
          loadResult <- loadGraphFromFile graphPath
          case loadResult of
            Left err -> do
              putStrLn $ "Error: " ++ T.unpack err
              exitWith (ExitFailure 1)
            Right loaded -> do
              mCfg <- decode <$> BL.readFile configPath
              case mCfg of
                Nothing -> do
                  hPutStrLn stderr $ "[graphos] subgraph: failed to parse config: " ++ configPath
                  exitWith (ExitFailure 1)
                Just cfg -> do
                  let subCfg = cfg { scMaxHops = boundaryHops, scIncludeDerived = not noDerive }
                      sub = extractSubgraph (toLabeledGraph (lrGraph loaded)) subCfg
                  putStrLn $ "[graphos] Subgraph: " ++ show (Map.size (LG.gNodes sub))
                           ++ " nodes, " ++ show (Map.size (LG.gEdges sub)) ++ " edges"
                  exportSubgraphJSON sub outPath
                  putStrLn $ "[graphos] Subgraph written to " ++ outPath

    LServers -> do
      putStrLn "[graphos] Discovering available LSP servers..."
      servers <- discoverLanguageServers
      if null servers
        then putStrLn "  No LSP servers found. Install language servers for the languages you use."
        else do
          putStrLn $ "  Found " ++ show (length servers) ++ " LSP server(s):"
          mapM_ (\s -> putStrLn $ "    " ++ T.unpack (lsiName s) ++ " (" ++ lsiCommand s ++ ") - " ++ show (lsiExtensions s)) servers

    MigrateGraphCmd graphPath mOutputPath -> do
      let outPath = maybe graphPath id mOutputPath
      putStrLn $ "[graphos] Migrate-graph: loading " ++ graphPath
      loadResult <- loadGraphFromFile graphPath
      case loadResult of
        Left err -> do
          putStrLn $ "Error: " ++ T.unpack err
          exitWith (ExitFailure 1)
        Right loaded -> do
          let g = lrGraph loaded
          exportGraphWithLabels g (emptyAnalysisForMigrate loaded) (Just (lrCommunityLabels loaded)) outPath
          putStrLn $ "[graphos] Migrated " ++ graphPath ++ " -> " ++ outPath
                     ++ " (JGF envelope, application/vnd.jgf+json)"
          putStrLn $ "[graphos] Nodes: " ++ show (Map.size (gNodes g))
                     ++ ", edges: " ++ show (Map.size (gEdges g))

    Serve dir graphPath port apiOnly noApi -> do
      putStrLn $ "[graphos] Serving " ++ dir ++ " on port " ++ show port
      startServeServer dir graphPath port apiOnly noApi

    Init agentsOpt -> do
      initConfigFile
      case agentsOpt of
        Nothing -> putStrLn "[init] Hint: use --agents to scaffold AI agent integration files."
        Just rawTargets -> do
          let targetStrs = case rawTargets of
                "auto" -> Nothing
                ""     -> Nothing
                ts     -> Just $ map (parseTarget . T.pack) $ splitCommas ts
          case targetStrs of
            Just parsed | Left err <- sequence parsed -> do
              putStrLn $ "[init] Error: " ++ T.unpack err
              exitWith (ExitFailure 1)
            _ -> do
              let validTargets = case targetStrs of
                    Just parsed -> rights parsed
                    Nothing -> []
              facts <- gatherDetectionFacts
              let selected = case validTargets of
                    [] -> selectTargets Nothing facts
                    ts -> case nonEmpty ts of
                      Nothing -> selectTargets Nothing facts
                      Just ne -> ne
              let req = ScaffoldRequest
                    { srTargets = selected
                    , srVersion = "0.1.0.0"
                    }
                  ref = CommandReference renderCommandReference
                  files = planScaffold req ref
              _ <- writeScaffold files
              pure ()

    InstallSkill target -> do
      let ref = CommandReference renderCommandReference
      runInstallSkill "0.1.0.0" target ref

  where
    opts = info (commandOpts <**> helper)
      ( fullDesc
     <> progDesc "Graphos - Universal knowledge graph builder using LSP"
     <> header "graphos - any input → knowledge graph → clustered communities → HTML + JSON + report"
      )

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Convert the rich 'Graph' (edges keyed by endpoint pair) into the plain
-- 'LabeledGraph' used by the pure subgraph module.
toLabeledGraph :: Graph -> LG.LabeledGraph
toLabeledGraph gr = LG.LabeledGraph
  { LG.gNodes   = gNodes gr
  , LG.gEdges   = Map.fromList [(edgeId e, e) | e <- Map.elems (gEdges gr)]
  , LG.gAdjFwd  = gAdjFwd gr
  , LG.gAdjBack = gAdjBack gr
  }


-- ───────────────────────────────────────────────
-- graphos init — generate config file
-- ───────────────────────────────────────────────

-- | Generate a graphos.yaml config file with defaults and comments
-- (multi-source-graphs 2.3 / workflow 15). The file is produced by rendering
-- the default 'GraphosConfig' through 'writeConfigYaml' (the Config-module
-- export) followed by the commented documentation sections — the commented
-- @sources:@ example and @output:@ key among them — so a generated file both
-- parses as a valid config and documents every optional section.
initConfigFile :: IO ()
initConfigFile = do
  let path = "graphos.yaml"
  exists <- doesFileExist path
  if exists
    then putStrLn $ "[init] " ++ path ++ " already exists. Delete it first if you want to regenerate."
    else do
      cfg <- generateDefaultConfig
      writeConfigYaml path cfg
      appendFile path configDocComments
      putStrLn $ "[init] Created " ++ path ++ " with default configuration."
      putStrLn "[init] Edit it to customize LSP servers, extractors, sources, and file extensions."

-- ───────────────────────────────────────────────
-- Helpers for --agents
-- ───────────────────────────────────────────────

splitCommas :: String -> [String]
splitCommas s = case break (== ',') s of
  (chunk, "")     -> [chunk | not (null chunk)]
  (chunk, _:rest) -> chunk : splitCommas rest

rights :: [Either a b] -> [b]
rights = foldr go []
  where go (Left _) acc = acc
        go (Right x) acc = x : acc

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty []     = Nothing
nonEmpty (x:xs) = Just (x :| xs)