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
import System.Process (createProcess, proc)
import System.Environment (getArgs, getExecutablePath, withArgs)

import Graphos.CLI.Parser
import Graphos.Domain.Types (PipelineConfig(..), Node(..), Edge(..), relationToText, edgeConfidence, Detection(..), emptyExclusionCounts, defaultConfig)
import qualified Graphos.Domain.Types.Graph as LG (LabeledGraph(..))
import Graphos.UseCase.Subgraph (extractSubgraph, SubgraphConfig(..))
import Graphos.Infrastructure.Export.JSON (exportSubgraphJSON)
import Graphos.Domain.Types.Pipeline (Neo4jPushMode(..), MemgraphPushMode(..))
import Graphos.UseCase.Pipeline (runPipeline, runIncrementalPipeline, runSingleFilePipeline, PipelineResult(..), SingleFileResult(..))
import Graphos.Infrastructure.Wiring (productionAppEnv)
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Load (loadGraphFromFile, loadGraphFromFileStrict, LoadResult(..))
import Graphos.UseCase.Query (queryGraphWithIndexScored, pathQueryWithIndex, explainNodeWithIndex, symbolLookup, neighborhoodExpansion, resolveNodeArg, NodeResolution(..))
import Graphos.Domain.Query.Cypher.Parser (parseStatement)
import Graphos.Domain.Query.Cypher.AST (CypherStatement(..))
import Graphos.Domain.Query.Cypher.Eval (evaluateStatement)
import qualified Graphos.Domain.Query.Cypher.Eval as MutEval (mrGraph)
import Graphos.UseCase.Query.Research (buildResearchViewIO, expandWithSeeds)
import Graphos.Domain.Community (computeCompositions)
import Graphos.UseCase.Merge (mergeGraphsAndAnalyze, MergeResult(..))
import qualified Graphos.UseCase.Merge as Merge (mrGraph)
import Graphos.Domain.Graph (Graph, gNodes, gEdges, gAdjFwd, gAdjBack, neighbors, degree)
import Graphos.Domain.Graph.Analysis (articulationPoints)
import Graphos.Domain.Graph.Index (communityOfNode)
import Graphos.UseCase.Query.Refine (RefineConfig(..), refineResponse)
import Graphos.UseCase.Query.Render (CommonQueryOpts(..), renderQueryResponseText, renderQueryResponseJSON, renderSymbolResultText, renderSymbolResultJSON, renderNeighborsResultText, renderNeighborsResultJSON, renderPathResultJSON, renderExplainResultJSON, renderAmbiguousText, renderAmbiguousJSON, renderNotFoundText, renderNotFoundJSON, renderMutationResultText, renderMutationResultJSON)
import Graphos.Infrastructure.Export.PersistMutation (persistMutatedGraph)
import Graphos.Domain.Community (detectCommunities, scoreAllCohesion, Resolution(..), MergeStrategy(..))
import Graphos.Infrastructure.LSP.Capabilities (LanguageServerInfo(..), discoverLanguageServers)
import Graphos.Infrastructure.Logging (LogLevel(..), defaultLogEnv, logInfo, logDebug, logError)
import Graphos.Infrastructure.Export.Neo4j (pushSubgraphToNeo4j, pushCommunityGraphToNeo4j, pushToNeo4jWithCommunities)
import Graphos.Infrastructure.Export.Memgraph (pushToMemgraphWithCommunities, pushSubgraphToMemgraph, pushCommunityGraphToMemgraph)
import Graphos.Infrastructure.Observability.SDK
  ( initObservability
  , shutdownObservability
  , ObservabilityEnv(..)
  , OtelConfig(..)
  , defaultOtelConfig
  )
import Graphos.Domain.Config (defaultGraphosConfig, ObservabilityConfig(..), gcObservability, VisionConfig(..), vcEnabled, gcVision, gcIngest, icEmbed, gcSemanticEdges)
import Graphos.Infrastructure.Config (loadConfig)
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

reexecWithRTS :: Bool -> Maybe String -> IO ()
reexecWithRTS profile heapStr = do
  originalArgs <- getArgs
  exePath <- getExecutablePath
  let rtsFlags = concat
        [ if profile then "+RTS -s -hT" else ""
        , if not (null rtsFlags) && isJust heapStr then " " else ""
        , maybe "" (\sz -> "+RTS -M " ++ sz) heapStr
        ]
  case rtsFlags of
    "" -> pure ()
    _ -> do
      hPutStrLn stderr $ "[graphos] Re-executing with RTS flags: " ++ rtsFlags
      hFlush stderr
      let (cleanArgs, _, _) = stripRTSFlags originalArgs
          finalArgs = filter (not . null) (words rtsFlags) ++ ["--"] ++ cleanArgs
      let spec = proc exePath finalArgs
      _ <- createProcess spec
      exitSuccess

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = dropWhile (/= "--") rawArgs
  cmd <- withArgs args (execParser opts)
  case cmd of
    Run config -> do
      let heapStr = fmap (\mb -> show mb ++ "M") (cfgMaxHeap config)
      when (cfgRtsProfile config || isJust (cfgMaxHeap config)) $
        reexecWithRTS (cfgRtsProfile config) heapStr
      -- Load graphos.yaml config and merge with CLI defaults
      graphosCfg <- loadConfig
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
                                    then cfgOutputDir config ++ "/traces"
                                    else obsDebugTraceDir obsCfg
          config' = config { cfgGraphosConfig = graphosCfg { gcVision = (gcVision graphosCfg) { vcEnabled = cfgVision config || vcEnabled (gcVision graphosCfg) } }
                            , cfgOtelConfig     = otelCfg
                            , cfgMetricsPort    = metricsPort
                            , cfgDebugTraceDir  = Just debugDir
                            }
      -- Initialize observability (tracing, metrics, debug trace)
      let logLevel = if cfgDebug config || obsDebug obsCfg then LogTrace
                      else if cfgVerbose config then LogDebug
                      else LogInfo
      obsEnv <- initObservability logLevel otelCfg metricsPort debugDir
      let _tracer = otelTracer obsEnv
          _metrics = otelMetrics obsEnv
          env = otelLogEnv obsEnv
          appEnv = productionAppEnv env obsEnv
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
              scoredResp = queryGraphWithIndexScored g idx question mode budget
              refineCfg = RefineConfig { rcEdgeMode = cqoEdges qopts, rcLabelWidth = cqoLabelWidth qopts }
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

    ResearchCmd termsArg seedsArg graphPath doHtml doJson termsFileArg labelArg researchMode commonOpts -> do
      hSetBuffering stdout NoBuffering
      loadResult <- loadGraphOpt (cqoStrictGraph commonOpts) graphPath
      case loadResult of
        Left err -> putStrLn $ "Error: " ++ T.unpack err
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
                  putStrLn $ "Error: terms file not found: " ++ path
                  exitWith (ExitFailure 1)
          let terms = termsArg <> termsFileTerms
              dedupedTerms = go mempty terms
               where
                 go _ [] = []
                 go seen (t:rest)
                   | Map.member t seen = go seen rest
                   | otherwise = t : go (Map.insert t () seen) rest
          let _expandedUnion = expandWithSeeds g idx Set.empty seedsArg
          rv <- buildResearchViewIO g idx commMap comps dedupedTerms edgeMode
          case labelArg of
            Just lbl -> putStrLn $ "Output label: " ++ lbl
            Nothing  -> putStrLn "No label provided"
          putStrLn $ "Research mode: " ++ case researchMode of
            Just m  -> T.unpack m
            Nothing -> "default"
          putStrLn $ "Seeds for expansion: " ++ show (length seedsArg)
          when doJson $ do
            BL.putStr (encode rv)
          when doHtml $ do
            putStrLn "HTML export not yet implemented"

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

-- | Generate a graphos.yaml config file with defaults and comments.
initConfigFile :: IO ()
initConfigFile = do
  let path = "graphos.yaml"
  exists <- doesFileExist path
  if exists
    then putStrLn $ "[init] " ++ path ++ " already exists. Delete it first if you want to regenerate."
    else do
      writeFile path defaultConfigYaml
      putStrLn $ "[init] Created " ++ path ++ " with default configuration."
      putStrLn "[init] Edit it to customize LSP servers, extractors, and file extensions."

-- | Default graphos.yaml content with comments explaining each section.
defaultConfigYaml :: String
defaultConfigYaml = unlines
  [ "# ───────────────────────────────────────────────"
  , "# Graphos Configuration"
  , "# ───────────────────────────────────────────────"
  , "# This file configures extraction, LLM, embedding, vision, and observability."
  , "# Any field left out will fall back to built-in defaults."
  , "# User values override defaults (for LSP/language_ids maps)."
  , "#"
  , "# Config resolution (later wins):"
  , "#   1. Built-in defaults"
  , "#   2. Global config: ~/.config/graphos/graphos.yaml"
  , "#   3. This file (project graphos.yaml)"
  , "#   4. CLI flags (--otel, --metrics, --embed, --no-embed, etc.)"
  , ""
  , "# ──── Ingest (single-file) ───────────────────"
  , "# Settings for `graphos ingest <file>`. Optimized for codebase analysis."
  , "ingest:"
  , "  embed: true                   # generate embeddings by default (override with --no-embed)"
  , "  merge: true                  # merge into existing graph.json"
  , "  deduplicate: true            # skip unchanged files via SHA256"
  , "  resolution: 0.8              # smaller communities for single-file graphs"
  , "  min_comm_size: 2             # minimum community size for ingest"
  , "  max_leiden_iter: 20          # converge quickly on small graphs"
  , "  index_path: graphos-out/index.json"
  , "  url:"
  , "    timeout: 30                # seconds to wait for URL response"
  , "    user_agent: graphos/0.1.0"
  , "    retry: 1                   # retries on download failure"
  , "  categories:                  # per-category overrides (Nothing = inherit top-level)"
  , "    image:"
  , "      embed: false             # images: don't embed by default"
  , "    video:"
  , "      embed: false             # videos: don't embed by default"
  , ""
  , "# ──── Extraction granularity ──────────────────"
  , "# fine     — statement-level nodes (verbose, ~100+ nodes/file)"
  , "# function — functions/types/classes + module-level constants (default)"
  , "# file     — one node per file"
  , "# Per-extension override: add `granularity: file` under an extractors entry."
  , "# CLI flag --granularity overrides both."
  , "granularity: function"
  , ""
  , "# ──── Extractors ─────────────────────────────"
  , "# How to extract symbols from each file type."
  , "#   tree-sitter — fast AST parsing, no server needed (default for all)"
  , "#   lsp         — Language Server Protocol (richer semantic info, requires server)"
  , "#   stub        — single node per file (no parsing)"
  , "#"
  , "# All languages default to tree-sitter. Uncomment `mode: lsp` to switch."
  , "extractors:"
  , "  # TypeScript (default: tree-sitter)"
  , "  \".ts\":"
  , "    mode: tree-sitter"
  , "    grammar: typescript"
  , "    language_id: typescript"
  , "  \".tsx\":"
  , "    mode: tree-sitter"
  , "    grammar: tsx"
  , "    language_id: typescriptreact"
  , "  \".js\":"
  , "    mode: tree-sitter"
  , "    grammar: javascript"
  , "    language_id: javascript"
  , "  \".jsx\":"
  , "    mode: tree-sitter"
  , "    grammar: javascript"
  , "    language_id: javascriptreact"
  , "  # Haskell (default: tree-sitter; uncomment for LSP)"
  , "  \".hs\":"
  , "    mode: tree-sitter"
  , "    grammar: haskell"
  , "    language_id: haskell"
  , "    # mode: lsp"
  , "    # language_id: haskell"
  , "  \".lhs\":"
  , "    mode: tree-sitter"
  , "    grammar: haskell"
  , "    language_id: haskell"
  , "  # Go (default: tree-sitter; uncomment for LSP)"
  , "  \".go\":"
  , "    mode: tree-sitter"
  , "    grammar: go"
  , "    language_id: go"
  , "    # mode: lsp"
  , "    # language_id: go"
  , "  # Rust (default: tree-sitter; uncomment for LSP)"
  , "  \".rs\":"
  , "    mode: tree-sitter"
  , "    grammar: rust"
  , "    language_id: rust"
  , "    # mode: lsp"
  , "    # language_id: rust"
  , "  # Python (default: tree-sitter; uncomment for LSP)"
  , "  \".py\":"
  , "    mode: tree-sitter"
  , "    grammar: python"
  , "    language_id: python"
  , "    # mode: lsp"
  , "    # language_id: python"
  , "  \".pyw\":"
  , "    mode: tree-sitter"
  , "    grammar: python"
  , "    language_id: python"
  , "  # C/C++ (default: tree-sitter; uncomment for LSP)"
  , "  \".c\":"
  , "    mode: tree-sitter"
  , "    grammar: c"
  , "    language_id: c"
  , "    # mode: lsp"
  , "    # language_id: c"
  , "  \".cpp\":"
  , "    mode: tree-sitter"
  , "    grammar: cpp"
  , "    language_id: cpp"
  , "  \".h\":"
  , "    mode: tree-sitter"
  , "    grammar: c"
  , "    language_id: c"
  , "  \".hpp\":"
  , "    mode: tree-sitter"
  , "    grammar: cpp"
  , "    language_id: cpp"
  , "  # Nix (default: tree-sitter; uncomment for LSP)"
  , "  \".nix\":"
  , "    mode: tree-sitter"
  , "    grammar: nix"
  , "    language_id: nix"
  , "    # mode: lsp"
  , "    # language_id: nix"
  , "  # Ruby (default: tree-sitter; uncomment for LSP)"
  , "  \".rb\":"
  , "    mode: tree-sitter"
  , "    grammar: ruby"
  , "    language_id: ruby"
  , "    # mode: lsp"
  , "    # language_id: ruby"
  , "  # Java (default: tree-sitter; uncomment for LSP)"
  , "  \".java\":"
  , "    mode: tree-sitter"
  , "    grammar: java"
  , "    language_id: java"
  , "    # mode: lsp"
  , "    # language_id: java"
  , "  # JSON: tree-sitter with file-level granularity (data files don't inflate the graph)"
  , "  \".json\":"
  , "    mode: tree-sitter"
  , "    grammar: json"
  , "    language_id: json"
  , "    granularity: file"
  , "  # Markdown: tree-sitter with built-in parser"
  , "  \".md\":"
  , "    mode: tree-sitter"
  , "    grammar: markdown"
  , "    language_id: markdown"
  , "  \".rst\":"
  , "    mode: tree-sitter"
  , "    grammar: markdown"
  , "    language_id: rest"
  , "  \".adoc\":"
  , "    mode: tree-sitter"
  , "    grammar: markdown"
  , "    language_id: asciidoc"
  , ""
  , "# ──── PDF Extraction ───────────────────────────"
  , "# How aggressively to extract content from PDF files."
  , "#   small  — file node + title only (minimal graph footprint)"
  , "#   medium — file + titles + sections, no subsections/paragraphs (default)"
  , "#   large  — full hierarchy: all section levels + paragraphs (~max nodes)"
  , "pdf_extraction: medium"
  , ""
  , "# ──── LSP Servers ─────────────────────────────"
  , "# Map file extension → {command, args, language_id}."
  , "# Set command to \"\" to explicitly disable an extension's LSP."
  , "# Unlisted extensions use defaults from Graphos.Domain.Config."
  , "# lsp:"
  , "#   \".ts\":"
  , "#     command: typescript-language-server"
  , "#     args: [\"--stdio\"]"
  , "#     language_id: typescript"
  , ""
  , "# ──── Language IDs ─────────────────────────────"
  , "# Override or add language IDs for file extensions."
  , "# language_ids:"
  , "#   \".nix\": nix"
  , ""
  , "# ──── File Extension Categories ────────────────"
  , "# Full override for each category (replaces defaults)."
  , "# file_extensions:"
  , "#   code: [\".py\", \".ts\", \".tsx\", \".js\", \".jsx\", \".go\", \".rs\", \".hs\", \".nix\"]"
  , "#   doc: [\".md\"]"
  , "#   paper: [\".pdf\"]"
  , "#   image: [\".png\", \".jpg\", \".jpeg\", \".webp\", \".gif\"]"
  , "#   video: [\".mp4\", \".mov\", \".mkv\", \".webm\"]"
  , "#   office: [\".docx\", \".pptx\", \".xlsx\", \".doc\", \".ppt\"]"
  , ""
  , "# ──── Neo4j ──────────────────────────────────────"
  , "# Used by: graphos . --neo4j --neo4j-push"
  , "# push_mode: full (all nodes), subgraph (communities + representatives), community (communities only)"
  , "neo4j:"
  , "  uri: \"http://localhost:7474\""
  , "  user: \"neo4j\""
  , "  password: \"graphos_dev\""
  , "  push_mode: \"subgraph\""
  , "  subgraph_size: 7"
  , ""
  , "# ──── Memgraph ────────────────────────────────────"
  , "# In-memory graph database, Bolt-protocol compatible."
  , "# Used by: graphos . --memgraph --memgraph-push bolt://localhost:7688"
  , "# No auth by default — leave user/password empty for local dev."
  , "memgraph:"
  , "  uri: \"bolt://localhost:7688\""
  , "  user: \"\""
  , "  password: \"\""
  , "  push_mode: \"subgraph\""
  , "  subgraph_size: 7"
  , ""
  , "# ──── LLM Labeling ────────────────────────────────"
  , "# Community labeling via LLM. Default: local Ollama (zero-config)."
  , "# Supports any OpenAI-compatible API (OpenAI, Ollama, LiteLLM, etc.)."
  , "# Set api_key to env var reference ${VAR} or a literal string."
  , "# Use headers for custom auth (e.g. X-API-Key for enterprise gateways)."
  , "# For OpenAI: uncomment provider/model/api_key/base_url below."
  , "labeling:"
  , "  provider: ollama              # ollama | openai | litellm (default: ollama)"
  , "  model: llama3.2               # default: llama3.2 (ollama); gpt-4o-mini (openai)"
  , "  api_key: \"\"                   # empty for ollama; ${OPENAI_API_KEY} for openai"
  , "  base_url: \"http://localhost:11434/v1\"  # default: ollama local"
  , "  batch_size: 20                # communities per LLM call (default: 20)"
  , "  # headers:                    # custom HTTP headers for auth (default: none)"
  , "  #   X-API-Key: \"${MY_TOKEN}\""
  , "  #   X-Tenant-ID: \"my-tenant\""
  , "  # --- OpenAI example (uncomment to use) ---"
  , "  # provider: openai"
  , "  # model: gpt-4o-mini"
  , "  # api_key: \"${OPENAI_API_KEY}\""
  , "  # base_url: \"https://api.openai.com/v1\""
  , ""
  , "# ──── Embedding ───────────────────────────────────"
  , "# Local embedding generation via Ollama. Disabled by default."
  , "# Enable with --embed flag or embedding.enabled: true."
  , "# Targets small local models (nomic-embed-text, all-minilm)."
  , "embedding:"
  , "  enabled: false               # default: false (enable with --embed)"
  , "  provider: ollama              # ollama (default, only local for now)"
  , "  model: nomic-embed-text       # default: nomic-embed-text"
  , "  base_url: \"http://localhost:11434/v1\"  # default: ollama local"
  , "  dimension: 0                  # 0 = auto-detect from model"
  , "  # headers:                    # custom HTTP headers for auth (default: none)"
  , "  #   X-API-Key: \"${MY_TOKEN}\""
  , ""
  , "# ──── Vision ──────────────────────────────────────"
  , "# Multimodal LLM for image analysis. Disabled by default."
  , "# Enable with --vision flag or vision.enabled: true."
  , "# When apiKey/baseUrl not set, inherits from labeling config."
  , "vision:"
  , "  enabled: false                # default: false (enable with --vision)"
  , "  model: qwen3.6-moe            # default: qwen3.6-moe (ollama)"
  , "  api_key: \"\"                   # empty: inherits from labeling"
  , "  base_url: \"http://localhost:11434/v1\"  # default: ollama local"
  , "  max_tokens: 1000             # max tokens for vision response"
  , "  batch_size: 5                 # images per batch with GC between"
  , "  # headers:                    # custom HTTP headers for auth (default: none)"
  , "  #   X-API-Key: \"${MY_TOKEN}\""
  , ""
  , "# ──── Observability ─────────────────────────────────"
  , "# Tracing, metrics, and debug instrumentation."
  , "# CLI flags (--otel, --metrics, --debug-trace) override these values."
  , "# Use --no-observability to completely disable all observability."
  , "observability:"
  , "  enabled: false               # default: false (enable with --otel)"
  , "  endpoint: \"http://localhost:4318\"  # OTLP collector endpoint"
  , "  metricsPort: 0               # Prometheus metrics port (0 = disabled)"
  , "  serviceName: graphos"
  , "  serviceVersion: \"0.1.0\""
  , "  exportInterval: 15           # metrics export interval in seconds"
  , "  debugTraceDir: \"\"            # directory for debug trace JSONL (empty = disabled)"
  ]

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
