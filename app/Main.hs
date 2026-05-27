-- | Graphos CLI - main entry point
module Main where

import Options.Applicative
import System.Exit (exitWith, ExitCode(..))
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_)
import Control.Concurrent.MVar (newMVar)
import Data.Maybe (isJust)

import Graphos.Domain.Types (PipelineConfig(..), EdgeDensity(..), Neo4jPushMode(..), MemgraphPushMode(..), Node(..), Edge(..), relationToText, edgeRelation, edgeConfidence, Detection(..), defaultConfig)
import Graphos.UseCase.Pipeline (runPipeline, runIncrementalPipeline, PipelineResult(..))
import Graphos.UseCase.Load (loadGraphFromFile, LoadResult(..))
import Graphos.UseCase.Query (queryGraphWithIndex, pathQueryWithIndex, explainNodeWithIndex, QueryResult(..))
import Graphos.UseCase.Merge (mergeGraphsAndAnalyze, MergeResult(..))
import Graphos.Domain.Graph (gNodes, gEdges, neighbors, degree)
import Graphos.Domain.Graph.Analysis (articulationPoints)
import Graphos.Domain.Graph.Index (communityOfNode)
import Graphos.Domain.Community (detectCommunities, scoreAllCohesion, Resolution(..), MergeStrategy(..))
import Graphos.Infrastructure.LSP.Capabilities (LanguageServerInfo(..), discoverLanguageServers)
import Graphos.Infrastructure.Logging (LogLevel(..), defaultLogEnv, logInfo, logDebug, logError)
import Graphos.Infrastructure.Export.Neo4j (pushSubgraphToNeo4j, pushCommunityGraphToNeo4j, pushToNeo4jWithCommunities)
import Graphos.Infrastructure.Export.Memgraph (pushToMemgraphWithCommunities, pushSubgraphToMemgraph, pushCommunityGraphToMemgraph)
import Graphos.Infrastructure.Observability
  ( initObservability
  , OtelConfig(..), defaultOtelConfig
  , ObservabilityEnv(..)
  )
import Graphos.Domain.Config (defaultGraphosConfig, ObservabilityConfig(..), gcObservability)
import Graphos.Infrastructure.Config (loadConfig)
import Graphos.Infrastructure.Server.Static (startStaticServer)
import Graphos.Infrastructure.Server.MCP (startMCPServerFromFile)
import Graphos.Infrastructure.FileSystem.Watcher (watchDirectory, defaultGraphosWatchConfig)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory (doesFileExist, createDirectoryIfMissing)

import qualified Graphos.UseCase.Export as Export


-- ───────────────────────────────────────────────
-- CLI argument parsing
-- ───────────────────────────────────────────────

data Command
  = Run PipelineConfig
  | QueryCmd Text Text Int FilePath
  | PathCmd Text Text FilePath
  | ExplainCmd Text FilePath
  | PushCmd FilePath String String String Neo4jPushMode Int
  | PushMemgraphCmd FilePath String String String MemgraphPushMode Int
  | MergeCmd FilePath FilePath FilePath EdgeDensity Double Int Int Bool Bool
  | LServers
  | Serve FilePath Int
  | Init

pipelineOpts :: Parser PipelineConfig
pipelineOpts = PipelineConfig
  <$> argument str (metavar "PATH" <> value "." <> help "Input directory (default: .)")
  <*> strOption (long "output" <> short 'o' <> value "graphos-out" <> help "Output directory")
  <*> switch (long "directed" <> help "Build directed graph")
  <*> switch (long "deep" <> help "Deep extraction mode")
  <*> switch (long "no-viz" <> help "Skip HTML visualization")
  <*> switch (long "update" <> help "Incremental update")
  <*> switch (long "cluster-only" <> help "Rerun clustering only")
  <*> switch (long "no-cluster" <> help "Skip clustering entirely")
  <*> switch (long "label" <> help "Use LLM to label communities (requires --neo4j or graphos.yaml config)")
  <*> switch (long "obsidian" <> help "Generate Obsidian vault")
  <*> optional (strOption (long "obsidian-dir" <> help "Obsidian vault output directory"))
  <*> switch (long "neo4j" <> help "Generate Cypher for Neo4j")
  <*> optional (strOption (long "neo4j-push" <> help "Push to Neo4j at URI"))
  <*> option auto (long "neo4j-push-mode" <> value SubgraphPush <> help "Neo4j push mode: full|subgraph|community (default: subgraph)")
  <*> option auto (long "neo4j-subgraph-size" <> value 7 <> help "Representatives per community for subgraph mode (default: 7)")
  <*> optional (strOption (long "mcp" <> metavar "GRAPH_JSON" <> help "Start MCP server with graph file"))
  <*> switch (long "svg" <> help "Export SVG")
  <*> switch (long "graphml" <> help "Export GraphML")
  <*> switch (long "watch" <> help "Watch for file changes")
  <*> switch (long "wiki" <> help "Build agent-crawlable wiki")
  <*> switch (long "verbose" <> short 'v' <> help "Verbose output: show DEBUG level logs")
  <*> switch (long "debug" <> help "Debug output: show TRACE level logs + internal details")
  <*> option auto (long "edge-density" <> value Normal <> help "Edge density: sparse|normal|dense|maximum (default: normal)")
  <*> option auto (long "resolution" <> value 1.0 <> help "Community resolution: higher = fewer larger communities (default: 1.0, try 0.3-0.5 for 100k+ nodes)")
   <*> option auto (long "min-comm-size" <> value 3 <> help "Minimum community size; smaller get merged (default: 3, try 10-20 for 100k+ nodes)")
   <*> option auto (long "max-leiden-iterations" <> value 50 <> help "Max Leiden iterations (default: 50, try 10-20 for 100k+ nodes)")
   <*> option auto (long "threads" <> short 'j' <> value 1 <> help "Number of parallel extraction threads (default: 1)")
   <*> switch (long "community-graph" <> help "Export community-level graph JSON for LLM navigation")
    <*> pure defaultGraphosConfig  -- placeholder; loaded from graphos.yaml at runtime
    <*> pure Nothing  -- cfgNeo4jStreaming: set programmatically when --neo4j is enabled
    <*> switch (long "memgraph" <> help "Generate Cypher for Memgraph")
    <*> optional (strOption (long "memgraph-push" <> help "Push to Memgraph at Bolt URI"))
    <*> option auto (long "memgraph-push-mode" <> value MemgraphSubgraph <> help "Memgraph push mode: MemgraphFull|MemgraphSubgraph|MemgraphCommunity (default: MemgraphSubgraph)")
    <*> option auto (long "memgraph-subgraph-size" <> value 7 <> help "Representatives per community for Memgraph subgraph mode (default: 7)")
    <*> optional (option auto (long "metrics" <> help "Start Prometheus metrics server on given port (e.g. 9090)"))
    <*> switch (long "otel" <> help "Enable OpenTelemetry trace/metric export via OTLP")
    <*> fmap (\ep -> case ep of Nothing -> defaultOtelConfig; Just e -> defaultOtelConfig { otelTracesEndpoint = e ++ "/v1/traces", otelMetricsEndpoint = e ++ "/v1/metrics", otelLogsEndpoint = e ++ "/v1/logs" })
             (optional (strOption (long "otel-endpoint" <> help "OTLP endpoint base (default: http://localhost:4318)")))
    <*> optional (strOption (long "debug-trace" <> help "Directory for debug trace JSONL files"))

queryOpts :: Parser Command
queryOpts = QueryCmd
  <$> argument str (metavar "QUESTION")
  <*> flag "bfs" "dfs" (long "dfs" <> help "Use DFS traversal instead of BFS")
  <*> option auto (long "budget" <> value 2000 <> help "Token budget for query")
  <*> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")

pathOpts :: Parser Command
pathOpts = PathCmd
  <$> argument str (metavar "FROM")
  <*> argument str (metavar "TO")
  <*> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")

serveOpts :: Parser Command
serveOpts = Serve
  <$> strOption (long "dir" <> value "graphos-out" <> help "Directory to serve (default: graphos-out)")
  <*> option auto (long "port" <> short 'p' <> value 8080 <> help "Port to serve on (default: 8080)")

pushOpts :: Parser Command
pushOpts = PushCmd
  <$> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")
  <*> strOption (long "uri" <> value "http://localhost:7474" <> help "Neo4j URI")
  <*> strOption (long "user" <> value "neo4j" <> help "Neo4j username")
  <*> strOption (long "password" <> value "graphos_dev" <> help "Neo4j password")
  <*> option auto (long "mode" <> value SubgraphPush <> help "Push mode: FullPush|SubgraphPush|CommunityPush")
  <*> option auto (long "subgraph-size" <> value 7 <> help "Representatives per community for subgraph mode")

pushMemgraphOpts :: Parser Command
pushMemgraphOpts = PushMemgraphCmd
  <$> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")
  <*> strOption (long "uri" <> value "bolt://localhost:7688" <> help "Memgraph Bolt URI")
  <*> strOption (long "user" <> value "" <> help "Memgraph username (empty = no auth)")
  <*> strOption (long "password" <> value "" <> help "Memgraph password (empty = no auth)")
  <*> option auto (long "mode" <> value MemgraphSubgraph <> help "Push mode: MemgraphFull|MemgraphSubgraph|MemgraphCommunity")
  <*> option auto (long "subgraph-size" <> value 7 <> help "Representatives per community for subgraph mode")

mergeOpts :: Parser Command
mergeOpts = MergeCmd
  <$> argument str (metavar "GRAPH_A" <> help "Path to first graph.json")
  <*> argument str (metavar "GRAPH_B" <> help "Path to second graph.json")
  <*> strOption (long "output" <> short 'o' <> value "graphos-out" <> help "Output directory")
  <*> option auto (long "edge-density" <> value Normal <> help "Edge density: sparse|normal|dense|maximum (default: normal)")
  <*> option auto (long "resolution" <> value 1.0 <> help "Community resolution: higher = fewer larger communities (default: 1.0)")
  <*> option auto (long "min-comm-size" <> value 3 <> help "Minimum community size; smaller get merged (default: 3)")
  <*> option auto (long "max-leiden-iterations" <> value 50 <> help "Max Leiden iterations (default: 50)")
  <*> switch (long "no-viz" <> help "Skip HTML visualization")
  <*> switch (long "verbose" <> short 'v' <> help "Verbose output: show DEBUG level logs")

commandOpts :: Parser Command
commandOpts = subparser
  ( command "query" (info queryOpts (progDesc "Query the knowledge graph"))
 <> command "path"  (info pathOpts (progDesc "Find shortest path between two nodes"))
 <> command "explain" (info (ExplainCmd <$> argument str (metavar "NODE") <*> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")) (progDesc "Explain a node"))
 <> command "push"  (info pushOpts (progDesc "Push graph.json to Neo4j (no extraction needed)"))
 <> command "push-memgraph" (info pushMemgraphOpts (progDesc "Push graph.json to Memgraph (no extraction needed)"))
 <> command "merge" (info mergeOpts (progDesc "Merge two graph.json files into one"))
 <> command "lservers" (info (pure LServers) (progDesc "List available LSP servers"))
 <> command "serve" (info serveOpts (progDesc "Serve HTML graph output via HTTP"))
 <> command "init" (info (pure Init) (progDesc "Generate a graphos.yaml config file"))
  )
  <|> Run <$> pipelineOpts

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Run config -> do
      -- Load graphos.yaml config and merge with CLI defaults
      graphosCfg <- loadConfig
      let obsCfg = gcObservability graphosCfg
          -- Merge config file + CLI flags: CLI flags override config file values
          -- SDK reads OTEL_* env vars; we set them from CLI flags via otelEndpoint/otelServiceName
          otelCfg = defaultOtelConfig
            { otelEnabled        = obsEnabled obsCfg || cfgOtelEnabled config || isJust (cfgMetricsPort config)
            , otelEndpoint       = obsEndpoint obsCfg
            , otelServiceName    = obsServiceName obsCfg
            , otelLogsEndpoint   = obsEndpoint obsCfg ++ "/v1/logs"
            }
          metricsPort = case cfgMetricsPort config of
                           Just p  -> Just p
                           Nothing -> if obsMetricsPort obsCfg > 0 then Just (obsMetricsPort obsCfg) else Nothing
          debugDir = case cfgDebugTraceDir config of
                       Just d  -> d
                       Nothing -> if null (obsDebugTraceDir obsCfg)
                                    then cfgOutputDir config ++ "/traces"
                                    else obsDebugTraceDir obsCfg
          config' = config { cfgGraphosConfig = graphosCfg
                           , cfgOtelConfig     = otelCfg
                           , cfgMetricsPort    = metricsPort
                           , cfgDebugTraceDir  = Just debugDir
                           }
      -- Initialize observability (tracing, metrics, debug trace)
      let logLevel = if cfgDebug config || obsDebug obsCfg then LevelTrace
                      else if cfgVerbose config then LevelDebug
                      else LevelInfo
      obsEnv <- initObservability logLevel otelCfg metricsPort debugDir
      let _tracer = otelTracer obsEnv
          _metrics = otelMetrics obsEnv
      -- MCP mode: start MCP server and exit
      case cfgMCP config' of
         Just graphPath -> do
           putStrLn $ "[graphos] Starting MCP server with " ++ graphPath
           startMCPServerFromFile graphPath
         Nothing ->
           -- Watch mode: run initial pipeline, then watch for changes
           if cfgWatch config'
             then do
               let env = otelLogEnv obsEnv
               logInfo env "Starting initial pipeline (watch mode)..."
               result <- runPipeline config'
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
                     incResult <- runIncrementalPipeline config' (map T.unpack filesList)
                     case incResult of
                       Left err' -> logError env $ T.pack $ "[watch] Incremental pipeline failed: " ++ T.unpack err'
                       Right _ -> logInfo env "[watch] Incremental update complete"
                     ) defaultGraphosWatchConfig shutdownVar
             else do
               -- Normal mode: run once and exit
               let env = otelLogEnv obsEnv
               logInfo env "Starting pipeline..."
               logDebug env $ "Config: " <> T.pack (show config')
               result <- runPipeline config'
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

    QueryCmd question mode budget graphPath -> do
      env <- defaultLogEnv LevelInfo
      logInfo env $ "Query: " <> question <> " (" <> mode <> ", budget=" <> T.pack (show budget) <> ")"
      loadResult <- loadGraphFromFile graphPath
      case loadResult of
        Left err -> putStrLn $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              idx = lrIndex loaded
              result = queryGraphWithIndex g idx question mode budget
          if null (qrNodes result)
            then putStrLn "No matching nodes found. Try different terms."
            else do
              putStrLn $ "# Query: " ++ T.unpack question
              putStrLn ""
              putStrLn $ "Found " ++ show (length (qrNodes result)) ++ " relevant nodes (" ++ T.unpack (qrTraverse result) ++ " traversal):"
              putStrLn ""
              mapM_ (\(nid, label) ->
                putStrLn $ "  - " ++ T.unpack label ++ " [" ++ T.unpack nid ++ "]"
                ) (take 30 (qrNodes result))
              let edges = qrEdges result
              if not (null edges)
                then do
                  putStrLn ""
                  putStrLn "Connections:"
                  mapM_ (\(from, to, rel, conf) ->
                    putStrLn $ "  " ++ T.unpack from ++ " --" ++ T.unpack rel ++ "--> " ++ T.unpack to ++ " [" ++ show conf ++ "]"
                    ) edges
                else pure ()

    PathCmd from to graphPath -> do
      env <- defaultLogEnv LevelInfo
      logInfo env $ "Path: " <> from <> " -> " <> to
      logDebug env "Loading graph from disk..."
      loadResult <- loadGraphFromFile graphPath
      case loadResult of
        Left err -> putStrLn $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              idx = lrIndex loaded
          case pathQueryWithIndex g idx from to of
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
                        putStrLn $ "  " ++ T.unpack (nodeLabel n) ++ " --" ++ relLabel ++ "-->" ++ confLabel
                      Nothing -> pure ()
                    go ns
              go path

    ExplainCmd node graphPath -> do
      putStrLn $ "[graphos] Explain: " ++ T.unpack node
      loadResult <- loadGraphFromFile graphPath
      case loadResult of
        Left err -> putStrLn $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              idx = lrIndex loaded
          case explainNodeWithIndex g idx node of
            Nothing -> putStrLn $ "Node not found: " ++ T.unpack node
            Just n -> do
              putStrLn $ "NODE: " ++ T.unpack (nodeLabel n)
              putStrLn $ "  ID: " ++ T.unpack (nodeId n)
              putStrLn $ "  Source: " ++ T.unpack (nodeSourceFile n)
              case nodeSourceLocation n of
                Just loc -> putStrLn $ "  Location: " ++ T.unpack loc
                Nothing  -> pure ()
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
                    putStrLn $ "  --" ++ relLabel ++ "--> " ++ T.unpack (nodeLabel nb) ++ confLabel
                  Nothing -> pure ()

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
          env <- defaultLogEnv LevelInfo
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
          env <- defaultLogEnv LevelInfo
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
      let logLevel = if verbose then LevelDebug else LevelInfo
      env <- defaultLogEnv logLevel
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
                  mergeResult = mergeGraphsAndAnalyze (lrGraph graphA) (lrGraph graphB) density res
                  mergedGraph = mrGraph mergeResult
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
                        }
              logInfo env "[merge] Exporting..."
              exports <- Export.exportAll mergedGraph analysis config detection
              logInfo env "[merge] Merge complete!"
              logInfo env $ T.pack $ "  Nodes: " ++ show (Map.size (gNodes mergedGraph))
              logInfo env $ T.pack $ "  Edges: " ++ show (Map.size (gEdges mergedGraph))
              logInfo env $ T.pack $ "  Communities: " ++ show (Map.size commMap)
              logInfo env $ T.pack $ "  Report: " ++ Export.erReport exports
              logInfo env $ T.pack $ "  Graph: " ++ Export.erJSON exports
              case Export.erHTML exports of
                Just html -> logInfo env $ T.pack $ "  HTML: " ++ html
                Nothing   -> pure ()

    LServers -> do
      putStrLn "[graphos] Discovering available LSP servers..."
      servers <- discoverLanguageServers
      if null servers
        then putStrLn "  No LSP servers found. Install language servers for the languages you use."
        else do
          putStrLn $ "  Found " ++ show (length servers) ++ " LSP server(s):"
          mapM_ (\s -> putStrLn $ "    " ++ T.unpack (lsiName s) ++ " (" ++ lsiCommand s ++ ") - " ++ show (lsiExtensions s)) servers

    Serve dir port -> do
      putStrLn $ "[graphos] Serving " ++ dir ++ " on port " ++ show port
      startStaticServer dir port

    Init -> do
      initConfigFile

  where
    opts = info (commandOpts <**> helper)
      ( fullDesc
     <> progDesc "Graphos - Universal knowledge graph builder using LSP"
     <> header "graphos - any input → knowledge graph → clustered communities → HTML + JSON + report"
      )

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

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
  [ "# Graphos configuration file"
  , "# Generated by: graphos init"
  , "#"
  , "# Config resolution (later wins):"
  , "#   1. Built-in defaults"
  , "#   2. Global config: ~/.config/graphos/graphos.yaml"
  , "#   3. This file (project graphos.yaml)"
  , "#   4. CLI flags (--otel, --metrics, etc.)"
  , "#"
  , "# Extractors: how to extract symbols from each file type."
  , "#   lsp          — use Language Server Protocol (requires server installed)"
  , "#   tree-sitter   — use tree-sitter CLI for fast AST parsing (no server needed)"
  , "#   stub          — create a single node per file (no parsing)"
  , "#"
  , "# Override any extension below. Missing extensions use defaults."
  , ""
  , "extractors:"
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
  , "  \".hs\":"
  , "    mode: lsp"
  , "    language_id: haskell"
  , "  \".go\":"
  , "    mode: lsp"
  , "    language_id: go"
  , "  \".py\":"
  , "    mode: tree-sitter"
  , "    grammar: python"
  , "    language_id: python"
  , "  \".rs\":"
  , "    mode: lsp"
  , "    language_id: rust"
  , "  \".nix\":"
  , "    mode: lsp"
  , "    language_id: nix"
  , "  \".md\":"
  , "    mode: tree-sitter"
  , "    grammar: markdown"
  , "    language_id: markdown"
  , "  \".json\":"
  , "    mode: tree-sitter"
  , "    grammar: json"
  , "    language_id: json"
  , ""
  , "# LSP server overrides (merged with defaults)"
  , "# Uncomment to customize:"
  , "# lsp:"
  , "#   \".ts\":"
  , "#     command: typescript-language-server"
  , "#     args: [\"--stdio\"]"
  , "#     language_id: typescript"
  , ""
  , "# Language ID overrides (merged with defaults)"
  , "# language_ids:"
  , "#   \".ts\": typescript"
  , ""
  , "# File extension categories (full override if specified)"
  , "# file_extensions:"
  , "#   code: [\".ts\", \".tsx\", \".js\", \".jsx\", \".py\", \".go\", \".rs\", \".hs\", \".nix\"]"
  , "#   doc: [\".md\", \".txt\", \".rst\"]"
  , "#   paper: [\".pdf\"]"
  , "#   image: [\".png\", \".jpg\", \".jpeg\", \".webp\", \".gif\"]"
  , "#   video: [\".mp4\", \".mov\", \".mkv\", \".webm\"]"
  , ""
  , "# Neo4j connection settings for --neo4j push"
  , "# Used by: graphos . --neo4j --neo4j-push"
  , "# push_mode: full (all nodes), subgraph (communities + representatives), community (communities only)"
  , "# subgraph_size: representatives per community for subgraph mode"
  , "neo4j:"
  , "  uri: \"http://localhost:7474\""
  , "  user: \"neo4j\""
  , "  password: \"graphos_dev\""
  , "  push_mode: \"subgraph\""
  , "  subgraph_size: 7"
  , ""
  , "# Memgraph connection settings for --memgraph push"
  , "# Memgraph uses Bolt protocol (bolt://) instead of HTTP"
  , "# No auth by default (leave user/password empty)"
  , "# push_mode: full (all nodes), subgraph (communities + representatives), community (communities only)"
  , "memgraph:"
  , "  uri: \"bolt://localhost:7688\""
  , "  user: \"\""
  , "  password: \"\""
  , "  push_mode: \"subgraph\""
  , "  subgraph_size: 7"
  , ""
  , "# LLM-based community labeling (use --label to enable)"
  , "# Supports any OpenAI-compatible API (OpenAI, Ollama, LiteLLM, etc.)"
  , "# Set api_key to an env var reference ${VAR} or a literal string."
  , "# For Ollama: set provider=ollama, base_url=http://localhost:11434/v1"
  , "labeling:"
  , "  provider: openai"
  , "  model: gpt-4o-mini"
  , "  api_key: \"${OPENAI_API_KEY}\""
  , "  base_url: \"https://api.openai.com/v1\""
  , "  batch_size: 10"
  , ""
  , "# Observability: tracing, metrics, and debug instrumentation"
  , "# CLI flags (--otel, --metrics, --debug-trace) override these values."
  , "observability:"
  , "  enabled: false"
  , "  endpoint: \"http://localhost:4318\""
  , "  metricsPort: 0"
  , "  serviceName: graphos"
  , "  serviceVersion: \"0.1.0\""
  , "  exportInterval: 15"
  , "  debugTraceDir: \"\""
  ]