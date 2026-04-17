-- | Graphos CLI - main entry point
module Main where

import Options.Applicative
import System.Exit (exitWith, ExitCode(..))
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_)

import Graphos.Domain.Types (PipelineConfig(..), EdgeDensity(..), Node(..), Edge(..), Confidence(..), relationToText, edgeRelation, edgeConfidence, CommunityMap)
import Graphos.UseCase.Pipeline (runPipeline, PipelineResult(..))
import Graphos.UseCase.Load (loadGraphFromFile, LoadResult(..))
import Graphos.UseCase.Query (queryGraph, pathQuery, explainNode, QueryResult(..))
import Graphos.Domain.Graph (Graph, gNodes, gEdges, neighbors, degree, shortestPath)
import Graphos.Infrastructure.LSP.Capabilities (LanguageServerInfo(..), discoverLanguageServers)
import Graphos.Infrastructure.Logging (LogLevel(..), LogEnv(..), defaultLogEnv, logInfo, logDebug, logTrace, logError, logWarn, withTiming)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import Data.Foldable (asum)

-- ───────────────────────────────────────────────
-- CLI argument parsing
-- ───────────────────────────────────────────────

data Command
  = Run PipelineConfig
  | QueryCmd Text Text Int FilePath
  | PathCmd Text Text FilePath
  | ExplainCmd Text FilePath
  | LServers             -- list available LSP servers

pipelineOpts :: Parser PipelineConfig
pipelineOpts = PipelineConfig
  <$> argument str (metavar "PATH" <> value "." <> help "Input directory (default: .)")
  <*> strOption (long "output" <> short 'o' <> value "graphos-out" <> help "Output directory")
  <*> switch (long "directed" <> help "Build directed graph")
  <*> switch (long "deep" <> help "Deep extraction mode")
  <*> switch (long "no-viz" <> help "Skip HTML visualization")
  <*> switch (long "update" <> help "Incremental update")
  <*> switch (long "cluster-only" <> help "Rerun clustering only")
  <*> switch (long "obsidian" <> help "Generate Obsidian vault")
  <*> optional (strOption (long "obsidian-dir" <> help "Obsidian vault output directory"))
  <*> switch (long "neo4j" <> help "Generate Cypher for Neo4j")
  <*> optional (strOption (long "neo4j-push" <> help "Push to Neo4j at URI"))
  <*> switch (long "mcp" <> help "Start MCP server")
  <*> switch (long "svg" <> help "Export SVG")
  <*> switch (long "graphml" <> help "Export GraphML")
  <*> switch (long "watch" <> help "Watch for file changes")
  <*> switch (long "wiki" <> help "Build agent-crawlable wiki")
  <*> switch (long "verbose" <> short 'v' <> help "Verbose output: show DEBUG level logs")
  <*> switch (long "debug" <> help "Debug output: show TRACE level logs + internal details")
  <*> option auto (long "edge-density" <> value Normal <> help "Edge density: sparse|normal|dense|maximum (default: normal)")
  <*> option auto (long "resolution" <> value 1.0 <> help "Community resolution: higher = fewer larger communities (default: 1.0)")
  <*> option auto (long "min-comm-size" <> value 3 <> help "Minimum community size; smaller get merged (default: 3)")
  <*> option auto (long "threads" <> short 'j' <> value 1 <> help "Number of parallel extraction threads (default: 1)")
  <*> switch (long "community-graph" <> help "Export community-level graph JSON for LLM navigation")

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

commandOpts :: Parser Command
commandOpts = subparser
  ( command "query" (info queryOpts (progDesc "Query the knowledge graph"))
 <> command "path"  (info pathOpts (progDesc "Find shortest path between two nodes"))
  <> command "explain" (info (ExplainCmd <$> argument str (metavar "NODE") <*> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")) (progDesc "Explain a node"))
 <> command "lservers" (info (pure LServers) (progDesc "List available LSP servers"))
   )
 <|> Run <$> pipelineOpts

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Run config -> do
      let logLevel = if cfgDebug config then LevelTrace
                     else if cfgVerbose config then LevelDebug
                     else LevelInfo
      env <- defaultLogEnv logLevel
      logInfo env "Starting pipeline..."
      logDebug env $ "Config: " <> T.pack (show config)
      result <- runPipeline config
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
            Just html -> logInfo env $ T.pack $ "  HTML: " ++ html
            Nothing  -> pure ()

    QueryCmd question mode budget graphPath -> do
      env <- defaultLogEnv LevelInfo
      logInfo env $ "Query: " <> question <> " (" <> mode <> ", budget=" <> T.pack (show budget) <> ")"
      loadResult <- loadGraphFromFile graphPath
      case loadResult of
        Left err -> putStrLn $ "Error: " ++ T.unpack err
        Right loaded -> do
          let g = lrGraph loaded
              result = queryGraph g question mode budget
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
          case pathQuery g from to of
            Nothing -> putStrLn $ "No path found between '" ++ T.unpack from ++ "' and '" ++ T.unpack to ++ "'"
            Just path -> do
              let hops = length path - 1
              putStrLn $ "Shortest path (" ++ show hops ++ " hops):"
              let go []     = pure ()
                  go [nid]  = case Map.lookup nid (gNodes g) of
                    Just n  -> putStrLn $ "  " ++ T.unpack (nodeLabel n)
                    Nothing -> putStrLn $ "  " ++ T.unpack nid
                  go (nid:ns) = do
                    let mNext = head ns
                        mEdge = Map.lookup (nid, mNext) (gEdges g)
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
          case explainNode g node of
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
              -- Show community
              let commMap = lrCommunities loaded
                  commId = findCommunityForNode (nodeId n) commMap
              case commId of
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

    LServers -> do
      putStrLn "[graphos] Discovering available LSP servers..."
      servers <- discoverLanguageServers
      if null servers
        then putStrLn "  No LSP servers found. Install language servers for the languages you use."
        else do
          putStrLn $ "  Found " ++ show (length servers) ++ " LSP server(s):"
          mapM_ (\s -> putStrLn $ "    " ++ T.unpack (lsiName s) ++ " (" ++ lsiCommand s ++ ") - " ++ show (lsiExtensions s)) servers

  where
    opts = info (commandOpts <**> helper)
      ( fullDesc
     <> progDesc "Graphos - Universal knowledge graph builder using LSP"
     <> header "graphos - any input → knowledge graph → clustered communities → HTML + JSON + report"
      )

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Find which community a node belongs to
findCommunityForNode :: Text -> CommunityMap -> Maybe Int
findCommunityForNode nid commMap =
  listToMaybe [cid | (cid, members) <- Map.toList commMap, nid `elem` members]