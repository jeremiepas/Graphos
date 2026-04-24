-- | Graphos CLI - main entry point
module Main where

import Options.Applicative
import System.Exit (exitWith, ExitCode(..))
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_)

import Graphos.Domain.Types (PipelineConfig(..), EdgeDensity(..), Node(..), Edge(..), relationToText, edgeRelation, edgeConfidence, CommunityMap)
import Graphos.UseCase.Pipeline (runPipeline, PipelineResult(..))
import Graphos.UseCase.Load (loadGraphFromFile, LoadResult(..))
import Graphos.UseCase.Query (queryGraph, pathQuery, explainNode, QueryResult(..))
import Graphos.Domain.Graph (gNodes, gEdges, neighbors, degree)
import Graphos.Infrastructure.LSP.Capabilities (LanguageServerInfo(..), discoverLanguageServers)
import Graphos.Infrastructure.Logging (LogLevel(..), defaultLogEnv, logInfo, logDebug, logError)
import Graphos.Domain.Config (defaultGraphosConfig)
import Graphos.Infrastructure.Config (loadConfig)
import Graphos.Infrastructure.Server.Static (startStaticServer)
import Graphos.Infrastructure.Server.MCP (startMCPServerFromFile)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import System.Directory (doesFileExist)


-- ───────────────────────────────────────────────
-- CLI argument parsing
-- ───────────────────────────────────────────────

data Command
  = Run PipelineConfig
  | QueryCmd Text Text Int FilePath
  | PathCmd Text Text FilePath
  | ExplainCmd Text FilePath
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
  <*> optional (strOption (long "mcp" <> metavar "GRAPH_JSON" <> help "Start MCP server with graph file"))
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
  <*> pure defaultGraphosConfig  -- placeholder; loaded from graphos.yaml at runtime

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

commandOpts :: Parser Command
commandOpts = subparser
  ( command "query" (info queryOpts (progDesc "Query the knowledge graph"))
 <> command "path"  (info pathOpts (progDesc "Find shortest path between two nodes"))
 <> command "explain" (info (ExplainCmd <$> argument str (metavar "NODE") <*> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")) (progDesc "Explain a node"))
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
      let config' = config { cfgGraphosConfig = graphosCfg }
      -- MCP mode: start MCP server and exit
      case cfgMCP config' of
        Just graphPath -> do
          putStrLn $ "[graphos] Starting MCP server with " ++ graphPath
          startMCPServerFromFile graphPath
        Nothing -> do
          let logLevel = if cfgDebug config then LevelTrace
                         else if cfgVerbose config then LevelDebug
                         else LevelInfo
          env <- defaultLogEnv logLevel
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

-- | Find which community a node belongs to
findCommunityForNode :: Text -> CommunityMap -> Maybe Int
findCommunityForNode nid commMap =
  listToMaybe [cid | (cid, members) <- Map.toList commMap, nid `elem` members]

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
  , "# Start a local server: docker compose up -d"
  , "neo4j:"
  , "  uri: \"http://localhost:7474\""
  , "  user: \"neo4j\""
  , "  password: \"graphos_dev\""
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
  ]