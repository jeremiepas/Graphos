{-# LANGUAGE StrictData #-}
module Graphos.CLI.Parser
  ( -- * Command type
    Command(..)
    -- * Parsers
  , pipelineOpts
  , queryOpts
  , researchOpts
  , symbolsOpts
  , neighborsOpts
  , pathOpts
  , serveOpts
  , pushOpts
  , pushMemgraphOpts
  , mergeOpts
  , ingestOpts
  , commandOpts
  , granularityReader
    -- * Install-skill target
  , InstallSkillTarget(..)
    -- * Command reference rendering
  , renderCommandReference
  ) where

import Options.Applicative
import Data.Text (Text)
import GHC.Conc (numCapabilities)
import Graphos.Domain.Types (PipelineConfig(..), EdgeDensity(..))
import Graphos.Domain.Types.Pipeline (Neo4jPushMode(..), MemgraphPushMode(..))
import Graphos.UseCase.Query.Refine (EdgeMode(..))
import Graphos.UseCase.Query.Render (CommonQueryOpts(..))
import Graphos.UseCase.Scaffold (InstallSkillTarget(..))
import Graphos.Domain.Config (Granularity(..), defaultGraphosConfig, defaultIngestConfig)
import Graphos.Infrastructure.Observability.SDK (OtelConfig(..), defaultOtelConfig)

data Command
  = Run PipelineConfig
  | QueryCmd Text Text Int FilePath
  | ResearchCmd [Text] [Text] FilePath Bool Bool (Maybe FilePath) (Maybe FilePath) (Maybe Text) CommonQueryOpts
  | PathCmd Text Text FilePath
  | ExplainCmd Text FilePath
  | SymbolsCmd Text CommonQueryOpts
  | NeighborsCmd Text Int CommonQueryOpts
  | PushCmd FilePath String String String Neo4jPushMode Int
  | PushMemgraphCmd FilePath String String String MemgraphPushMode Int
  | MergeCmd FilePath FilePath FilePath EdgeDensity Double Int Int Bool Bool
   | IngestCmd FilePath (Maybe Bool) FilePath Bool
  | LServers
   | Serve FilePath FilePath Int Bool Bool

  | Init (Maybe String)
  | InstallSkill InstallSkillTarget
  deriving (Show, Eq)

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
    <*> option auto (long "threads" <> short 'j' <> value (fromIntegral numCapabilities) <> help "Number of parallel extraction threads (default: numCapabilities)")
   <*> switch (long "community-graph" <> help "Export community-level graph JSON for LLM navigation")
    <*> pure defaultGraphosConfig
    <*> pure Nothing
    <*> switch (long "memgraph" <> help "Generate Cypher for Memgraph")
    <*> optional (strOption (long "memgraph-push" <> help "Push to Memgraph at Bolt URI"))
    <*> option auto (long "memgraph-push-mode" <> value MemgraphSubgraph <> help "Memgraph push mode: MemgraphFull|MemgraphSubgraph|MemgraphCommunity (default: MemgraphSubgraph)")
    <*> option auto (long "memgraph-subgraph-size" <> value 7 <> help "Representatives per community for Memgraph subgraph mode (default: 7)")
    <*> optional (option auto (long "metrics" <> help "Start Prometheus metrics server on given port (e.g. 9190)"))
    <*> switch (long "otel" <> help "Enable OpenTelemetry trace/metric export via OTLP")
    <*> fmap (\ep -> case ep of Nothing -> defaultOtelConfig; Just e -> defaultOtelConfig { otelEndpoint = e, otelLogsEndpoint = e ++ "/v1/logs" })
             (optional (strOption (long "otel-endpoint" <> help "OTLP endpoint base (default: http://localhost:4318)")))
    <*> optional (strOption (long "debug-trace" <> help "Directory for debug trace JSONL files"))
    <*> switch (long "embed" <> help "Generate embeddings for ingested files via local Ollama")
    <*> option auto (long "otel-shutdown-timeout" <> value 10 <> help "OTel shutdown timeout in seconds (default: 10)")
    <*> switch (long "vision" <> help "Enable image analysis via vision LLM")
     <*> switch (long "no-observability" <> help "Disable all observability (no tracing, metrics, or log shipping)")
      <*> optional (option granularityReader (long "granularity" <> metavar "LEVEL" <> help "Extraction granularity: fine|function|file (default: function; overrides config)"))
      <*> pure defaultIngestConfig
      <*> optional (option auto (long "timeout" <> help "Pipeline timeout in seconds (e.g. 300)"))

granularityReader :: ReadM Granularity
granularityReader = eitherReader $ \s -> case s of
  "fine"     -> Right GranularityFine
  "function" -> Right GranularityFunction
  "file"     -> Right GranularityFile
  other      -> Left $ "Unknown granularity: " ++ other ++ ". Expected fine, function, or file"

queryOpts :: Parser Command
queryOpts = QueryCmd
  <$> argument str (metavar "QUESTION")
  <*> flag "bfs" "dfs" (long "dfs" <> help "Use DFS traversal instead of BFS")
  <*> option auto (long "budget" <> value 2000 <> help "Token budget for query")
  <*> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")

researchOpts :: Parser Command
researchOpts = do
  let graphPath = "graphos-out/graph.json"
  ResearchCmd
    <$> some (argument str (metavar "TERM"))
    <*> many (strOption (long "subgraph" <> metavar "TERM" <> help "Seed terms for 1-hop BFS expansion (added to union before inducing edges)"))
    <*> strOption (long "graph" <> value graphPath <> help "Path to graph.json file")
    <*> switch (long "html" <> help "Render interactive HTML research view")
    <*> switch (long "json" <> help "Output ResearchView as JSON")
    <*> optional (strOption (long "terms-file" <> metavar "PATH" <> help "File with newline-delimited query terms (appended to positional terms)"))
    <*> optional (strOption (long "label" <> metavar "TEXT" <> help "Label for output file (default: timestamp)"))
    <*> optional (strOption (long "mode" <> value "default" <> metavar "MODE" <> help "Research mode (default, deep, etc.)"))
    <*> pure (CommonQueryOpts
          { cqoGraphPath  = graphPath
          , cqoBudget     = 2000
          , cqoJson       = False
          , cqoLabelWidth = 120
          , cqoEdges      = Semantic
          })

symbolsOpts :: Parser Command
symbolsOpts = SymbolsCmd
  <$> argument str (metavar "NAME")
  <*> (CommonQueryOpts
       <$> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")
       <*> option auto (long "budget" <> value 2000 <> help "Token budget for output")
       <*> switch (long "json" <> help "Output as JSON")
       <*> option auto (long "label-width" <> value 120 <> help "Max label width before elision")
       <*> flag Semantic All (long "edges" <> help "Edge mode: semantic (default) or all")
      )

neighborsOpts :: Parser Command
neighborsOpts = NeighborsCmd
  <$> argument str (metavar "NODE_ID")
  <*> option auto (long "depth" <> value 2 <> help "BFS depth (default: 2)")
  <*> (CommonQueryOpts
       <$> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")
       <*> option auto (long "budget" <> value 2000 <> help "Token budget for output")
       <*> switch (long "json" <> help "Output as JSON")
       <*> option auto (long "label-width" <> value 120 <> help "Max label width before elision")
       <*> flag Semantic All (long "edges" <> help "Edge mode: semantic (default) or all")
      )

pathOpts :: Parser Command
pathOpts = PathCmd
  <$> argument str (metavar "FROM")
  <*> argument str (metavar "TO")
  <*> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")

serveOpts :: Parser Command
serveOpts = Serve
  <$> strOption (long "dir" <> value "graphos-out" <> help "Directory to serve (default: graphos-out)")
  <*> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json (default: graphos-out/graph.json)")
  <*> option auto (long "port" <> short 'p' <> value 8080 <> help "Port to serve on (default: 8080)")
  <*> switch (long "api-only" <> help "Serve only the query API, not static files")
  <*> switch (long "no-api" <> help "Serve only static files, not the query API")

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
  <*> option auto (long "subgraph-size" <> value 7 <> help "Representatives per community for Memgraph subgraph mode")

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

ingestOpts :: Parser Command
ingestOpts = IngestCmd
  <$> argument str (metavar "FILE" <> help "Single file to ingest")
  <*> optional (flag' True (long "embed" <> help "Generate embeddings via local Ollama (nomic-embed-text)")
          <|> flag' False (long "no-embed" <> help "Disable embeddings for this ingest"))
  <*> strOption (long "output" <> short 'o' <> value "graphos-out" <> help "Output directory")
  <*> switch (long "label" <> help "Use LLM to label communities (requires graphos.yaml labeling config)")

commandOpts :: Parser Command
commandOpts = subparser
  ( command "query" (info queryOpts (progDesc "Query the knowledge graph"))
  <> command "path"  (info pathOpts (progDesc "Find shortest path between two nodes"))
  <> command "explain" (info (ExplainCmd <$> argument str (metavar "NODE") <*> strOption (long "graph" <> value "graphos-out/graph.json" <> help "Path to graph.json file")) (progDesc "Explain a node"))
  <> command "symbols" (info symbolsOpts (progDesc "Look up an exact symbol by name"))
  <> command "neighbors" (info neighborsOpts (progDesc "Expand neighborhood around a node"))
  <> command "push"  (info pushOpts (progDesc "Push graph.json to Neo4j (no extraction needed)"))
  <> command "push-memgraph" (info pushMemgraphOpts (progDesc "Push graph.json to Memgraph (no extraction needed)"))
  <> command "merge" (info mergeOpts (progDesc "Merge two graph.json files into one"))
  <> command "ingest" (info ingestOpts (progDesc "Ingest a single file into the knowledge graph (optionally with embeddings)"))
  <> command "lservers" (info (pure LServers) (progDesc "List available LSP servers"))
  <> command "serve" (info serveOpts (progDesc "Serve HTML graph output via HTTP"))
    <> command "init" (info initOpts (progDesc "Generate a graphos.yaml config file"))
    <> command "install-skill" (info installSkillOpts (progDesc "Install user-level AI assistant skills (e.g., opencode)"))
    <> command "research" (info (researchOpts <**> helper) (progDesc "Multi-query knowledge research with subgraph extraction"))
    )
  <|> Run <$> pipelineOpts

initOpts :: Parser Command
initOpts = Init <$> optional (strOption (long "agents" <> metavar "TARGETS" <> help "Scaffold agent integration files. Comma-separated targets: opencode,claude,generic. Use 'auto' to auto-detect from .opencode/ and .claude/ dirs."))

installSkillOpts :: Parser Command
installSkillOpts = InstallSkill
  <$> option (eitherReader parseInstallSkillTarget)
       ( long "target"
      <> metavar "TARGET"
      <> help "Assistant target to install skills for. Supported: opencode."
       )

parseInstallSkillTarget :: String -> Either String InstallSkillTarget
parseInstallSkillTarget s = case map lowerChar s of
  "opencode" -> Right OpencodeTarget
  other      -> Left $ "Unknown install-skill target: " ++ other
                         ++ ". Supported targets: " ++ showInstallSkillTargets
  where
    lowerChar c
      | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

showInstallSkillTargets :: String
showInstallSkillTargets = "opencode"

-- | Render a compact command/flag reference from the parser.
-- Produces a fenced code block suitable for embedding in generated docs.
renderCommandReference :: String
renderCommandReference = unlines $
  [ "```"
  , "graphos [PATH]                  Build the knowledge graph (default: .)"
  , "  --output, -o DIR              Output directory"
  , "  --directed / --deep / --no-viz / --update"
  , "  --cluster-only / --no-cluster / --label"
  , "  --community-graph / --embed / --vision / --watch"
  , "  --neo4j / --memgraph / --svg / --graphml / --wiki"
  , "  --verbose, -v / --debug"
  , "  --granularity LEVEL           fine|function|file"
  , "  --threads, -j N / --edge-density MODE"
  , "  --resolution FLOAT / --mcp GRAPH_JSON"
  , ""
  , "graphos query QUESTION          Query the knowledge graph"
  , "  --dfs / --budget N / --graph FILE"
  , ""
  , "graphos path FROM TO             Find shortest path"
  , "  --graph FILE"
  , ""
  , "graphos explain NODE            Explain a node"
  , "  --graph FILE"
  , ""
  , "graphos symbols NAME            Look up symbol by name"
  , "  --graph FILE / --budget N / --json"
  , "  --label-width N / --edges"
  , ""
  , "graphos neighbors NODE_ID       Expand neighborhood"
  , "  --depth N / --graph FILE / --budget N"
  , "  --json / --label-width N / --edges"
  , ""
  , "graphos ingest FILE             Ingest single file"
  , "  --embed / --no-embed / --output, -o DIR"
  , ""
  , "graphos init                    Generate graphos.yaml"
  , ""
  , "graphos push [opts]             Push to Neo4j"
  , "graphos push-memgraph [opts]    Push to Memgraph"
  , "graphos merge A B               Merge graph files"
  , "graphos serve [opts]            Serve HTML + query API"
  , "  --dir DIR / --graph FILE / --port N"
  , "  --api-only / --no-api"
  , "graphos lservers                List LSP servers"
  , "```"
  ]