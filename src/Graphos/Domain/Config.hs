-- | Domain configuration types for Graphos.
-- Pure data types — no IO. Config file loading lives in Infrastructure.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config
  ( -- * Extractor mode
    ExtractorMode(..)
  , ExtractorConfig(..)
  , defaultExtractors

    -- * LSP configuration
  , LSPServerConfig(..)
  , defaultLSPServers
  , defaultLanguageIds

    -- * File extension configuration
  , FileExtensionConfig(..)
  , defaultFileExtensions

     -- * Neo4j configuration
  , Neo4jConfig(..)
  , defaultNeo4jConfig

     -- * Memgraph configuration
  , MemgraphConfig(..)
  , defaultMemgraphConfig

     -- * LLM labeling configuration
  , LabelingConfig(..)
  , defaultLabelingConfig

     -- * Observability configuration
  , ObservabilityConfig(..)
  , defaultObservabilityConfig

     -- * Embedding configuration
  , EmbeddingConfig(..)
  , defaultEmbeddingConfig

     -- * Top-level configuration
  , GraphosConfig(..)
  , defaultGraphosConfig

     -- * Config merging
  , mergeGraphosConfig
  , mergeObservabilityConfig
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), genericParseJSON, genericToJSON, withObject, (.:?), (.!=))
import Data.Char (toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)

-- ───────────────────────────────────────────────
-- Extractor Mode
-- ───────────────────────────────────────────────

-- | How to extract symbols from a file.
--   'ExtractLSP' uses a Language Server Protocol server (requires the server to be installed).
--   'ExtractTreeSitter' uses tree-sitter CLI for fast, reliable AST parsing (no server needed).
--   'ExtractStub' creates a single node per file (no parsing).
data ExtractorMode
  = ExtractLSP
  | ExtractTreeSitter
  | ExtractStub
  deriving (Eq, Show, Generic)

instance ToJSON ExtractorMode where
  toJSON ExtractLSP        = "lsp"
  toJSON ExtractTreeSitter = "tree-sitter"
  toJSON ExtractStub       = "stub"

instance FromJSON ExtractorMode where
  parseJSON (String "lsp")         = pure ExtractLSP
  parseJSON (String "tree-sitter") = pure ExtractTreeSitter
  parseJSON (String "stub")        = pure ExtractStub
  parseJSON v = fail $ "Unknown extractor mode: " ++ show v ++ ". Expected lsp, tree-sitter, or stub"

-- | Per-extension extractor configuration.
data ExtractorConfig = ExtractorConfig
  { ecMode        :: ExtractorMode
  , ecGrammar     :: Maybe String   -- ^ tree-sitter grammar name (e.g. "typescript")
  , ecLanguageId  :: Maybe Text      -- ^ LSP language ID (e.g. "typescript")
  } deriving (Eq, Show, Generic)

instance ToJSON ExtractorConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 2 }

instance FromJSON ExtractorConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 2 }

-- | Lowercase the first character of a string.
lowerFirst :: String -> String
lowerFirst []     = []
lowerFirst (c:cs) = toLower c : cs

-- | Default extractor assignments per extension.
-- Languages where LSP is unreliable (TypeScript) prefer tree-sitter.
-- Languages where LSP is mature (Haskell, Go, Rust) prefer LSP.
defaultExtractors :: Map String ExtractorConfig
defaultExtractors = Map.fromList
  [ -- TypeScript: LSP is fragile (typescript-language-server crashes), prefer tree-sitter
    (".ts",   ExtractorConfig ExtractTreeSitter (Just "typescript") (Just "typescript"))
  , (".tsx",  ExtractorConfig ExtractTreeSitter (Just "tsx")       (Just "typescriptreact"))
  , (".js",  ExtractorConfig ExtractTreeSitter (Just "javascript") (Just "javascript"))
  , (".jsx", ExtractorConfig ExtractTreeSitter (Just "javascript") (Just "javascriptreact"))
  -- Mature LSP servers — prefer LSP for richer semantic info
  , (".hs",  ExtractorConfig ExtractLSP Nothing (Just "haskell"))
  , (".lhs", ExtractorConfig ExtractLSP Nothing (Just "haskell"))
  , (".go",  ExtractorConfig ExtractLSP Nothing (Just "go"))
  , (".rs",  ExtractorConfig ExtractLSP Nothing (Just "rust"))
  , (".py",  ExtractorConfig ExtractLSP Nothing (Just "python"))
  , (".pyw", ExtractorConfig ExtractLSP Nothing (Just "python"))
  , (".c",   ExtractorConfig ExtractLSP Nothing (Just "c"))
  , (".cpp", ExtractorConfig ExtractLSP Nothing (Just "cpp"))
  , (".h",   ExtractorConfig ExtractLSP Nothing (Just "c"))
  , (".hpp", ExtractorConfig ExtractLSP Nothing (Just "cpp"))
  , (".nix", ExtractorConfig ExtractLSP Nothing (Just "nix"))
  , (".rb",  ExtractorConfig ExtractLSP Nothing (Just "ruby"))
  , (".java",ExtractorConfig ExtractLSP Nothing (Just "java"))
    -- JSON: tree-sitter parser (no reliable LSP for non-VSCode environments)
  , (".json",ExtractorConfig ExtractTreeSitter (Just "json") (Just "json"))
    -- Markdown: use tree-sitter mode with our built-in parser
    -- (no external LSP server needed for docs)
  , (".md",  ExtractorConfig ExtractTreeSitter (Just "markdown") (Just "markdown"))
  , (".rst", ExtractorConfig ExtractTreeSitter (Just "markdown") (Just "rest"))
  , (".adoc",ExtractorConfig ExtractTreeSitter (Just "markdown") (Just "asciidoc"))
  ]

-- ───────────────────────────────────────────────
-- LSP Server Configuration
-- ───────────────────────────────────────────────

-- | Configuration for a single LSP server.
-- Maps a file extension to a language server command.
data LSPServerConfig = LSPServerConfig
  { lspCommand    :: String
  , lspArgs       :: [String]
  , lspLanguageId :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON LSPServerConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 3 }

instance FromJSON LSPServerConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }

-- | Default LSP server configurations (hardcoded fallback).
-- Users can override these via graphos.yaml.
defaultLSPServers :: Map String LSPServerConfig
defaultLSPServers = Map.fromList
  [ (".ts",    LSPServerConfig "typescript-language-server" ["--stdio"] "typescript")
  , (".tsx",   LSPServerConfig "typescript-language-server" ["--stdio"] "typescriptreact")
  , (".js",    LSPServerConfig "typescript-language-server" ["--stdio"] "javascript")
  , (".jsx",   LSPServerConfig "typescript-language-server" ["--stdio"] "javascriptreact")
  , (".py",    LSPServerConfig "pyright-langserver" ["--stdio"] "python")
  , (".pyw",   LSPServerConfig "pyright-langserver" ["--stdio"] "python")
  , (".go",    LSPServerConfig "gopls" [] "go")
  , (".rs",    LSPServerConfig "rust-analyzer" [] "rust")
  , (".c",     LSPServerConfig "clangd" [] "c")
  , (".cpp",   LSPServerConfig "clangd" [] "cpp")
  , (".h",     LSPServerConfig "clangd" [] "c")
  , (".hpp",   LSPServerConfig "clangd" [] "cpp")
  , (".java",  LSPServerConfig "jdtls" [] "java")
  , (".cs",    LSPServerConfig "omnisharp" [] "csharp")
  , (".rb",    LSPServerConfig "solargraph" ["--stdio"] "ruby")
  , (".hs",    LSPServerConfig "haskell-language-server" ["--lsp"] "haskell")
  , (".lhs",   LSPServerConfig "haskell-language-server" ["--lsp"] "haskell")
  , (".php",   LSPServerConfig "phpactor" [] "php")
  , (".swift", LSPServerConfig "sourcekit-lsp" [] "swift")
  , (".kt",    LSPServerConfig "kotlin-language-server" [] "kotlin")
  , (".kts",   LSPServerConfig "kotlin-language-server" [] "kotlin")
  , (".scala", LSPServerConfig "metals" [] "scala")
  , (".lua",   LSPServerConfig "lua-language-server" [] "lua")
  , (".zig",   LSPServerConfig "zls" [] "zig")
  , (".ex",    LSPServerConfig "elixir-ls" [] "elixir")
  , (".exs",   LSPServerConfig "elixir-ls" [] "elixir")
  , (".dart",  LSPServerConfig "dart" ["analyze", "--stdio"] "dart")
  , (".vue",   LSPServerConfig "vue-language-server" [] "vue")
  , (".svelte",LSPServerConfig "svelte-language-server" [] "svelte")
  -- NEW: Nix and JSON LSP servers
  , (".nix",   LSPServerConfig "nixd" [] "nix")
  ]

-- | Default language ID mapping for extensions.
-- This replaces the hardcoded `languageIdFromExt` function.
defaultLanguageIds :: Map String Text
defaultLanguageIds = Map.fromList
  [ (".py",    "python")
  , (".pyw",   "python")
  , (".hs",    "haskell")
  , (".lhs",   "haskell")
  , (".js",    "javascript")
  , (".jsx",   "javascriptreact")
  , (".ts",    "typescript")
  , (".tsx",   "typescriptreact")
  , (".go",    "go")
  , (".rs",    "rust")
  , (".c",     "c")
  , (".cpp",   "cpp")
  , (".h",     "c")
  , (".hpp",   "cpp")
  , (".java",  "java")
  , (".cs",    "csharp")
  , (".rb",    "ruby")
  , (".php",   "php")
  , (".swift", "swift")
  , (".kt",    "kotlin")
  , (".kts",   "kotlin")
  , (".scala", "scala")
  , (".lua",   "lua")
  , (".zig",   "zig")
  , (".ex",    "elixir")
  , (".exs",   "elixir")
  , (".dart",  "dart")
  , (".vue",   "vue")
  , (".svelte","svelte")
  -- NEW: Nix, JSON, text, RAML
  , (".nix",   "nix")
  ]

-- ───────────────────────────────────────────────
-- File Extension Configuration
-- ───────────────────────────────────────────────

-- | File extension categories for detection.
-- Mirrors FileCategory from Domain.Types but as simple strings for config.
data FileExtensionConfig = FileExtensionConfig
  { fecCode    :: [String]
  , fecDoc     :: [String]
  , fecPaper   :: [String]
  , fecImage   :: [String]
  , fecVideo   :: [String]
  } deriving (Eq, Show, Generic)

instance ToJSON FileExtensionConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 3 }

instance FromJSON FileExtensionConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }

-- | Default file extension categories (hardcoded fallback).
defaultFileExtensions :: FileExtensionConfig
defaultFileExtensions = FileExtensionConfig
  { fecCode  = [ ".py", ".ts", ".tsx", ".js", ".jsx", ".go", ".rs", ".java", ".c", ".cpp", ".h", ".hpp"
               , ".rb", ".cs", ".kt", ".kts", ".scala", ".php", ".swift", ".lua", ".zig", ".hs", ".lhs"
               , ".ex", ".exs", ".m", ".mm", ".jl", ".vue", ".svelte", ".dart", ".ps1"
                , ".nix"  -- Nix DSL
                ]
  , fecDoc   = [ ".md", ".txt", ".rst", ".adoc", ".org"
               , ".text", ".raml"  -- NEW
               ]
  , fecPaper = [ ".pdf" ]
  , fecImage = [ ".png", ".jpg", ".jpeg", ".webp", ".gif" ]
  , fecVideo = [ ".mp4", ".mov", ".mkv", ".webm", ".avi", ".m4v", ".mp3", ".wav", ".m4a", ".ogg" ]
  }

-- ───────────────────────────────────────────────
-- Neo4j Configuration
-- ───────────────────────────────────────────────

-- | Neo4j connection configuration for Cypher export and push.
-- Used by --neo4j and --neo4j-push flags, overridable via graphos.yaml.
--
-- All fields except URI are optional in the YAML file (they have defaults).
-- The FromJSON instance uses .:? so partial neo4j sections are valid.
data Neo4jConfig = Neo4jConfig
  { neo4jUri          :: String  -- ^ Neo4j HTTP URI (e.g. "http://localhost:7474")
  , neo4jUser         :: String  -- ^ Username for authentication (e.g. "neo4j")
  , neo4jPassword     :: String  -- ^ Password for authentication
  , neo4jPushMode     :: String  -- ^ Push mode: "full", "subgraph", or "community" (default: "subgraph")
  , neo4jSubgraphSize :: Int     -- ^ Representatives per community for subgraph mode (default: 7)
  } deriving (Eq, Show, Generic)

instance ToJSON Neo4jConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 5 }

-- | Custom FromJSON: allows partial neo4j sections in graphos.yaml.
-- All fields are optional — missing values fall back to defaults.
instance FromJSON Neo4jConfig where
  parseJSON = withObject "Neo4jConfig" $ \v -> Neo4jConfig
    <$> v .:? "uri"           .!= "http://localhost:7474"
    <*> v .:? "user"          .!= "neo4j"
    <*> v .:? "password"      .!= "graphos_dev"
    <*> v .:? "pushMode"      .!= "subgraph"
    <*> v .:? "subgraphSize"  .!= 7

-- | Default Neo4j configuration for local development.
defaultNeo4jConfig :: Neo4jConfig
defaultNeo4jConfig = Neo4jConfig
  { neo4jUri          = "http://localhost:7474"
  , neo4jUser         = "neo4j"
  , neo4jPassword     = "graphos_dev"
  , neo4jPushMode     = "subgraph"
  , neo4jSubgraphSize = 7
  }

-- ───────────────────────────────────────────────
-- Memgraph Configuration
-- ───────────────────────────────────────────────

-- | Memgraph connection configuration for Bolt-protocol export and push.
-- Memgraph uses Bolt protocol (not HTTP) — the URI format is bolt://host:port.
-- All fields are optional in graphos.yaml — missing values fall back to defaults.
data MemgraphConfig = MemgraphConfig
  { mgUri          :: String  -- ^ Memgraph Bolt URI (e.g. "bolt://localhost:7688")
  , mgUser         :: String  -- ^ Username (often "" for Memgraph — no auth by default)
  , mgPassword     :: String  -- ^ Password (often "" for Memgraph — no auth by default)
  , mgPushMode     :: String  -- ^ Push mode: "full", "subgraph", or "community" (default: "subgraph")
  , mgSubgraphSize :: Int     -- ^ Representatives per community for subgraph mode (default: 7)
  } deriving (Eq, Show, Generic)

instance ToJSON MemgraphConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 2 }

instance FromJSON MemgraphConfig where
  parseJSON = withObject "MemgraphConfig" $ \v -> MemgraphConfig
    <$> v .:? "uri"           .!= "bolt://localhost:7688"
    <*> v .:? "user"          .!= ""
    <*> v .:? "password"      .!= ""
    <*> v .:? "pushMode"      .!= "subgraph"
    <*> v .:? "subgraphSize"  .!= 7

defaultMemgraphConfig :: MemgraphConfig
defaultMemgraphConfig = MemgraphConfig
  { mgUri          = "bolt://localhost:7688"
  , mgUser         = ""
  , mgPassword     = ""
  , mgPushMode     = "subgraph"
  , mgSubgraphSize = 7
  }

-- ───────────────────────────────────────────────
-- LLM Labeling Configuration
-- ───────────────────────────────────────────────

-- | Configuration for LLM-based community labeling.
-- Supports any OpenAI-compatible API (OpenAI, Ollama, LiteLLM, etc.)
--
-- All fields are optional in graphos.yaml — missing values fall back to defaults.
data LabelingConfig = LabelingConfig
  { labelingProvider  :: String  -- ^ Provider: "openai" | "ollama" | "litellm"
  , labelingModel     :: String  -- ^ Model name: "gpt-4o-mini" | "llama3" etc.
  , labelingApiKey    :: String  -- ^ API key (env var ${VAR} resolved at runtime)
  , labelingBaseUrl   :: String  -- ^ API base URL (e.g. "https://api.openai.com/v1")
  , labelingBatchSize :: Int     -- ^ Communities per LLM call (default: 10)
  } deriving (Eq, Show, Generic)

instance ToJSON LabelingConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 8 }

-- | Custom FromJSON: all fields optional with sensible defaults for graphos.yaml.
instance FromJSON LabelingConfig where
  parseJSON = withObject "LabelingConfig" $ \v -> LabelingConfig
    <$> v .:? "provider"   .!= "openai"
    <*> v .:? "model"      .!= "gpt-4o-mini"
    <*> v .:? "apiKey"     .!= "${OPENAI_API_KEY}"
    <*> v .:? "baseUrl"    .!= "https://api.openai.com/v1"
    <*> v .:? "batchSize"  .!= 10

-- | Default labeling configuration (OpenAI gpt-4o-mini).
defaultLabelingConfig :: LabelingConfig
defaultLabelingConfig = LabelingConfig
  { labelingProvider  = "openai"
  , labelingModel     = "gpt-4o-mini"
  , labelingApiKey    = "${OPENAI_API_KEY}"
  , labelingBaseUrl   = "https://api.openai.com/v1"
  , labelingBatchSize = 10
  }

-- ───────────────────────────────────────────────
-- Observability Configuration
-- ───────────────────────────────────────────────

-- | Configuration for tracing, metrics, and debug instrumentation.
-- All fields are optional in graphos.yaml — missing values fall back to defaults.
--
-- CLI flags (--otel, --metrics, --debug-trace) override these values.
data ObservabilityConfig = ObservabilityConfig
   { obsEnabled        :: Bool     -- ^ Enable OpenTelemetry trace/metric export
   , obsEndpoint       :: String   -- ^ OTLP endpoint base URL (e.g. "http://localhost:14319")
   , obsMetricsPort    :: Int      -- ^ Prometheus metrics server port (0 = disabled)
   , obsServiceName    :: String   -- ^ Service name for spans
   , obsServiceVersion :: String   -- ^ Service version for spans
   , obsExportInterval :: Int      -- ^ Metrics export interval in seconds
   , obsDebugTraceDir  :: String   -- ^ Directory for debug trace JSONL files ("" = disabled)
   , obsDebug          :: Bool     -- ^ Enable debug mode: TRACE logs + structured log shipping to Loki
   } deriving (Eq, Show, Generic)

instance ToJSON ObservabilityConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }

instance FromJSON ObservabilityConfig where
  parseJSON = withObject "ObservabilityConfig" $ \v -> ObservabilityConfig
    <$> v .:? "enabled"         .!= False
    <*> v .:? "endpoint"        .!= "http://localhost:4318"
    <*> v .:? "metricsPort"     .!= 0
    <*> v .:? "serviceName"     .!= "graphos"
    <*> v .:? "serviceVersion"  .!= "0.1.0"
    <*> v .:? "exportInterval" .!= 15
    <*> v .:? "debugTraceDir"  .!= ""
    <*> v .:? "debug"           .!= False

defaultObservabilityConfig :: ObservabilityConfig
defaultObservabilityConfig = ObservabilityConfig
  { obsEnabled        = False
  , obsEndpoint       = "http://localhost:4318"
  , obsMetricsPort    = 0
  , obsServiceName    = "graphos"
  , obsServiceVersion = "0.1.0"
  , obsExportInterval = 15
  , obsDebugTraceDir  = ""
  , obsDebug          = False
  }

-- ───────────────────────────────────────────────
-- Embedding Configuration
-- ───────────────────────────────────────────────

-- | Configuration for local embedding generation via Ollama.
-- Disabled by default — only runs when --embed flag is passed or
-- embedding.enabled is set in graphos.yaml.
--
-- Targets small local models (nomic-embed-text, all-minilm) via
-- Ollama's OpenAI-compatible /embeddings endpoint.
data EmbeddingConfig = EmbeddingConfig
  { embEnabled   :: Bool     -- ^ Enable embedding generation (default: False)
  , embProvider  :: String   -- ^ Provider: "ollama" (only local for now)
  , embModel     :: String   -- ^ Model name (e.g. "nomic-embed-text")
  , embBaseUrl   :: String   -- ^ Ollama API base URL (e.g. "http://localhost:11434/v1")
  , embDimension :: Int      -- ^ Embedding vector dimension (0 = auto-detect from model)
  } deriving (Eq, Show, Generic)

instance ToJSON EmbeddingConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }

instance FromJSON EmbeddingConfig where
  parseJSON = withObject "EmbeddingConfig" $ \v -> EmbeddingConfig
    <$> v .:? "enabled"   .!= False
    <*> v .:? "provider"  .!= "ollama"
    <*> v .:? "model"     .!= "nomic-embed-text"
    <*> v .:? "baseUrl"   .!= "http://localhost:11434/v1"
    <*> v .:? "dimension" .!= 0

-- | Default embedding configuration (disabled, local Ollama).
defaultEmbeddingConfig :: EmbeddingConfig
defaultEmbeddingConfig = EmbeddingConfig
  { embEnabled   = False
  , embProvider  = "ollama"
  , embModel     = "nomic-embed-text"
  , embBaseUrl   = "http://localhost:11434/v1"
  , embDimension = 0
  }

-- ───────────────────────────────────────────────
-- Top-level Configuration
-- ───────────────────────────────────────────────

-- | Top-level Graphos configuration.
-- Loaded from graphos.yaml, with defaults for missing fields.
data GraphosConfig = GraphosConfig
  { gcLsp            :: Map String LSPServerConfig  -- ^ extension → LSP server config
  , gcLanguageIds    :: Map String Text              -- ^ extension → language ID
  , gcFileExtensions :: FileExtensionConfig          -- ^ file extension categories
  , gcExtractors     :: Map String ExtractorConfig  -- ^ extension → extractor config
  , gcNeo4j          :: Neo4jConfig                  -- ^ Neo4j connection settings
  , gcMemgraph       :: MemgraphConfig               -- ^ Memgraph connection settings
  , gcLabeling       :: LabelingConfig               -- ^ LLM labeling settings
  , gcObservability  :: ObservabilityConfig           -- ^ Tracing, metrics, debug settings
  , gcEmbedding      :: EmbeddingConfig               -- ^ Local embedding settings (Ollama)
  } deriving (Eq, Show, Generic)

-- | Default Graphos configuration (used when no config file is found).
defaultGraphosConfig :: GraphosConfig
defaultGraphosConfig = GraphosConfig
  { gcLsp            = defaultLSPServers
  , gcLanguageIds    = defaultLanguageIds
  , gcFileExtensions = defaultFileExtensions
  , gcExtractors     = defaultExtractors
  , gcNeo4j          = defaultNeo4jConfig
  , gcMemgraph       = defaultMemgraphConfig
  , gcLabeling       = defaultLabelingConfig
  , gcObservability  = defaultObservabilityConfig
  , gcEmbedding      = defaultEmbeddingConfig
  }

-- ───────────────────────────────────────────────
-- Config merging (global + project + CLI)
-- ───────────────────────────────────────────────

-- | Merge two GraphosConfig values: project overrides global.
--
-- Merge rules:
--   * Maps (LSP, language IDs, extractors): 'Map.union', project wins on key collision
--   * Scalar sections (Neo4j, Labeling, Observability): project wins if it differs
--     from defaults; otherwise global wins
--   * File extensions: full override (project wins if set)
mergeGraphosConfig :: GraphosConfig -> GraphosConfig -> GraphosConfig
mergeGraphosConfig global project = GraphosConfig
  { gcLsp = Map.union (gcLsp project) (gcLsp global)
  , gcLanguageIds = Map.union (gcLanguageIds project) (gcLanguageIds global)
  , gcFileExtensions = if gcFileExtensions project == defaultFileExtensions
                          then gcFileExtensions global
                          else gcFileExtensions project
  , gcExtractors = Map.union (gcExtractors project) (gcExtractors global)
  , gcNeo4j = if gcNeo4j project == defaultNeo4jConfig
                 then gcNeo4j global
                 else gcNeo4j project
  , gcMemgraph = if gcMemgraph project == defaultMemgraphConfig
                   then gcMemgraph global
                   else gcMemgraph project
  , gcLabeling = if gcLabeling project == defaultLabelingConfig
                   then gcLabeling global
                   else gcLabeling project
  , gcObservability = mergeObservabilityConfig (gcObservability global)
                                                (gcObservability project)
  , gcEmbedding = if gcEmbedding project == defaultEmbeddingConfig
                     then gcEmbedding global
                     else gcEmbedding project
  }

-- | Merge two ObservabilityConfig values: project overrides global.
-- A field in project is considered "explicit" if it differs from the default.
mergeObservabilityConfig :: ObservabilityConfig -> ObservabilityConfig -> ObservabilityConfig
mergeObservabilityConfig global project = ObservabilityConfig
  { obsEnabled        = if obsEnabled project /= obsEnabled defaultObservabilityConfig
                           then obsEnabled project
                           else obsEnabled global
  , obsEndpoint        = if obsEndpoint project /= obsEndpoint defaultObservabilityConfig
                           then obsEndpoint project
                           else obsEndpoint global
  , obsMetricsPort     = if obsMetricsPort project /= obsMetricsPort defaultObservabilityConfig
                           then obsMetricsPort project
                           else obsMetricsPort global
  , obsServiceName     = if obsServiceName project /= obsServiceName defaultObservabilityConfig
                           then obsServiceName project
                           else obsServiceName global
  , obsServiceVersion  = if obsServiceVersion project /= obsServiceVersion defaultObservabilityConfig
                           then obsServiceVersion project
                           else obsServiceVersion global
  , obsExportInterval  = if obsExportInterval project /= obsExportInterval defaultObservabilityConfig
                           then obsExportInterval project
                           else obsExportInterval global
  , obsDebugTraceDir   = if obsDebugTraceDir project /= obsDebugTraceDir defaultObservabilityConfig
                           then obsDebugTraceDir project
                           else obsDebugTraceDir global
  , obsDebug            = if obsDebug project /= obsDebug defaultObservabilityConfig
                           then obsDebug project
                           else obsDebug global
  }