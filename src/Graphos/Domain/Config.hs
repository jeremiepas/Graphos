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

     -- * LLM labeling configuration
  , LabelingConfig(..)
  , defaultLabelingConfig

     -- * Top-level configuration
  , GraphosConfig(..)
  , defaultGraphosConfig
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), genericParseJSON, genericToJSON)
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
data Neo4jConfig = Neo4jConfig
  { neo4jUri      :: String  -- ^ Neo4j HTTP URI (e.g. "http://localhost:7474")
  , neo4jUser     :: String  -- ^ Username for authentication (e.g. "neo4j")
  , neo4jPassword :: String  -- ^ Password for authentication
  } deriving (Eq, Show, Generic)

instance ToJSON Neo4jConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 5 }

instance FromJSON Neo4jConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 5 }

-- | Default Neo4j configuration for local development.
defaultNeo4jConfig :: Neo4jConfig
defaultNeo4jConfig = Neo4jConfig
  { neo4jUri      = "http://localhost:7474"
  , neo4jUser     = "neo4j"
  , neo4jPassword = "graphos_dev"
  }

-- ───────────────────────────────────────────────
-- LLM Labeling Configuration
-- ───────────────────────────────────────────────

-- | Configuration for LLM-based community labeling.
-- Supports any OpenAI-compatible API (OpenAI, Ollama, LiteLLM, etc.)
data LabelingConfig = LabelingConfig
  { labelingProvider  :: String  -- ^ Provider: "openai" | "ollama" | "litellm"
  , labelingModel     :: String  -- ^ Model name: "gpt-4o-mini" | "llama3" etc.
  , labelingApiKey    :: String  -- ^ API key (env var ${VAR} resolved at runtime)
  , labelingBaseUrl   :: String  -- ^ API base URL (e.g. "https://api.openai.com/v1")
  , labelingBatchSize :: Int     -- ^ Communities per LLM call (default: 10)
  } deriving (Eq, Show, Generic)

instance ToJSON LabelingConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 8 }

instance FromJSON LabelingConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 8 }

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
  , gcLabeling       :: LabelingConfig               -- ^ LLM labeling settings
  } deriving (Eq, Show, Generic)

-- | Default Graphos configuration (used when no config file is found).
defaultGraphosConfig :: GraphosConfig
defaultGraphosConfig = GraphosConfig
  { gcLsp            = defaultLSPServers
  , gcLanguageIds    = defaultLanguageIds
  , gcFileExtensions = defaultFileExtensions
  , gcExtractors     = defaultExtractors
  , gcNeo4j          = defaultNeo4jConfig
  , gcLabeling       = defaultLabelingConfig
  }