-- | Domain configuration types for Graphos.
-- Pure data types — no IO. Config file loading lives in Infrastructure.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config
  ( -- * LSP configuration
    LSPServerConfig(..)
  , defaultLSPServers
  , defaultLanguageIds

    -- * File extension configuration
  , FileExtensionConfig(..)
  , defaultFileExtensions

    -- * Top-level configuration
  , GraphosConfig(..)
  , defaultGraphosConfig
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), genericParseJSON, genericToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)

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
  , (".json",  LSPServerConfig "vscode-json-language-server" ["--stdio"] "json")
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
  , (".json",  "json")
  , (".text",  "plaintext")
  , (".raml",  "raml")
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
               , ".nix", ".json"  -- NEW
               ]
  , fecDoc   = [ ".md", ".txt", ".rst", ".adoc", ".org"
               , ".text", ".raml"  -- NEW
               ]
  , fecPaper = [ ".pdf" ]
  , fecImage = [ ".png", ".jpg", ".jpeg", ".webp", ".gif" ]
  , fecVideo = [ ".mp4", ".mov", ".mkv", ".webm", ".avi", ".m4v", ".mp3", ".wav", ".m4a", ".ogg" ]
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
  } deriving (Eq, Show, Generic)

-- | Default Graphos configuration (used when no config file is found).
defaultGraphosConfig :: GraphosConfig
defaultGraphosConfig = GraphosConfig
  { gcLsp            = defaultLSPServers
  , gcLanguageIds    = defaultLanguageIds
  , gcFileExtensions = defaultFileExtensions
  }