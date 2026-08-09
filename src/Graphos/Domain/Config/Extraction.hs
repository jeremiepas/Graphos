-- | Extraction configuration types.
-- ExtractorMode, Granularity, ExtractorConfig, LSPServerConfig, FileExtensionConfig,
-- PdfExtractionMode and their defaults.
-- Pure data types — no IO.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config.Extraction
  ( -- * Extractor mode
    ExtractorMode(..)
  , ExtractorConfig(..)
  , defaultExtractors

    -- * Extraction granularity
  , Granularity(..)
  , defaultGranularity

    -- * PDF extraction modes
  , PdfExtractionMode(..)
  , defaultPdfExtractionMode

    -- * LSP configuration
  , LSPServerConfig(..)
  , defaultLSPServers
  , defaultLanguageIds

    -- * File extension configuration
  , FileExtensionConfig(..)
  , defaultFileExtensions

    -- * Helpers
  , lowerFirst
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

-- ───────────────────────────────────────────────
-- Extraction Granularity
-- ───────────────────────────────────────────────

-- | Node granularity for tree-sitter extraction.
--
--   * 'GranularityFine'     — all whitelisted AST node types with full tree
--     recursion (statements, parameters, locals, JSON pairs). ~100+ nodes/file.
--   * 'GranularityFunction' — module/structure nodes, API-surface definitions
--     (functions, classes, types, fields, imports/exports) and module-level
--     constants; extraction stops at function bodies. ~15-25 nodes/file.
--   * 'GranularityFile'     — a single module node per file.
--
-- Resolution order (most specific wins): CLI @--granularity@ flag →
-- per-extension 'ecGranularity' → global config → 'defaultGranularity'.
data Granularity
  = GranularityFine
  | GranularityFunction
  | GranularityFile
  deriving (Eq, Show, Generic)

instance ToJSON Granularity where
  toJSON GranularityFine     = "fine"
  toJSON GranularityFunction = "function"
  toJSON GranularityFile     = "file"

instance FromJSON Granularity where
  parseJSON (String "fine")     = pure GranularityFine
  parseJSON (String "function") = pure GranularityFunction
  parseJSON (String "file")     = pure GranularityFile
  parseJSON v = fail $ "Unknown granularity: " ++ show v ++ ". Expected fine, function, or file"

-- | Built-in default granularity: 'GranularityFunction'.
-- The previous statement-level behavior is available via @granularity: fine@.
defaultGranularity :: Granularity
defaultGranularity = GranularityFunction

-- ───────────────────────────────────────────────
-- PDF Extraction Modes
-- ───────────────────────────────────────────────

-- | How aggressively to extract content from PDF files.
--
--   * 'PdfSmall'       — file node + title only (no sections, minimal graph footprint)
--   * 'PdfMedium'      — file node + top-level titles and sections (no subsections or paragraphs)
--   * 'PdfLarge'       — full hierarchy: all section levels + paragraphs (default, ~max nodes)
--
-- Resolution order: per-extension extractor config → global granularity → 'PdfMedium'.
data PdfExtractionMode
  = PdfSmall       -- ^ File + title only
  | PdfMedium      -- ^ File + titles + sections (no subsections/paragraphs)
  | PdfLarge       -- ^ Full hierarchy: all levels + paragraphs
  deriving (Eq, Show, Generic)

instance ToJSON PdfExtractionMode where
  toJSON PdfSmall  = "small"
  toJSON PdfMedium = "medium"
  toJSON PdfLarge  = "large"

instance FromJSON PdfExtractionMode where
  parseJSON (String "small")  = pure PdfSmall
  parseJSON (String "medium") = pure PdfMedium
  parseJSON (String "large")  = pure PdfLarge
  parseJSON v = fail $ "Unknown PDF extraction mode: " ++ show v ++ ". Expected small, medium, or large"

-- | Default PDF extraction mode: 'PdfMedium'.
-- Medium gives a good balance between graph size and information density.
defaultPdfExtractionMode :: PdfExtractionMode
defaultPdfExtractionMode = PdfMedium

-- | Per-extension extractor configuration.
data ExtractorConfig = ExtractorConfig
  { ecMode        :: ExtractorMode
  , ecGrammar     :: Maybe String   -- ^ tree-sitter grammar name (e.g. "typescript")
  , ecLanguageId  :: Maybe Text      -- ^ LSP language ID (e.g. "typescript")
  , ecGranularity :: Maybe Granularity  -- ^ per-extension granularity override
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
  [ -- TypeScript: tree-sitter (fast, reliable AST parsing; uncomment for LSP)
    (".ts",   ExtractorConfig ExtractTreeSitter (Just "typescript") (Just "typescript") Nothing)
  , (".tsx",  ExtractorConfig ExtractTreeSitter (Just "tsx")       (Just "typescriptreact") Nothing)
  , (".js",  ExtractorConfig ExtractTreeSitter (Just "javascript") (Just "javascript") Nothing)
  , (".jsx", ExtractorConfig ExtractTreeSitter (Just "javascript") (Just "javascriptreact") Nothing)
    -- Haskell: tree-sitter (zero-dependency; uncomment for LSP)
  , (".hs",  ExtractorConfig ExtractTreeSitter (Just "haskell") (Just "haskell") Nothing)
  , (".lhs", ExtractorConfig ExtractTreeSitter (Just "haskell") (Just "haskell") Nothing)
    -- Go: tree-sitter (zero-dependency; uncomment for LSP)
  , (".go",  ExtractorConfig ExtractTreeSitter (Just "go") (Just "go") Nothing)
    -- Rust: tree-sitter (zero-dependency; uncomment for LSP)
  , (".rs",  ExtractorConfig ExtractTreeSitter (Just "rust") (Just "rust") Nothing)
    -- Python: tree-sitter (zero-dependency; uncomment for LSP)
  , (".py",  ExtractorConfig ExtractTreeSitter (Just "python") (Just "python") Nothing)
  , (".pyw", ExtractorConfig ExtractTreeSitter (Just "python") (Just "python") Nothing)
    -- C/C++: tree-sitter (zero-dependency; uncomment for LSP)
  , (".c",   ExtractorConfig ExtractTreeSitter (Just "c")   (Just "c") Nothing)
  , (".cpp", ExtractorConfig ExtractTreeSitter (Just "cpp") (Just "cpp") Nothing)
  , (".h",   ExtractorConfig ExtractTreeSitter (Just "c")   (Just "c") Nothing)
  , (".hpp", ExtractorConfig ExtractTreeSitter (Just "cpp") (Just "cpp") Nothing)
    -- Nix: tree-sitter (zero-dependency; uncomment for LSP)
  , (".nix", ExtractorConfig ExtractTreeSitter (Just "nix") (Just "nix") Nothing)
    -- Ruby: tree-sitter (zero-dependency; uncomment for LSP)
  , (".rb",  ExtractorConfig ExtractTreeSitter (Just "ruby") (Just "ruby") Nothing)
    -- Java: tree-sitter (zero-dependency; uncomment for LSP)
  , (".java",ExtractorConfig ExtractTreeSitter (Just "java") (Just "java") Nothing)
    -- JSON: tree-sitter parser; data files collapse to one node per file
    -- so lock files and config JSON do not inflate the graph
  , (".json",ExtractorConfig ExtractTreeSitter (Just "json") (Just "json") (Just GranularityFile))
    -- Markdown: use tree-sitter mode with built-in parser
    -- (no external LSP server needed for docs)
  , (".md",  ExtractorConfig ExtractTreeSitter (Just "markdown") (Just "markdown") Nothing)
  , (".rst", ExtractorConfig ExtractTreeSitter (Just "markdown") (Just "rest") Nothing)
  , (".adoc",ExtractorConfig ExtractTreeSitter (Just "markdown") (Just "asciidoc") Nothing)
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
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }

-- | Custom FromJSON: accept both camelCase and snake_case field names.
-- The YAML config uses snake_case (command, args, language_id).
instance FromJSON LSPServerConfig where
  parseJSON = withObject "LSPServerConfig" $ \v -> LSPServerConfig
    <$> v .:? "command"     .!= ""
    <*> v .:? "args"        .!= []
    <*> v .:? "language_id" .!= "plaintext"

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
  , fecOffice  :: [String]
  } deriving (Eq, Show, Generic)

instance ToJSON FileExtensionConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }

instance FromJSON FileExtensionConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }

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
  , fecOffice = [ ".docx", ".pptx", ".xlsx", ".doc", ".ppt" ]
  }