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
  , defaultCommandTimeoutSeconds

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

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), genericParseJSON, genericToJSON, withObject, (.:), (.:?), (.!=), object, (.=))
import Data.Aeson.Key (Key)
import Data.Char (toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (Object, Parser, defaultOptions, fieldLabelModifier)

-- ───────────────────────────────────────────────
-- Extractor Mode
-- ───────────────────────────────────────────────

-- | How to extract symbols from a file.
--   'ExtractLSP' uses a Language Server Protocol server (requires the server to be installed).
--   'ExtractTreeSitter' uses tree-sitter via C FFI for fast, reliable AST parsing (no server needed).
--   'ExtractStub' creates a single node per file (no parsing).
--   'ExtractCommand' runs a user-declared external command (argv, no shell) and
--   ingests its graph.json-shaped stdout (custom-extractors 1.1).
data ExtractorMode
  = ExtractLSP
  | ExtractTreeSitter
  | ExtractStub
  | ExtractCommand
  deriving (Eq, Show, Generic)

instance ToJSON ExtractorMode where
  toJSON ExtractLSP        = "lsp"
  toJSON ExtractTreeSitter = "tree-sitter"
  toJSON ExtractStub       = "stub"
  toJSON ExtractCommand    = "command"

instance FromJSON ExtractorMode where
  parseJSON (String "lsp")         = pure ExtractLSP
  parseJSON (String "tree-sitter") = pure ExtractTreeSitter
  parseJSON (String "stub")        = pure ExtractStub
  parseJSON (String "command")     = pure ExtractCommand
  parseJSON v = fail $ "Unknown extractor mode: " ++ show v
    ++ ". Expected lsp, tree-sitter, stub, or command"

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
--
--   * @command@ / @batch@ / @timeout@ belong to 'ExtractCommand': the argv to
--     run (no shell), whether one invocation serves every file, and the
--     per-invocation timeout (the runner applies 'defaultCommandTimeoutSeconds'
--     when unset).
--   * @grammar_path@ / @queries@ belong to 'ExtractTreeSitter': a user-supplied
--     shared library loaded at runtime plus the directory holding the optional
--     @definitions.scm@ / @imports.scm@ / @calls.scm@ queries.
data ExtractorConfig = ExtractorConfig
  { ecMode           :: ExtractorMode
  , ecGrammar        :: Maybe String   -- ^ tree-sitter grammar name (e.g. "typescript")
  , ecLanguageId     :: Maybe Text      -- ^ LSP language ID (e.g. "typescript")
  , ecGranularity    :: Maybe Granularity  -- ^ per-extension granularity override
  , ecCommand        :: Maybe [String]  -- ^ argv for 'ExtractCommand' ({file}/{files} placeholders, no shell)
  , ecBatch          :: Maybe Bool      -- ^ command mode: one invocation for all files
  , ecTimeoutSeconds :: Maybe Int       -- ^ command mode: per-invocation timeout
  , ecGrammarPath    :: Maybe FilePath  -- ^ tree-sitter shared library loaded at runtime
  , ecQueries        :: Maybe FilePath  -- ^ directory of optional runtime .scm queries
  } deriving (Eq, Show, Generic)

-- | Default timeout for one command-extractor invocation (seconds).
defaultCommandTimeoutSeconds :: Int
defaultCommandTimeoutSeconds = 60

instance ToJSON ExtractorConfig where
  toJSON ec = object $ concat
    [ [ "mode"          .= ecMode ec ]
    , maybeField "grammar"       (ecGrammar ec)
    , maybeField "language_id"   (ecLanguageId ec)
    , maybeField "granularity"   (ecGranularity ec)
    , maybeField "command"       (ecCommand ec)
    , maybeField "batch"         (ecBatch ec)
    , maybeField "timeout"       (ecTimeoutSeconds ec)
    , maybeField "grammar_path"  (ecGrammarPath ec)
    , maybeField "queries"       (ecQueries ec)
    ]
    where
      maybeField _ Nothing  = []
      maybeField k (Just x) = [ k .= x ]

-- | Parse both the documented snake_case keys (@language_id@, @grammar_path@)
-- and their camelCase spellings, defaulting every optional field.
instance FromJSON ExtractorConfig where
  parseJSON = withObject "ExtractorConfig" $ \v -> ExtractorConfig
    <$> v .:  "mode"
    <*> v .:? "grammar"
    <*> optionalAlias v ["language_id", "languageId"]
    <*> v .:? "granularity"
    <*> v .:? "command"
    <*> v .:? "batch"
    <*> v .:? "timeout"
    <*> optionalAlias v ["grammar_path", "grammarPath"]
    <*> v .:? "queries"

-- | First present optional key among @keys@.
optionalAlias :: FromJSON a => Object -> [Key] -> Parser (Maybe a)
optionalAlias _ []       = pure Nothing
optionalAlias v (k : ks) = do
  mv <- v .:? k
  case mv of
    Just x  -> pure (Just x)
    Nothing -> optionalAlias v ks

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
    ts ".ts"   "typescript" "typescript"
  , ts ".tsx"  "tsx"        "typescriptreact"
  , ts ".js"   "javascript" "javascript"
  , ts ".jsx"  "javascript" "javascriptreact"
  , ts ".mjs"  "javascript" "javascript"
  , ts ".cjs"  "javascript" "javascript"
    -- Haskell: tree-sitter (zero-dependency; uncomment for LSP)
  , ts ".hs"   "haskell"    "haskell"
  , ts ".lhs"  "haskell"    "haskell"
    -- Go: tree-sitter (zero-dependency; uncomment for LSP)
  , ts ".go"   "go"         "go"
    -- Rust: tree-sitter (zero-dependency; uncomment for LSP)
  , ts ".rs"   "rust"       "rust"
    -- Python: tree-sitter (zero-dependency; uncomment for LSP)
  , ts ".py"   "python"     "python"
  , ts ".pyw"  "python"     "python"
    -- C/C++: tree-sitter (zero-dependency; uncomment for LSP)
  , ts ".c"    "c"          "c"
  , ts ".cpp"  "cpp"        "cpp"
  , ts ".h"    "c"          "c"
  , ts ".hpp"  "cpp"        "cpp"
  , ts ".cc"   "cpp"        "cpp"
  , ts ".cxx"  "cpp"        "cpp"
  , ts ".hh"   "cpp"        "cpp"
  , ts ".hxx"  "cpp"        "cpp"
    -- Nix: tree-sitter (zero-dependency; uncomment for LSP)
  , ts ".nix"  "nix"        "nix"
    -- Ruby: tree-sitter (zero-dependency; uncomment for LSP)
  , ts ".rb"   "ruby"       "ruby"
    -- Java: tree-sitter (zero-dependency; uncomment for LSP)
  , ts ".java" "java"       "java"
    -- PHP: tree-sitter
  , ts ".php"  "php"        "php"
    -- Kotlin
  , ts ".kt"   "kotlin"     "kotlin"
  , ts ".kts"  "kotlin"     "kotlin"
    -- Perl
  , ts ".pl"   "perl"       "perl"
  , ts ".pm"   "perl"       "perl"
  , ts ".psgi" "perl"       "perl"
    -- C#
  , ts ".cs"   "csharp"     "csharp"
    -- JSON: tree-sitter parser; data files collapse to one node per file
    -- so lock files and config JSON do not inflate the graph
  , atFileGranularity (ts ".json" "json" "json")
    -- YAML: same data-file treatment as JSON (one node per document stream)
  , atFileGranularity (ts ".yaml" "yaml" "yaml")
  , atFileGranularity (ts ".yml"  "yaml" "yaml")
    -- SQL: tree-sitter (DDL statements become tables/functions/types)
  , ts ".sql"  "sql"        "sql"
    -- Markdown: use tree-sitter mode with built-in parser
    -- (no external LSP server needed for docs)
  , ts ".md"   "markdown"   "markdown"
  , ts ".rst"  "markdown"   "rest"
  , ts ".adoc" "markdown"   "asciidoc"
  ]
  where
    ts ext grammar lang = (ext, ExtractorConfig
      { ecMode           = ExtractTreeSitter
      , ecGrammar        = Just grammar
      , ecLanguageId     = Just lang
      , ecGranularity    = Nothing
      , ecCommand        = Nothing
      , ecBatch          = Nothing
      , ecTimeoutSeconds = Nothing
      , ecGrammarPath    = Nothing
      , ecQueries        = Nothing
      })
    atFileGranularity (ext, ec) = (ext, ec { ecGranularity = Just GranularityFile })

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
  , (".mjs",   LSPServerConfig "typescript-language-server" ["--stdio"] "javascript")
  , (".cjs",   LSPServerConfig "typescript-language-server" ["--stdio"] "javascript")
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
  , (".php",   LSPServerConfig "phpactor" ["language-server"] "php")
  , (".pl",    LSPServerConfig "perlnavigator" [] "perl")
  , (".pm",    LSPServerConfig "perlnavigator" [] "perl")
  , (".psgi",  LSPServerConfig "perlnavigator" [] "perl")
  , (".t",     LSPServerConfig "perlnavigator" [] "perl")
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
  , (".mjs",   "javascript")
  , (".cjs",   "javascript")
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
  , (".pl",    "perl")
  , (".pm",    "perl")
  , (".psgi",  "perl")
  , (".t",     "perl")
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
  { fecCode  = [ ".py", ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs", ".go", ".rs", ".java", ".c", ".cpp", ".h", ".hpp"
                , ".rb", ".cs", ".kt", ".kts", ".scala", ".php", ".swift", ".lua", ".zig", ".hs", ".lhs"
                , ".ex", ".exs", ".m", ".mm", ".jl", ".vue", ".svelte", ".dart", ".ps1"
                 , ".nix"  -- Nix DSL
                 , ".cc", ".cxx", ".hh", ".hxx"  -- C++ (vendored tree-sitter-cpp)
                 , ".yaml", ".yml"               -- YAML (vendored tree-sitter-yaml)
                 , ".sql"                        -- SQL (vendored tree-sitter-sql)
                 , ".pl", ".pm", ".psgi"         -- Perl (vendored tree-sitter-perl)
                 ]
  , fecDoc   = [ ".md", ".txt", ".rst", ".adoc", ".org"
               , ".text", ".raml"  -- NEW
               ]
  , fecPaper = [ ".pdf" ]
  , fecImage = [ ".png", ".jpg", ".jpeg", ".webp", ".gif" ]
  , fecVideo = [ ".mp4", ".mov", ".mkv", ".webm", ".avi", ".m4v", ".mp3", ".wav", ".m4a", ".ogg" ]
  , fecOffice = [ ".docx", ".pptx", ".xlsx", ".doc", ".ppt" ]
  }