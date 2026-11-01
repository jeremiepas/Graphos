-- | Tree-sitter grammar registry.
-- Maps file extensions to their tree-sitter language parsers.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Extract.TreeSitter.Grammar
  ( LanguageInfo(..)
  , languageForExt
  , knownExtensions
  ) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Graphos.Domain.Types (FileType(..))

-- | Information about a tree-sitter language.
data LanguageInfo = LanguageInfo
  { liGrammarName :: String      -- ^ e.g. "typescript", "python"
  , liLanguageId  :: Text        -- ^ LSP language ID for fallback, e.g. "typescript"
  , liFileType    :: FileType    -- ^ Always CodeFile for tree-sitter
  }

-- | Map from file extension to language info.
-- Languages where tree-sitter is the primary extractor.
knownExtensions :: Map String LanguageInfo
knownExtensions = Map.fromList
  [ -- TypeScript family
    (".ts",   LanguageInfo "typescript" "typescript" CodeFile)
  , (".tsx",  LanguageInfo "tsx" "typescriptreact" CodeFile)
  , (".js",   LanguageInfo "javascript" "javascript" CodeFile)
  , (".jsx",  LanguageInfo "javascript" "javascriptreact" CodeFile)
  , (".mjs",  LanguageInfo "javascript" "javascript" CodeFile)
  , (".cjs",  LanguageInfo "javascript" "javascript" CodeFile)
  , -- Other languages with tree-sitter support
    (".py",   LanguageInfo "python" "python" CodeFile)
  , (".pyw",  LanguageInfo "python" "python" CodeFile)
  , (".rs",   LanguageInfo "rust" "rust" CodeFile)
  , (".go",   LanguageInfo "go" "go" CodeFile)
  , (".hs",   LanguageInfo "haskell" "haskell" CodeFile)
  , (".lhs",  LanguageInfo "haskell" "haskell" CodeFile)
  , (".json", LanguageInfo "json" "json" CodeFile)
  , (".c",     LanguageInfo "c" "c" CodeFile)
  , (".cpp",   LanguageInfo "cpp" "cpp" CodeFile)
  , (".hpp",   LanguageInfo "cpp" "cpp" CodeFile)
  , (".h",     LanguageInfo "cpp" "cpp" CodeFile)
  , (".cc",    LanguageInfo "cpp" "cpp" CodeFile)
  , (".cxx",   LanguageInfo "cpp" "cpp" CodeFile)
  , (".hh",    LanguageInfo "cpp" "cpp" CodeFile)
  , (".hxx",   LanguageInfo "cpp" "cpp" CodeFile)
  , (".java",  LanguageInfo "java" "java" CodeFile)
  , (".php",   LanguageInfo "php" "php" CodeFile)
  , (".kt",    LanguageInfo "kotlin" "kotlin" CodeFile)
  , (".kts",   LanguageInfo "kotlin" "kotlin" CodeFile)
  , (".pl",    LanguageInfo "perl" "perl" CodeFile)
  , (".pm",    LanguageInfo "perl" "perl" CodeFile)
  , (".psgi",  LanguageInfo "perl" "perl" CodeFile)
  , (".cs",    LanguageInfo "csharp" "csharp" CodeFile)
  , (".rb",    LanguageInfo "ruby" "ruby" CodeFile)
  , (".ruby",  LanguageInfo "ruby" "ruby" CodeFile)
  , (".nix",   LanguageInfo "nix" "nix" CodeFile)
  , (".yaml",  LanguageInfo "yaml" "yaml" CodeFile)
  , (".yml",   LanguageInfo "yaml" "yaml" CodeFile)
  , (".sql",   LanguageInfo "sql" "sql" CodeFile)
  , (".md",    LanguageInfo "markdown" "markdown" CodeFile)
  , (".markdown", LanguageInfo "markdown" "markdown" CodeFile)
  ]

-- | Look up a language by file extension.
-- Returns Nothing if no tree-sitter grammar is available.
languageForExt :: String -> Maybe LanguageInfo
languageForExt ext = Map.lookup ext knownExtensions