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
  , -- Other languages with tree-sitter support
    (".py",   LanguageInfo "python" "python" CodeFile)
  , (".pyw",  LanguageInfo "python" "python" CodeFile)
  , (".rs",   LanguageInfo "rust" "rust" CodeFile)
  , (".go",   LanguageInfo "go" "go" CodeFile)
  , (".hs",   LanguageInfo "haskell" "haskell" CodeFile)
  , (".lhs",  LanguageInfo "haskell" "haskell" CodeFile)
  , (".json", LanguageInfo "json" "json" CodeFile)
  , (".c",    LanguageInfo "c" "c" CodeFile)
  , (".cpp",  LanguageInfo "cpp" "cpp" CodeFile)
  , (".java", LanguageInfo "java" "java" CodeFile)
  ]

-- | Look up a language by file extension.
-- Returns Nothing if no tree-sitter grammar is available.
languageForExt :: String -> Maybe LanguageInfo
languageForExt ext = Map.lookup ext knownExtensions