-- | FFI bindings for tree-sitter grammars vendored under @vendor/@.
--
-- These languages have no Hackage @tree-sitter-*@ package, so their C/C++
-- sources are compiled directly into the library (see the @c-sources@ /
-- @cxx-sources@ stanzas in @graphos.cabal@). The bundled tree-sitter runtime
-- (Haskell @tree-sitter-0.9@) only accepts language ABI 9–11, so each vendored
-- @parser.c@ is pinned to (or regenerated at) ABI 11 — see @vendor/README.md@.
module Graphos.Infrastructure.Extract.TreeSitter.Vendored
  ( tree_sitter_cpp
  , tree_sitter_yaml
  , tree_sitter_sql
  , tree_sitter_perl
  , tree_sitter_c_sharp
  ) where

import Foreign.Ptr (Ptr)
import TreeSitter.Language (Language)

foreign import ccall unsafe "vendor/tree-sitter-cpp/src/parser.c tree_sitter_cpp"
  tree_sitter_cpp :: Ptr Language

foreign import ccall unsafe "vendor/tree-sitter-yaml/src/parser.c tree_sitter_yaml"
  tree_sitter_yaml :: Ptr Language

foreign import ccall unsafe "vendor/tree-sitter-sql/src/parser.c tree_sitter_sql"
  tree_sitter_sql :: Ptr Language

foreign import ccall unsafe "vendor/tree-sitter-perl/src/parser.c tree_sitter_perl"
  tree_sitter_perl :: Ptr Language

foreign import ccall unsafe "vendor/tree-sitter-c-sharp/src/parser.c tree_sitter_c_sharp"
  tree_sitter_c_sharp :: Ptr Language
