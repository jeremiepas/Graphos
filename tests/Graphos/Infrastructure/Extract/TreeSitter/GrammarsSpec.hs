{-# LANGUAGE OverloadedStrings #-}
-- | End-to-end grammar checks: real tree-sitter parse through the FFI binding,
-- then conversion at the default 'GranularityFunction'. Guards the vendored
-- grammars' ABI (an incompatible parser.c makes 'parseWithGrammar' fail) and
-- the per-language node-type whitelist in Convert.
module Graphos.Infrastructure.Extract.TreeSitter.GrammarsSpec (spec) where

import Test.Hspec
import Data.ByteString (ByteString)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (toText)

import Graphos.Domain.Config (Granularity(..), ExtractorConfig(..), defaultExtractors)
import Graphos.Domain.Types (Node(..))
import Graphos.Domain.Types.Graph (extractionNodes)
import Graphos.Infrastructure.Extract.TreeSitter.Core (parseWithGrammar)
import Graphos.Infrastructure.Extract.TreeSitter.Convert (tsNodesToExtraction)
import Graphos.Infrastructure.Wiring (getGrammarPtr)

-- | Parse @src@ with @grammar@ and return the (kind, label) of every node.
extract :: String -> Granularity -> FilePath -> ByteString -> IO [(Text, Text)]
extract grammar gran path src =
  case getGrammarPtr grammar of
    Nothing -> expectationFailure ("no binding for " ++ grammar) >> pure []
    Just lang -> do
      parsed <- parseWithGrammar lang src
      case parsed of
        Nothing -> expectationFailure ("parse failed for " ++ grammar) >> pure []
        Just ts -> pure
          [ (maybe "" toText (nodeKind n), toText (nodeLabel n))
          | n <- Map.elems (extractionNodes (tsNodesToExtraction gran path ts)) ]

-- | Assert that some node has @kind@ and a label containing @needle@.
hasNode :: [(Text, Text)] -> Text -> Text -> Expectation
hasNode nodes kind needle =
  nodes `shouldSatisfy` any (\(k, l) -> k == kind && needle `T.isInfixOf` l)

kinds :: [(Text, Text)] -> [Text]
kinds = map fst

spec :: Spec
spec = describe "tree-sitter grammars" $ do
  it "binds every grammar named in defaultExtractors except c/nix/markdown" $ do
    let grammars = mapMaybe ecGrammar (Map.elems defaultExtractors)
        unbound = filter (not . isJust . getGrammarPtr) grammars
    filter (`notElem` ["c", "nix", "markdown"]) unbound `shouldBe` []

  it "Ruby: modules, classes, instance and singleton methods" $ do
    ns <- extract "ruby" GranularityFunction "a.rb" $
      "module Shop\n  class Cart\n    def total; 1; end\n    def self.build; new; end\n  end\nend\n"
    hasNode ns "Class" "class Cart"
    hasNode ns "Method" "def total"
    hasNode ns "Method" "def self.build"

  it "Go: package, types, functions and methods" $ do
    ns <- extract "go" GranularityFunction "a.go" $
      "package shop\n\ntype Cart struct{ n int }\n\nfunc New() *Cart { return &Cart{} }\n\nfunc (c *Cart) Total() int { return c.n }\n"
    hasNode ns "Module" "package shop"
    hasNode ns "Function" "func New()"
    hasNode ns "Method" "Total()"

  it "C++: namespaces, classes, structs and functions" $ do
    ns <- extract "cpp" GranularityFunction "a.cpp" $
      "#include <vector>\nnamespace shop {\nclass Cart { public: int total() const { return 1; } };\nstruct Item { int n; };\nint helper(int x) { return x; }\n}\n"
    hasNode ns "Module" "namespace shop"
    hasNode ns "Namespace" "namespace shop"
    hasNode ns "Class" "class Cart"
    hasNode ns "Struct" "struct Item"
    hasNode ns "Function" "int helper"
    hasNode ns "Import" "#include <vector>"

  it "Python: classes and functions" $ do
    ns <- extract "python" GranularityFunction "a.py" $
      "import os\n\nclass Cart:\n    def total(self):\n        return 1\n\ndef helper():\n    pass\n"
    hasNode ns "Class" "class Cart"
    hasNode ns "Function" "def helper"

  it "Rust: structs, impls and functions" $ do
    ns <- extract "rust" GranularityFunction "a.rs" $
      "struct Cart { n: i32 }\nimpl Cart { fn total(&self) -> i32 { self.n } }\nfn helper() {}\n"
    hasNode ns "Struct" "struct Cart"
    hasNode ns "Impl" "impl Cart"
    hasNode ns "Function" "fn helper"

  it "SQL: tables, functions and indexes" $ do
    ns <- extract "sql" GranularityFunction "a.sql" $
      "CREATE TABLE cart (id INT PRIMARY KEY, total INT);\nCREATE INDEX cart_total ON cart (total);\nSELECT id FROM cart;\n"
    hasNode ns "Module" "CREATE TABLE cart"
    hasNode ns "Table" "CREATE TABLE cart"
    hasNode ns "Index" "cart_total"
    -- DML is implementation detail: absent at function granularity
    kinds ns `shouldNotContain` ["Query"]

  it "SQL: DML statements appear at fine granularity" $ do
    ns <- extract "sql" GranularityFine "a.sql" "SELECT id FROM cart;\n"
    hasNode ns "Query" "SELECT id"

  it "YAML: one document node per file at file granularity" $ do
    ns <- extract "yaml" GranularityFile "a.yaml" "name: shop\nitems:\n  - a\n  - b\n"
    kinds ns `shouldBe` ["Document"]

  it "JSON: one document node per file at file granularity" $ do
    ns <- extract "json" GranularityFile "a.json" "{\"name\": \"shop\", \"items\": [1, 2]}"
    kinds ns `shouldBe` ["Document"]
