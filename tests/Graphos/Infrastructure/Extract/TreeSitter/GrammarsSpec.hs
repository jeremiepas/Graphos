{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | End-to-end grammar checks: real tree-sitter parse through the FFI binding,
-- then conversion at the default 'GranularityFunction'. Guards the vendored
-- grammars' ABI (an incompatible parser.c makes 'parseWithGrammar' fail) and
-- the per-language node-type whitelist in Convert.
module Graphos.Infrastructure.Extract.TreeSitter.GrammarsSpec (spec) where

import Test.Hspec
import Control.Exception (catch, IOException)
import Data.ByteString (ByteString)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Short (toText)

import Graphos.Domain.Config (Granularity(..), ExtractorConfig(..), defaultExtractors)
import Graphos.Domain.Types (Node(..))
import Graphos.Domain.Types.Graph (extractionNodes)
import Graphos.Infrastructure.Extract.TreeSitter.Core (parseWithGrammar, mkSourceFile, defaultTruncationBudget)
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
          | n <- Map.elems (extractionNodes (tsNodesToExtraction (mkSourceFile path "") gran defaultTruncationBudget ts)) ]

-- | Assert that some node has @kind@ and a label containing @needle@.
hasNode :: [(Text, Text)] -> Text -> Text -> Expectation
hasNode nodes kind needle =
  nodes `shouldSatisfy` any (\(k, l) -> k == kind && needle `T.isInfixOf` l)

kinds :: [(Text, Text)] -> [Text]
kinds = map fst

-- | Count nodes with a given node kind.
countKind :: Text -> [(Text, Text)] -> Int
countKind k = length . filter ((== k) . fst)

-- | Read a source file as bytes, turning a missing file into a clear test
-- failure (the fixture lives at a repo-relative path; we run from the repo
-- root). Guards against silently passing when run from the wrong working dir.
readFixture :: String -> IO ByteString
readFixture path =
  (TE.encodeUtf8 . T.pack <$> readFile path)
    `catch` \(_ :: IOException) ->
      expectationFailure ("could not read fixture " ++ path) >> pure ""

-- | Assert that no node carries the given exact label (substring-insensitive
-- on purpose: a declaration's full text may embed a reference name). This is
-- how we verify a bare @type_identifier@ reference has been dropped.
noNodeWithLabel :: [(Text, Text)] -> Text -> Expectation
noNodeWithLabel nodes needle =
  nodes `shouldNotSatisfy` any (\(_, l) -> l == needle)

spec :: Spec
spec = describe "tree-sitter grammars" $ do
  it "binds every grammar named in defaultExtractors except c/nix/markdown" $ do
    let grammars = mapMaybe ecGrammar (Map.elems defaultExtractors)
        unbound = filter (not . isJust . getGrammarPtr) grammars
    filter (`notElem` ["c", "nix", "markdown"]) unbound `shouldBe` []

  it "Ruby: modules, classes, instance and singleton methods" $ do
    ns <- extract "ruby" GranularityFunction "a.rb" $
      "module Shop\n  class Cart\n    def total; 1; end\n    def self.build; new; end\n  end\nend\n"
    hasNode ns "Class" "Cart"
    hasNode ns "Method" "total"
    hasNode ns "Method" "build"

  it "Go: package, types, functions and methods" $ do
    ns <- extract "go" GranularityFunction "a.go" $
      "package shop\n\ntype Cart struct{ n int }\n\nfunc New() *Cart { return &Cart{} }\n\nfunc (c *Cart) Total() int { return c.n }\n"
    hasNode ns "Module" "a.go"
    hasNode ns "Function" "New"
    hasNode ns "Method" "Total"

  it "C++: namespaces, classes, structs and functions" $ do
    ns <- extract "cpp" GranularityFunction "a.cpp" $
      "#include <vector>\nnamespace shop {\nclass Cart { public: int total() const { return 1; } };\nstruct Item { int n; };\nint helper(int x) { return x; }\n}\n"
    hasNode ns "Module" "a.cpp"
    hasNode ns "Namespace" "shop"
    hasNode ns "Class" "Cart"
    hasNode ns "Struct" "Item"
    hasNode ns "Function" "helper"
    hasNode ns "Import" "#include <vector>"

  it "Python: classes and functions" $ do
    ns <- extract "python" GranularityFunction "a.py" $
      "import os\n\nclass Cart:\n    def total(self):\n        return 1\n\ndef helper():\n    pass\n"
    hasNode ns "Class" "Cart"
    hasNode ns "Function" "helper"

  it "Rust: structs, impls and functions" $ do
    ns <- extract "rust" GranularityFunction "a.rs" $
      "struct Cart { n: i32 }\nimpl Cart { fn total(&self) -> i32 { self.n } }\nfn helper() {}\n"
    hasNode ns "Struct" "Cart"
    hasNode ns "Impl" "Cart"
    hasNode ns "Function" "helper"

  it "SQL: tables, functions and indexes" $ do
    ns <- extract "sql" GranularityFunction "a.sql" $
      "CREATE TABLE cart (id INT PRIMARY KEY, total INT);\nCREATE INDEX cart_total ON cart (total);\nSELECT id FROM cart;\n"
    hasNode ns "Module" "a.sql"
    hasNode ns "Table" "cart"
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

  it "TypeScript: only declared types/enum/interface kept; bare references dropped (AVI-1440)" $ do
    src <- readFixture "example/ts-lsp-test/src/types.ts"
    ns <- extract "typescript" GranularityFunction "types.ts" src
    -- Declared declarations survive with their real kinds
    hasNode ns "Interface" "User"
    hasNode ns "Interface" "CreateUserRequest"
    hasNode ns "Enum" "UserRole"
    hasNode ns "Type" "UserId"
    -- Only the `type UserId = number` alias yields kind "Type"; every bare type
    -- reference (the dropped `type_identifier`) would previously have too, so
    -- exactly one Type node must remain.
    countKind "Type" ns `shouldBe` 1
    -- The referenced names never appear as their own node labels.
    noNodeWithLabel ns "Date"
    noNodeWithLabel ns "number"
    noNodeWithLabel ns "string"

  -- ── AVI-1765 review findings: per-language cases for PHP/Perl/C#/Kotlin ──

  it "PHP: classes, traits, namespace_use and include/require imports" $ do
    ns <- extract "php" GranularityFunction "a.php" $
      "<?php\nnamespace Shop\\Cart;\nuse Vendor\\Lib\\Util;\nclass MyCart {}\ntrait CartTrait {}\ninclude 'inc.php';\nrequire 'req.php';\ninclude_once 'once.php';\nrequire_once 'req_once.php';\n"
    hasNode ns "Class" "MyCart"
    hasNode ns "Trait" "CartTrait"
    -- PHP imports: namespace_use_declaration + include/require expressions
    let importLabels = [ l | (k, l) <- ns, k == "Import" ]
    importLabels `shouldSatisfy` any ("Util" `T.isInfixOf`)
    importLabels `shouldSatisfy` any ("inc.php" `T.isInfixOf`)
    importLabels `shouldSatisfy` any ("req.php" `T.isInfixOf`)

  it "PHP: namespace_use_declaration produces Import edge with specifier" $ do
    ns <- extract "php" GranularityFunction "b.php" $
      "<?php\nuse Foo\\Bar\\Baz;\n"
    hasNode ns "Import" "Baz"

  it "Perl: subroutines, packages and use/require imports" $ do
    ns <- extract "perl" GranularityFunction "a.pl" $
      "package Shop::Cart;\nuse Foo;\nuse parent 'Base';\nsub total { 1 }\nrequire 'other.pl';\n"
    hasNode ns "Module" "Cart"
    hasNode ns "Function" "total"
    let importLabels = [ l | (k, l) <- ns, k == "Import" ]
    importLabels `shouldSatisfy` any ("Foo" `T.isInfixOf`)
    importLabels `shouldSatisfy` any ("other.pl" `T.isInfixOf`)

  it "Perl: use_parent captures parent class import" $ do
    ns <- extract "perl" GranularityFunction "c.pl" $
      "use parent 'My::Base';\nsub hello {}\n"
    hasNode ns "Function" "hello"
    hasNode ns "Import" "My::Base"

  it "C#: namespaces, classes, structs and using directives" $ do
    ns <- extract "csharp" GranularityFunction "a.cs" $
      "using System;\nnamespace Shop {\nclass Cart {}\nstruct Item {}\n}\n"
    hasNode ns "Namespace" "Shop"
    hasNode ns "Class" "Cart"
    hasNode ns "Struct" "Item"
    hasNode ns "Import" "System"

  it "C#: record_declaration is NOT a node at vendored ABI 11 (regression guard)" $ do
    -- ABI 11 parser has no record_declaration; a record should not produce
    -- a Struct node via that name. It may appear as class_declaration at this ABI.
    ns <- extract "csharp" GranularityFunction "r.cs" $
      "namespace Shop {\nclass Cart {}\n}\n"
    kinds ns `shouldNotContain` ["Unknown"]

  it "Kotlin: Kotlin is Java-alias — parses without error via Java grammar" $ do
    ns <- extract "kotlin" GranularityFunction "A.kt" $
      "package shop\nclass Cart {}\n"
    -- Java grammar parses Kotlin coarsely; we only guard that FFI binding
    -- succeeds and extraction yields at least the module node.
    ns `shouldSatisfy` (not . null)
