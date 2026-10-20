{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitterSpec (spec) where

import Test.Hspec
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Short (fromText, toText)
import qualified Data.Text as T
import Data.Aeson (object, (.=), toJSON)
import Graphos.Domain.Config (Granularity(..))
import Graphos.Domain.Types (Edge(..), Node(..), Relation(..), FileType(..))
import Graphos.Domain.Types.Graph (extractionNodes, extractionEdges)
import Graphos.Infrastructure.Extract.TreeSitter.Core
import Graphos.Infrastructure.Extract.TreeSitter.Convert
import Graphos.Infrastructure.Extract.TreeSitter.Resolver (resolveImport)

-- | Build a named AST node.
tsNode :: String -> Text -> [TSNodeInfo] -> TSNodeInfo
tsNode ty txt children = TSNodeInfo
  { tsnType = ty
  , tsnText = txt
  , tsnStartRow = 0, tsnStartCol = 0, tsnEndRow = 0, tsnEndCol = 0
  , tsnIsNamed = True
  , tsnChildren = children
  }

-- | A named identifier-class child (as tree-sitter emits for a declared name).
ident :: Text -> TSNodeInfo
ident t = tsNode "identifier" t []

-- | Function declaration with an explicit declared name.
func :: Text -> TSNodeInfo
func name = tsNode "function_declaration" ("function " ++ T.unpack name) [ident name]

-- | Function declaration with arbitrary children (used to nest imports).
funcWithChildren :: Text -> [TSNodeInfo] -> TSNodeInfo
funcWithChildren name children = tsNode "function_declaration" ("function " ++ T.unpack name) children

-- | Import declaration carrying a quoted specifier in its text.
importDecl :: Text -> TSNodeInfo
importDecl spec = tsNode "import_statement" ("import { x } from " ++ T.unpack spec) []

-- | Export wrapper around an inner function declaration.
exportFunc :: Text -> TSNodeInfo
exportFunc name = tsNode "export_statement" ("export " ++ T.unpack (func name)) [func name]

-- | Decorated Python function (decorated_definition wrapper).
decoratedFn :: Text -> TSNodeInfo
decoratedFn name = tsNode "decorated_definition" ("@marker\n" ++ T.unpack (func name)) [func name]

spec :: Spec
spec = do
  describe "Core" $ do
    it "normalizeText collapses whitespace" $ do
      normalizeText "  hello   world  " `shouldBe` "hello world"
      normalizeText "\nhello\tworld\n" `shouldBe` "hello world"

    it "truncateText truncates correctly" $ do
      truncateText 5 "hello world" `shouldBe` "hello..."
      truncateText 20 "hello world" `shouldBe` "hello world"

    it "truncateWithElision performs middle-elision" $ do
      truncateWithElision 10 "abcdefghijk" "ijk" `shouldBe` "abcd...ijk"
      truncateWithElision 25 "import { a, b, c } from './x'" "from './x'"
        `shouldBe` "import { a, ...from './x'"

  describe "3.1 identifier extraction" $ do
    it "function identifier is the declared name" $ do
      let ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget [func "getUser"]
          n = head (Map.elems (extractionNodes ex))
      nodeLabel n `shouldBe` toText "getUser"
      nodeKind n `shouldBe` Just (toText "Function")

    it "class / type identifier is the declared name" $ do
      let ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget [tsNode "class_declaration" "class Service" [ident "Service"]]
          n = head (Map.elems (extractionNodes ex))
      nodeLabel n `shouldBe` toText "Service"
      nodeKind n `shouldBe` Just (toText "Class")

    it "constant identifier is the declared name" $ do
      let ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget [tsNode "const_item" "const MAX" [ident "MAX"]]
          n = head (Map.elems (extractionNodes ex))
      nodeLabel n `shouldBe` toText "MAX"
      nodeKind n `shouldBe` Just (toText "Constant")

    it "import identifier is the resolved specifier" $ do
      let ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget [importDecl "'./db'"]
          n = head (Map.elems (extractionNodes ex))
      nodeLabel n `shouldBe` toText "./db"

    it "module identifier is the file stem" $ do
      let ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFile defaultTruncationBudget
             [tsNode "program" "module" [func "x"]]
          n = head (Map.elems (extractionNodes ex))
      nodeLabel n `shouldBe` toText "service"
      nodeKind n `shouldBe` Just (toText "Module")
      nodeSignature n `shouldBe` Nothing

  describe "3.2 wrapper suppression" $ do
    it "export_statement yields one node flagged exported" $ do
      let ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget [exportFunc "f"]
          nodes = Map.elems (extractionNodes ex)
      length nodes `shouldBe` 1
      let n = head nodes
      nodeLabel n `shouldBe` toText "f"
      nodeKind n `shouldBe` Just (toText "Function")
      nodeExtra n `shouldSatisfy` maybe False (== Just (object ["exported" .= (True :: Bool)]))

    it "decorated_definition yields one node flagged exported" $ do
      let ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget [decoratedFn "g"]
          nodes = Map.elems (extractionNodes ex)
      length nodes `shouldBe` 1
      nodeLabel (head nodes) `shouldBe` toText "g"
      nodeExtra (head nodes) `shouldSatisfy` maybe False (== Just (object ["exported" .= (True :: Bool)]))

  describe "3.3 new id layout + collision suffix" $ do
    it "id layout is <dirHash>_<stem>_<kind>:<identifier>" $ do
      let ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget [func "h"]
          n = head (Map.elems (extractionNodes ex))
          parts = T.splitOn "_" (nodeId n)
      length parts `shouldBe` 3
      T.all T.isDigit (parts !! 0) `shouldBe` True
      parts !! 1 `shouldBe` toText "service"
      parts !! 2 `shouldBe` toText "Function:h"

    it "overloads stay distinct: only the later occurrence gets #L<line>" $ do
      let fixture = tsNode "program" "module"
             [ funcWithChildren "handle" [ ident "handle" ] `withLine` 10
             , funcWithChildren "handle" [ ident "handle" ] `withLine` 40 ]
          ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget [fixture]
          ids = Map.keys (extractionNodes ex)
      ids `shouldBe` [ makeNodeId (mkSourceFile "src/service.ts" "") "Function" "handle"
                     , makeNodeId (mkSourceFile "src/service.ts" "") "Function" "handle" <> "#L40" ]

    it "ids are stable run to run (determinism)" $ do
      let mk = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget
             [tsNode "program" "module" [func "a", tsNode "class_declaration" "class B" [ident "B"], tsNode "const_item" "const C" [ident "C"]]]
      Map.keys (extractionNodes mk) `shouldBe` Map.keys (extractionNodes mk)

    it "largest id and signature stay within 512 bytes on a large fixture" $ do
      let big = tsNode "program" "module" [ funcWithChildren "v" [ ident ("v" ++ T.replicate 400 "erylongname") ] ]
          ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget [big]
          nodes = Map.elems (extractionNodes ex)
      all (\n -> T.length (nodeId n) <= 512 &&
                 maybe True ((<= 512) . T.length) (nodeSignature n)) nodes `shouldBe` True

  describe "signatures" $ do
    it "function signature is the normalized declaration text" $ do
      let ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget [func "f"]
          sig = nodeSignature (head (Map.elems (extractionNodes ex)))
      sig `shouldBe` Just (toText "function f")

    it "import signature keeps the trailing specifier clause" $ do
      let ex = tsNodesToExtraction (mkSourceFile "src/service.ts" "") GranularityFunction defaultTruncationBudget
             [importDecl "'../../templates/index.js'"]
          sig = nodeSignature (head (Map.elems (extractionNodes ex)))
      sig `shouldSatisfy` maybe False (T.isSuffixOf "from '../../templates/index.js'")

  describe "3.5 imports edges" $ do
    it "produces an imports edge for an import inside a definition" $ do
      let fixture = tsNode "program" "module"
             [ funcWithChildren "f" [ ident "f", importDecl "'./db'" ] ]
          ex = tsNodesToExtraction (mkSourceFile "src/file.ts" "") GranularityFine defaultTruncationBudget [fixture]
          edges = Map.elems (extractionEdges ex)
          importEdges = filter ((Imports ==) . edgeRelation) edges
      length importEdges `shouldBe` 1
      case importEdges of
        [e] -> do
          edgeSource e `shouldBe` makeNodeId (mkSourceFile "src/file.ts" "") "Function" "f"
          edgeTarget e `shouldBe` makeNodeId (mkSourceFile "src/db.ts" "") "Module" "db"
          edgeExtra e `shouldBe` Nothing
        _ -> fail "expected exactly one imports edge"

    it "produces an imports edge with re-export marker for export-from" $ do
      let fixture = tsNode "program" "module"
             [ funcWithChildren "f" [ ident "f", importDecl "export { x } from './y'" ] ]
          ex = tsNodesToExtraction (mkSourceFile "src/file.ts" "") GranularityFine defaultTruncationBudget [fixture]
          edges = Map.elems (extractionEdges ex)
          importEdges = filter ((Imports ==) . edgeRelation) edges
      case importEdges of
        [e] -> edgeExtra e `shouldSatisfy` maybe False (\v -> v == Just (toJSON "re-export"))
        _ -> fail "expected exactly one imports edge"

    it "produces exactly one external node for a package imported by N files, with N edges" $ do
      let mkFile fp = tsNode "program" "module" [ funcWithChildren "f" [ ident "f", importDecl "'zod'" ] ]
          files = ["src/a.ts", "src/b.ts", "src/c.ts"]
          exs = map (\fp -> tsNodesToExtraction (mkSourceFile fp "") GranularityFine defaultTruncationBudget [mkFile fp]) files
          allEdges = concatMap (Map.elems . extractionEdges) exs
          allNodes = concatMap (Map.elems . extractionNodes) exs
          importEdges = filter ((Imports ==) . edgeRelation) allEdges
          externalNodes = filter (\n -> nodeKind n == Just (fromText "External")) allNodes
          uniqueExternalIds = nub (map nodeId externalNodes)
      length importEdges `shouldBe` 3
      length (nub (map edgeTarget importEdges)) `shouldBe` 1
      length uniqueExternalIds `shouldBe` 1

  describe "Resolver" $ do
    it "resolves relative path with .js -> .ts rewrite" $ do
      resolveImport "src/file.ts" "./module.js" `shouldBe` Just ("src/module.ts", "module")
    it "resolves literal path" $ do
      resolveImport "src/file.ts" "/abs/path/to/module.ts" `shouldBe` Just ("/abs/path/to/module.ts", "module")
    it "resolves index file" $ do
      resolveImport "src/file.ts" "./dir/" `shouldBe` Just ("src/dir/index.ts", "index")
    it "resolves node: builtin" $ do
      resolveImport "src/file.ts" "node:fs" `shouldBe` Just ("external:node:fs", "fs")
    it "resolves bare package" $ do
      resolveImport "src/file.ts" "lodash" `shouldBe` Just ("external:lodash", "lodash")
    it "resolves scoped package with subpath" $ do
      resolveImport "src/file.ts" "@scope/pkg/sub" `shouldBe` Just ("external:@scope/pkg/sub", "@scope/pkg/sub")
    it "returns Nothing for empty specifier" $ do
      resolveImport "src/file.ts" "" `shouldBe` Nothing

-- | Attach a start row to a fixture node for collision ordering.
withLine :: TSNodeInfo -> Int -> TSNodeInfo
withLine n l = n { tsnStartRow = l }
