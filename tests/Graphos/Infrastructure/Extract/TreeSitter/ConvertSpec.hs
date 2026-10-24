{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitter.ConvertSpec where

import Test.Hspec
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Short (fromText, toText)

import Graphos.Domain.Types
import Graphos.Infrastructure.Extract.TreeSitter.Convert
import Graphos.Infrastructure.Extract.TreeSitter.Core (TSNodeInfo(..))

-- | Build a named AST node.
tsNode :: String -> T.Text -> [TSNodeInfo] -> TSNodeInfo
tsNode ty txt children = TSNodeInfo
  { tsnType = ty
  , tsnText = txt
  , tsnStartRow = 0, tsnStartCol = 0, tsnEndRow = 0, tsnEndCol = 0
  , tsnIsNamed = True
  , tsnChildren = children
  }

-- | A TypeScript-like fixture:
--
-- program
-- ├── import_statement            "import { db } from './db'"
-- ├── lexical_declaration         "const API_URL = 'x'"  (module-level const)
-- ├── class_declaration "Service"
-- │   ├── property_definition     "role"                 (class field)
-- │   └── method_definition       "getUser"
-- │       ├── lexical_declaration "const user = dbGet(id)"  (LOCAL)
-- │       ├── if_statement        "if (!user) ..."          (statement)
-- │       └── return_statement    "return user;"            (statement)
-- └── function_declaration "helper"
--     └── expression_statement    "logger.info('x')"        (statement)
tsFixture :: TSNodeInfo
tsFixture =
  tsNode "program" "module"
    [ tsNode "import_statement" "import { db } from './db'" []
    , tsNode "lexical_declaration" "const API_URL = 'x'" []
    , tsNode "class_declaration" "Service"
        [ tsNode "property_definition" "role" []
        , tsNode "method_definition" "getUser"
            [ tsNode "lexical_declaration" "const user = dbGet(id)" []
            , tsNode "if_statement" "if (!user) throw" []
            , tsNode "return_statement" "return user;" []
            ]
        ]
    , tsNode "function_declaration" "helper"
        [ tsNode "expression_statement" "logger.info('x')" [] ]
    ]

-- | JSON fixture: document with nested pairs/objects/arrays.
jsonFixture :: TSNodeInfo
jsonFixture =
  tsNode "document" "doc"
    [ tsNode "object" "{...}"
        [ tsNode "pair" "\"name\": \"pkg\"" []
        , tsNode "pair" "\"deps\": {...}"
            [ tsNode "object" "{...}"
                [ tsNode "pair" "\"a\": \"1.0\"" [] ]
            ]
        ]
    ]

labelsAt :: Granularity -> TSNodeInfo -> [String]
labelsAt gran fixture =
  map (T.unpack . toText . nodeLabel) (Map.elems (extractionNodes (tsNodesToExtraction gran "src/service.ts" [fixture])))

-- | Same-file call: `function b() { a(); }` at fine granularity.
sameFileCallFixture :: TSNodeInfo
sameFileCallFixture =
  tsNode "program" "module"
    [ tsNode "function_declaration" "a" []
    , tsNode "function_declaration" "b"
        [ tsNode "expression_statement" "a()"
          [ tsNode "call_expression" "a()"
            [ tsNode "arguments" "()"
              [ tsNode "identifier" "a" [] ] ] ] ]
    ]

-- | `new Foo()` where `Foo` is imported via `./Foo`: the relative specifier
-- resolves to a file node (src/Foo), NOT an External node.
newImportedFixture :: TSNodeInfo
newImportedFixture =
  tsNode "program" "module"
    [ tsNode "import_statement" "import Foo from './Foo'" []
    , tsNode "function_declaration" "main"
        [ tsNode "expression_statement" "new Foo()"
          [ tsNode "new_expression" "new Foo()"
            [ tsNode "type" "Foo"
              [ tsNode "identifier" "Foo" [] ]
            , tsNode "arguments" "()" [] ] ] ]
    ]

-- | Nested call `f(g())`; only the top-level `f` should emit.
nestedCallsFixture :: TSNodeInfo
nestedCallsFixture =
  tsNode "program" "module"
    [ tsNode "function_declaration" "f" []
    , tsNode "function_declaration" "g" []
    , tsNode "function_declaration" "outer"
        [ tsNode "expression_statement" "f(g())"
          [ tsNode "call_expression" "f(g())"
            [ tsNode "callee" "f"
              [ tsNode "identifier" "f" [] ]
            , tsNode "arguments" "(g())"
              [ tsNode "call_expression" "g()"
                [ tsNode "callee" "g"
                  [ tsNode "identifier" "g" [] ]
                , tsNode "arguments" "()" [] ] ] ] ]
        ]
    ]

-- | Only the `calls` edges, in document order.
callEdges :: Granularity -> FilePath -> TSNodeInfo -> [Edge]
callEdges gran filePath fixture =
  filter ((== Calls) . edgeRelation) (fst (tsNodeToGraphEdges gran filePath Nothing fixture))

-- | All edges (document order) for a given extraction.
allEdges :: Granularity -> FilePath -> TSNodeInfo -> [Edge]
allEdges gran filePath fixture =
  fst (tsNodeToGraphEdges gran filePath Nothing fixture)

-- | Count edges of a given relation in an extraction.
countRel :: Relation -> Granularity -> FilePath -> TSNodeInfo -> Int
countRel rel gran filePath fixture =
  length (filter ((== rel) . edgeRelation) (allEdges gran filePath fixture))

spec :: Spec
spec = do
  describe "fine granularity (backward compatible)" $ do
    it "includes statement-level nodes" $ do
      let labels = labelsAt GranularityFine tsFixture
      labels `shouldSatisfy` elem "return user;"
      labels `shouldSatisfy` elem "if (!user) throw"
      labels `shouldSatisfy` elem "const user = dbGet(id)"
      labels `shouldSatisfy` elem "logger.info('x')"

    it "includes JSON pairs" $ do
      let labels = labelsAt GranularityFine jsonFixture
      labels `shouldSatisfy` elem "\"a\": \"1.0\""

  describe "function granularity" $ do
    it "keeps module, class, method, field, import, and module-level const" $ do
      let labels = labelsAt GranularityFunction tsFixture
      labels `shouldSatisfy` elem "module"
      labels `shouldSatisfy` elem "Service"
      labels `shouldSatisfy` elem "getUser"
      labels `shouldSatisfy` elem "role"
      labels `shouldSatisfy` elem "import { db } from './db'"
      labels `shouldSatisfy` elem "const API_URL = 'x'"
      labels `shouldSatisfy` elem "helper"

    it "emits nothing from inside function bodies" $ do
      let labels = labelsAt GranularityFunction tsFixture
      labels `shouldSatisfy` notElem "return user;"
      labels `shouldSatisfy` notElem "if (!user) throw"
      labels `shouldSatisfy` notElem "const user = dbGet(id)"
      labels `shouldSatisfy` notElem "logger.info('x')"

    it "emits no contains-edges into function bodies" $ do
      let ex = tsNodesToExtraction GranularityFunction "src/service.ts" [tsFixture]
          targets = map edgeTarget (Map.elems (extractionEdges ex))
      any ("return user" `T.isInfixOf`) targets `shouldBe` False

  describe "file granularity" $ do
    it "emits exactly one node and zero edges for a JSON document" $ do
      let ex = tsNodesToExtraction GranularityFile "package-lock.json" [jsonFixture]
      Map.size (extractionNodes ex) `shouldBe` 1
      Map.size (extractionEdges ex) `shouldBe` 0

    it "emits only the root node for code files" $ do
      let ex = tsNodesToExtraction GranularityFile "src/service.ts" [tsFixture]
      Map.size (extractionNodes ex) `shouldBe` 1
      map nodeKind (Map.elems (extractionNodes ex)) `shouldBe` [Just (fromText "Module")]

  describe "calls edge emission (AVI-1447)" $ do

    describe "same-file call" $ do
      it "emits exactly one calls edge" $ do
        let es = callEdges GranularityFine "src/service.ts" sameFileCallFixture
        length es `shouldBe` 1

      it "source is the enclosing definition `b`, target is the callee `a`" $ do
        let es = callEdges GranularityFine "src/service.ts" sameFileCallFixture
        (map edgeSource es) `shouldBe` [makeNodeId "src/service.ts" "b"]
        (map edgeTarget es) `shouldBe` [makeNodeId "src/service.ts" "a"]

      it "edge shape matches parseIncomingCall (id, Calls, conf 0.9, weight 0.9)" $ do
        let es = callEdges GranularityFine "src/service.ts" sameFileCallFixture
            e = fromMaybe (error "expected exactly one calls edge") (listToMaybe es)
        edgeId e `shouldBe` EdgeId (makeNodeId "src/service.ts" "b" <> "->call:" <> makeNodeId "src/service.ts" "a")
        edgeRelation e `shouldBe` Calls
        edgeConfidence e `shouldBe` Confidence 0.9
        edgeWeight e `shouldBe` 0.9
        edgeExtra e `shouldBe` Nothing

    describe "new expression" $ do
      it "targets the resolved file node (not External)" $ do
        let es = callEdges GranularityFine "src/new.ts" newImportedFixture
        length es `shouldBe` 1
        (map edgeTarget es) `shouldBe` [makeNodeId "src/Foo" "Foo"]

      it "source is the enclosing definition `main`" $ do
        let es = callEdges GranularityFine "src/new.ts" newImportedFixture
        (map edgeSource es) `shouldBe` [makeNodeId "src/new.ts" "main"]

    describe "nested calls" $ do
      it "emits only the top-level call, excluding the nested call" $ do
        let es = callEdges GranularityFine "src/nested.ts" nestedCallsFixture
        length es `shouldBe` 1
        (map edgeTarget es) `shouldBe` [makeNodeId "src/nested.ts" "f"]

    describe "no regression of other edge types" $ do
      it "contains / imports / depends_on counts are unchanged by calls" $ do
        let -- contains(3): module->import, module->main, main->expression_statement
            containsCount = countRel Contains GranularityFine "src/new.ts" newImportedFixture
            importsCount  = countRel Imports  GranularityFine "src/new.ts" newImportedFixture
            dependsCount  = countRel DependsOn GranularityFine "src/new.ts" newImportedFixture
        containsCount `shouldBe` 3
        importsCount  `shouldBe` 1
        dependsCount  `shouldBe` 0
