{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitter.ConvertSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

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

labelsAt :: Granularity -> TSNodeInfo -> [T.Text]
labelsAt gran fixture =
  map nodeLabel (Map.elems (extractionNodes (tsNodesToExtraction gran "src/service.ts" [fixture])))

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
      map nodeKind (Map.elems (extractionNodes ex)) `shouldBe` [Just "Module"]
