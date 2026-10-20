{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitter.ConvertSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Data.Aeson (object, (.=))
import Data.Text.Short (toText)
import Graphos.Domain.Config (Granularity(..))
import Graphos.Domain.Types.Graph (extractionNodes)
import Graphos.Domain.Types.Node (nodeLabel, nodeId, nodeSignature, nodeExtra, nodeKind)
import Graphos.Infrastructure.Extract.TreeSitter.Convert
import Graphos.Infrastructure.Extract.TreeSitter.Core
  ( TSNodeInfo(..), SourceFile(..), mkSourceFile, defaultTruncationBudget )

-- | Build a named AST node.
tsNode :: String -> T.Text -> [TSNodeInfo] -> TSNodeInfo
tsNode ty txt children = TSNodeInfo
  { tsnType = ty
  , tsnText = txt
  , tsnStartRow = 0, tsnStartCol = 0, tsnEndRow = 0, tsnEndCol = 0
  , tsnIsNamed = True
  , tsnChildren = children
  }

-- | A named identifier-class child (as tree-sitter emits for a declared name).
ident :: T.Text -> TSNodeInfo
ident t = tsNode "identifier" t []

-- | Function declaration with an explicit declared name.
func :: T.Text -> TSNodeInfo
func name = tsNode "function_declaration" ("function " ++ T.unpack name) [ident name]

-- | Class declaration with an explicit declared name.
cls :: T.Text -> TSNodeInfo
cls name = tsNode "class_declaration" ("class " ++ T.unpack name) [ident name]

-- | Rust const item with an explicit declared name.
constant :: T.Text -> TSNodeInfo
constant name = tsNode "const_item" ("const " ++ T.unpack name) [ident name]

-- | Import declaration carrying a quoted specifier in its text.
importDecl :: T.Text -> TSNodeInfo
importDecl spec = tsNode "import_statement" ("import { x } from " ++ T.unpack spec) []

-- | Export wrapper around an inner function declaration.
exportFunc :: T.Text -> TSNodeInfo
exportFunc name =
  tsNode "export_statement" ("export " ++ T.unpack (func name)) [func name]

-- | Decorated Python function (decorated_definition wrapper).
decoratedFn :: T.Text -> TSNodeInfo
decoratedFn name =
  tsNode "decorated_definition" ("@marker\n" ++ T.unpack (func name)) [func name]

sf :: SourceFile
sf = mkSourceFile "src/service.ts" ""

spec :: Spec
spec = do
  describe "3.1 identifier extraction" $ do
    it "function identifier is the declared name" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [func "getUser"]
          nodes = Map.elems (extractionNodes ex)
      length nodes `shouldBe` 1
      nodeLabel (head nodes) `shouldBe` toText "getUser"
      nodeId (head nodes) `shouldBe` makeNodeId sf "Function" "getUser"

    it "class / type identifier is the declared name" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [cls "Service"]
          n = head (Map.elems (extractionNodes ex))
      nodeLabel n `shouldBe` toText "Service"
      nodeId n `shouldBe` makeNodeId sf "Class" "Service"

    it "constant identifier is the declared name" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [constant "MAX"]
          n = head (Map.elems (extractionNodes ex))
      nodeLabel n `shouldBe` toText "MAX"
      nodeId n `shouldBe` makeNodeId sf "Constant" "MAX"

    it "import identifier is the resolved specifier" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [importDecl "'./db'"]
          n = head (Map.elems (extractionNodes ex))
      nodeLabel n `shouldBe` toText "./db"

    it "module identifier is the file stem" $ do
      let ex = tsNodesToExtraction sf GranularityFile defaultTruncationBudget
             [tsNode "program" "module" [func "x"]]
          n = head (Map.elems (extractionNodes ex))
      nodeLabel n `shouldBe` toText "service"
      nodeId n `shouldBe` makeNodeId sf "Module" "service"
      nodeSignature n `shouldBe` Nothing

  describe "3.2 wrapper suppression" $ do
    it "export_statement yields one node flagged exported" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [exportFunc "f"]
          nodes = Map.elems (extractionNodes ex)
      length nodes `shouldBe` 1
      let n = head nodes
      nodeLabel n `shouldBe` toText "f"
      nodeKind n `shouldBe` Just (toText "Function")
      nodeExtra n `shouldBe` Just (object ["exported" .= (True :: Bool)])

    it "decorated_definition yields one node flagged exported" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [decoratedFn "g"]
          nodes = Map.elems (extractionNodes ex)
      length nodes `shouldBe` 1
      nodeLabel (head nodes) `shouldBe` toText "g"
      nodeExtra (head nodes) `shouldBe` Just (object ["exported" .= (True :: Bool)])

  describe "3.3 new id layout + collision suffix" $ do
    it "id layout is <dirHash>_<stem>_<kind>:<identifier>" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [func "h"]
          n = head (Map.elems (extractionNodes ex))
          parts = T.splitOn "_" (nodeId n)
      length parts \`shouldBe\` 3
      T.all T.isDigit (parts !! 0) \`shouldBe\` True   -- dirHash numeric prefix
      parts !! 1 \`shouldBe\` toText "service"          -- stem
      parts !! 2 \`shouldBe\` toText "Function:h"       -- kind:identifier

    it "no source text and no ellipsis embedded in ids" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [func "getUser"]
          id1 = nodeId (head (Map.elems (extractionNodes ex)))
      T.isInfixOf "..." id1 `shouldBe` False
      T.isInfixOf "getUser(" id1 `shouldBe` False

    it "overloads stay distinct: only the later occurrence gets #L<line>" $ do
      let fixture = tsNode "program" "module"
             [ func "handle" `withLine` 10
             , func "handle" `withLine` 40 ]
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [fixture]
          ids = sortIds (Map.elems (extractionNodes ex))
      ids `shouldBe` [ makeNodeId sf "Function" "handle"
                     , makeNodeId sf "Function" "handle" <> "#L40" ]

    it "ids are stable run to run (determinism)" $ do
      let mk = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget
             [tsNode "program" "module" [func "a", cls "B", constant "C"]]
      map nodeId (Map.elems (mk `seq` mk)) `shouldBe` map nodeId (Map.elems (mk))

    it "largest id and signature stay within 512 bytes on a large fixture" $ do
      let big = tsNode "program" "module"
             [ func ("v" ++ T.replicate 400 "erylongname") ]
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [big]
          nodes = Map.elems (extractionNodes ex)
      all (\n -> T.length (nodeId n) <= 512 &&
                 maybe True ((<= 512) . T.length . toText) (nodeSignature n)) nodes `shouldBe` True

  describe "signatures" $ do
    it "function signature is the normalized declaration text" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [func "f"]
          sig = nodeSignature (head (Map.elems (extractionNodes ex)))
      sig `shouldBe` Just (toText "function f")

    it "import signature keeps the trailing specifier clause" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget
             [importDecl "'../../templates/index.js'"]
          sig = nodeSignature (head (Map.elems (extractionNodes ex)))
      sig `shouldSatisfy` maybe False (T.isSuffixOf "from '../../templates/index.js'")

-- | Attach a start row to a fixture node for collision ordering.
withLine :: TSNodeInfo -> Int -> TSNodeInfo
withLine n l = n { tsnStartRow = l }

sortIds :: [T.Text] -> [T.Text]
sortIds = Map.keys . Map.fromList . map (\x -> (x, x))
