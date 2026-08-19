{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitterSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (toJSON)
import Graphos.Domain.Config (Granularity(..))
import Graphos.Domain.Types (Edge(..), Relation(..))
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

  describe "Convert" $ do
    it "makeNodeId derives stable IDs from normalized text" $ do
      let id1 = makeNodeId "src/file.ts" "  import { a } from './b'  "
      let id2 = makeNodeId "src/file.ts" "import { a } from './b'"
      id1 `shouldBe` id2

    it "tsNodeLabel returns normalized text" $ do
      let node = tsNode "import" "  import { a } from './b'  " []
      tsNodeLabel node `shouldBe` "import { a } from './b'"

    it "tsNodeLabel handles multi-line imports with elision" $ do
      let longImport = T.replicate 50 "import { a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t } " <> "from './long-path/module.js';"
      let node = tsNode "import_declaration" longImport []
      let label = tsNodeLabel node
      label `shouldSatisfy` T.isInfixOf "..."
      label `shouldSatisfy` T.isSuffixOf "from './long-path/module.js';"

    it "tsNodeLabel leaves short declarations unchanged" $ do
      let shortImport = "import { a } from './b'"
      let node = tsNode "import_declaration" shortImport []
      tsNodeLabel node `shouldBe` "import { a } from './b'"

    it "makeNodeId is stable and ellipsis-free" $ do
      let longImport = T.replicate 50 "import { a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t } " <> "from './long-path/module.js';"
      let node = tsNode "import_declaration" longImport []
      let id1 = makeNodeId "src/file.ts" (tsNodeUntruncatedLabel node)
      let id2 = makeNodeId "src/file.ts" (tsNodeUntruncatedLabel node)
      id1 `shouldBe` id2
      T.isInfixOf "..." id1 `shouldBe` False

    it "tsNodeLabel does not elide non-import types" $ do
      let longType = T.replicate 50 "function_declaration"
      let node = tsNode "function_declaration" longType []
      tsNodeLabel node `shouldNotSatisfy` T.isInfixOf "..."

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
      it "returns Nothing for unresolvable specifier" $ do
        -- Since it's pure, we can't really check if it's unresolvable on disk, 
        -- but we can check if it returns something that doesn't look like a valid path?
        -- Actually, our current implementation always returns something.
        -- Let's see if we can make it return Nothing for empty specifier.
        resolveImport "src/file.ts" "" `shouldBe` Nothing

    describe "Convert - Imports/Exports" $ do
      it "produces an imports edge for an import declaration" $ do
        let node = tsNode "import_declaration" "import { db } from './db.js'" []
            (edges, _) = tsNodeToGraphEdges GranularityFine "src/file.ts" (Just "module") node
            imports = filter ((Imports ==) . edgeRelation) edges
        length imports `shouldBe` 1
        case imports of
          [e] -> do
            edgeSource e `shouldBe` makeNodeId "src/file.ts" "module"
            edgeTarget e `shouldBe` makeNodeId "src/db.ts" "db"
            edgeExtra e `shouldBe` Nothing
          _ -> fail "expected exactly one imports edge"

      it "produces an imports edge with re-export marker for export-from" $ do
        let node = tsNode "export_statement" "export { x } from './y.js'" []
            (edges, _) = tsNodeToGraphEdges GranularityFine "src/file.ts" (Just "module") node
            imports = filter ((Imports ==) . edgeRelation) edges
        length imports `shouldBe` 1
        case imports of
          [e] -> do
            edgeTarget e `shouldBe` makeNodeId "src/y.ts" "y"
            edgeExtra e `shouldBe` Just (toJSON (T.pack "re-export"))
          _ -> fail "expected exactly one imports edge"

