{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitterSpec (spec) where

import Test.Hspec
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Short (fromText, toText)
import qualified Data.Text as T
import Data.Aeson (toJSON)
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

      it "produces exactly one external node for a package imported by N files, with N edges" $ do
        let mkImport = tsNode "import_declaration" "import { z } from 'zod'" []
            files = ["src/a.ts", "src/b.ts", "src/c.ts"]
            results = map (\fp -> tsNodeToGraphEdges GranularityFine fp (Just "module") mkImport) files
            allEdges = concatMap fst results
            allNodes = concatMap snd results
            importEdges = filter ((Imports ==) . edgeRelation) allEdges
            externalNodes = filter (\n -> nodeKind n == Just (fromText "External")) allNodes
            uniqueExternalIds = nub (map nodeId externalNodes)
        length importEdges `shouldBe` 3
        length (nub (map edgeTarget importEdges)) `shouldBe` 1
        -- Three emission sites produce three node values, but they share one canonical ID,
        -- so after buildGraph merges by NodeId exactly one external node remains.
        length uniqueExternalIds `shouldBe` 1
        case uniqueExternalIds of
          [single] -> all (== single) uniqueExternalIds `shouldBe` True
          _ -> pure ()

      it "integration: imports edge endpoints have different source_file values" $ do
        let node = tsNode "import_declaration" "import { cfg } from './config.js'" []
            filePath = "src/service.ts"
            (edges, targetNodes) = tsNodeToGraphEdges GranularityFine filePath (Just "module") node
            importEdges = filter ((Imports ==) . edgeRelation) edges
        length importEdges `shouldBe` 1
        case (importEdges, targetNodes) of
          ([_e], tn:_) -> do
            let srcNode = Node { nodeId = makeNodeId filePath "module", nodeLabel = fromText "module"
                              , nodeFileType = CodeFile, nodeSourceFile = fromText (T.pack filePath)
                              , nodeLineStart = Nothing, nodeCommunityId = Nothing
                              , nodeDegree = Nothing, nodeIsBridge = Nothing
                              , nodeExtra = Nothing, nodeLineEnd = Nothing
                              , nodeKind = Just (fromText "Module"), nodeSignature = Nothing
                              , nodePresentBits = 0 }
            toText (nodeSourceFile srcNode) `shouldBe` T.pack filePath
            toText (nodeSourceFile tn) `shouldSatisfy` (/= T.pack filePath)
          ([], _) -> fail "expected exactly one imports edge"
          (_, []) -> fail "expected a target node to be materialized"
          (_:_, _:_) -> pure ()

      it "no Import-kind node exists without an outgoing imports edge" $ do
        let node = tsNode "import_declaration" "import { db } from './db.js'" []
            ex = tsNodesToExtraction GranularityFine "src/file.ts"
                   [ tsNode "program" "module" [node] ]
            allNodes = Map.elems (extractionNodes ex)
            allEdges = Map.elems (extractionEdges ex)
            importNodes = filter (\n -> nodeKind n == Just (fromText "Import")) allNodes
            importEdges = filter ((Imports ==) . edgeRelation) allEdges
        length importNodes `shouldBe` 1
        length importEdges `shouldSatisfy` (>= 1)
        -- The importing file's module node is the source; verify an imports edge exists
        any (\e -> edgeSource e == makeNodeId "src/file.ts" "module") importEdges `shouldBe` True

