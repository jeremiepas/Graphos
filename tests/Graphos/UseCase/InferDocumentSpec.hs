{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.InferDocumentSpec where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph, Graph)
import Graphos.UseCase.Infer.Document
import Graphos.UseCase.Infer (inferNonSemanticEdgesWith)
import Graphos.Domain.Config (defaultSemanticEdgesConfig)

-- | A (id, label, file-type, source-file) node specification.
type NSpec = (NodeId, Text, FileType, Text)

mkNode :: NSpec -> Node
mkNode (nid, lbl, ft, sf) =
  Node nid (fromText lbl) ft (fromText sf) Nothing
    (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

mkGraph :: [NSpec] -> Graph
mkGraph specs = buildGraph False $ extractionFromLists (map mkNode specs) []

-- | Node specification with an explicit kind.
mkGraphK :: [(NodeId, Text, FileType, Text, Maybe Text)] -> Graph
mkGraphK specs = buildGraph False $ extractionFromLists
  [ (mkNode (nid, lbl, ft, sf)) { nodeKind = fromText <$> k } | (nid, lbl, ft, sf, k) <- specs ] []

-- | Convenience: endpoint pairs of the produced edges.
endpoints :: [Edge] -> [(NodeId, NodeId)]
endpoints es = map (\e -> (edgeSource e, edgeTarget e)) es

hasEdge :: NodeId -> NodeId -> [Edge] -> Bool
hasEdge a b es = elem ((a, b)) (endpoints es)

coEdges :: [NSpec] -> [Edge]
coEdges = inferCoLocationEdges . mkGraph

symEdges :: Map.Map NodeId Text -> [NSpec] -> [Edge]
symEdges dt = inferSymbolMentionEdges dt . mkGraph

pathEdges :: Map.Map NodeId Text -> [NSpec] -> [Edge]
pathEdges dt = inferPathReferenceEdges dt . mkGraph

spec :: Spec
spec = do
  describe "inferCoLocationEdges" $ do
    it "links a README to code in the same directory subtree" $ do
      let specs = [ ("readme", "JwtVerifier", DocFile, "libraries/jwt-verifier/README.md"), ("lib", "lib", CodeFile, "libraries/jwt-verifier/src/lib.rs") ]
      let edges = coEdges specs
      length edges `shouldBe` 1
      hasEdge "readme" "lib" edges `shouldBe` True

    it "links a doc to code in a descendant directory" $ do
      let specs = [ ("readme", "Arch", DocFile, "docs/arch/overview.md"), ("deep", "Mod", CodeFile, "docs/arch/nested/deep.rs") ]
      hasEdge "readme" "deep" (coEdges specs) `shouldBe` True

    it "links a doc to code in its own directory" $ do
      let specs = [ ("a", "D", DocFile, "shared/readme.md")
                  , ("b", "C", CodeFile, "shared/util.rs") ]
      hasEdge "a" "b" (coEdges specs) `shouldBe` True

    it "does not link a README in a different top-level directory" $ do
      let specs = [ ("readme", "Other", DocFile, "docs/readme.md")
                  , ("lib",    "lib",   CodeFile, "libraries/jwt-verifier/src/lib.rs") ]
      coEdges specs `shouldBe` []

    it "guards top-level READMEs: no cross-subtree links from repo root" $ do
      let specs = [ ("readme", "Readme", DocFile, "README.md")
                  , ("lib",    "Lib",    CodeFile, "libraries/jwt-verifier/src/lib.rs") ]
      coEdges specs `shouldBe` []

    it "produces deterministic output (stable across runs)" $ do
      let specs = [ ("r", "D", DocFile, "a/README.md")
                  , ("c1", "X", CodeFile, "a/x.rs")
                  , ("c2", "Y", CodeFile, "a/b/y.rs") ]
          edges = coEdges specs
      edges `shouldBe` coEdges specs
      length (nub edges) `shouldBe` length edges

  describe "inferFileCoLocationEdges (pipeline co-location)" $ do
    it "links one node per doc file to one node per code file, not every symbol" $ do
      let es = inferFileCoLocationEdges maxCoLocationFiles $ mkGraphK
            [ ("readme", "README", DocFile, "src/README.md", Just "File")
            , ("h1", "Overview", DocFile, "src/README.md", Just "Header")
            , ("h2", "Layout", DocFile, "src/README.md", Just "Header")
            , ("modA", "a.ts", CodeFile, "src/a.ts", Just "Module")
            , ("fA1", "f1", CodeFile, "src/a.ts", Just "Function")
            , ("fA2", "f2", CodeFile, "src/a.ts", Just "Function")
            , ("modB", "b.ts", CodeFile, "src/lib/b.ts", Just "Module")
            , ("fB1", "g1", CodeFile, "src/lib/b.ts", Just "Function")
            ]
      endpoints es `shouldBe` [("readme", "modA"), ("readme", "modB")]

    it "skips a doc whose subtree holds more code files than the cap" $ do
      let codeFiles = [ (T.pack ("m" ++ show i), "f", CodeFile, T.pack ("src/m" ++ show i ++ ".ts"), Just "Module")
                      | i <- [1 .. maxCoLocationFiles + 1] ]
          es = inferFileCoLocationEdges maxCoLocationFiles $ mkGraphK
                 (("readme", "README", DocFile, "src/README.md", Just "File") : codeFiles)
      es `shouldBe` []

    it "agrees with inferCoLocationEdges when every file has a single node" $ do
      let specs = [ ("readme", "README", DocFile, "pkg/README.md")
                  , ("c1", "c1", CodeFile, "pkg/src/lib.rs")
                  , ("c2", "c2", CodeFile, "pkg/app/Main.hs")
                  , ("c3", "c3", CodeFile, "other/x.rs")
                  ]
      endpoints (inferFileCoLocationEdges maxCoLocationFiles (mkGraph specs))
        `shouldBe` endpoints (coEdges specs)

  describe "documents edges in the pipeline inference (inferNonSemanticEdgesWith)" $ do
    let codeOnly = mkGraphK
          [ ("modA", "a.ts", CodeFile, "src/a.ts", Just "Module")
          , ("fA1", "parseConfig", CodeFile, "src/a.ts", Just "Function")
          , ("modB", "b.ts", CodeFile, "src/lib/b.ts", Just "Module")
          ]
        withDoc = mkGraphK
          [ ("readme", "README", DocFile, "src/README.md", Just "File")
          , ("modA", "a.ts", CodeFile, "src/a.ts", Just "Module")
          , ("modB", "b.ts", CodeFile, "src/lib/b.ts", Just "Module")
          ]
        docsOf es = [ e | e <- es, edgeRelation e == Documents ]
        infer d g = inferNonSemanticEdgesWith d defaultSemanticEdgesConfig Map.empty g Map.empty
    it "a code-only graph yields no documents edge at any density" $
      mapM_ (\d -> docsOf (infer d codeOnly) `shouldBe` []) [Sparse, Normal, Dense, Maximum]
    it "Sparse drops the co-location heuristic" $
      docsOf (infer Sparse withDoc) `shouldBe` []
    it "Normal keeps file-level co-location" $
      endpoints (docsOf (infer Normal withDoc)) `shouldBe` [("readme", "modA"), ("readme", "modB")]

  describe "inferSymbolMentionEdges" $ do
    it "links a doc to the unique definition of a mentioned symbol" $ do
      let specs = [ ("doc", "Guide", DocFile, "docs/guide.md")
                  , ("sym", "set_remote_execution", CodeFile, "src/ops.rs") ]
          dt = Map.fromList [("doc", "Use set_remote_execution to configure the runner")]
      hasEdge "doc" "sym" (symEdges dt specs) `shouldBe` True

    it "does not link a common word that is not a defined identifier" $ do
      let specs = [ ("doc", "Guide", DocFile, "docs/guide.md")
                  , ("sym", "set_remote_execution", CodeFile, "src/ops.rs") ]
          dt = Map.fromList [("doc", "This guide explains the general process")]
      symEdges dt specs `shouldBe` []

    it "skips identifiers shorter than the minimum length" $ do
      let specs = [ ("doc", "Guide", DocFile, "docs/guide.md")
                  , ("sym", "run", CodeFile, "src/ops.rs") ]
          dt = Map.fromList [("doc", "call run now")]
      symEdges dt specs `shouldBe` []

    it "skips identifiers defined by more than one node (ambiguous)" $ do
      let specs = [ ("doc", "Guide", DocFile, "docs/guide.md")
                  , ("a", "config", CodeFile, "src/a.rs")
                  , ("b", "config", CodeFile, "src/b.rs") ]
          dt = Map.fromList [("doc", "see config here")]
      symEdges dt specs `shouldBe` []

  describe "inferPathReferenceEdges" $ do
    it "links a doc to code in another subtree via a cited path" $ do
      let specs = [ ("doc", "ADR", DocFile, "docs/adr/0007.md")
                  , ("file", "Task", CodeFile, "src/domain/workflow/task-definition.ts") ]
          dt = Map.fromList [("doc", "Implement the flow described in src/domain/workflow/task-definition.ts")]
      hasEdge "doc" "file" (pathEdges dt specs) `shouldBe` True

    it "links a doc to code in the same subtree via a cited path" $ do
      let specs = [ ("doc", "ADR", DocFile, "docs/adr/0007.md")
                  , ("file", "Local", CodeFile, "docs/adr/local.ts") ]
          dt = Map.fromList [("doc", "see docs/adr/local.ts for details")]
      hasEdge "doc" "file" (pathEdges dt specs) `shouldBe` True

    it "normalises a leading ./ when resolving the path" $ do
      let specs = [ ("doc", "ADR", DocFile, "docs/adr/0007.md")
                  , ("file", "Task", CodeFile, "src/domain/workflow/task-definition.ts") ]
          dt = Map.fromList [("doc", "see ./src/domain/workflow/task-definition.ts below")]
      hasEdge "doc" "file" (pathEdges dt specs) `shouldBe` True

    it "does not link a dangling path with no matching code node" $ do
      let specs = [("doc", "ADR", DocFile, "docs/adr/0007.md")]
          dt = Map.fromList [("doc", "see src/does-not-exist.ts")]
      pathEdges dt specs `shouldBe` []

    it "skips a bare filename with no directory separator" $ do
      let specs = [ ("doc", "ADR", DocFile, "docs/adr/0007.md")
                  , ("file", "Task", CodeFile, "src/task-definition.ts") ]
          dt = Map.fromList [("doc", "see task-definition.ts in the repo")]
      pathEdges dt specs `shouldBe` []

    it "skips a path with an unknown extension" $ do
      let specs = [ ("doc", "ADR", DocFile, "docs/adr/0007.md")
                  , ("file", "Task", CodeFile, "src/notes.unknownext") ]
          dt = Map.fromList [("doc", "see src/notes.unknownext")]
      pathEdges dt specs `shouldBe` []
