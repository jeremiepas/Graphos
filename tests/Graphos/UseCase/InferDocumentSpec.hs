{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.InferDocumentSpec where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Short (fromText)
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph, Graph)
import Graphos.UseCase.Infer.Document

-- | A (id, label, file-type, source-file) node specification.
type NSpec = (NodeId, Text, FileType, Text)

mkNode :: NSpec -> Node
mkNode (nid, lbl, ft, sf) =
  Node nid (fromText lbl) ft (fromText sf)
    (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

mkGraph :: [NSpec] -> Graph
mkGraph specs = buildGraph False $ extractionFromLists (map mkNode specs) []

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
