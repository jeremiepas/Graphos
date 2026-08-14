{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.SubgraphSpec (spec) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust)
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Graphos.Domain.Types
import Graphos.Domain.Types.Graph (LabeledGraph(..))
import Graphos.UseCase.Subgraph

spec :: Spec
spec = do
  describe "extractSubgraph" $ do
    it "returns an empty graph when given an empty graph" $ do
      let g = LabeledGraph Map.empty Map.empty Map.empty Map.empty
          config = SubgraphConfig [] 1 False
      gNodes (extractSubgraph g config) `shouldBe` Map.empty

    it "extracts a single node as core if it matches patterns" $ do
      let n1 = Node "n1" "module1" CodeFile "src/file1.hs" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          node1Id = nodeId n1
          g = LabeledGraph (Map.singleton node1Id n1) Map.empty Map.empty Map.empty
          config = SubgraphConfig [SubsystemConfig "core" ["src/file1.hs"]] 1 False
          subgraph = extractSubgraph g config
      Map.member node1Id (gNodes subgraph) `shouldBe` True
      case Map.lookup node1Id (gNodes subgraph) of
        Just n -> nodeExtra n `shouldSatisfy` isJust
        Nothing -> expectationFailure "node should be in subgraph"

    it "tags a core node with tier, subsystem and layer" $ do
      let n1 = Node "n1" "module1" CodeFile "src/UseCase/Core.hs" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          g = LabeledGraph (Map.singleton (nodeId n1) n1) Map.empty Map.empty Map.empty
          config = SubgraphConfig [SubsystemConfig "svc" ["src/UseCase/*"]] 1 False
          subgraph = extractSubgraph g config
          extra = nodeExtra (gNodes subgraph Map.! nodeId n1)
      extra `shouldSatisfy` isJust
      case extra of
        Just (Object km) -> do
          KM.lookup (Key.fromText "tier") km `shouldBe` Just (String "core")
          KM.lookup (Key.fromText "subsystem") km `shouldBe` Just (String "svc")
          KM.lookup (Key.fromText "layer") km `shouldBe` Just (String "usecase")
        _ -> expectationFailure "expected object extra"

    it "derives imports edges from Import-kind nodes with derived provenance" $ do
      -- a.ts imports './b.ts' (quoted); both files exist as nodes; no real edge.
      let fileA = "src/a.ts"
          fileB = "src/b.ts"
          imp = Node "i1" "import { x } from './b.ts'" CodeFile fileA Nothing Nothing Nothing Nothing (Just "Import") Nothing Nothing Nothing
          nodeA = Node "a" "A" CodeFile fileA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          nodeB = Node "b" "B" CodeFile fileB Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          g = LabeledGraph
                (Map.fromList [(nodeId imp, imp), (nodeId nodeA, nodeA), (nodeId nodeB, nodeB)])
                Map.empty Map.empty Map.empty
          config = SubgraphConfig [SubsystemConfig "core" ["src/a.ts"]] 1 True
          subgraph = extractSubgraph g config
      Map.member (nodeId nodeB) (gNodes subgraph) `shouldBe` True
      let derivedEdges = [ e | e <- Map.elems (gEdges subgraph)
                            , edgeRelation e == Imports
                            , edgeSource e == nodeId nodeA
                            , edgeTarget e == nodeId nodeB ]
      derivedEdges `shouldSatisfy` (not . null)
      case derivedEdges of
        (e : _) -> case edgeExtra e of
          Just (Object km) -> KM.lookup (Key.fromText "provenance") km `shouldBe` Just (String "derived")
          _ -> expectationFailure "expected provenance object"
        [] -> expectationFailure "expected at least one derived edge"

    it "creates external nodes for package imports with no source file" $ do
      let fileA = "src/a.ts"
          imp = Node "i1" "import { x } from 'pkg-lib'" CodeFile fileA Nothing Nothing Nothing Nothing (Just "Import") Nothing Nothing Nothing
          nodeA = Node "a" "A" CodeFile fileA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          g = LabeledGraph
                (Map.fromList [(nodeId imp, imp), (nodeId nodeA, nodeA)])
                Map.empty Map.empty Map.empty
          config = SubgraphConfig [SubsystemConfig "core" ["src/a.ts"]] 1 True
          subgraph = extractSubgraph g config
      Map.lookup "ext:pkg-lib" (gNodes subgraph) `shouldSatisfy` isJust

    it "does not duplicate existing imports edges (derivation is idempotent)" $ do
      let fileA = "src/a.ts"
          fileB = "src/b.ts"
          imp = Node "i1" "import { x } from './b.ts'" CodeFile fileA Nothing Nothing Nothing Nothing (Just "Import") Nothing Nothing Nothing
          nodeA = Node "a" "A" CodeFile fileA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          nodeB = Node "b" "B" CodeFile fileB Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          edge = Edge (EdgeId "e1") (nodeId nodeA) (nodeId nodeB) Imports 1.0 (Confidence 1.0) Nothing
          g = LabeledGraph
                (Map.fromList [(nodeId imp, imp), (nodeId nodeA, nodeA), (nodeId nodeB, nodeB)])
                (Map.singleton (EdgeId "e1") edge)
                Map.empty Map.empty
          config = SubgraphConfig [SubsystemConfig "core" ["src/a.ts"]] 1 True
          subgraph = extractSubgraph g config
      Map.size (gEdges subgraph) `shouldBe` 1
      case Map.elems (gEdges subgraph) of
        (e : _) -> case edgeExtra e of
          Just (Object km) -> KM.lookup (Key.fromText "provenance") km `shouldBe` Just (String "source")
          _ -> expectationFailure "expected provenance object"
        [] -> expectationFailure "expected exactly one edge"

    it "keeps the same import edge set with derivation disabled when real edges exist" $ do
      let fileA = "src/a.ts"
          fileB = "src/b.ts"
          nodeA = Node "a" "A" CodeFile fileA Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          nodeB = Node "b" "B" CodeFile fileB Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          edge = Edge (EdgeId "e1") (nodeId nodeA) (nodeId nodeB) Imports 1.0 (Confidence 1.0) Nothing
          g = LabeledGraph
                (Map.fromList [(nodeId nodeA, nodeA), (nodeId nodeB, nodeB)])
                (Map.singleton (EdgeId "e1") edge)
                (Map.fromList [(nodeId nodeA, Set.singleton (nodeId nodeB))])
                (Map.fromList [(nodeId nodeB, Set.singleton (nodeId nodeA))])
          withDerived = SubgraphConfig [SubsystemConfig "core" ["src/a.ts"]] 1 True
          withoutDerived = SubgraphConfig [SubsystemConfig "core" ["src/a.ts"]] 1 False
          importEdgesOf gr = Set.fromList
            [ (edgeSource e, edgeTarget e)
            | e <- Map.elems (gEdges gr), edgeRelation e == Imports ]
      importEdgesOf (extractSubgraph g withDerived) `shouldBe`
        importEdgesOf (extractSubgraph g withoutDerived)
