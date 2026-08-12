{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.SubgraphSpec (spec) where

import Data.Maybe (isJust)
import Test.Hspec
import qualified Data.Map.Strict as Map
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
