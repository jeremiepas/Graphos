module Graphos.UseCase.SelectContextSpec where

import Test.Hspec

import Graphos.Domain.Types (emptyExtraction)
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Context (QueryComplexity(..), cbGraphRatio, cbTotalTokens)
import Graphos.UseCase.SelectContext (classifyComplexity, computeBudget)

spec :: Spec
spec = describe "SelectContext" $ do
  describe "classifyComplexity" $ do
    it "classifies short specific queries as Focused" $ do
      let g = buildGraph False emptyExtraction
      classifyComplexity "connectToLSP" g `shouldBe` Focused

    it "classifies overview queries as Architectural" $ do
      let g = buildGraph False emptyExtraction
      classifyComplexity "how does the architecture work" g `shouldBe` Architectural

  describe "computeBudget" $ do
    it "computes budget for Focused" $ do
      let budget = computeBudget Focused 3000
      cbGraphRatio budget `shouldBe` 0.10
      cbTotalTokens budget `shouldBe` 3000