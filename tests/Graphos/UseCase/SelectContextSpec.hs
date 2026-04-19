module Graphos.UseCase.SelectContextSpec where

import Test.Hspec

import qualified Data.Map.Strict as Map

import Graphos.Domain.Types (emptyExtraction)
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Context (QueryComplexity(..), cbGraphRatio, cbTotalTokens, chatCommunityId)
import Graphos.UseCase.SelectContext (classifyComplexity, computeBudget, filterChatCommunity)

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

  describe "filterChatCommunity" $ do
    it "removes community 0 from CommunityMap" $ do
      let commMap = Map.fromList [(0, ["chat1", "chat2"]), (1, ["A", "B"]), (2, ["C"])]
          filtered = filterChatCommunity commMap
      Map.lookup chatCommunityId filtered `shouldBe` Nothing
      Map.lookup 1 filtered `shouldBe` Just ["A", "B"]
      Map.lookup 2 filtered `shouldBe` Just ["C"]

    it "preserves CommunityMap without community 0" $ do
      let commMap = Map.fromList [(1, ["A"]), (2, ["B"])]
          filtered = filterChatCommunity commMap
      filtered `shouldBe` commMap