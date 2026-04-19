module Graphos.Domain.ContextSpec where

import Test.Hspec

import Graphos.Domain.Context

spec :: Spec
spec = describe "Context domain types" $ do
  describe "budgetForComplexity" $ do
    it "Focused has graph ratio 0.10" $ do
      cbGraphRatio (budgetForComplexity Focused 3000) `shouldBe` 0.10

    it "Architectural has graph ratio 0.40" $ do
      cbGraphRatio (budgetForComplexity Architectural 3000) `shouldBe` 0.40

    it "preserves total tokens" $ do
      cbTotalTokens (budgetForComplexity ModuleLevel 5000) `shouldBe` 5000

    it "ModuleLevel graph ratio is 0.20" $ do
      cbGraphRatio (budgetForComplexity ModuleLevel 3000) `shouldBe` 0.20

    it "CrossModule graph ratio is 0.30" $ do
      cbGraphRatio (budgetForComplexity CrossModule 3000) `shouldBe` 0.30

    it "Exploratory graph ratio is 0.25" $ do
      cbGraphRatio (budgetForComplexity Exploratory 3000) `shouldBe` 0.25

    it "defaults max nodes correctly per complexity" $ do
      cbMaxNodes (budgetForComplexity Focused 3000) `shouldBe` 10
      cbMaxNodes (budgetForComplexity ModuleLevel 3000) `shouldBe` 30
      cbMaxNodes (budgetForComplexity CrossModule 3000) `shouldBe` 50
      cbMaxNodes (budgetForComplexity Architectural 3000) `shouldBe` 40
      cbMaxNodes (budgetForComplexity Exploratory 3000) `shouldBe` 40

  describe "emptySelectedContext" $ do
    it "has empty nodes" $ do
      let ctx = emptySelectedContext (budgetForComplexity Focused 3000)
      length (scNodes ctx) `shouldBe` 0

    it "has empty edges" $ do
      let ctx = emptySelectedContext (budgetForComplexity Focused 3000)
      length (scEdges ctx) `shouldBe` 0

    it "has zero match score" $ do
      let ctx = emptySelectedContext (budgetForComplexity Focused 3000)
      scMatchScore ctx `shouldBe` 0.0

  describe "ConversationNode" $ do
    it "stores fields correctly" $ do
      let conv = ConversationNode
            { convId            = "conv_001"
            , convQuestion      = "How does MCP work?"
            , convSummary       = "MCP uses JSON-RPC over stdio"
            , convTimestamp     = "2026-04-17T18:00:00Z"
            , convRelevantNodes = ["node1", "node2"]
            , convTokensUsed    = 1500
            }
      convId conv `shouldBe` "conv_001"
      convQuestion conv `shouldBe` "How does MCP work?"
      convTokensUsed conv `shouldBe` 1500