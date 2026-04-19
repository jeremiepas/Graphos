module Graphos.Domain.ContextSpec where

import Test.Hspec

import qualified Data.Map.Strict as Map

import Graphos.Domain.Types
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

  describe "Chat history community" $ do
    it "chatCommunityId is 0" $ do
      chatCommunityId `shouldBe` 0

    it "enrichWithChatHistory adds conversations to community 0" $ do
      let conv = ConversationNode
            { convId            = "conv_001"
            , convQuestion      = "How does parsing work?"
            , convSummary       = "Parser uses AST"
            , convTimestamp     = "2026-04-19T10:00:00Z"
            , convRelevantNodes = ["Parser", "AST"]
            , convTokensUsed    = 500
            }
          commMap = Map.fromList [(1, ["A", "B"]), (2, ["C", "D"])]
          enriched = enrichWithChatHistory commMap [conv]
      Map.lookup chatCommunityId enriched `shouldBe` Just ["conv_001"]

    it "enrichWithChatHistory preserves existing communities" $ do
      let conv = ConversationNode
            { convId            = "conv_001"
            , convQuestion      = "Test?"
            , convSummary       = "Test"
            , convTimestamp     = "2026-04-19T10:00:00Z"
            , convRelevantNodes = []
            , convTokensUsed    = 0
            }
          commMap = Map.fromList [(1, ["A", "B"]), (2, ["C"])]
          enriched = enrichWithChatHistory commMap [conv]
      Map.lookup 1 enriched `shouldBe` Just ["A", "B"]
      Map.lookup 2 enriched `shouldBe` Just ["C"]

    it "enrichWithChatHistory appends to existing community 0" $ do
      let conv2 = ConversationNode { convId = "c2", convQuestion = "Q2", convSummary = "A2", convTimestamp = "t2", convRelevantNodes = [], convTokensUsed = 0 }
          commMap = Map.fromList [(chatCommunityId, ["c1"])]
          enriched = enrichWithChatHistory commMap [conv2]
      Map.lookup chatCommunityId enriched `shouldBe` Just ["c1", "c2"]

    it "chatEdgesForConversation creates one-way edges" $ do
      let conv = ConversationNode
            { convId            = "conv_001"
            , convQuestion      = "How does parsing work?"
            , convSummary       = "Parser uses AST"
            , convTimestamp     = "2026-04-19T10:00:00Z"
            , convRelevantNodes = ["Parser", "AST"]
            , convTokensUsed    = 500
            }
          edges = chatEdgesForConversation conv
      length edges `shouldBe` 2
      -- All edges go FROM the conversation TO code nodes (one-way)
      map edgeSource edges `shouldBe` ["conv_001", "conv_001"]
      map edgeTarget edges `shouldBe` ["Parser", "AST"]

    it "conversationNodeToNode creates a DocumentFile node" $ do
      let conv = ConversationNode
            { convId            = "conv_001"
            , convQuestion      = "How does parsing work?"
            , convSummary       = "Parser uses AST"
            , convTimestamp     = "2026-04-19T10:00:00Z"
            , convRelevantNodes = ["Parser"]
            , convTokensUsed    = 500
            }
          node = conversationNodeToNode conv
      nodeId node `shouldBe` "conv_001"
      nodeLabel node `shouldBe` "How does parsing work?"
      nodeCapturedAt node `shouldBe` Just "2026-04-19T10:00:00Z"
      nodeSourceFile node `shouldBe` "memory/conv_001.md"