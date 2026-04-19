module Graphos.UseCase.ConversationSpec where

import Test.Hspec

import Graphos.Domain.Context

spec :: Spec
spec = describe "Conversation" $ do
  describe "ConversationNode" $ do
    it "has required fields" $ do
      let conv = ConversationNode
            { convId            = "conv_001"
            , convQuestion      = "How does MCP work?"
            , convSummary       = "MCP uses JSON-RPC"
            , convTimestamp     = "2026-04-17T18:00:00Z"
            , convRelevantNodes = ["node1"]
            , convTokensUsed    = 500
            }
      convId conv `shouldBe` "conv_001"
      convQuestion conv `shouldBe` "How does MCP work?"
      convTokensUsed conv `shouldBe` 500