module Graphos.UseCase.FormatContextSpec where

import Test.Hspec
import qualified Data.Text as T

import Data.Text.Short (fromText)
import Graphos.Domain.Types (Node(..), FileType(..))
import Graphos.UseCase.FormatContext

spec :: Spec
spec = describe "FormatContext" $ do
  describe "countContextTokens" $ do
    it "approximates token count from text" $ do
      countContextTokens "hello world" `shouldSatisfy` (> 0)

    it "returns 0 for empty text" $ do
      countContextTokens "" `shouldBe` 0

  describe "formatNodeCompact" $ do
    it "includes label and file type" $ do
      let node = Node
            { nodeId           = "test_node"
            , nodeLabel        = fromText "TestNode"
            , nodeFileType     = CodeFile
            , nodeSourceFile   = fromText "src/Test.hs"
            , nodeLineStart    = Just 42
            , nodeCommunityId  = Nothing
            , nodeDegree       = Nothing
            , nodeIsBridge     = Nothing
            , nodeExtra        = Nothing
            , nodeLineEnd      = Just 50
            , nodeKind         = Nothing
            , nodeSignature    = Nothing
            , nodePresentBits  = 0
            }
          result = formatNodeCompact "test_node" node
      T.isInfixOf "TestNode" result `shouldBe` True
      T.isInfixOf "code" result `shouldBe` True
      T.isInfixOf "src:src/Test.hs:42-50" result `shouldBe` True

    it "omits location when line fields are absent" $ do
      let node = Node
            { nodeId           = "test_node2"
            , nodeLabel        = fromText "TestNode"
            , nodeFileType     = CodeFile
            , nodeSourceFile   = fromText "src/Test.hs"
            , nodeLineStart    = Nothing
            , nodeCommunityId  = Nothing
            , nodeDegree       = Nothing
            , nodeIsBridge     = Nothing
            , nodeExtra        = Nothing
            , nodeLineEnd      = Nothing
            , nodeKind         = Nothing
            , nodeSignature    = Nothing
            , nodePresentBits  = 0
            }
          result = formatNodeCompact "test_node2" node
      T.isInfixOf "src:src/Test.hs:42" result `shouldBe` False
