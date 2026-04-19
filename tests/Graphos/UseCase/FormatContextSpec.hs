module Graphos.UseCase.FormatContextSpec where

import Test.Hspec
import qualified Data.Text as T

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
            , nodeLabel        = "TestNode"
            , nodeFileType     = CodeFile
            , nodeSourceFile   = "src/Test.hs"
            , nodeSourceLocation = Just "42"
            , nodeLineEnd      = Nothing
            , nodeKind         = Nothing
            , nodeSignature    = Nothing
            , nodeSourceUrl    = Nothing
            , nodeCapturedAt   = Nothing
            , nodeAuthor       = Nothing
            , nodeContributor  = Nothing
            }
          result = formatNodeCompact "test_node" node
      T.isInfixOf "TestNode" result `shouldBe` True
      T.isInfixOf "code" result `shouldBe` True