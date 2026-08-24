module Graphos.Domain.Query.Cypher.MappingSpec where

import Test.Hspec
import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map

import Graphos.Domain.Types (Node(..), Edge(..), EdgeId(..), FileType(CodeFile), Relation(Calls), Confidence(..))
import Graphos.Domain.Query.Cypher.Mapping

mkNode :: Node
mkNode = Node
  { nodeId          = "n1"
  , nodeLabel       = "foo"
  , nodeFileType    = CodeFile
  , nodeSourceFile  = "src/foo.hs"
  , nodeLineStart   = Just 10
  , nodeLineEnd     = Just 20
  , nodeSignature   = Just "foo :: Int -> Int"
  , nodeCommunityId = Just 3
  , nodeKind        = Just "Function"
  , nodeDegree      = Just 5
  , nodeIsBridge    = Just False
  , nodeExtra       = Nothing
  }

mkEdge :: Edge
mkEdge = Edge
  { edgeId         = EdgeId "e1"
  , edgeSource     = "n1"
  , edgeTarget     = "n2"
  , edgeRelation   = Calls
  , edgeWeight     = 1.0
  , edgeConfidence = Confidence 0.9
  , edgeExtra      = Nothing
  }

spec :: Spec
spec = do
  describe "nodeCypherLabel" $ do
    it "maps nodeKind to the Cypher label" $
      nodeCypherLabel mkNode `shouldBe` Just "Function"

    it "is Nothing when the node has no kind" $
      nodeCypherLabel mkNode { nodeKind = Nothing } `shouldBe` Nothing

  describe "nodeProperties" $ do
    it "returns the declared fields" $ do
      let props = nodeProperties mkNode
      Map.lookup "id" props          `shouldBe` Just (String "n1")
      Map.lookup "label" props       `shouldBe` Just (String "foo")
      Map.lookup "source_file" props `shouldBe` Just (String "src/foo.hs")
      Map.lookup "community" props   `shouldBe` Just (Number 3)
      Map.lookup "degree" props      `shouldBe` Just (Number 5)
      Map.lookup "is_bridge" props   `shouldBe` Just (Bool False)
      Map.lookup "line_start" props  `shouldBe` Just (Number 10)

    it "exposes the signature as the text snippet" $
      Map.lookup "text" (nodeProperties mkNode) `shouldBe` Just (String "foo :: Int -> Int")

    it "omits the text snippet when there is no signature" $
      Map.member "text" (nodeProperties mkNode { nodeSignature = Nothing }) `shouldBe` False

  describe "nodeProperty" $ do
    it "returns the value for a declared property" $
      nodeProperty mkNode "source_file" `shouldBe` Just (String "src/foo.hs")

    it "returns Nothing for an unknown property" $
      nodeProperty mkNode "nonexistent" `shouldBe` Nothing

  describe "edgeCypherType" $ do
    it "maps edgeRelation to the relationship type" $
      edgeCypherType mkEdge `shouldBe` "calls"

  describe "edgeProperties" $ do
    it "returns the declared fields" $ do
      let props = edgeProperties mkEdge
      Map.lookup "id" props         `shouldBe` Just (String "e1")
      Map.lookup "source" props     `shouldBe` Just (String "n1")
      Map.lookup "target" props     `shouldBe` Just (String "n2")
      Map.lookup "weight" props     `shouldBe` Just (Number 1.0)
      Map.lookup "confidence" props `shouldBe` Just (Number 0.9)

  describe "edgeProperty" $ do
    it "returns the value for a declared property" $
      edgeProperty mkEdge "weight" `shouldBe` Just (Number 1.0)

    it "returns Nothing for an unknown property" $
      edgeProperty mkEdge "nonexistent" `shouldBe` Nothing
