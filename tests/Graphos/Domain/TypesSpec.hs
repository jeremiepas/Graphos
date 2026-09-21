{-# OPTIONS_GHC -Wno-orphans #-}
module Graphos.Domain.TypesSpec where

import Data.Aeson (ToJSON(..), decode, encode, object, (.=))
import Data.Maybe (isJust)
import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "Confidence" $ do
    it "wraps a Double value" $ do
      let Confidence c = Confidence 0.7
      c `shouldBe` 0.7

    it "serializes to JSON as a number" $ do
      toJSON (Confidence 1.0) `shouldBe` toJSON (1.0 :: Double)

  describe "Relation" $ do
    it "has exactly 14 constructors (8 code + 5 spec-artifact + Documents per deterministic doc-code edges)" $ do
      length [minBound .. maxBound :: Relation] `shouldBe` 14

    it "round-trips through text representation" $ do
      property $ \rel -> textToRelation (relationToText rel) == Just rel

    it "includes spec-required constructors" $ do
      [Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred, Documents]
        `shouldSatisfy` (not . null)

    it "includes the spec-artifact relations (graph-json-contract delta)" $ do
      map relationToText [Refines, ConflictsWith, Satisfies, Supersedes, Constrains]
        `shouldBe` ["refines", "conflicts_with", "satisfies", "supersedes", "constrains"]

    it "serializes the documents relation (deterministic doc-code edges)" $ do
      relationToText Documents `shouldBe` "documents"
      textToRelation "documents" `shouldBe` Just Documents

    it "round-trips the documents relation through JSON" $ do
      let edge = Edge (EdgeId "d->c:documents") "d" "c" Documents 0.9 (Confidence 0.9) Nothing
          enc = encode edge
      (decode enc :: Maybe Edge) `shouldBe` Just edge

  describe "EdgeId" $ do
    it "wraps Text" $ do
      let EdgeId t = EdgeId "test-edge"
      t `shouldBe` "test-edge"

  describe "emptyExtraction" $ do
    it "has zero nodes and edges" $ do
      Map.size (extractionNodes emptyExtraction) `shouldBe` 0
      Map.size (extractionEdges emptyExtraction) `shouldBe` 0

  describe "Node JSON" $ do
    it "ignores legacy keys on parse" $ do
      let legacyJson = object
            [ "id"            .= ("n1" :: T.Text)
            , "label"         .= ("n1" :: T.Text)
            , "file_type"     .= ("code" :: T.Text)
            , "source_file"   .= ("src/X.hs" :: T.Text)
            , "line_start"    .= (1 :: Int)
            , "line_end"      .= (2 :: Int)
            , "signature"     .= ("sig" :: T.Text)
            , "community_id"  .= (0 :: Int)
            , "kind"          .= ("func" :: T.Text)
            , "degree"        .= (1 :: Int)
            , "is_bridge"     .= False
            , "extra"         .= object ["x" .= (1 :: Int)]
            -- legacy keys must be ignored
            , "source_location" .= ("L1" :: T.Text)
            , "source_url"      .= ("http://x" :: T.Text)
            , "captured_at"     .= ("t" :: T.Text)
            , "author"          .= ("a" :: T.Text)
            , "contributor"     .= ("c" :: T.Text)
            ]
          decoded = decode (encode legacyJson) :: Maybe Node
      decoded `shouldSatisfy` isJust
      case decoded of
        Nothing   -> expectationFailure "expected Just Node"
        Just n    -> do
          nodeId n `shouldBe` "n1"
          nodeLineStart n `shouldBe` Just 1
          nodeLineEnd n `shouldBe` Just 2
          nodeExtra n `shouldBe` Just (object ["x" .= (1 :: Int)])

instance Arbitrary Relation where
  arbitrary = elements [Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred]
  {-# INLINE arbitrary #-}