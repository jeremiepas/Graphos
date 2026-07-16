{-# OPTIONS_GHC -Wno-orphans #-}
module Graphos.Domain.TypesSpec where

import Data.Aeson (ToJSON(..))
import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "Confidence" $ do
    it "wraps a Double value" $ do
      let Confidence c = Confidence 0.7
      c `shouldBe` 0.7

    it "serializes to JSON as a number" $ do
      toJSON (Confidence 1.0) `shouldBe` toJSON (1.0 :: Double)

  describe "Relation" $ do
    it "has exactly 8 constructors" $ do
      length [minBound .. maxBound :: Relation] `shouldBe` 8

    it "round-trips through text representation" $ do
      property $ \rel -> textToRelation (relationToText rel) == Just rel

    it "includes spec-required constructors" $ do
      [Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred]
        `shouldSatisfy` (not . null)

  describe "EdgeId" $ do
    it "wraps Text" $ do
      let EdgeId t = EdgeId "test-edge"
      t `shouldBe` "test-edge"

  describe "emptyExtraction" $ do
    it "has zero nodes and edges" $ do
      Map.size (extractionNodes emptyExtraction) `shouldBe` 0
      Map.size (extractionEdges emptyExtraction) `shouldBe` 0

instance Arbitrary Relation where
  arbitrary = elements [Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred]
  {-# INLINE arbitrary #-}