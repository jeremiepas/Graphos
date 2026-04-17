{-# OPTIONS_GHC -Wno-orphans #-}
module Graphos.Domain.TypesSpec where

import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types

spec :: Spec
spec = do
  describe "Confidence" $ do
    it "converts to numeric score correctly" $ do
      confidenceScore Extracted `shouldBe` 1.0
      confidenceScore Inferred  `shouldBe` 0.7
      confidenceScore Ambiguous  `shouldBe` 0.2

  describe "Relation" $ do
    it "round-trips through text representation" $ do
      property $ \rel -> textToRelation (relationToText rel) == Just rel

  describe "emptyExtraction" $ do
    it "has zero nodes and edges" $ do
      length (extractionNodes emptyExtraction) `shouldBe` 0
      length (extractionEdges emptyExtraction) `shouldBe` 0

instance Arbitrary Relation where
  arbitrary = elements [Calls, Implements, References, Cites, ConceptuallyRelatedTo
                       ,SharesDataWith, SemanticallySimilarTo, RationaleFor, Imports
                       ,ImportsFrom, Contains, Method, Extends, Overrides, DependsOn]
  {-# INLINE arbitrary #-}

instance Arbitrary Confidence where
  arbitrary = elements [Extracted, Inferred, Ambiguous]
  {-# INLINE arbitrary #-}