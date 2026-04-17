module Graphos.UseCase.ExtractSpec where

import Test.Hspec

import Graphos.Domain.Types

spec :: Spec
spec = do
  describe "Extraction" $ do
    it "emptyExtraction has zero counts" $ do
      extractionInputTokens emptyExtraction `shouldBe` 0
      extractionOutputTokens emptyExtraction `shouldBe` 0