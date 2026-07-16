module Graphos.UseCase.ExtractSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map

import Graphos.Domain.Types

spec :: Spec
spec = do
  describe "Extraction" $ do
    it "emptyExtraction has zero nodes and edges" $ do
      Map.size (extractionNodes emptyExtraction) `shouldBe` 0
      Map.size (extractionEdges emptyExtraction) `shouldBe` 0