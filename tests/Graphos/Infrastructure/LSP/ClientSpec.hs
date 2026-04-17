module Graphos.Infrastructure.LSP.ClientSpec where

import Test.Hspec

import Graphos.Infrastructure.LSP.Client (languageServerCommands)
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "languageServerCommands" $ do
    it "contains entries for major languages" $ do
      Map.member ".ts" languageServerCommands `shouldBe` True
      Map.member ".py" languageServerCommands `shouldBe` True
      Map.member ".go" languageServerCommands `shouldBe` True
      Map.member ".rs" languageServerCommands `shouldBe` True
      Map.member ".hs" languageServerCommands `shouldBe` True

    it "has at least 20 language entries" $ do
      Map.size languageServerCommands `shouldSatisfy` (>= 20)