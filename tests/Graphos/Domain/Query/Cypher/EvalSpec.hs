module Graphos.Domain.Query.Cypher.EvalSpec where

import Test.Hspec

spec :: Spec
spec = describe "EvalSpec" $ do
  it "placeholder" $ True `shouldBe` True
