{-# LANGUAGE ScopedTypeVariables #-}
-- | SplitMix64 (openspec @bunch-search-based-modularization@, task 2.1).
module Graphos.Domain.RandomSpec (spec) where

import Data.Word (Word64)
import Test.Hspec
import Test.QuickCheck hiding (Gen)

import Graphos.Domain.Random

spec :: Spec
spec = describe "Graphos.Domain.Random (SplitMix64)" $ do
  it "matches the reference SplitMix64 outputs for seed 0" $ do
    let (a, g1) = nextWord64 (mkGen 0)
        (b, g2) = nextWord64 g1
        (c, _)  = nextWord64 g2
    [a, b, c] `shouldBe`
      [ 16294208416658607535, 7960286522194355700, 487617019471545679 ]

  it "uniformInt n lies in [0, n) for every n > 0" $
    property $ \(seed :: Word64) (Positive n) ->
      let (x, _) = uniformInt n (mkGen seed)
      in x >= 0 && x < n

  it "uniformInt 0 is 0 and leaves the generator untouched" $
    property $ \(seed :: Word64) ->
      uniformInt 0 (mkGen seed) == (0, mkGen seed)

  it "uniformDouble lies in [0, 1)" $
    property $ \(seed :: Word64) ->
      let (x, _) = uniformDouble (mkGen seed)
      in x >= 0 && x < 1

  it "is deterministic: the same seed gives the same stream" $
    property $ \(seed :: Word64) ->
      take 8 (stream (mkGen seed)) == take 8 (stream (mkGen seed))

  it "split yields two generators that differ from each other and from the parent" $
    property $ \(seed :: Word64) ->
      let g = mkGen seed
          (l, r) = split g
      in fst (nextWord64 l) /= fst (nextWord64 r) && fst (nextWord64 r) /= fst (nextWord64 g)

stream :: Gen -> [Word64]
stream g = let (w, g') = nextWord64 g in w : stream g'
