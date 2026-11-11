-- | A pure, seeded pseudo-random generator for Domain code: SplitMix64
-- (Steele, Lea & Flood 2014, as in Vigna's reference implementation).
--
-- Twenty lines in-tree instead of a package dependency (openspec change
-- @bunch-search-based-modularization@, design D5). Every consumer threads
-- the generator explicitly, so a result is a deterministic function of its
-- seed; the Bunch search and the landscape evaluation rely on this for
-- reproducibility and permutation invariance.
module Graphos.Domain.Random
  ( Gen
  , mkGen
  , nextWord64
  , split
  , uniformInt
  , uniformDouble
  ) where

import Data.Bits (shiftR, xor)
import Data.Word (Word64)

-- | Generator state (the SplitMix64 seed).
newtype Gen = Gen Word64
  deriving (Eq, Show)

-- | Generator from a seed.
mkGen :: Word64 -> Gen
mkGen = Gen

golden :: Word64
golden = 0x9e3779b97f4a7c15

mix64 :: Word64 -> Word64
mix64 z0 =
  let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
  in z2 `xor` (z2 `shiftR` 31)

-- | Next 64-bit output and the advanced generator.
nextWord64 :: Gen -> (Word64, Gen)
nextWord64 (Gen s) =
  let s' = s + golden
  in (mix64 s', Gen s')

-- | Two independent generators: the first is seeded by one output of the
-- input, the second is the input advanced past that output.
split :: Gen -> (Gen, Gen)
split g =
  let (w, g') = nextWord64 g
  in (Gen w, g')

-- | Uniform integer in @[0, n)@ for @n > 0@ (@0@ for @n ≤ 0@). Reduction
-- modulo @n@: the bias is below @n / 2^64@, negligible for the cluster and
-- node counts this is used for.
uniformInt :: Int -> Gen -> (Int, Gen)
uniformInt n g
  | n <= 0    = (0, g)
  | otherwise =
      let (w, g') = nextWord64 g
      in (fromIntegral (w `mod` fromIntegral n), g')

-- | Uniform 'Double' in @[0, 1)@ from the top 53 bits.
uniformDouble :: Gen -> (Double, Gen)
uniformDouble g =
  let (w, g') = nextWord64 g
  in (fromIntegral (w `shiftR` 11) * (1 / 9007199254740992), g')
