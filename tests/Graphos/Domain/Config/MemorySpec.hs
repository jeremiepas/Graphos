{-# LANGUAGE OverloadedStrings #-}
-- | Memory budget policy tests (fix-oom-memory-budget-guard, tasks 1.1/3.2).
module Graphos.Domain.Config.MemorySpec where

import Test.Hspec
import Data.Aeson (decode)

import Graphos.Domain.Config.Memory

gib :: Integer -> Bytes
gib n = n * 1024 * 1024 * 1024

mib :: Integer -> Bytes
mib n = n * 1024 * 1024

spec :: Spec
spec = do
  describe "deriveBudget" $ do
    it "explicit value wins and ignores MemInfo" $ do
      deriveBudget (Just (MemInfo (gib 20))) (Just (gib 2)) memorySafetyReserve
        `shouldBe` Just (gib 2)
      deriveBudget Nothing (Just (gib 2)) memorySafetyReserve
        `shouldBe` Just (gib 2)

    it "derives available minus the 4 GB reserve" $
      deriveBudget (Just (MemInfo (gib 20))) Nothing memorySafetyReserve
        `shouldBe` Just (gib 16)

    it "clamps the derived budget at the 512 MB floor" $
      deriveBudget (Just (MemInfo (gib 4))) Nothing memorySafetyReserve
        `shouldBe` Just memoryBudgetFloor

    it "clamps the derived budget at the 32 GB ceiling" $
      deriveBudget (Just (MemInfo (gib 62))) Nothing memorySafetyReserve
        `shouldBe` Just memoryBudgetCeiling

    it "no MemInfo and no explicit value means no budget" $
      deriveBudget Nothing Nothing memorySafetyReserve `shouldBe` Nothing

  describe "preFlightVerdict (vs an 8 GB budget)" $ do
    it "aborts at 2 GB available" $
      preFlightVerdict (gib 8) (Just (MemInfo (gib 2))) `shouldBe` Abort

    it "warns at 8 GB available (budget fits, headroom below the reserve)" $
      preFlightVerdict (gib 8) (Just (MemInfo (gib 8))) `shouldBe` Warn

    it "is Ok at 16 GB available" $
      preFlightVerdict (gib 8) (Just (MemInfo (gib 16))) `shouldBe` Ok

    it "is Ok without memory info (nothing to compare)" $
      preFlightVerdict (gib 8) Nothing `shouldBe` Ok

  describe "projectedEmbeddingBytes" $
    it "is nodes × dims × 8 B" $
      projectedEmbeddingBytes 100000 1024 `shouldBe` (100000 * 1024 * 8)

  describe "parseByteSize" $ do
    it "parses G/M suffixes (case-insensitive)" $ do
      parseByteSize "8G" `shouldBe` Just (gib 8)
      parseByteSize "1.5g" `shouldBe` Just (round (1.5 * 1024 * 1024 * 1024 :: Double))
      parseByteSize "512M" `shouldBe` Just (mib 512)
      parseByteSize "512m" `shouldBe` Just (mib 512)

    it "treats plain numbers as MB, matching --max-heap" $
      parseByteSize "2048" `shouldBe` Just (mib 2048)

    it "rejects garbage and non-positive sizes" $ do
      parseByteSize "" `shouldBe` Nothing
      parseByteSize "big" `shouldBe` Nothing
      parseByteSize "0" `shouldBe` Nothing
      parseByteSize "-2G" `shouldBe` Nothing

  describe "MemoryConfig parsing (graphos.yaml memory.budget, task 3.2)" $ do
    it "defaults to auto when the key is absent" $
      decode "{}" `shouldBe` Just (MemoryConfig BudgetAuto)

    it "parses budget: auto" $
      decode "{\"budget\": \"auto\"}" `shouldBe` Just (MemoryConfig BudgetAuto)

    it "parses budget: off" $
      decode "{\"budget\": \"off\"}" `shouldBe` Just (MemoryConfig BudgetOff)

    it "parses budget: <size>" $ do
      decode "{\"budget\": \"8G\"}" `shouldBe` Just (MemoryConfig (BudgetFixed (gib 8)))
      decode "{\"budget\": 2048}" `shouldBe` Just (MemoryConfig (BudgetFixed (mib 2048)))

    it "rejects an unparsable budget value" $
      (decode "{\"budget\": \"lots\"}" :: Maybe MemoryConfig) `shouldBe` Nothing

  describe "formatBytes" $ do
    it "renders GiB and MiB glosses" $ do
      formatBytes (gib 8) `shouldBe` "8.0 GiB"
      formatBytes (mib 512) `shouldBe` "512.0 MiB"
      formatBytes 100 `shouldBe` "100 B"
