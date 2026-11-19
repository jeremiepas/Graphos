{-# LANGUAGE OverloadedStrings #-}
-- | clustering-evaluation-and-selection, group 1: partitions, directory ground
-- truth, MoJo / MoJoFM and stability (Domain.Community.Partition), plus the
-- JSON truth file reader.
module Graphos.Domain.Community.PartitionSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (elements)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Graphos.Domain.Community.Partition
import Graphos.Infrastructure.ClusterEval.TruthFile (parseTruth)

-- | Every partition of @[0 .. n-1]@ (Bell(n) of them).
allPartitions :: Int -> [Partition Int]
allPartitions n = map fromGroups (go [0 .. n - 1])
  where
    go [] = [[]]
    go (x : xs) = concat [ insertions x p | p <- go xs ]
    -- x alone, or added to each existing group in turn
    insertions x p = ([x] : p) : [ addAt i | i <- [0 .. length p - 1] ]
      where addAt i = [ if j == i then x : g else g | (j, g) <- zip [0 :: Int ..] p ]

-- | A random partition of @[0 .. n-1]@ into at most @k@ groups.
genPartition :: Int -> Gen (Partition Int)
genPartition n = do
  k <- choose (1, max 1 n)
  lbls <- vectorOf n (choose (0, k - 1 :: Int))
  pure (fromAssignment (Map.fromList (zip [0 .. n - 1] lbls)))

genPair :: Gen (Partition Int, Partition Int)
genPair = do
  n <- choose (1, 12)
  (,) <$> genPartition n <*> genPartition n

spec :: Spec
spec = do
  describe "1.1 partitions: meet and restriction" $ do
    it "meet is commutative" $ forAll genPair $ \(p, q) ->
      samePartition (meet p q) (meet q p)
    it "meet is idempotent" $ forAll genPair $ \(p, _) ->
      samePartition (meet p p) p
    it "meet refines both inputs" $ forAll genPair $ \(p, q) ->
      let m = meet p q in refines m p && refines m q
    it "restrictCommon keeps the shared elements and counts the rest" $ do
      let p = fromGroups [[1, 2], [3 :: Int]]
          q = fromGroups [[2, 3, 4 :: Int]]
          (p', q', dp, dq) = restrictCommon p q
      (elements p', elements q', dp, dq) `shouldBe` (Set.fromList [2, 3], Set.fromList [2, 3], 1, 1)

  describe "1.2 directory ground truth" $ do
    it "depth-2 grouping" $
      groups (directoryTruth 2 ["src/a/x.ts", "src/a/y.ts", "src/b/z.ts"])
        `shouldBe` [["src/a/x.ts", "src/a/y.ts"], ["src/b/z.ts"]]
    it "ignores a leading ./ and keeps shallow files in their directory" $ do
      directoryGroup 2 "./src/a/b/x.ts" `shouldBe` "src/a"
      directoryGroup 2 "src/x.ts" `shouldBe` "src"
      directoryGroup 2 "README.md" `shouldBe` "."
    it "multi-ref intersection splits a moved grouping" $ do
      let ref1 = fromGroups [["x", "y"], ["z" :: String]]
          ref2 = fromGroups [["x"], ["y", "z"]]
          (t, _) = meetAll [ref1, ref2]
      samePartition t (fromGroups [["x"], ["y"], ["z"]]) `shouldBe` True
    it "a file missing from one ref is dropped and counted" $ do
      let ref1 = fromGroups [["x", "y" :: String]]
          ref2 = fromGroups [["x", "y", "w"]]
          (t, dropped) = meetAll [ref1, ref2]
      (elements t, dropped) `shouldBe` (Set.fromList ["x", "y"], 1)
    it "reads an explicit truth file" $ do
      fmap groups (parseTruth "{\"a.ts\": \"core\", \"b.ts\": \"core\", \"c.ts\": \"api\"}")
        `shouldBe` Right [["a.ts", "b.ts"], ["c.ts"]]
      parseTruth "{\"a.ts\": 3}" `shouldSatisfy` either (const True) (const False)
      parseTruth "[1]" `shouldSatisfy` either (const True) (const False)

  describe "1.3 MoJo and MoJoFM" $ do
    it "identical partitions: mno = 0 and MoJoFM = 100" $ forAll genPair $ \(p, _) ->
      mno p p == 0 && mjScore (mojoFM p p) == 100
    it "closed-form maximum matches brute force for every reference of n <= 6 nodes" $
      and [ maxMno b == maximum [ mno a b | a <- parts ]
          | n <- [1 .. 6], let parts = allPartitions n, b <- parts ]
        `shouldBe` True
    it "MoJoFM lies in [0, 100]" $ forAll genPair $ \(a, b) ->
      let s = mjScore (mojoFM a b) in s >= 0 && s <= 100
    it "reports the elements dropped from each side" $ do
      let r = mojoFM (fromGroups [[1, 2, 3 :: Int]]) (fromGroups [[2, 3, 4, 5]])
      (mjCommon r, mjDroppedA r, mjDroppedB r) `shouldBe` (2, 1, 2)
    it "worked example: one move and one join" $ do
      -- A = {1,2,3},{4},{5,6}; B = {1,2},{3,4},{5,6}: move 3 out of {1,2,3}
      -- into the cluster tagged {3,4} (the one holding 4): mno = 1
      mno (fromGroups [[1, 2, 3], [4], [5, 6 :: Int]]) (fromGroups [[1, 2], [3, 4], [5, 6]])
        `shouldBe` 1
      -- A = {1},{2} vs B = {1,2}: one join
      mno (fromGroups [[1], [2 :: Int]]) (fromGroups [[1, 2]]) `shouldBe` 1
    it "the empty reference scores 100" $
      mjScore (mojoFM (fromGroups [] :: Partition Int) (fromGroups [])) `shouldBe` 100

  describe "1.4 stability" $ do
    it "an unchanged build is stable both ways" $ forAll genPair $ \(p, _) ->
      let (ab, ba) = stability p p in mjScore ab == 100 && mjScore ba == 100
    it "is MoJoFM in both directions" $ forAll genPair $ \(a, b) ->
      stability a b == (mojoFM a b, mojoFM b a)
