{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Graphos.Domain.Graph.CollisionSpec where

import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph, gNodes, shortestPath)
import qualified Graphos.Domain.Graph.Analysis as Analysis

-- ───────────────────────────────────────────────
-- Collision pair
-- ───────────────────────────────────────────────
-- Two NodeIds that collide under the OLD polynomial hash (nidToInt):
--
--   nidToInt nid = foldl (\acc c -> acc*31 + fromEnum c) 0 nid `mod` (2^63 - 1)
--
-- nidA is the base-31 digit expansion of M = 2^63 - 1, so its hash is M ≡ 0 (mod M).
-- nidB is the base-31 digit expansion of 2M, so its hash is 2M ≡ 0 (mod M).
-- Both are distinct Texts but hash to the same value, so the old FGL adapter
-- mapped them to the same fgl node index and mkGraph collapsed them into one,
-- silently dropping a node. The bijective sequential-index mapping must keep
-- both nodes distinct.
nidA :: Text
nidA = T.pack (map chr [11, 22, 0, 3, 18, 9, 5, 17, 18, 10, 4, 3, 7])

nidB :: Text
nidB = T.pack (map chr [23, 13, 0, 7, 5, 18, 11, 4, 5, 20, 8, 6, 14])

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────
mkNode :: Text -> Node
mkNode nid = Node
  { nodeId           = nid
  , nodeLabel        = nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = "test.hs"
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeSignature    = Nothing
  , nodeCommunityId  = Nothing
  , nodeKind         = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  }

mkEdge :: Text -> Text -> Edge
mkEdge src tgt = Edge
  { edgeId         = EdgeId (src <> "->" <> tgt)
  , edgeSource     = src
  , edgeTarget     = tgt
  , edgeRelation   = Calls
  , edgeWeight     = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra      = Nothing
  }

-- ───────────────────────────────────────────────
-- Arbitrary Graph (for the bijective-coverage property)
-- ───────────────────────────────────────────────
instance Arbitrary Graph where
  arbitrary = do
    n <- choose (0, 50 :: Int)
    ids <- vectorOf n (choose (0, 1000000 :: Int))
    let nids  = map (T.pack . show) ids
        nodes = [mkNode nid | nid <- nids]
    if n < 2
      then pure (buildGraph False (extractionFromLists nodes []))
      else do
        m     <- choose (0, n :: Int)
        pairs <- vectorOf m (do i <- choose (0, n - 1 :: Int)
                                j <- choose (0, n - 1 :: Int)
                                return (i, j))
        let es = [mkEdge (nids !! i) (nids !! j) | (i, j) <- pairs, i /= j]
        pure (buildGraph False (extractionFromLists nodes es))

-- ───────────────────────────────────────────────
-- Spec
-- ───────────────────────────────────────────────
spec :: Spec
spec = do
  describe "nidToInt collision regression" $ do
    let ext = extractionFromLists
                [mkNode nidA, mkNode nidB, mkNode "c"]
                [mkEdge nidA "c", mkEdge nidB "c"]
        g   = buildGraph False ext
        cfg = Analysis.toCachedFGL g
    it "keeps both colliding NodeIds as distinct fgl nodes" $ do
      let iA = Analysis.cachedFindIdx cfg nidA
          iB = Analysis.cachedFindIdx cfg nidB
      iA `shouldBe` Just 0
      iB `shouldBe` Just 1
      iA `shouldNotBe` iB
    it "preserves both shortest paths through the shared hub node" $ do
      shortestPath g nidA "c" `shouldBe` Just [nidA, "c"]
      shortestPath g nidB "c" `shouldBe` Just [nidB, "c"]

  describe "bijective FGL index coverage" $ do
    it "no node is dropped: cfgIdxMap keys == gNodes keys" $ property $
      \(g :: Graph) -> Set.fromList (Map.keys (Analysis.cfgIdxMap (Analysis.toCachedFGL g)))
             == Set.fromList (Map.keys (gNodes g))
