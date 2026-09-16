{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Conflict quantification for last-write-wins (LWW) views — the coequalizer
-- half of J2 (AVI-661, M4; design.md §3 theorem T4 / SHALL-M4).
--
-- These tests verify the measurable quantities in Graphos.Domain.Graph.Conflict:
--   * @D_N(σ,τ)@ node disagreement, @D_E(σ,τ)@ edge disagreement, @δ(σ,τ)@ delta.
--   * @Δ_cluster(σ,τ)@ community-label flips between the two merged views.
--
-- Acceptance mapping (AVI-661):
--   * AC-1 construct LWW-conflicting views; orders diverge only on conflicting
--     labels and @|D_N| ≤ |K_conflict>@ — golden + property tests below.
--   * AC-2 @δ@ and @Δ_cluster@ are reproducible for a fixed order across runs.
--   * AC-3 for consistent views @Δ_cluster = ∅@ (zero non-commutativity).
--
-- Structure-confluence note: 'mergeGraphs' unions every edge key regardless of
-- operand order, so the adjacency structure of @merge_σ@ equals that of
-- @merge_τ@ for any two orders. Leiden here is unweighted (it clusters the
-- unweighted support graph), therefore @Δ_cluster@ is measured from real
-- clustering and comes back empty even under payload conflict — the community
-- layer is confluent under merge. The tests assert this honestly rather than
-- assuming it.
module Graphos.Domain.Graph.ConflictSpec (spec) where

import Data.List (reverse)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (replicateM)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Short (fromText)
import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types
import Graphos.Domain.Graph
import Graphos.Domain.Graph.Conflict
import Graphos.Domain.Community
  ( Resolution
  , defaultResolution
  , detectCommunitiesWithResolution
  , buildReverseIndex
  )

-- ── builders ───────────────────────────────────────────────────────────

mkNodeKV :: (Text, FileType, Text) -> Node
mkNodeKV (k, ft, src) = Node
  { nodeId           = k
  , nodeLabel        = fromText k
  , nodeFileType     = ft
  , nodeSourceFile   = fromText src
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodePresentBits  = 0
  }

mkEdge :: Text -> Text -> Relation -> Edge
mkEdge src tgt rel = Edge
  { edgeId = EdgeId (src <> "->" <> tgt)
  , edgeSource = src
  , edgeTarget = tgt
  , edgeRelation = rel
  , edgeWeight = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra = Nothing
  }

-- | Edge with an explicit weight — used to build an edge-weight conflict whose
-- node payloads are identical, isolating the edge term of 'deltaDivergence'.
mkEdgeW :: Text -> Text -> Double -> Edge
mkEdgeW src tgt w = Edge
  { edgeId = EdgeId (src <> "->" <> tgt)
  , edgeSource = src
  , edgeTarget = tgt
  , edgeRelation = Calls
  , edgeWeight = w
  , edgeConfidence = Confidence 1.0
  , edgeExtra = Nothing
  }

undirectedGraph :: [Node] -> [Edge] -> Graph
undirectedGraph nodes edges = buildGraph False (extractionFromLists nodes edges)

-- | Fold a view family into one graph with last-write-wins ('mergeGraphs' is
-- right-wins), so the forward fold keeps the LAST view's payloads and the
-- reverse fold keeps the FIRST view's.
foldLeft :: [Graph] -> Graph
foldLeft = foldr mergeGraphs emptyGraph

emptyGraph :: Graph
emptyGraph = buildGraph False (extractionFromLists [] [])

-- ── property test machinery: random LWW view families (T4) ─────────────

newtype ViewFamily = ViewFamily [Graph]

instance Arbitrary ViewFamily where
  arbitrary = do
    nKeys  <- choose (3, 5 :: Int)
    let keys = [ T.pack ("k" ++ show i) | i <- [0 .. nKeys - 1] ]
    -- stable keys keep an identical payload across views; conflict keys vary.
    mask <- mapM (\_ -> arbitrary :: Gen Bool) keys
    let conflictKeys = [ k | (k, True) <- zip keys mask ]
        stableKeys   = [ k | (k, False) <- zip keys mask ]
    nViews <- choose (2, 4 :: Int)
    views  <- replicateM nViews (genViewFor keys conflictKeys stableKeys)
    pure (ViewFamily views)

instance Show ViewFamily where
  show _ = "<view family>"

genViewFor :: [Text] -> [Text] -> [Text] -> Gen Graph
genViewFor keys conflictKeys _stableKeys = do
  nodeTypes <- mapM pickKey keys
  let nodes = [ mkNodeKV (k, ft, "src") | (k, ft) <- zip keys nodeTypes ]
      edges = [ mkEdge a b Calls | (a, b) <- zip keys (drop 1 keys) ]
  pure (undirectedGraph nodes edges)
  where
    pickKey k = do
      if k `elem` conflictKeys
        then do v <- arbitrary :: Gen Bool
                -- two distinct payloads for a conflict key; one fixed payload for stable keys
                pure (if v then DocFile else CodeFile)
        else pure CodeFile

-- ── golden tests: node / edge divergence (T4) ──────────────────────────

spec :: Spec
spec = do

  describe "M4 node/edge divergence (T4)" $ do

    it "consistent views: D_N empty and delta = 0" $ do
      -- Shared key "k0" carries an identical payload in both views; each view has
      -- a disjoint private node. Different views, no shared conflict.
      let a = undirectedGraph
              [ mkNodeKV ("k0", CodeFile, "shared.hs")
              , mkNodeKV ("pA", CodeFile, "a.hs") ]
              [ mkEdge "k0" "pA" Calls ]
          b = undirectedGraph
              [ mkNodeKV ("k0", CodeFile, "shared.hs")
              , mkNodeKV ("pB", DocFile, "b.md") ]
              [ mkEdge "k0" "pB" Calls ]
          gF = foldLeft [a, b]
          gR = foldLeft (reverse [a, b])
      nodeDisagreement gF gR `shouldBe` Set.empty
      edgeDisagreement gF gR `shouldBe` Set.empty
      deltaDivergence gF gR `shouldBe` 0

    it "conflicting node payload: D_N = {the key}, D_N subset K_conflict, |D_N| <= |K_conflict>" $ do
      -- "c" carries two distinct payloads across the views; "x" is consistent.
      let a = undirectedGraph
              [ mkNodeKV ("x", CodeFile, "same.hs")
              , mkNodeKV ("c", CodeFile, "a.hs") ]
              [ mkEdge "x" "c" Calls ]
          b = undirectedGraph
              [ mkNodeKV ("x", CodeFile, "same.hs")
              , mkNodeKV ("c", DocFile, "b.hs") ]
              [ mkEdge "x" "c" Calls ]
      let family = [a, b]
          gF     = foldLeft family
          gR     = foldLeft (reverse family)
          dn     = nodeDisagreement gF gR
          kconf  = conflictingNodeIds family
      dn `shouldBe` Set.fromList ["c"]
      Set.isSubsetOf dn kconf `shouldBe` True
      Set.size dn `shouldSatisfy` (\n -> n <= Set.size kconf)
      deltaDivergence gF gR `shouldSatisfy` (> 0)

    it "edge-weight conflict: D_N empty but edgeDisagreement and delta are non-zero" $ do
      -- Nodes identical; only the weight of (a,b) differs, so the node term is 0
      -- and the edge term is the absolute weight gap.
      let a = undirectedGraph
              [ mkNodeKV ("a", CodeFile, "s.hs"), mkNodeKV ("b", CodeFile, "s.hs") ]
              [ mkEdgeW "a" "b" 1.0 ]
          b = undirectedGraph
              [ mkNodeKV ("a", CodeFile, "s.hs"), mkNodeKV ("b", CodeFile, "s.hs") ]
              [ mkEdgeW "a" "b" 5.0 ]
      let gF = foldLeft [a, b]
          gR = foldLeft (reverse [a, b])
      nodeDisagreement gF gR `shouldBe` Set.empty
      edgeDisagreement gF gR `shouldBe` Set.fromList [("a", "b")]
      deltaDivergence gF gR `shouldBe` 4.0

  -- ── property: |D_N| <= |K_conflict> over random families (T4) ───────────

  describe "M4 |D_N| <= |K_conflict> and D_N subset K_conflict (T4)" $ do
    it "holds for every random view family" $ property $ \(ViewFamily family) ->
      let gF    = foldLeft family
          gR    = foldLeft (reverse family)
          dn    = nodeDisagreement gF gR
          kconf = conflictingNodeIds family
      in conjoin
           [ label "D_N subset K_conflict" (Set.isSubsetOf dn kconf)
           , label "|D_N| <= |K_conflict>" (Set.size dn <= Set.size kconf)
           ]

  -- ── determinism: delta and cluster flips reproducible (T4) ──────────────

  describe "M4 divergence + cluster flips reproducible (T4)" $ do

    it "quantifyConflict is identical across repeated runs for a fixed order" $ do
      let a  = undirectedGraph
               [ mkNodeKV ("x", CodeFile, "same.hs"), mkNodeKV ("c", CodeFile, "a.hs") ]
               [ mkEdge "x" "c" Calls ]
          b  = undirectedGraph
               [ mkNodeKV ("x", CodeFile, "same.hs"), mkNodeKV ("c", DocFile, "b.hs") ]
               [ mkEdge "x" "c" Calls ]
      let gF = foldLeft [a, b]
          gR = foldLeft (reverse [a, b])
      let r1 = quantifyConflict defaultResolution gF gR
      let r2 = quantifyConflict defaultResolution gF gR
      r1 `shouldBe` r2
      cmNodeDisagreement r1 `shouldBe` cmNodeDisagreement r2
      cmClusterFlips r1 `shouldBe` cmClusterFlips r2

    it "consistent views: Δ_cluster is the measured empty set (zero non-commutativity)" $ do
      let a  = undirectedGraph
               [ mkNodeKV ("k0", CodeFile, "shared.hs"), mkNodeKV ("pA", CodeFile, "a.hs") ]
               [ mkEdge "k0" "pA" Calls ]
          b  = undirectedGraph
               [ mkNodeKV ("k0", CodeFile, "shared.hs"), mkNodeKV ("pB", DocFile, "b.md") ]
               [ mkEdge "k0" "pB" Calls ]
      let gF = foldLeft [a, b]
          gR = foldLeft (reverse [a, b])
      clusterFlips defaultResolution gF gR `shouldBe` Set.empty

    it "conflicting views: Δ_cluster is measured (not assumed) and equals real cluster-label flips" $ do
      -- Despite a payload conflict on "c", Leiden clusters the confluent adjacency,
      -- so the measured flip set (recomputed independently here) is empty.
      let a  = undirectedGraph
               [ mkNodeKV ("x", CodeFile, "same.hs"), mkNodeKV ("c", CodeFile, "a.hs") ]
               [ mkEdge "x" "c" Calls ]
          b  = undirectedGraph
               [ mkNodeKV ("x", CodeFile, "same.hs"), mkNodeKV ("c", DocFile, "b.hs") ]
               [ mkEdge "x" "c" Calls ]
      let gF = foldLeft [a, b]
          gR = foldLeft (reverse [a, b])
          commOf g = buildReverseIndex (detectCommunitiesWithResolution g defaultResolution)
          flipped = Set.filter (\n -> Map.lookup n (commOf gF) /= Map.lookup n (commOf gR))
                      (Map.keysSet (gNodes gF) `Set.union` Map.keysSet (gNodes gR))
      cmClusterFlips (quantifyConflict defaultResolution gF gR) `shouldBe` flipped
