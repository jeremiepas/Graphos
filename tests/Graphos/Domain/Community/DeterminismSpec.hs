{-# LANGUAGE OverloadedStrings #-}

-- | Determinism of @merge × cluster@ for a fixed view order (AVI-660 — M3;
-- design.md §3 T2 / fragility note, SHALL-M3).
--
-- Three properties are asserted here:
--
--   * INV-DETERMINISTIC-CLUSTER — for a fixed graph and fixed resolution,
--     @detectCommunitiesWithResolution@ yields a unique assignment vector that
--     is reproducible across repeated runs.
--
--   * ARGMAX-TIE-BREAK-CANONICAL — the move-delta argmax in @bestCommunityFor@
--     selects the smallest community index among the maximum-gain candidates,
--     independent of neighbour-traversal order. This is the concrete reading of
--     the design.md fragility note: the previous @maximumBySnd@ tie-break kept
--     the first element among ties, so it was canonical only because adjacency
--     was traversed sorted and would silently flip if adjacency were ever
--     represented unsorted (e.g. a 'HashMap'). Feeding the same multiset of
--     neighbour communities in a scrambled order must still resolve to the
--     smallest index.
--
--   * MERGE-CLUSTER-DETERMINISM — @mergeGraphsAndAnalyze@ is reproducible for a
--     fixed view order across repeated runs.
module Graphos.Domain.Community.DeterminismSpec
  ( spec )
where

import Data.List (permutations, sort)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types
import Graphos.Domain.Graph
import Graphos.Domain.Community
import Graphos.Domain.Config (defaultSemanticEdgesConfig)
import Graphos.UseCase.Merge (MergeResult(..), mergeGraphsAndAnalyze)

-- ── builders ───────────────────────────────────────────────────────────

mkNode :: Text -> Node
mkNode nid = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "test.hs"
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

-- | Undirected clique over a node list.
cliqueGraph :: [Text] -> Graph
cliqueGraph ns = buildGraph False (extractionFromLists (map mkNode ns) [mkEdge a b | a <- ns, b <- ns, a < b])

-- ── generators ─────────────────────────────────────────────────────────

-- | Random small graph over ids "n0".."n(k-1)" with a random edge subset. Used
-- for the INV-DETERMINISTIC-CLUSTER property, which must hold for arbitrary G.
genAnyGraph :: Gen Graph
genAnyGraph = do
  k <- choose (2, 8 :: Int)
  let ids = [T.pack ("n" ++ show i) | i <- [0 .. k - 1]]
      pairs = [ (a, b) | a <- ids, b <- ids, a < b ]
  keep <- mapM (\_ -> arbitrary :: Gen Bool) pairs
  let edges = [ mkEdge a b | ((a, b), b') <- zip pairs keep, b' ]
  pure (buildGraph False (extractionFromLists [mkNode nid | nid <- ids] edges))

-- | A pair of graphs with distinct id namespaces ("a*" vs "b*") so the two
-- halves remain separate communities after merging.
genMergePair :: Gen (Graph, Graph)
genMergePair = do
  ga <- genCliquePair "a"
  gb <- genCliquePair "b"
  pure (ga, gb)

genCliquePair :: Text -> Gen Graph
genCliquePair p = do
  k <- choose (3, 6 :: Int)
  let ns = [p <> T.pack (show i) | i <- [1 .. k]]
  pure (cliqueGraph ns)

-- | Distinct, sorted community indices that all have EQUAL modularity gain for
-- a node. Used to probe the argmax tie-break in isolation.
newtype TieBreakCids = TieBreakCids [Int]
  deriving Show
instance Arbitrary TieBreakCids where
  arbitrary = do
    k <- choose (2, 6 :: Int)
    base <- replicate k <$> choose (0, 100 :: Int)
    -- scale by position so every value is distinct while preserving order
    pure (TieBreakCids (sort [b * 1000 + i | (b, i) <- zip base [0 ..]]))

-- ── tie-break oracle ───────────────────────────────────────────────────

-- | @bestCommunityFor@ over a neighbour order with every community given equal
-- gain, so the canonical answer is always the smallest index.
pickAmongTies :: [Int] -> Int
pickAmongTies cids =
  let m         = 1.0
      gamma     = 1.0
      ki        = 1
      sigTotMap = IntMap.fromList [(c, 2 :: Double) | c <- cids]
      countMap  = IntMap.fromList [(c, 2 :: Int)    | c <- cids]
  in bestCommunityFor m gamma sigTotMap ki 999 countMap cids

-- ── spec ─────────────────────────────────────────────────────────────────

spec :: Spec
spec = do
  describe "INV-DETERMINISTIC-CLUSTER (SHALL-M3, design.md T2)" $ do
    it "cluster(G) is reproducible across repeated runs for a fixed graph + resolution" $
      property $ \(AnyGraph g) ->
        detectCommunitiesWithResolution g defaultResolution
          `shouldBe` detectCommunitiesWithResolution g defaultResolution

    it "two cliques joined by a bridge cluster identically on every run" $
      property $ do
        let g = buildGraph False (extractionFromLists
                  (map mkNode ["a","b","c","d","e","f","g","h"])
                  ([mkEdge "a" "b", mkEdge "b" "c", mkEdge "c" "d", mkEdge "d" "a"
                   , mkEdge "e" "f", mkEdge "f" "g", mkEdge "g" "h", mkEdge "h" "e"
                   , mkEdge "d" "e"]) )
            r1 = detectCommunities g
            r2 = detectCommunities g
            r3 = detectCommunities g
        r1 `shouldBe` r2
        r2 `shouldBe` r3

  describe "ARGMAX-TIE-BREAK-CANONICAL (SHALL-M3, design.md fragility note)" $ do
    it "resolves to the smallest community index among equal-gain neighbours for every traversal order" $
      property $ \(TieBreakCids cids) ->
         all (\perm -> pickAmongTies perm == minimum cids) (permutations cids)

    it "explicit: two equal-gain neighbours resolve to the smaller index regardless of order" $
      let c1 = 0 :: Int
          c2 = 7 :: Int
      in do
        pickAmongTies [c1, c2] `shouldBe` c1
        pickAmongTies [c2, c1] `shouldBe` c1

  describe "mergeGraphsAndAnalyze determinism (SHALL-M3)" $ do
    it "is reproducible for a fixed view order across repeated runs" $
      property $ \(MergePair (ga, gb)) ->
        let cfg = defaultSemanticEdgesConfig
            r1  = mergeGraphsAndAnalyze ga gb Normal defaultResolution cfg False
            r2  = mergeGraphsAndAnalyze ga gb Normal defaultResolution cfg False
            r3  = mergeGraphsAndAnalyze ga gb Normal defaultResolution cfg False
        in do
          mrCommunities r1 `shouldBe` mrCommunities r2
          mrCommunities r2 `shouldBe` mrCommunities r3

    it "two merged 4-cliques deterministically yield a non-empty community map" $
      let ga  = cliqueGraph ["a","b","c","d"]
          gb  = cliqueGraph ["e","f","g","h"]
          cfg = defaultSemanticEdgesConfig
          r1  = mergeGraphsAndAnalyze ga gb Normal defaultResolution cfg False
          r2  = mergeGraphsAndAnalyze ga gb Normal defaultResolution cfg False
      in do
        r1 `shouldBe` r2
        Map.size (mrCommunities r1) `shouldSatisfy` (> 0)

-- ── newtype wrappers (avoid orphan Arbitrary Graph) ────────────────────

newtype AnyGraph = AnyGraph Graph
  deriving Show
instance Arbitrary AnyGraph where
  arbitrary = AnyGraph <$> genAnyGraph

newtype MergePair = MergePair (Graph, Graph)
  deriving Show
instance Arbitrary MergePair where
  arbitrary = MergePair <$> genMergePair
