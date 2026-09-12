{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Content-confluence of 'mergeGraphs' (AVI-658 — M1, design.md §3 theorem T1).
--
-- Maps each acceptance criterion of AVI-658 to a property/golden test:
--   * AC-1  two consistent views merged in both orders yield identical
--           node/edge/adjFwd/adjBack maps and equal gHash.
--   * AC-2  merging views that disagree on 'gDirected' yields a result whose
--           directed flag is set by the pinned canonical rule (directed iff any
--           input view is directed), not by operand order.
--   * AC-3  content-confluence holds for every permutation sigma of a consistent
--           family of views.
--
-- Generators deliberately avoid an orphan 'Arbitrary Graph' instance (one
-- already exists in CollisionSpec); they expose plain 'Gen' values wrapped in
-- newtypes so the consistency / directed-disagreement invariants stay explicit.
module Graphos.Domain.Graph.MergeConfluenceSpec where

import Data.List (permutations)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Short (fromText)
import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types
import Graphos.Domain.Graph

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

smallGraph :: Bool -> [Node] -> [Edge] -> Graph
smallGraph directed nodes edges = buildGraph directed (extractionFromLists nodes edges)

-- | Canonical payloads. Shared keys carry identical values in every view, so
-- views built over the same base are consistent on node payloads by construction.
nodeValues :: [(Text, FileType, Text)]
nodeValues =
  [ ("k0", CodeFile, "a.hs")
  , ("k1", DocFile, "doc.md")
  , ("k2", CodeFile, "b.hs")
  , ("k3", PaperFile, "c.tex")
  ]

canonicalNodes :: Map Text Node
canonicalNodes = Map.fromList [(k, mkNodeKV kv) | kv@(k, _, _) <- nodeValues]

-- | Canonical (filetype, sourceFile) per key — the payload half of nodeValues.
canonicalBase :: Map Text (FileType, Text)
canonicalBase = Map.fromList [ (k, (ft, src)) | (k, ft, src) <- nodeValues ]

-- ── generators ─────────────────────────────────────────────────────────

-- | One view over the shared base: every key is present with its canonical
-- payload (consistency), a random subset of edges between consecutive keys, and
-- the supplied 'directed' flag. Key "k0" is always kept so any two views share
-- at least one node.
genView :: Bool -> Map Text (FileType, Text) -> Gen Graph
genView directed base = do
  let keys = Map.keys base
  keep <- mapM (\k -> if k == "k0" then pure True else arbitrary :: Gen Bool) keys
  let keptKeys = [ k | (k, b) <- zip keys keep, b ]
      nodes = [ Map.findWithDefault (mkNodeKV (k, CodeFile, "?")) k canonicalNodes | k <- keptKeys ]
      edges = [ mkEdge a b Calls | (a, b) <- zip keptKeys (tail keptKeys) ]
  pure (smallGraph directed nodes edges)

-- | Consistent pair: two views over the same base sharing one directed flag.
genConsistentPair :: Gen (Graph, Graph)
genConsistentPair = do
  let base = canonicalBase
  directed <- arbitrary
  a <- genView directed base
  b <- genView directed base
  pure (a, b)

-- | Directed-disagreement pair: two views over the same base with DIFFERENT
-- directed flags (they disagree on the single directed flag, so this is NOT a
-- consistent pair under M1).
genDirectedDisagreement :: Gen (Graph, Graph)
genDirectedDisagreement = do
  let base = canonicalBase
  dirA <- arbitrary
  -- force dirB /= dirA by flipping; the two views disagree on directed
  dirB <- pure (not dirA)
  a <- genView dirA base
  b <- genView dirB base
  pure (a, b)

-- | Consistent triple: three views over the same base, one shared directed flag.
genConsTriple :: Gen (Graph, Graph, Graph)
genConsTriple = do
  let base = canonicalBase
  directed <- arbitrary
  a <- genView directed base
  b <- genView directed base
  c <- genView directed base
  pure (a, b, c)

-- ── newtype wrappers (avoid orphan Arbitrary Graph) ────────────────────

newtype ConsistentPair = ConsistentPair (Graph, Graph)
  deriving (Show)
instance Arbitrary ConsistentPair where
  arbitrary = ConsistentPair <$> genConsistentPair

newtype DirectedDisagreement = DirectedDisagreement (Graph, Graph)
  deriving (Show)
instance Arbitrary DirectedDisagreement where
  arbitrary = DirectedDisagreement <$> genDirectedDisagreement

newtype ConsTriple = ConsTriple (Graph, Graph, Graph)
  deriving (Show)
instance Arbitrary ConsTriple where
  arbitrary = ConsTriple <$> genConsTriple

-- ── spec ───────────────────────────────────────────────────────────────

spec :: Spec
spec = do
  describe "AVI-658 / M1 merge content-confluence" $ do
    it "AC-1 consistent views: both operand orders yield identical nodes/edges/adjFwd/adjBack/gHash" $
      property $ \(ConsistentPair (a, b)) ->
        let left  = mergeGraphs a b
            right = mergeGraphs b a
        in conjoin
           [ label "node maps"     (gNodes left  === gNodes right)
           , label "edge maps"     (gEdges left  === gEdges right)
           , label "adjFwd maps"   (gAdjFwd left === gAdjFwd right)
           , label "adjBack maps"  (gAdjBack left === gAdjBack right)
           , label "gHash equal"   (gHash left   === gHash right)
           ]

    it "AC-1 merged node set equals the union of source node sets" $
      property $ \(ConsistentPair (a, b)) ->
        Map.keysSet (gNodes (mergeGraphs a b))
          `shouldBe` (Map.keysSet (gNodes a) `Set.union` Map.keysSet (gNodes b))

    it "AC-2 directed disagreement: gDirected pinned by canonical OR rule, not operand order" $
      property $ \(DirectedDisagreement (a, b)) ->
        let left  = mergeGraphs a b
            right = mergeGraphs b a
        in conjoin
           [ label "directed (a,b)"  (gDirected left === True)
           , label "directed (b,a)"  (gDirected right === True)
           , label "node maps"       (gNodes left === gNodes right)
           , label "edge maps"       (gEdges left === gEdges right)
           , label "adjFwd maps"     (gAdjFwd left === gAdjFwd right)
           , label "adjBack maps"    (gAdjBack left === gAdjBack right)
           , label "gHash equal"     (gHash left === gHash right)
           ]

    it "AC-2 canonical rule value: undirected||undirected = False; directed||anything = True" $ do
      let u = smallGraph False [mkNodeKV ("k0", CodeFile, "a.hs")] []
          d = smallGraph True  [mkNodeKV ("k0", CodeFile, "a.hs")] []
      (gDirected (mergeGraphs u u)) `shouldBe` False
      (gDirected (mergeGraphs d u)) `shouldBe` True
      (gDirected (mergeGraphs u d)) `shouldBe` True

    it "AC-3 content-confluence for a consistent triple folded in opposite operand orders" $
      property $ \(ConsTriple (a, b, c)) ->
        conjoin
           [ label "abc vs cba" (foldMerge [a, b, c] === foldMerge [c, b, a])
           , label "abc vs bac" (foldMerge [a, b, c] === foldMerge [b, a, c])
           , label "hash abc vs cba"
               (gHash (foldMerge [a, b, c]) === gHash (foldMerge [c, b, a]))
           ]

  describe "AVI-658 AC-3 permutations (literal S_n)" $ do
    it "all view permutations fold to the same graph and gHash" $
      property $ \(ConsTriple (a, b, c)) ->
        let vs = permutations [a, b, c]
            folds = map foldMerge vs
            base = foldMerge (head vs)
        in conjoin
           [ map gNodes folds   === replicate (length folds) (gNodes base)
           , map gEdges folds   === replicate (length folds) (gEdges base)
           , map gAdjFwd folds  === replicate (length folds) (gAdjFwd base)
           , map gAdjBack folds === replicate (length folds) (gAdjBack base)
           , map gHash folds    === replicate (length folds) (gHash base)
           ]

  describe "AVI-658 idempotency (M5, independent)" $ do
    it "mergeGraphs A A = A (nodes, edges, adjFwd/adjBack, gDirected, gHash preserved)" $
      property $ \(ConsistentPair (a, _)) ->
        let m = mergeGraphs a a
        in conjoin
           [ gNodes m === gNodes a
           , gEdges m === gEdges a
           , gAdjFwd m === gAdjFwd a
           , gAdjBack m === gAdjBack a
           , gDirected m === gDirected a
           , gHash m === gHash a
           ]

-- | Left-fold a non-empty view list; total (unreachable empty case).
foldMerge :: [Graph] -> Graph
foldMerge []     = error "foldMerge: empty view list (unreachable in MergeConfluenceSpec)"
foldMerge (x:xs) = foldr mergeGraphs x xs
