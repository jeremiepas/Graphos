{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Property tests + feasibility gate for the merged-context-graph-as-colimit
-- universal property (AVI-537 impl of AVI-536; issue AVI-645).
--
-- These checks port the claims in §6 of
-- docs/math-requirements/AVI-536-colimit-merged-context-graph.md into
-- executable Haskell assertions over 'mergeGraphs' and 'mergeGraphsAndAnalyze'.
--
-- The generators deliberately avoid an orphan 'Arbitrary Graph' instance (one
-- already exists in CollisionSpec); they expose plain 'Gen' values wrapped in
-- newtypes so the consistency/inconsistency invariants stay explicit.
module Graphos.Domain.Graph.ColimitPropertySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Short (fromText)
import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types
import Graphos.Domain.Graph
import Graphos.Domain.Community (defaultResolution, detectCommunitiesWithResolution)
import Graphos.Domain.Config.Vision (defaultSemanticEdgesConfig)
import Graphos.UseCase.Merge (MergeResult(..), mergeGraphsAndAnalyze)

-- ── builders ───────────────────────────────────────────────────────────

mkNode :: Text -> Text -> FileType -> Text -> Node
mkNode nid lbl ft src = Node
  { nodeId = nid
  , nodeLabel = fromText lbl
  , nodeFileType = ft
  , nodeSourceFile = fromText src
  , nodeCommunityId = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodeLineStart = Just 1
  , nodeLineEnd = Nothing
  , nodeKind = Nothing
  , nodeSignature = Nothing
  , nodePresentBits = 0
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

kvLabel :: KeyVal -> Text
kvLabel (l, _, _) = l

kvFileType :: KeyVal -> FileType
kvFileType (_, f, _) = f

kvSource :: KeyVal -> Text
kvSource (_, _, s) = s

-- ── value model + generators ───────────────────────────────────────────

-- | Per-key value: (label, filetype, sourceFile). Shared keys carry identical
-- values in a consistent pair; a conflicting key carries different values.
type KeyVal = (Text, FileType, Text)

canonicalValues :: [KeyVal]
canonicalValues =
  [ ("foo", CodeFile, "a.hs")
  , ("bar", DocFile, "doc.md")
  , ("baz", CodeFile, "b.hs")
  , ("qux", PaperFile, "c.tex")
  ]

baseValues :: Gen (Map.Map Text KeyVal)
baseValues = do
  let keys = ["k0", "k1", "k2", "k3"]
  Map.fromList <$> mapM (\k -> do v <- elements canonicalValues; pure (k, v)) keys

-- | Build one view over the shared key base. Keys "k0","k1","k2" are always
-- carried so the views overlap on real multi-node structure; each remaining
-- shared key is included with high probability. The previous per-key Bool gate
-- degenerated (it reduced to "always include every base key" via the trivially
-- true `any ((== k) . fst) mem`, or to a near-empty graph when the random Bool
-- was False), so associativity/old-wins properties rarely exercised overlapping
-- multi-node structure.
genView :: Map.Map Text KeyVal -> Map.Map Text KeyVal -> Gen Graph
genView base mut = do
  let keys = Map.keys base
      edgePairs = zip keys (tail keys)
      alwaysIn k = k `elem` ["k0", "k1", "k2"]
  inclMap <- Map.fromList <$>
    mapM (\k -> if alwaysIn k
                then pure (k, True)
                else do r <- frequency [(4, pure True), (1, pure False)]; pure (k, r)) keys
  let inK k = Map.findWithDefault True k inclMap
      baseVal k = Map.findWithDefault (head canonicalValues) k base
      val k = case Map.lookup k mut of Just v -> v; Nothing -> baseVal k
      nodes =
        [ mkNode k (kvLabel (val k)) (kvFileType (val k)) (kvSource (val k))
        | k <- keys, inK k ]
      edges = [ mkEdge k k' Calls | (k, k') <- edgePairs, inK k, inK k' ]
  pure (smallGraph False nodes edges)

genConsistentPair :: Gen (Graph, Graph)
genConsistentPair = do
  base <- baseValues
  g1 <- genView base Map.empty
  g2 <- genView base Map.empty
  pure (g1, g2)

genInconsistentPair :: Gen (Graph, Graph)
genInconsistentPair = do
  base <- baseValues
  let baseK0 = Map.findWithDefault (head canonicalValues) "k0" base
      altK0 = head [v | v <- canonicalValues, v /= baseK0]
      mutated = Map.insert "k0" altK0 base
  g1 <- genView base Map.empty
  g2 <- genView base mutated
  pure (g1, g2)

genTriple :: Gen (Graph, Graph, Graph)
genTriple = do
  base <- baseValues
  g1 <- genView base Map.empty
  g2 <- genView base Map.empty
  g3 <- genView base Map.empty
  pure (g1, g2, g3)

-- | A fixed consistent pair used by the deterministic (non-randomised) checks.
samplePair :: (Graph, Graph)
samplePair =
  ( smallGraph False
      [ mkNode "k0" "foo" CodeFile "a.hs"
      , mkNode "k1" "bar" DocFile "doc.md"
      , mkNode "k2" "baz" CodeFile "b.hs" ]
      [ mkEdge "k0" "k1" Calls, mkEdge "k1" "k2" Calls ]
  , smallGraph False
      [ mkNode "k0" "foo" CodeFile "a.hs"
      , mkNode "k1" "bar" DocFile "doc.md" ]
      [ mkEdge "k0" "k1" Calls ]
  )

-- ── newtype wrappers (avoid orphan Arbitrary Graph) ────────────────────

newtype ConsistentPair = ConsistentPair (Graph, Graph)
  deriving Show
instance Arbitrary ConsistentPair where
  arbitrary = ConsistentPair <$> genConsistentPair

newtype InconsistentPair = InconsistentPair (Graph, Graph)
  deriving Show
instance Arbitrary InconsistentPair where
  arbitrary = InconsistentPair <$> genInconsistentPair

newtype ColimitTriple = ColimitTriple (Graph, Graph, Graph)
  deriving Show
instance Arbitrary ColimitTriple where
  arbitrary = ColimitTriple <$> genTriple

-- ── spec ───────────────────────────────────────────────────────────────

spec :: Spec
spec = do
  describe "AC-1 cocone existence + order-independence (consistent)" $ do
    it "mergeGraphs is order-independent for consistent pairs" $
      property $ \(ConsistentPair (a, b)) ->
        mergeGraphs a b `shouldBe` mergeGraphs b a
    it "merged node set equals the union of source node sets" $
      property $ \(ConsistentPair (a, b)) ->
        Map.keysSet (gNodes (mergeGraphs a b))
          `shouldBe` (Map.keysSet (gNodes a) `Set.union` Map.keysSet (gNodes b))
    it "merged edge set equals the union of source edge sets" $
      property $ \(ConsistentPair (a, b)) ->
        Map.keysSet (gEdges (mergeGraphs a b))
          `shouldBe` (Map.keysSet (gEdges a) `Set.union` Map.keysSet (gEdges b))

  describe "AC-2 unique mediating morphism (colimit = minimal cocone)" $ do
    it "canonical cocone contains exactly the union of keys and edges (minimality ⇒ uniqueness)" $ do
      let (a, b) = samplePair
          u = mergeGraphs a b
          keysU = Map.keysSet (gNodes u)
          keysAB = Map.keysSet (gNodes a) `Set.union` Map.keysSet (gNodes b)
          edgesU = Map.keysSet (gEdges u)
          edgesAB = Map.keysSet (gEdges a) `Set.union` Map.keysSet (gEdges b)
      keysU `shouldBe` keysAB
      edgesU `shouldBe` edgesAB
    it "the two canonical cocone morphisms commute (symmetric union)" $ do
      let (a, b) = samplePair
      mergeGraphs a b `shouldBe` mergeGraphs b a

  describe "AC-3 associativity (strict, consistent triple)" $ do
    it "((A·B)·C) == (A·(B·C)) field-for-field" $
      property $ \(ColimitTriple (a, b, c)) ->
        mergeGraphs (mergeGraphs a b) c `shouldBe` mergeGraphs a (mergeGraphs b c)

  describe "AC-4 non-commutative merge × cluster interaction" $ do
    it "AC-4(a): mergeGraphs is order-dependent for inconsistent pairs" $
      property $ \(InconsistentPair (a, b)) ->
        mergeGraphs a b `shouldNotBe` mergeGraphs b a
    -- AC-4(b) pins the *community partition* (mrCommunities), not the whole
    -- MergeResult. Empirically the partition is order-INDEPENDENT even under
    -- value conflicts: mergeGraphs is commutative on node/edge key sets (Data
    -- Map union) and detectCommunitiesWithResolution (Leiden) derives community
    -- labels purely from adjacency, ignoring node label/filetype/source values
    -- (see Community.hs buildLeidenState / computeCommunityStats). The only
    -- order-dependent residue is node/edge VALUE reconciliation (mrGraph), which
    -- AC-4(a) already covers; the re-detected partition does not reorder. Assert
    -- the true property so a regression that made detection value-dependent is
    -- caught here instead of being masked by an mrGraph-only difference.
    it "AC-4(b): merge x cluster re-detection is order-independent on the community partition under conflicts" $
      property $ \(InconsistentPair (a, b)) ->
        let cfg g1 g2 = mergeGraphsAndAnalyze g1 g2 Normal defaultResolution defaultSemanticEdgesConfig False
        in mrCommunities (cfg a b) `shouldBe` mrCommunities (cfg b a)
    it "AC-4 consistent: mergeGraphsAndAnalyze is order-independent for consistent pairs" $
      property $ \(ConsistentPair (a, b)) ->
        let cfg g1 g2 = mergeGraphsAndAnalyze g1 g2 Normal defaultResolution defaultSemanticEdgesConfig False
        in cfg a b `shouldBe` cfg b a
    it "AC-4 re-detection: source community IDs are discarded on merge (not inherited)" $ do
      property $ \(ConsistentPair (a, b)) ->
        all (\n -> nodeCommunityId n == Nothing) (Map.elems (gNodes (mergeGraphs a b)))
    it "AC-4 re-detection: freshly-detected community partition covers every node" $ do
      property $ \(ConsistentPair (a, b)) ->
        let merged = mergeGraphs a b
            comms = detectCommunitiesWithResolution merged defaultResolution
            members = Set.fromList (concat (Map.elems comms))
            nodeSet = Map.keysSet (gNodes merged)
        in members `shouldSatisfy` (\m -> Set.isSubsetOf m nodeSet && Set.size m == Set.size nodeSet)

  describe "AC-5 old-wins semantics (consistent + forced)" $ do
    it "AC-5 consistent-key sub-case keeps view A's value at a shared key with differing values" $ do
      -- 'x' is shared across both views but carries DIFFERING values, so this
      -- actually disambiguates old-wins (Aval) from new-wins (Bval). The prior
      -- samplePair variant had k0 identical in both views, which could only ever
      -- test consistency preservation, never the old-wins direction.
      let a = smallGraph False
            [ mkNode "x" "Aval" CodeFile "a.hs"
            , mkNode "y" "shared" DocFile "y.md" ]
            []
          b = smallGraph False
            [ mkNode "x" "Bval" CodeFile "b.hs"
            , mkNode "y" "shared" DocFile "y.md" ]
            []
          merged = mergeGraphs a b
      nodeLabel (Map.findWithDefault (mkNode "" "" CodeFile "") "x" (gNodes merged))
        `shouldBe` fromText "Aval"
    it "AC-5 forced (both present, differing value) keeps view A's value" $ do
      let a = smallGraph False [mkNode "x" "Aval" CodeFile "a.hs"] []
          b = smallGraph False [mkNode "x" "Bval" CodeFile "b.hs"] []
          merged = mergeGraphs a b
      nodeLabel (Map.findWithDefault (mkNode "" "" CodeFile "") "x" (gNodes merged))
        `shouldBe` fromText "Aval"
    it "AC-5 exclusive-B node is included from view B" $ do
      let a = smallGraph False [mkNode "x" "Aval" CodeFile "a.hs"] []
          b = smallGraph False
            [ mkNode "x" "Bval" CodeFile "b.hs"
            , mkNode "y" "Bonly" DocFile "c.md" ]
            []
          merged = mergeGraphs a b
      Map.member "y" (gNodes merged) `shouldBe` True
      nodeLabel (Map.findWithDefault (mkNode "" "" CodeFile "") "y" (gNodes merged))
        `shouldBe` fromText "Bonly"
    it "AC-5 edge-key old-wins: mergeGraphs keeps the left operand's edge value on a conflicting edge key" $ do
      -- Both views carry the same edge key (k1,k2) with a different relation;
      -- gEdges old <> gEdges new (Core.hs:143) is left/old-wins, so the merged
      -- edge must retain view A's relation (References), not view B's (DependsOn).
      let a = smallGraph False
            [ mkNode "k0" "foo" CodeFile "a.hs"
            , mkNode "k1" "bar" CodeFile "b.hs"
            , mkNode "k2" "baz" CodeFile "c.hs" ]
            [ mkEdge "k0" "k1" Calls, mkEdge "k1" "k2" References ]
          b = smallGraph False
            [ mkNode "k0" "foo" CodeFile "a.hs"
            , mkNode "k1" "bar" CodeFile "b.hs"
            , mkNode "k2" "baz" CodeFile "c.hs" ]
            [ mkEdge "k0" "k1" Calls, mkEdge "k1" "k2" DependsOn ]
          merged = mergeGraphs a b
      edgeRelation (Map.findWithDefault (mkEdge "" "" References) ("k1", "k2") (gEdges merged))
        `shouldBe` References

  describe "Feasibility gate (Q1–Q4)" $ do
    it "Q1: force (full identity-inclusion) commutes — canonical cocone is symmetric" $ do
      let (a, b) = samplePair
      mergeGraphs a b `shouldBe` mergeGraphs b a
    it "Q2: consistency ⇒ order-independence (consistent pairs merge commutatively)" $
      property $ \(ConsistentPair (a, b)) ->
        mergeGraphs a b `shouldBe` mergeGraphs b a
    it "Q3: non-commutativity — inconsistent merges differ (merge x cluster interaction)" $
      property $ \(InconsistentPair (a, b)) ->
        mergeGraphs a b `shouldNotBe` mergeGraphs b a
    it "Q4: community re-detection — source IDs discarded AND fresh partition covers all nodes" $
      property $ \(ConsistentPair (a, b)) -> do
        let merged = mergeGraphs a b
            allCleared = all (\n -> nodeCommunityId n == Nothing) (Map.elems (gNodes merged))
            comms = detectCommunitiesWithResolution merged defaultResolution
            members = Set.fromList (concat (Map.elems comms))
            nodeSet = Map.keysSet (gNodes merged)
        allCleared `shouldBe` True
        members `shouldSatisfy` (\m -> Set.isSubsetOf m nodeSet && Set.size m == Set.size nodeSet)
