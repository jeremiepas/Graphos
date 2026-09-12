{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Cluster invariance under view permutation (AVI-659 — M2; design.md §3 T3).
--
-- SHALL-M2 requires that for consistent views,
-- @cluster(merge(V)) ~= cluster(merge(V_σ))@: the community map is invariant
-- under view permutation, with the community-label bijection being the identity.
--
-- Two families of properties are asserted here:
--
--   * INV-PARTITION — for any graph every node lands in exactly one community
--     (no node lost, no duplicate membership, every block non-empty). This is
--     the structural invariant underpinning the whole claim.
--
--   * VIEW-PERMUTATION-INVARIANCE — consistent views merged in two different
--     orders yield an identical partition with identical community labels per
--     node. Because @mergeGraphs@ is content-confluent for consistent views
--     (AVI-658 / M1) and @detectCommunitiesWithResolution@ is a deterministic
--     function of graph content (@buildLeidenState@ assigns indices via sorted
--     @Map.keys@, neighbour order via sorted @Set.toList@, Leiden loops are
--     strictly bounded, cids canonically ordered by @IntMap.toList@), the two
--     orders feed cluster() an identical graph and therefore produce an
--     identical CommunityMap — cids included, so the identity bijection holds.
--
-- The generators deliberately avoid an orphan 'Arbitrary Graph' instance: they
-- expose plain 'Gen' values wrapped in newtypes so the consistency invariant is
-- explicit (see ColimitPropertySpec / MergeConfluenceSpec for the same pattern).
module Graphos.Domain.Community.ClusterInvarianceSpec
  ( spec )
where

import Data.List (permutations)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types
import Graphos.Domain.Graph
import Graphos.Domain.Community

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

-- | A view node carrying an explicit (label, filetype, sourceFile) payload.
mkNodeKV :: Text -> (Text, FileType, Text) -> Node
mkNodeKV nid (l, ft, src) = Node
  { nodeId           = nid
  , nodeLabel        = fromText l
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

smallGraph :: Bool -> [Node] -> [Edge] -> Graph
smallGraph directed nodes edges = buildGraph directed (extractionFromLists nodes edges)

-- ── consistent-view generators ─────────────────────────────────────────

-- | Per-key payload: (label, filetype, sourceFile). Shared keys carry the same
-- payload in every view, so a family built over one base is consistent by
-- construction (the consistency invariant M2 relies on).
type KeyVal = (Text, FileType, Text)

canonicalValues :: [KeyVal]
canonicalValues =
  [ ("foo", CodeFile, "a.hs")
  , ("bar", DocFile, "doc.md")
  , ("baz", CodeFile, "b.hs")
  , ("qux", PaperFile, "c.tex")
  ]

-- | One view over the shared key base. Key "k0" is always present so any two
-- views share at least one node; the remaining keys are kept with a random bit.
-- Every view reads payloads from the same @base@, so shared keys are identical
-- across views — the family is consistent regardless of which subset each view
-- happens to contain.
genView :: Map.Map Text KeyVal -> Gen Graph
genView base = do
  let keys   = Map.keys base
      pairs  = zip keys (tail keys)
  keep <- mapM (\k -> if k == "k0" then pure True else arbitrary :: Gen Bool) keys
  let keptKeys = [ k | (k, b) <- zip keys keep, b ]
      nodes    = [ mkNodeKV k (Map.findWithDefault (head canonicalValues) k base)
                 | k <- keptKeys ]
      edges    = [ mkEdge a b | (a, b) <- pairs, a `elem` keptKeys, b `elem` keptKeys ]
  pure (smallGraph False nodes edges)

genConsistentPair :: Gen (Graph, Graph)
genConsistentPair = do
  base <- genBase
  a <- genView base
  b <- genView base
  pure (a, b)

genConsTriple :: Gen (Graph, Graph, Graph)
genConsTriple = do
  base <- genBase
  a <- genView base
  b <- genView base
  c <- genView base
  pure (a, b, c)

genBase :: Gen (Map.Map Text KeyVal)
genBase = Map.fromList <$> mapM (\k -> do v <- elements canonicalValues; pure (k, v))
                     [ "k0", "k1", "k2", "k3" ]

-- | Random small graph over ids "n0".."n(k-1)" with a random edge subset. Used
-- for the INV-PARTITION property, which must hold for arbitrary graphs.
genAnyGraph :: Gen Graph
genAnyGraph = do
  k <- choose (0, 6 :: Int)
  let ids = [T.pack ("n" ++ show i) | i <- [0 .. k - 1]]
      pairs = [ (a, b) | a <- ids, b <- ids, a < b ]
  keep <- mapM (\_ -> arbitrary :: Gen Bool) pairs
  let edges = [ mkEdge a b | ((a, b), b') <- zip pairs keep, b' ]
  pure (smallGraph False [mkNode nid | nid <- ids] edges)

-- ── newtype wrappers (avoid orphan Arbitrary Graph) ────────────────────

newtype ConsistentPair = ConsistentPair (Graph, Graph)
  deriving Show
instance Arbitrary ConsistentPair where
  arbitrary = ConsistentPair <$> genConsistentPair

newtype ConsTriple = ConsTriple (Graph, Graph, Graph)
  deriving Show
instance Arbitrary ConsTriple where
  arbitrary = ConsTriple <$> genConsTriple

newtype AnyGraph = AnyGraph Graph
  deriving Show
instance Arbitrary AnyGraph where
  arbitrary = AnyGraph <$> genAnyGraph

-- ── comparison helpers ───────────────────────────────────────────────────

-- | Community partition as a set of member sets (community ids ignored).
communityMembershipSets :: CommunityMap -> Set.Set (Set.Set NodeId)
communityMembershipSets = Set.fromList . map Set.fromList . Map.elems

-- | Per-node community label — the map the identity-cid-bijection compares.
nodeCommunityMap :: CommunityMap -> Map.Map NodeId CommunityId
nodeCommunityMap = buildReverseIndex

-- ── spec ─────────────────────────────────────────────────────────────────

spec :: Spec
spec = do
  describe "INV-PARTITION (design.md §2)" $ do
    it "every node is covered by exactly one community (no loss, no extras)" $
      property $ \(AnyGraph g) ->
        let comms  = detectCommunities g
            members = Set.fromList (concat (Map.elems comms))
            nodeSet = Map.keysSet (gNodes g)
        in members `shouldBe` nodeSet

    it "no node appears in two communities" $
      property $ \(AnyGraph g) ->
        let comms  = detectCommunities g
            totalMembers  = sum (map length (Map.elems comms))
            nodeSet = Map.keysSet (gNodes g)
        in totalMembers `shouldBe` Set.size nodeSet

    it "every community block is non-empty" $
      property $ \(AnyGraph g) ->
        all ((> 0) . length) (Map.elems (detectCommunities g))

  describe "VIEW-PERMUTATION-INVARIANCE — cluster(merge(V)) ~= cluster(merge(V_σ)) (SHALL-M2)" $ do
    it "consistent pair: both operand orders give identical CommunityMap (cids included)" $
      property $ \(ConsistentPair (a, b)) ->
        let cmAB = detectCommunitiesWithResolution (mergeGraphs a b) defaultResolution
            cmBA = detectCommunitiesWithResolution (mergeGraphs b a) defaultResolution
        in cmAB `shouldBe` cmBA

    it "consistent pair: identical per-node community labels (identity cid-bijection)" $
      property $ \(ConsistentPair (a, b)) ->
        let cmAB = detectCommunitiesWithResolution (mergeGraphs a b) defaultResolution
            cmBA = detectCommunitiesWithResolution (mergeGraphs b a) defaultResolution
        in nodeCommunityMap cmAB `shouldBe` nodeCommunityMap cmBA

    it "consistent pair: identical partition structure" $
      property $ \(ConsistentPair (a, b)) ->
        let cmAB = detectCommunitiesWithResolution (mergeGraphs a b) defaultResolution
            cmBA = detectCommunitiesWithResolution (mergeGraphs b a) defaultResolution
        in communityMembershipSets cmAB `shouldBe` communityMembershipSets cmBA

    it "consistent triple: all six view permutations cluster identically" $
      property $ \(ConsTriple (a, b, c)) ->
        let folds = map merge3 (permutations [a, b, c])
            cmOf g = detectCommunitiesWithResolution g defaultResolution
            cms   = map cmOf folds
            base  = head cms
        in map (nodeCommunityMap) cms `shouldBe` replicate (length cms) (nodeCommunityMap base)

  describe "Determinism of detectCommunitiesWithResolution given a fixed graph" $ do
    it "re-producible on the same graph (deterministic function of content)" $
      property $ \(AnyGraph g) ->
        detectCommunitiesWithResolution g defaultResolution
          `shouldBe` detectCommunitiesWithResolution g defaultResolution

-- | Left-fold a non-empty view list. Total on the unreachable empty case.
merge3 :: [Graph] -> Graph
merge3 []     = error "merge3: empty view list (unreachable)"
merge3 (x:xs) = foldr mergeGraphs x xs
