-- | Idempoteness of 'mergeGraphs' — the independent half of J2's graph/merge
-- determinism (AVI-662, M5; design.md §3 theorem T5).
--
-- Unlike the confluence / permutation properties in 'MergeConfluenceSpec' (which
-- compare *distinct* views folded in different orders), this module asserts
-- @merge(A, A) = A@ as an axiom over a single operand. The equality is exact:
-- field-by-field structural equality including @gHash@, not merely up to
-- isomorphism. Grounding: @Map.union@ and @Set.union@ are idempotent
-- (@x <> x = x@), so the node map, edge map and adjacency sets are unchanged;
-- @gDirected@ reads the left operand (which is @A@) and @gHash@ depends only on
-- content. This is independent of T1/T3, which concern permutations of distinct
-- views.
module Graphos.Domain.Graph.MergeIdempotencySpec (spec) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

nodeValues :: [(Text, FileType, Text)]
nodeValues =
  [ ("k0", CodeFile, "a.hs")
  , ("k1", DocFile, "doc.md")
  , ("k2", CodeFile, "b.hs")
  , ("k3", PaperFile, "c.tex")
  , ("k4", CodeFile, "d.hs")
  ]

nodeKeys :: [Text]
nodeKeys = [ k | (k, _, _) <- nodeValues ]

allNodes :: Map Text Node
allNodes = Map.fromList [ (k, mkNodeKV kv) | kv@(k, _, _) <- nodeValues ]

-- | Canonical graph over a fixed key set with the full consecutive edge chain.
canonicalGraph :: Bool -> Graph
canonicalGraph directed = buildGraph directed $
  extractionFromLists
    [ mkNodeKV kv | kv <- nodeValues ]
    [ mkEdge a b Calls
        | (a, b) <- zip nodeKeys (drop 1 nodeKeys) ]

-- | Small graph over a random subset of 'nodeValues' (always keeping "k0") with
-- a random subset of the consecutive edges; built via 'buildGraph' so it is a
-- canonical domain graph (non-dangling edges, empty compositions/embeddings).
genSmallGraph :: Gen Graph
genSmallGraph = do
  directed <- arbitrary
  keep <- mapM (\k -> if k == "k0" then pure True else arbitrary :: Gen Bool) nodeKeys
  let kept = [ k | (k, b) <- zip nodeKeys keep, b ]
      nodeList = [ m | k <- kept, Just m <- [Map.lookup k allNodes] ]
      edgePairs = zip kept (drop 1 kept)
  edgeList <- mapM mkPair edgePairs
  pure $ buildGraph directed $ extractionFromLists nodeList edgeList
  where
    mkPair (a, b) = do
      r <- arbitrary :: Gen Bool
      pure (if r then mkEdge a b Calls else mkEdge a b DependsOn)

newtype SmallGraph = SmallGraph Graph
  deriving (Show)
instance Arbitrary SmallGraph where
  arbitrary = SmallGraph <$> genSmallGraph

-- ── spec ───────────────────────────────────────────────────────────────

spec :: Spec
spec = do
  describe "AVI-662 / M5 merge idempoteness (independent axiom)" $ do
    it "AC: merge(A, A) equals A exactly — every field preserved" $ do
      let a = canonicalGraph False
          m = mergeGraphs a a
      gNodes m        `shouldBe` gNodes a
      gEdges m        `shouldBe` gEdges a
      gAdjFwd m       `shouldBe` gAdjFwd a
      gAdjBack m      `shouldBe` gAdjBack a
      gDirected m     `shouldBe` gDirected a
      gCompositions m `shouldBe` gCompositions a
      gHash m         `shouldBe` gHash a
      m `shouldBe` a

    it "AC: merge(A, A) = A for a directed graph too" $ do
      let a = canonicalGraph True
          m = mergeGraphs a a
      gNodes m        `shouldBe` gNodes a
      gEdges m        `shouldBe` gEdges a
      gAdjFwd m       `shouldBe` gAdjFwd a
      gAdjBack m      `shouldBe` gAdjBack a
      gDirected m     `shouldBe` gDirected a
      gCompositions m `shouldBe` gCompositions a
      gHash m         `shouldBe` gHash a
      m `shouldBe` a

    it "AC: full structural equality merge(A, A) === A (QuickCheck)" $
      property $ \(SmallGraph a) ->
        mergeGraphs a a === a

    it "AC: each field is individually idempotent under merge(A, A)" $
      property $ \(SmallGraph a) ->
        conjoin
          [ label "node map"      (gNodes (mergeGraphs a a) === gNodes a)
          , label "edge map"      (gEdges (mergeGraphs a a) === gEdges a)
          , label "adjFwd set"    (gAdjFwd (mergeGraphs a a) === gAdjFwd a)
          , label "adjBack set"   (gAdjBack (mergeGraphs a a) === gAdjBack a)
          , label "directed flag" (gDirected (mergeGraphs a a) === gDirected a)
          , label "gHash"         (gHash (mergeGraphs a a) === gHash a)
          ]

    it "AC: repeated self-merge is stable — merge(merge(A,A),A) = A" $
      property $ \(SmallGraph a) ->
        mergeGraphs (mergeGraphs a a) a === a

    it "AC: idempoteness is a single-operand property (no permutation involved)" $ do
      let a = canonicalGraph False
          b = canonicalGraph True
      mergeGraphs a a `shouldBe` a
      mergeGraphs b b `shouldBe` b
