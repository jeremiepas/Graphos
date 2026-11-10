{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphos.Domain.Graph.UQIOrderSpec where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph(..), buildGraph, mergeGraphs, gNodes, gEdges, gAdjFwd, gAdjBack, gHash, degree)
import Data.Text.Short (fromText)
import Graphos.Domain.Graph.CollisionSpec ()

mkNodeKV :: (Text, Text) -> Node
mkNodeKV (k, lbl) = Node
  { nodeId = k, nodeLabel = fromText lbl, nodeFileType = CodeFile, nodeSourceFile = fromText "t.hs"
  , nodeSource = Nothing, nodeCommunityId = Nothing, nodeDegree = Nothing, nodeIsBridge = Nothing
  , nodeExtra = Nothing, nodeLineStart = Just 1, nodeLineEnd = Nothing, nodeKind = Nothing
  , nodeSignature = Nothing, nodePresentBits = 0 }

mkEdge :: Text -> Text -> Relation -> Edge
mkEdge s t r = Edge (EdgeId (s <> "->" <> t)) s t r 1.0 (Confidence 1.0) Nothing

smallGraph :: Bool -> [Node] -> [Edge] -> Graph
smallGraph d ns es = buildGraph d (extractionFromLists ns es)

nodeVals :: [(Text, Text)]
nodeVals = [ ("k0","foo"), ("k1","bar"), ("k2","baz"), ("k3","qux")]

canonicalNodes :: Map.Map Text Node
canonicalNodes = Map.fromList [(k, mkNodeKV (k, lbl)) | (k,lbl) <- nodeVals]

canonicalBase :: Map.Map Text Text
canonicalBase = Map.fromList nodeVals

genConsistentView :: Bool -> Gen Graph
genConsistentView directed = do
  let keys = map fst nodeVals
  keep <- mapM (\k -> if k == "k0" then pure True else arbitrary :: Gen Bool) keys
  let kept = [ k | (k, b) <- zip keys keep, b ]
      nodes = [ Map.findWithDefault (mkNodeKV ("k0","foo")) k canonicalNodes | k <- kept ]
      edges = [ mkEdge a b Calls | (a, b) <- zip kept (drop 1 kept) ]
  pure (smallGraph directed nodes edges)

genConsistentPair :: Gen (Graph, Graph)
genConsistentPair = do
  directed <- arbitrary
  a <- genConsistentView directed
  b <- genConsistentView directed
  pure (a, b)

newtype ConsistentPair = ConsistentPair (Graph, Graph) deriving Show
instance Arbitrary ConsistentPair where
  arbitrary = ConsistentPair <$> genConsistentPair

spec :: Spec
spec = do
  describe "UQI-8 order-independence on consistent diagrams" $ do
    it "mergeGraphs is equal in both orders" $ property $
      \(ConsistentPair (a, b)) -> mergeGraphs a b == mergeGraphs b a
    it "node/edge/adjFwd/adjBack/gHash all agree across orders" $ property $
      \(ConsistentPair (a, b)) ->
        let l = mergeGraphs a b; r = mergeGraphs b a
        in  gNodes l == gNodes r && gEdges l == gEdges r
            && gAdjFwd l == gAdjFwd r && gAdjBack l == gAdjBack r
            && gHash l == gHash r
    it "degree map agrees across orders" $ property $
      \(ConsistentPair (a, b)) ->
        let l = mergeGraphs a b; r = mergeGraphs b a
            ks = Set.toList (Map.keysSet (gNodes l))
        in and [ degree l k == degree r k | k <- ks ]

  describe "C3 LG-C3 fiber-product identity" $ do
    it "Map.size (gNodes M) == |keys A union keys B|" $ property $
      \(ConsistentPair (a, b)) ->
        let m = mergeGraphs a b
            ua = Set.union (Map.keysSet (gNodes a)) (Map.keysSet (gNodes b))
        in Map.size (gNodes m) == Set.size ua
    it "dedup identity |A|+|B|-n for n agreeing keys" $ property $
      \(ConsistentPair (a, b)) ->
        let m = mergeGraphs a b
            nOverlap = Set.size (Set.intersection (Map.keysSet (gNodes a)) (Map.keysSet (gNodes b)))
            expected = Map.size (gNodes a) + Map.size (gNodes b) - nOverlap
        in Map.size (gNodes m) == expected
    it "fiber-product shared keys agree on payload" $ property $
      \(ConsistentPair (a, b)) ->
        let m = mergeGraphs a b
            overlap = Set.intersection (Map.keysSet (gNodes a)) (Map.keysSet (gNodes b))
        in and [ Map.lookup k (gNodes m) == Map.lookup k (gNodes a)
                && Map.lookup k (gNodes m) == Map.lookup k (gNodes b)
               | k <- Set.toList overlap ]

  describe "LG-C2 scope: identical payloads" $ do
    it "shared identical-payload key agrees in both orders" $ property $
      \(ConsistentPair (a, b)) ->
        let m1 = mergeGraphs a b; m2 = mergeGraphs b a
            overlap = Set.intersection (Map.keysSet (gNodes a)) (Map.keysSet (gNodes b))
        in and [ Map.lookup k (gNodes m1) == Map.lookup k (gNodes m2) | k <- Set.toList overlap ]
    it "Arbitrary graphs: identical payload survivors are kept (left-biased agreeing)" $ property $
      \(a :: Graph, b :: Graph) ->
        let overlap = Set.intersection (Map.keysSet (gNodes a)) (Map.keysSet (gNodes b))
            agreeing = [ k | k <- Set.toList overlap, Map.lookup k (gNodes a) == Map.lookup k (gNodes b) ]
            m = mergeGraphs a b
        in and [ Map.lookup k (gNodes m) == Map.lookup k (gNodes a) | k <- agreeing ]

  describe "UQI-8 conflicting merge old-wins (Delta-bounded)" $ do
    it "old-wins: shared conflict key retains left operand payload" $ do
      let a = smallGraph False [mkNodeKV ("x","Aval")] [ ]
          b = smallGraph False [mkNodeKV ("x","Bval")] [ ]
          m = mergeGraphs a b
      case Map.lookup "x" (gNodes m) of
        Just n -> nodeLabel n `shouldBe` fromText "Aval"
        Nothing -> expectationFailure "missing x"
