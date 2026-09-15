{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for the four natural transformations between Graphos graph
-- representations introduced by AVI-526:
--
--   * @eta@      : FGL index-scheme switch (@toFGL ==> toSeqFGL@)
--   * @forgetful@: @Graph ==> LabeledGraph@ (drops directedness / metadata)
--   * @serialize@: @Graph ==> JSON@ (transient embeddings)
--   * @push@     : @Graph ==> Neo4j statements@ (representative selection)
--
-- Each transformation is asserted as an HUnit/QuickCheck property. The one
-- non-trivial bijection condition — that @nidToInt@ is injective iff the graph
-- has no duplicate node ids — is captured both as a property and via the
-- committed counterexample pair (see @CollisionSpec@).
module Graphos.Domain.Graph.NaturTransformationSpec where

import Test.Hspec
import Test.QuickCheck (property)
import Control.Monad (guard)
import Data.List (nub, sort)
import Data.Maybe (isJust)
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Graph.Inductive.Graph as FGL
import qualified Data.Graph.Inductive.Query.ArtPoint as ArtPoint
import qualified Data.Graph.Inductive.Query.BCC as BCC
import qualified Data.Text as T
import Data.Text.Short (fromText)

import Graphos.Domain.Graph.Core (computeGraphHash)
import Graphos.Domain.Graph (Graph(..), buildGraph, mergeGraphs, articulationPoints, dominators)
import Graphos.Domain.Types.Graph (LabeledGraph(LabeledGraph), extractionFromLists)
import Graphos.Domain.Types.Node (NodeId, Node(..), FileType(..))
import Graphos.Domain.Types.Edge (Edge(..), EdgeId(..), Relation(..), Confidence(..), relationToText)
import Graphos.Domain.Graph.FGL (toFGL, nidToInt, FGLGraph)
import Graphos.Domain.Graph.Analysis (toCachedFGL, cfgGraph, CachedFGL(..))
import Graphos.Domain.Community (detectCommunities, selectRepresentatives, buildReverseIndex, communityOf)
import Graphos.Infrastructure.Export.Neo4j (generateParameterizedStatements)
-- | Reuse the canonical 'Arbitrary Graph' instance declared in CollisionSpec.
-- The empty export list pulls in only that instance; our own fixtures live
-- locally in this module.
import Graphos.Domain.Graph.CollisionSpec ()

-- ────────────────────────────────────────────────────────────────────────────
-- Fixtures
-- ────────────────────────────────────────────────────────────────────────────

mkGraph :: Bool -> [Node] -> [Edge] -> Graph
mkGraph directed nodes edges = buildGraph directed (extractionFromLists nodes edges)

mkNodes :: [NodeId] -> [Node]
mkNodes nids = map mkNode nids

mkNode :: NodeId -> Node
mkNode nid = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "test.hs"
  , nodeLineStart    = Nothing
  , nodeLineEnd      = Nothing
  , nodeSignature    = Nothing
  , nodeCommunityId  = Nothing
  , nodeKind         = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodePresentBits  = 0
  }

nidA :: NodeId
nidA = T.pack (map chr [11, 22, 0, 3, 18, 9, 5, 17, 18, 10, 4, 3, 7])

nidB :: NodeId
nidB = T.pack (map chr [23, 13, 0, 7, 5, 18, 11, 4, 5, 20, 8, 6, 14])

mkEdge :: Relation -> NodeId -> NodeId -> Edge
mkEdge rel src tgt = Edge
  { edgeId         = EdgeId (src <> "->" <> tgt <> ":" <> relationToText rel)
  , edgeSource     = src
  , edgeTarget     = tgt
  , edgeRelation   = rel
  , edgeWeight     = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra      = Nothing
  }

-- ────────────────────────────────────────────────────────────────────────────
-- FGL index-scheme helpers (AC-1)
-- ────────────────────────────────────────────────────────────────────────────

-- | The sequential-index FGL view: @toSeqFGL g@ is what the analysis layer
-- actually consumes (the exported helper name from the requirements doc).
toSeqFGL :: Graph -> FGLGraph
toSeqFGL g = cfgGraph (toCachedFGL g)

-- | Node ids present in an FGL graph, decoded from fgl indices back to NodeIds.
fglNodeIds :: FGLGraph -> Set.Set NodeId
fglNodeIds gr = Set.fromList [nid | (_, (nid, _)) <- FGL.labNodes gr]

-- | Edge incidence as (src, tgt, relation, confidence), decoded to NodeIds.
fglIncidence :: FGLGraph -> Set.Set (NodeId, NodeId, Relation, Confidence)
fglIncidence gr =
  let nodeOf i = decodeIdx gr i
  in Set.fromList
       [ (nodeOf s, nodeOf t, rel, conf)
       | (s, t, (rel, conf, _)) <- FGL.labEdges gr ]

-- | Articulation point node ids.
fglArtPoints :: FGLGraph -> Set.Set NodeId
fglArtPoints gr = Set.fromList [nodeOf i | i <- ArtPoint.ap gr]
  where
    nodeOf i = decodeIdx gr i

-- | Biconnected components as a set of node-id sets.
fglBCC :: FGLGraph -> Set.Set (Set.Set NodeId)
fglBCC gr =
  Set.fromList [Set.fromList [nid | (_, (nid, _)) <- FGL.labNodes comp] | comp <- BCC.bcc gr]

decodeIdx :: FGLGraph -> Int -> NodeId
decodeIdx gr i =
  case [ nid | (j, (nid, _)) <- FGL.labNodes gr, j == i ] of
    (nid : _) -> nid
    []        -> error ("NaturTransformationSpec: unknown fgl index " ++ show i)

-- | The bijection condition: @nidToInt@ is injective over the given ids iff
-- no two distinct ids share an fgl index.
nidInjective :: [NodeId] -> Bool
nidInjective nids =
  let hashed = map nidToInt nids
  in length hashed == length (nub hashed)

-- ────────────────────────────────────────────────────────────────────────────
-- Forgetful functor U : Graph ==> LabeledGraph (AC-2)
-- ────────────────────────────────────────────────────────────────────────────

forgetfulU :: Graph -> LabeledGraph
forgetfulU g =
  LabeledGraph
    (gNodes g)
    (Map.fromList [(edgeId e, e) | e <- Map.elems (gEdges g)])
    (gAdjFwd g)
    (gAdjBack g)

lgNodes :: LabeledGraph -> Map.Map NodeId Node
lgNodes (LabeledGraph n _ _ _) = n

lgEdges :: LabeledGraph -> Map.Map EdgeId Edge
lgEdges (LabeledGraph _ e _ _) = e

lgAdjFwd :: LabeledGraph -> Map.Map NodeId (Set.Set NodeId)
lgAdjFwd (LabeledGraph _ _ f _) = f

lgAdjBack :: LabeledGraph -> Map.Map NodeId (Set.Set NodeId)
lgAdjBack (LabeledGraph _ _ _ b) = b

-- | Edge content sorted so that key-ordering differences don't affect equality.
edgeContent :: Map.Map EdgeId Edge -> [(NodeId, NodeId, Relation, Double, Confidence)]
edgeContent m =
   sort [(edgeSource e, edgeTarget e, edgeRelation e, edgeWeight e, edgeConfidence e)
       | (_, e) <- Map.toList m]

-- | Edge content for a Graph whose edges are keyed by (@src@, @tgt@).
edgeContentG :: Map.Map (NodeId, NodeId) Edge -> [(NodeId, NodeId, Relation, Double, Confidence)]
edgeContentG m =
   sort [(edgeSource e, edgeTarget e, edgeRelation e, edgeWeight e, edgeConfidence e)
       | (_, e) <- Map.toList m]

-- | Structural hash of a LabeledGraph, recomputed after re-keying edges back to
-- (@src@, @tgt@) tuples — the same scheme @computeGraphHash@ uses for 'Graph'.
labeledHash :: LabeledGraph -> Text
labeledHash lg =
  computeGraphHash (lgNodes lg)
    (Map.fromList [((edgeSource e, edgeTarget e), e) | (_, e) <- Map.toList (lgEdges lg)])

-- ────────────────────────────────────────────────────────────────────────────
-- Node relabeling (used by AC-1 dominator invariance + AC-2)
-- ────────────────────────────────────────────────────────────────────────────

-- | Relabel every node id via an injective function, preserving structure.
-- Returns 'Nothing' when the map is not injective (which would change the graph).
relabelNodes :: (NodeId -> NodeId) -> Graph -> Maybe Graph
relabelNodes f g = do
  let oldIds = Map.keys (gNodes g)
      newIds = map f oldIds
  guard (length newIds == length (nub newIds))
  let newNodes = map (\n -> n { nodeId = f (nodeId n) }) (Map.elems (gNodes g))
      newEdges =
        map (\e -> e
              { edgeId       = EdgeId (f (edgeSource e) <> "->" <> f (edgeTarget e) <> ":" <> relationToText (edgeRelation e))
              , edgeSource   = f (edgeSource e)
              , edgeTarget   = f (edgeTarget e)
              })
            (Map.elems (gEdges g))
  pure $ buildGraph (gDirected g) (extractionFromLists newNodes newEdges)

-- | Set of nodes that have a well-defined immediate dominator.
dominatorSet :: Graph -> NodeId -> Set.Set NodeId
dominatorSet g start = Map.keysSet (Map.filter isJust (dominators g start))

-- ────────────────────────────────────────────────────────────────────────────
-- Spec
-- ────────────────────────────────────────────────────────────────────────────

spec :: Spec
spec = do
  describe "AC-1 eta : toFGL ==> toSeqFGL" $ do
    describe "structure-preserving iso when nidToInt is injective" $ do
      it "node id sets agree between hashed and sequential FGL" $ property $ \g ->
        let g'  = g { gDirected = True }
            fgl = toFGL (gNodes g') (gEdges g')
            seqFGL = toSeqFGL g'
            nids = Map.keys (gNodes g')
        in if nidInjective nids then fglNodeIds fgl == fglNodeIds seqFGL else True

      it "edge incidence sets agree between hashed and sequential FGL" $ property $ \g ->
        let g'  = g { gDirected = True }
            fgl = toFGL (gNodes g') (gEdges g')
            seqFGL = toSeqFGL g'
            nids = Map.keys (gNodes g')
         in if nidInjective nids then fglIncidence fgl == fglIncidence seqFGL else True

    describe "bijection condition: eta_G iso iff nidToInt injective (counterexample)" $ do
      it "toFGL merges the colliding nodes while toSeqFGL keeps them" $ do
        let ext = extractionFromLists
                    [mkNode nidA, mkNode nidB, mkNode "c"]
                    [mkEdge Calls nidA "c", mkEdge Calls nidB "c"]
            g   = buildGraph False ext
            fgl = toFGL (gNodes g) (gEdges g)
            seqFGL = toSeqFGL g
        Set.size (fglNodeIds fgl) `shouldBe` 2   -- nidA, nidB collapse to one
        Set.size (fglNodeIds seqFGL) `shouldBe` 3    -- kept distinct

      it "nidA, nidB are a counterexample generator: distinct yet colliding under nidToInt" $ do
        nidA `shouldNotBe` nidB
        (nidToInt nidA) `shouldBe` (nidToInt nidB)

      it "nidToInt is injective on decimal ids 0..200" $ do
        let sample = [T.pack (show k) | k <- [0 .. 200 :: Int]]
        nidInjective sample `shouldSatisfy` (== True)

  describe "AC-2 forgetful U : Graph ==> LabeledGraph" $ do
    it "preserves nodes, edges by content, and adjacency" $ property $ \g ->
      let u = forgetfulU g
      in (lgNodes u == gNodes g)
           && (edgeContent (lgEdges u) == edgeContentG (gEdges g))
          && (lgAdjFwd u == gAdjFwd g)
          && (lgAdjBack u == gAdjBack g)

    it "preserves the structural hash through the forgetful image" $ property $ \g ->
      labeledHash (forgetfulU g) `shouldBe` gHash g

    it "drops directedness: forgetful image is identical when only gDirected differs" $ do
      let base = mkGraph True  [mkNode "a", mkNode "b"] [mkEdge Calls "a" "b"]
          same = forgetfulU (base { gDirected = False })
      shouldBe (forgetfulU base) same

  describe "AC-3 serialization : Graph ==> JSON" $ do
    it "round-trips nodes, edges, adjacency, directedness, and hash" $ property $ \g ->
      case Aeson.decodeStrict (BSL.toStrict (Aeson.encode g)) of
        Just g' -> (gNodes g' == gNodes g)
                && (gEdges g' == gEdges g)
                && (gAdjFwd g' == gAdjFwd g)
                && (gAdjBack g' == gAdjBack g)
                && (gDirected g' == gDirected g)
                && (gHash g' == gHash g)
        Nothing -> False

    it "drops transient embeddings on decode" $ do
      let g = mkGraph True [mkNode "a"] []
          g' = case Aeson.decodeStrict (BSL.toStrict (Aeson.encode g)) of Just x -> x; Nothing -> error "decode failed"
      gEmbeddings g' `shouldBe` Nothing

    it "may carry an embeddings path across round-trip" $ do
      let g = (mkGraph True [mkNode "a"] []) { gEmbeddingsPath = Just "/tmp/embeds.json" }
          g' = case Aeson.decodeStrict (BSL.toStrict (Aeson.encode g)) of Just x -> x; Nothing -> error "decode failed"
      gEmbeddingsPath g' `shouldBe` Just "/tmp/embeds.json"

  describe "AC-4 push : Graph ==> Neo4j statements" $ do
    it "representatives are members of their community" $ property $ \g ->
      let cm   = detectCommunities g
          artPts = articulationPoints g
          reps   = selectRepresentatives g cm 3 artPts
       in all (\(cid, mems) -> case Map.lookup cid cm of
                               Just members -> all (`elem` members) mems
                               Nothing      -> True)
               (Map.toList reps)

    it "retains every articulation point in its community's representatives" $ property $ \g ->
      let cm       = detectCommunities g
          reverseIdx = buildReverseIndex cm
       in if Map.null (gNodes g) then True
             else let artPts   = articulationPoints g
                      reps     = selectRepresentatives g cm 3 artPts
                   in all (\nid -> case communityOf nid reverseIdx of
                                      Just cid -> (nid `elem`) (Map.findWithDefault [] cid reps)
                                      Nothing  -> True)
                          artPts

    it "bounds representatives per community by topN plus its articulation points" $ property $ \g ->
      let cm       = detectCommunities g
          artPts   = articulationPoints g
          reps     = selectRepresentatives g cm 3 artPts
          reverseIdx = buildReverseIndex cm
          artCount = Map.fromListWith (+)
             [(case communityOf nid reverseIdx of Just c -> c; Nothing -> -1, 1 :: Int) | nid <- artPts]
       in all (\(cid, mems) ->
                length mems <= 3 + Map.findWithDefault 0 cid artCount)
              (Map.toList reps)

    it "generateParameterizedStatements commutes with graph merge" $ property $ \g ->
      let g2   = mkGraph True [mkNode "disjoint-extra"] []
          merged = mergeGraphs g g2
          s1     = Set.fromList (generateParameterizedStatements g)
          s2     = Set.fromList (generateParameterizedStatements g2)
          sM     = Set.fromList (generateParameterizedStatements merged)
      in sM `shouldBe` Set.union s1 s2

  describe "AC-5 integrator : naturality holds by construction" $ do
    it "eta naturality: toFGL and toSeqFGL agree on a multi-node graph" $ do
      let g   = mkGraph True (mkNodes ["0", "1", "2", "3"])
                    [mkEdge Calls "0" "1"
                    , mkEdge Calls "1" "2"
                    , mkEdge References "0" "2"
                    , mkEdge Imports "2" "3"]
          fgl = toFGL (gNodes g) (gEdges g)
          seqFGL = toSeqFGL g
      fglNodeIds fgl `shouldBe` fglNodeIds seqFGL
      fglIncidence fgl `shouldBe` fglIncidence seqFGL
      fglArtPoints fgl `shouldBe` fglArtPoints seqFGL
      fglBCC fgl `shouldBe` fglBCC seqFGL

    it "forgetful U preserves the structural hash" $ do
      let g = mkGraph True (mkNodes ["0", "1", "2"])
                    [mkEdge Calls "0" "1", mkEdge Calls "1" "2"]
      labeledHash (forgetfulU g) `shouldBe` gHash g

    it "serialization round-trips structure and drops embeddings" $ do
      let g = mkGraph True (mkNodes ["0", "1"]) [mkEdge Calls "0" "1"]
          decoded = case Aeson.decodeStrict (BSL.toStrict (Aeson.encode g)) of Just x -> x; Nothing -> error "decode failed"
      (gNodes decoded == gNodes g) && (gEdges decoded == gEdges g)
        `shouldSatisfy` (== True)
      gEmbeddings decoded `shouldBe` Nothing

    it "push representative selection is well-formed" $
      let g     = mkGraph True (mkNodes ["0", "1", "2", "3", "4"])
                    [mkEdge Calls "0" "1"
                    , mkEdge Calls "1" "2"
                    , mkEdge References "2" "3"
                    , mkEdge Imports "3" "4"]
          cm      = detectCommunities g
          artPts  = articulationPoints g
          reps    = selectRepresentatives g cm 3 artPts
          mem     = fglNodeIds (toSeqFGL g)
          good    = and
              [ all (\cid -> all (`elem` (Map.findWithDefault [] cid cm)) (Map.findWithDefault [] cid reps)) (Map.keys reps)
              , all (\cid -> all (`elem` mem) (Map.findWithDefault [] cid reps)) (Map.keys reps) ]
       in good `shouldSatisfy` (== True)
