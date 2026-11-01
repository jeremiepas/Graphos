{-# LANGUAGE OverloadedStrings #-}
-- | Fixture parity + property tests for intent-graph-verification unit 4
-- (Domain/IntentCheck.hs + Domain/Impact.hs against the Lean toy instance in
-- openspec/changes/intent-graph-verification/lean/IntentGraph.lean §5).
module Graphos.Domain.IntentGraphVerificationSpec (spec) where

import Test.Hspec
import Test.QuickCheck (Arbitrary(..), Gen, property, sized, choose, elements, vectorOf, forAll)

import Data.Aeson (object, (.=))
import Data.List (sort, nub)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.Query.BFS (bfs)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Graphos.Domain.Graph.Core (Graph, buildGraph)
import Graphos.Domain.Types.Node (Node (..), computePresentBits, FileType (..))
import Graphos.Domain.Types.Edge (Edge (..), Relation (..), EdgeId (..), Confidence (..), relationToText)
import Graphos.Domain.Types.Graph (extractionFromLists)
import qualified Graphos.Domain.IntentCheck as IC
import qualified Graphos.Domain.Impact as IM

--------------------------------------------------------------------------------
-- Toy fixtures (Lean §5: G₀, G₁, G₂)
--------------------------------------------------------------------------------

-- | Build a Node with the Lean toy node's fields; @body@ goes under the
-- @body_hash@ key of @nodeExtra@ (the body/signature split of the model).
specNode :: Text -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> Node
specNode nid mKind label mSig mBody = withBits Node
  { nodeId           = nid
  , nodeLabel        = fromText label
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText label
  , nodeSource       = Nothing
  , nodeLineStart    = Nothing
  , nodeLineEnd      = Nothing
  , nodeSignature    = fromText <$> mSig
  , nodeCommunityId  = Nothing
  , nodeKind         = fromText <$> mKind
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = bodyExtra mBody
  , nodePresentBits  = 0
  }
  where
    withBits n = n { nodePresentBits = computePresentBits n }
    bodyExtra Nothing  = Nothing
    bodyExtra (Just b) = Just (object [ "body_hash" .= b ])

mkEdge :: Text -> Relation -> Text -> Edge
mkEdge s r t = Edge
  { edgeId         = EdgeId (s <> "->" <> relationToText r <> "->" <> t)
  , edgeSource     = s
  , edgeTarget     = t
  , edgeRelation   = r
  , edgeWeight     = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra      = Nothing
  }

graphOf :: [Node] -> [Edge] -> Graph
graphOf ns es = buildGraph False (extractionFromLists ns es)

validatorNode :: Text -> Node
validatorNode body = specNode "2" (Just "validator") "validateOrder" (Just "(Order) -> Result") (Just body)

n1 :: Node
n1 = specNode "1" (Just "handler") "POST /orders" (Just "(Req) -> Res") Nothing

n3, n4, n5, n6, n7 :: Node
n3 = specNode "3" (Just "test") "orders.spec" Nothing Nothing
n4 = specNode "4" (Just "schemaField") "orders.total" Nothing Nothing
n5 = specNode "5" (Just "contractField") "Order.total" Nothing Nothing
n6 = specNode "6" (Just "schemaField") "orders.currency" Nothing Nothing
n7 = specNode "7" (Just "contractField") "Order.currency" Nothing Nothing

n9Legacy :: Node
n9Legacy = specNode "9" (Just "module") "legacy/old" Nothing Nothing

g0Edges :: [Edge]
g0Edges = [ mkEdge "1" Calls "2", mkEdge "3" Tests "1", mkEdge "4" Maps "5" ]

-- | Lean G₀: handler, validator, test, schema field + its contract field.
g0 :: Graph
g0 = graphOf [n1, validatorNode "h1", n3, n4, n5] g0Edges

-- | Lean G₁ (change A): validator body h1→h2 (signature stable) + schema
-- field orders.currency added without its contract field. The typical omission.
g1 :: Graph
g1 = graphOf [n1, validatorNode "h2", n3, n4, n5, n6] g0Edges

-- | Lean G₂ (change B): same omission fixed.
g2 :: Graph
g2 = graphOf [n1, validatorNode "h2", n3, n4, n5, n6, n7]
             (g0Edges ++ [ mkEdge "6" Maps "7" ])

-- | Signature-change variant of G₁ (NOT body-only): the validator's
-- signature moves — the case stableSignature must refuse to discharge.
g1sig :: Graph
g1sig = graphOf [n1, specNode "2" (Just "validator") "validateOrder" (Just "(Order) -> Verified") (Just "h1"), n3, n4, n5] g0Edges

-- | G₀ plus a legacy module node (for the absent pattern).
gLegacy :: Graph
gLegacy = graphOf [n1, validatorNode "h1", n3, n4, n5, n9Legacy] g0Edges

-- | Lean Φ: the four validated specs, named for the admissibility gate.
phi :: IC.SpecDocument
phi = IC.SpecDocument
  { IC.docName = Just "toy"
  , IC.docSpecs =
      [ (Just "specHandlerValidates", IC.Reaches "handler" "POST /orders" Calls "validator")
      , (Just "specHandlersTested", IC.Covered "handler" Tests "test")
      , (Just "specSchemaContract", IC.Parallel "schemaField" "contractField" Maps)
      , (Just "specNoLegacy", IC.Absent "module" "legacy/" [])
      ]
  }

newSpecName :: Maybe Text
newSpecName = Just "specHandlerValidates"

--------------------------------------------------------------------------------
-- Property domain: small random graphs, mirrored into fgl
--------------------------------------------------------------------------------

-- | Property-domain graph: node ids are 1..n (Text form "n⟨i⟩", single digit
-- so the Text and Int orders coincide), node kinds are arbitrary, edges carry
-- one of the five relations relevant to the model (incl. non-propagating
-- @tests@).
data PGraph = PGraph
  { pgNodes :: [(Int, Text)]
  , pgEdges :: [(Int, Int, Relation)]
  } deriving (Eq, Show)

instance Arbitrary PGraph where
  arbitrary = sized genPGraph

genPGraph :: Int -> Gen PGraph
genPGraph size = do
  n <- choose (1, 1 + max 0 (min size 8))
  kinds <- vectorOf n (elements ["handler", "validator", "test", "schemaField"])
  let ids = [1 .. n]
      pairs = [(a, b) | a <- ids, b <- ids, a /= b]
  rels <- vectorOf (length pairs) (elements [Calls, Tests, References, Implements, Maps])
  keep <- vectorOf (length pairs) (arbitrary :: Gen Bool)
  let es = [ (a, b, r) | ((a, b), r, True) <- zip3 pairs rels keep ]
  pure (PGraph (zip ids kinds) es)

nT :: Int -> Text
nT = T.pack . show

pgGraph :: PGraph -> Graph
pgGraph pg = graphOf
  [ specNode (nT i) (Just k) (nT i) Nothing Nothing | (i, k) <- pgNodes pg ]
  [ mkEdge (nT a) r (nT b) | (a, b, r) <- pgEdges pg ]

pgIdsOfKind :: PGraph -> Text -> [Int]
pgIdsOfKind pg k = [i | (i, kk) <- pgNodes pg, kk == k]

-- | fgl projection of the property graph restricted to one relation's edges.
pgGr :: PGraph -> Relation -> Gr Int ()
pgGr pg rel = mkGraph
  [ (i, i) | (i, _) <- pgNodes pg ]
  [ (a, b, ()) | (a, b, r) <- pgEdges pg, r == rel ]

-- | fgl closure from one node (single-digit ids, so sort order == Text order).
fglClosure :: PGraph -> Relation -> Int -> [Int]
fglClosure pg rel u = sort (nub (bfs u (pgGr pg rel)))

-- | reachable (reaches' primitive) agrees with fgl reachability per label.
propReach :: PGraph -> Bool
propReach pg = all perLabel [Calls, Tests, References, Implements, Maps]
  where
    g = pgGraph pg
    perLabel rel = all (\u -> sort (Set.toList (IC.reachable g rel (nT u))) == map nT (fglClosure pg rel u))
                       (map fst (pgNodes pg))

-- | covered verdicts agree with the fgl closure (k nodes reached by k' nodes).
expectedCovered :: PGraph -> Relation -> Text -> Text -> [Int]
expectedCovered pg rel k r' =
  [ i | i <- pgIdsOfKind pg k
      , not (any (\w -> i `elem` fglClosure pg rel w) (pgIdsOfKind pg r')) ]

expectedCoveredVerdict :: PGraph -> Relation -> Text -> Text -> IC.Verdict
expectedCoveredVerdict pg rel k r' = case expectedCovered pg rel k r' of
  []  -> IC.Satisfied []
  bad -> IC.Violated (map nT bad)

propCovered :: PGraph -> Bool
propCovered pg = all parity
  [ ("validator", Tests, "handler")
  , ("handler", Calls, "validator")
  , ("schemaField", Maps, "contractField")
  ]
  where
    g = pgGraph pg
    parity (k, rel, r') = IC.evaluateSpec g (IC.Covered k rel r') == expectedCoveredVerdict pg rel k r'

-- | reaches verdicts agree with the fgl closure (named node reaching kind k').
expectedReaches :: PGraph -> Relation -> Int -> Text -> IC.Verdict
expectedReaches pg rel u tk =
  let us = [ i | (i, kk) <- pgNodes pg, kk == "handler", nT i == nT u ]
      cl = fglClosure pg rel u
      bad = [ i | i <- us, not (any (`elem` cl) (pgIdsOfKind pg tk)) ]
  in if null bad then IC.Satisfied [] else IC.Violated (map nT bad)

propReaches :: PGraph -> Bool
propReaches pg = all parity
  [ (Calls, "validator")
  , (Tests, "test")
  , (Implements, "schemaField")
  ]
  where
    g = pgGraph pg
    parity (rel, tk) = all
      (\u -> IC.evaluateSpec g (IC.Reaches "handler" (nT u) rel tk) == expectedReaches pg rel u tk)
      (pgIdsOfKind pg "handler")

-- | impact is exactly the fgl reverse-dependency closure of touched (over
-- 'depLabels', tests excluded); frontier = impact \ touched.
propImpact :: PGraph -> [Bool] -> Bool
propImpact pg tSel =
  let ids = map fst (pgNodes pg)
      touchedIds = [i | (i, True) <- zip ids tSel]
      t = map nT touchedIds
      g = pgGraph pg
      revGr :: Gr Int ()
      revGr = mkGraph
        [ (i, i) | i <- ids ]
        [ (b, a, ()) | (a, b, r) <- pgEdges pg, r `elem` IM.depLabels ]
      closed = sort (nub (concat [bfs i revGr | i <- touchedIds]))
  in IM.impact g t == map nT closed
     && IM.frontier g t == map nT (sort [i | i <- closed, i `notElem` touchedIds])

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Graphos.Domain.IntentCheck / Impact (unit 4, Lean toy parity)" $ do
  describe "IntentCheck — task 4.1 (four decidable patterns + and/not)" $ do
    it "reaches: the orders handler reaches a validator on G0" $
      IC.evaluateSpec g0 (IC.Reaches "handler" "POST /orders" Calls "validator") `shouldBe` IC.Satisfied []

    it "covered: the handler is exercised by a test on G0" $
      IC.evaluateSpec g0 (IC.Covered "handler" Tests "test") `shouldBe` IC.Satisfied []

    it "absent: no legacy module on G0" $
      IC.evaluateSpec g0 (IC.Absent "module" "legacy/" []) `shouldBe` IC.Satisfied []

    it "parallel: schema <-> contract on G0" $
      IC.evaluateSpec g0 (IC.Parallel "schemaField" "contractField" Maps) `shouldBe` IC.Satisfied []

    it "sat G1 specSchemaContract is FALSE — the omission is detected, witness [\"6\"]" $
      IC.evaluateSpec g1 (IC.Parallel "schemaField" "contractField" Maps) `shouldBe` IC.Violated ["6"]

    it "sat G2 specSchemaContract is TRUE — the fix is accepted" $
      IC.evaluateSpec g2 (IC.Parallel "schemaField" "contractField" Maps) `shouldBe` IC.Satisfied []

    it "a reaches spec with no matching named node is violated (empty existential)" $
      IC.evaluateSpec g0 (IC.Reaches "handler" "GET /missing" Calls "validator")
        `shouldBe` IC.Violated ["GET /missing"]

    it "absent flags violations and honours the whitelist" $ do
      IC.evaluateSpec gLegacy (IC.Absent "module" "legacy/" []) `shouldBe` IC.Violated ["9"]
      IC.evaluateSpec gLegacy (IC.Absent "module" "legacy/" ["legacy/old"]) `shouldBe` IC.Satisfied []

    it "and composes the four toy specs: holds on G0, fails on G1" $ do
      let conj = IC.And
            [ IC.Reaches "handler" "POST /orders" Calls "validator"
            , IC.Covered "handler" Tests "test"
            , IC.Parallel "schemaField" "contractField" Maps
            , IC.Absent "module" "legacy/" []
            ]
      IC.evaluateSpec g0 conj `shouldBe` IC.Satisfied []
      IC.evaluateSpec g1 conj `shouldBe` IC.Violated ["6"]

    it "not is strict negation (Lean ¬ sat): false on G0, true on G1" $ do
      let paranoia = IC.Parallel "schemaField" "contractField" Maps
      IC.holds (IC.evaluateSpec g0 (IC.Not paranoia)) `shouldBe` False
      IC.holds (IC.evaluateSpec g1 (IC.Not paranoia)) `shouldBe` True

    it "evaluate / allHolds agree with the per-spec verdicts" $ do
      IC.allHolds g0 phi `shouldBe` True
      IC.allHolds g1 phi `shouldBe` False

    it "parses an intent-spec JSON document" $
      IC.parseSpecDocument
        "{\"name\":\"toy\",\"specs\":[{\"kind\":\"reaches\",\"fromKind\":\"handler\",\"fromName\":\"orders\",\"rel\":\"calls\",\"toKind\":\"validator\"}]}"
        `shouldBe` Right (IC.SpecDocument (Just "toy") [(Nothing, IC.Reaches "handler" "orders" Calls "validator")])

  describe "Impact — task 4.2 (Lean toy change/impact analysis)" $ do
    let d = IM.Discharge { IM.acks = [], IM.rules = [IM.stableSignature] }
        cA = IM.Change { IM.before = g0, IM.after = g1 }
        cB = IM.Change { IM.before = g0, IM.after = g2 }
        cSig = IM.Change { IM.before = g0, IM.after = g1sig }

    it "touched cA = [6, 2] (added schema field; body-changed validator)" $
      IM.touched cA `shouldBe` ["6", "2"]

    it "impact = [1, 2, 6]: closed under reverse dependency" $
      IM.impact g0 (IM.touched cA) `shouldBe` ["1", "2", "6"]

    it "frontier = impact \\ touched = [1] (the handler)" $
      IM.frontier g0 (IM.touched cA) `shouldBe` ["1"]

    it "openObligations cA d = [] — stableSignature discharges the handler" $
      IM.openObligations cA d `shouldBe` []

    it "complete cA d = true" $
      IM.complete cA d `shouldBe` True

    it "assess cA = (false, [Regression specSchemaContract \"6\"])" $
      IM.assess cA d phi newSpecName
        `shouldBe` (False, [IM.Regression (Just "specSchemaContract") "6"])

    it "admissible cA d Φ specHandlerValidates = false" $
      IM.admissible cA d phi newSpecName `shouldBe` False

    it "admissible cB d Φ specHandlerValidates = true (assess = (true, []))" $
      IM.assess cB d phi newSpecName `shouldBe` (True, [])

    it "a signature-change variant is NOT discharged by stableSignature" $ do
      IM.touched cSig `shouldBe` ["2"]
      IM.openObligations cSig d `shouldBe` ["1"]
      IM.complete cSig d `shouldBe` False
      IM.assess cSig d phi newSpecName `shouldBe` (False, [IM.Uncomplete ["1"]])

    it "explicit human acks discharge the signature-change obligation" $ do
      let dAck = IM.Discharge { IM.acks = ["1"], IM.rules = [] }
      IM.openObligations cSig dAck `shouldBe` []
      IM.complete cSig dAck `shouldBe` True

    it "nontrivial [G0, G1, G2] for specSchemaContract = true" $ do
      let s = IC.Parallel "schemaField" "contractField" Maps
          probes = [g0, g1, g2]
      any (\p -> IC.holds (IC.evaluateSpec p s)) probes `shouldBe` True
      any (\p -> not (IC.holds (IC.evaluateSpec p s))) probes `shouldBe` True

  describe "Properties — FGL agreement (task 4.1/4.2 acceptance)" $ do
    it "reachable agrees with fgl reachability on every label projection" $
      property propReach

    it "covered agrees with fgl reachability" $
      property propCovered

    it "reaches agrees with fgl reachability" $
      property propReaches

    it "impact is the fgl reverse-dependency closure (tests excluded); frontier = impact \\ touched" $ property $ \pg ->
      forAll (vectorOf (length (pgNodes pg)) (arbitrary :: Gen Bool)) (propImpact pg)