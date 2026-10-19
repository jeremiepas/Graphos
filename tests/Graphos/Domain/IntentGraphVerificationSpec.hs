{-# LANGUAGE OverloadedStrings #-}
-- | Parity and property tests for the intent-graph-verification Domain checkers
-- (task 4.1 `Domain/IntentCheck.hs`, task 4.2 `Domain/Impact.hs`).
--
-- The fixture is the Lean toy instance in
-- openspec/changes/intent-graph-verification/lean/IntentGraph.lean (§5): G₀ with
-- one handler/validator/test/schema-field/contract-field, change A (G₁) that
-- changes the validator body and adds a schema field with no contract field, and
-- change B (G₂) that repairs the omission. Every expected verdict below is the
-- one the Lean kernel accepts (`by decide`) or prints (`#eval`).
--
-- The property tests cross-check @reachable@ and @covered@ against FGL's own
-- reachability on random graphs, mirroring the intent of O1/O2.
module Graphos.Domain.IntentGraphVerificationSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText, toText)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Graphos.Domain.Types.Node (Node (..), FileType (..))
import Graphos.Domain.Types.Edge
  ( Edge (..), EdgeId (..), Relation (..), Confidence (..), relationToText )
import Graphos.Domain.Types.Graph (extractionFromLists)
import Graphos.Domain.Graph.Core (Graph, buildGraph, gNodes, gEdges)

import qualified Graphos.Domain.IntentCheck as IC
import Graphos.Domain.Impact
  ( Change (..), touched, depLabels, impact, frontier
  , Discharge (..), stableSignature, discharged, openObligations, complete
  , admissible, assess, FailedConjunction (..) )

import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.Query.BFS (bfs)

-- ───────────────────────────────────────────────
-- Fixture construction (a Graphos projection of the Lean toy)
-- ───────────────────────────────────────────────

-- | A node with an optional @body_hash@ (the first-class field from task 2.2).
toyNode :: Text -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> Node
toyNode i k lbl sig body = Node
  { nodeId          = i
  , nodeLabel       = fromText lbl
  , nodeFileType    = CodeFile
  , nodeSourceFile  = fromText "toy"
  , nodeSource      = Nothing
  , nodeLineStart   = Nothing
  , nodeLineEnd     = Nothing
  , nodeSignature   = fmap fromText sig
  , nodeCommunityId = Nothing
  , nodeKind        = fmap fromText k
  , nodeDegree      = Nothing
  , nodeIsBridge    = Nothing
  , nodeExtra       = Nothing
  , nodeBodyHash    = fmap fromText body
  , nodePresentBits = 0
  }

toyEdge :: Text -> Text -> Relation -> Edge
toyEdge s d r = Edge
  { edgeId         = EdgeId (s <> "|" <> d <> "|" <> relationToText r)
  , edgeSource     = s
  , edgeTarget     = d
  , edgeRelation   = r
  , edgeWeight     = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra      = Nothing
  }

g0 :: Graph
g0 = buildGraph True (extractionFromLists
  [ toyNode "1" (Just "handler")       "POST /orders"    (Just "(Req) -> Res")      Nothing
  , toyNode "2" (Just "validator")     "validateOrder"   (Just "(Order) -> Result") (Just "h1")
  , toyNode "3" (Just "test")          "orders.spec"     Nothing                    Nothing
  , toyNode "4" (Just "schemaField")   "orders.total"    Nothing                    Nothing
  , toyNode "5" (Just "contractField") "Order.total"     Nothing                    Nothing
  ]
  [ toyEdge "1" "2" Calls, toyEdge "3" "1" Tests, toyEdge "4" "5" Maps ])

-- | Change A: validator body h1 → h2 (signature stable) and a new schema field
-- @orders.currency@ with no contract field.
g1 :: Graph
g1 = buildGraph True (extractionFromLists
  [ toyNode "1" (Just "handler")       "POST /orders"    (Just "(Req) -> Res")      Nothing
  , toyNode "2" (Just "validator")     "validateOrder"   (Just "(Order) -> Result") (Just "h2")
  , toyNode "3" (Just "test")          "orders.spec"     Nothing                    Nothing
  , toyNode "4" (Just "schemaField")   "orders.total"    Nothing                    Nothing
  , toyNode "5" (Just "contractField") "Order.total"     Nothing                    Nothing
  , toyNode "6" (Just "schemaField")   "orders.currency" Nothing                    Nothing
  ]
  [ toyEdge "1" "2" Calls, toyEdge "3" "1" Tests, toyEdge "4" "5" Maps ])

-- | Change B: the same, omission repaired (contract field + maps edge).
g2 :: Graph
g2 = buildGraph True (extractionFromLists
  [ toyNode "1" (Just "handler")       "POST /orders"    (Just "(Req) -> Res")      Nothing
  , toyNode "2" (Just "validator")     "validateOrder"   (Just "(Order) -> Result") (Just "h2")
  , toyNode "3" (Just "test")          "orders.spec"     Nothing                    Nothing
  , toyNode "4" (Just "schemaField")   "orders.total"    Nothing                    Nothing
  , toyNode "5" (Just "contractField") "Order.total"     Nothing                    Nothing
  , toyNode "6" (Just "schemaField")   "orders.currency" Nothing                    Nothing
  , toyNode "7" (Just "contractField") "Order.currency"  Nothing                    Nothing
  ]
  [ toyEdge "1" "2" Calls, toyEdge "3" "1" Tests
  , toyEdge "4" "5" Maps, toyEdge "6" "7" Maps ])

-- | Signature-change variant of change A: the validator's observable signature
-- changes, so the body-only rule must NOT discharge the caller.
g1sig :: Graph
g1sig = buildGraph True (extractionFromLists
  [ toyNode "1" (Just "handler")       "POST /orders"    (Just "(Req) -> Res")         Nothing
  , toyNode "2" (Just "validator")     "validateOrder"   (Just "(Order) -> NewResult") (Just "h2")
  , toyNode "3" (Just "test")          "orders.spec"     Nothing                       Nothing
  , toyNode "4" (Just "schemaField")   "orders.total"    Nothing                       Nothing
  , toyNode "5" (Just "contractField") "Order.total"     Nothing                       Nothing
  , toyNode "6" (Just "schemaField")   "orders.currency" Nothing                       Nothing
  ]
  [ toyEdge "1" "2" Calls, toyEdge "3" "1" Tests, toyEdge "4" "5" Maps ])

-- | G₀ with the lone test node removed (used to exercise the removed case).
g0no3 :: Graph
g0no3 = buildGraph True (extractionFromLists
  [ toyNode "1" (Just "handler")       "POST /orders"  (Just "(Req) -> Res")      Nothing
  , toyNode "2" (Just "validator")     "validateOrder" (Just "(Order) -> Result") (Just "h1")
  , toyNode "4" (Just "schemaField")   "orders.total"  Nothing                    Nothing
  , toyNode "5" (Just "contractField") "Order.total"   Nothing                    Nothing
  ]
  [ toyEdge "1" "2" Calls, toyEdge "4" "5" Maps ])

cA, cB, cSig :: Change
cA   = Change g0 g1
cB   = Change g0 g2
cSig = Change g0 g1sig

specHandlerValidates, specHandlersTested, specSchemaContract, specNoLegacy :: IC.Spec
specHandlerValidates = IC.Reaches "handler" "POST /orders" Calls "validator"
specHandlersTested   = IC.Covered "handler" Tests "test"
specSchemaContract   = IC.Parallel "schemaField" "contractField" Maps
specNoLegacy         = IC.Absent "module" "legacy/" []

phi :: IC.SpecDocument
phi = IC.SpecDocument Nothing
  [ (Just "specHandlerValidates", specHandlerValidates)
  , (Just "specHandlersTested",   specHandlersTested)
  , (Just "specSchemaContract",   specSchemaContract)
  , (Just "specNoLegacy",         specNoLegacy)
  ]

discharge :: Discharge
discharge = Discharge [] [stableSignature]

holdsOn :: Graph -> IC.Spec -> Bool
holdsOn g s = IC.holds (IC.evaluateSpec g s)

-- ───────────────────────────────────────────────
-- 4.1 IntentCheck — fixture parity with the kernel-accepted verdicts
-- ───────────────────────────────────────────────

intentCheckSpec :: Spec
intentCheckSpec = describe "Domain.IntentCheck (Lean toy parity)" $ do
  describe "reaches" $ do
    it "holds on G0 (handler POST /orders calls a validator)" $
      holdsOn g0 specHandlerValidates `shouldBe` True
    it "is unaffected by the added schema field on G1" $
      holdsOn g1 specHandlerValidates `shouldBe` True

  describe "covered" $ do
    it "holds on G0 (every handler is exercised by a test)" $
      holdsOn g0 specHandlersTested `shouldBe` True

  describe "parallel" $ do
    it "holds on G0 (schema <-> contract total correspondence)" $
      holdsOn g0 specSchemaContract `shouldBe` True
    it "is violated on G1 with witness schema field 6 (the omission)" $
      IC.evaluateSpec g1 specSchemaContract `shouldBe` IC.Violated ["6"]
    it "holds again on G2 (omission repaired)" $
      holdsOn g2 specSchemaContract `shouldBe` True

  describe "absent" $ do
    it "holds on G0 (no legacy/ module node)" $
      holdsOn g0 specNoLegacy `shouldBe` True

  describe "and / not" $ do
    it "conjunction holds on G0" $
      holdsOn g0 (IC.And [specHandlerValidates, specSchemaContract]) `shouldBe` True
    it "conjunction fails on G1" $
      holdsOn g1 (IC.And [specHandlerValidates, specSchemaContract]) `shouldBe` False
    it "negation of the broken spec holds on G1" $
      holdsOn g1 (IC.Not specSchemaContract) `shouldBe` True
    it "negation of the holding spec fails on G0" $
      holdsOn g0 (IC.Not specSchemaContract) `shouldBe` False

  it "only specSchemaContract regresses after change A (Lean #eval parity)" $
    [ nm | (nm, (_, v)) <- IC.evaluate g1 phi, not (IC.holds v) ]
      `shouldBe` [Just "specSchemaContract"]

  it "specSchemaContract is non-trivial on the toy probes" $
    (any (\g -> holdsOn g specSchemaContract) [g0, g1, g2]
      && any (not . (\g -> holdsOn g specSchemaContract)) [g0, g1, g2]) `shouldBe` True

-- ───────────────────────────────────────────────
-- 4.2 Impact — touched / impact / frontier / discharge / admissibility
-- ───────────────────────────────────────────────

impactSpec :: Spec
impactSpec = describe "Domain.Impact (Lean toy parity)" $ do
  describe "touched = removed ∪ added ∪ changed" $ do
    it "change A touches added 6 and body-changed 2 (Lean [6,2])" $
      sort (touched cA) `shouldBe` ["2", "6"]
    it "change B touches added 6,7 and body-changed 2" $
      sort (touched cB) `shouldBe` ["2", "6", "7"]
    it "a removed node is touched" $
      sort (touched (Change g0 g0no3)) `shouldBe` ["3"]

  describe "impact closure under reverse dependency (O3)" $ do
    it "change A impact is the handler closure of {2,6}" $
      sort (impact g0 (touched cA)) `shouldBe` ["1", "2", "6"]

  describe "frontier = impact \\ touched" $ do
    it "change A frontier is the calling handler" $
      sort (frontier g0 (touched cA)) `shouldBe` ["1"]
    it "change B frontier is the calling handler" $
      sort (frontier g0 (touched cB)) `shouldBe` ["1"]

  describe "discharge discipline" $ do
    it "stableSignature discharges the body-only handler (change A)" $
      stableSignature cA "1" `shouldBe` True
    it "discharged via the rule" $
      discharged cA discharge "1" `shouldBe` True
    it "no open obligations on change A" $
      openObligations cA discharge `shouldBe` []
    it "change A is complete" $
      complete cA discharge `shouldBe` True

  describe "signature-change variant is NOT discharged" $ do
    it "stableSignature rejects the handler when the dependency's signature changed" $
      stableSignature cSig "1" `shouldBe` False
    it "the obligation stays open" $
      sort (openObligations cSig discharge) `shouldBe` ["1"]
    it "the change is not complete" $
      complete cSig discharge `shouldBe` False

  describe "admissibility gate" $ do
    it "change A is inadmissible: schemaContract regresses after (Lean false)" $
      assess cA discharge phi (Just "specHandlerValidates")
        `shouldBe` (False, [Regression (Just "specSchemaContract") "6"])
    it "change B is admissible (Lean true)" $
      admissible cB discharge phi (Just "specHandlerValidates") `shouldBe` True
    it "boolean form agrees with assess for change A" $
      admissible cA discharge phi (Just "specHandlerValidates")
        `shouldBe` fst (assess cA discharge phi (Just "specHandlerValidates"))

-- ───────────────────────────────────────────────
-- Property tests (reaches/covered vs FGL; impact closure)
-- ───────────────────────────────────────────────

kindPool :: [Maybe Text]
kindPool =
  [ Just "handler", Just "validator", Just "test"
  , Just "schemaField", Just "contractField", Nothing ]

kindTexts :: [Text]
kindTexts = ["handler", "validator", "test", "schemaField", "contractField"]

testRelations :: [Relation]
testRelations =
  [Calls, Tests, Maps, Implements, References, DependsOn, Imports, Contains]

genGraph :: Gen Graph
genGraph = do
  n <- choose (0 :: Int, 8)
  let ids = [ T.pack ("n" <> show (i :: Int)) | i <- [0 .. n - 1] ]
  kinds <- vectorOf n (elements kindPool)
  let nodes = [ toyNode (ids !! i) (kinds !! i) (ids !! i) Nothing Nothing
              | i <- [0 .. n - 1] ]
  m <- choose (0 :: Int, 12)
  es <- if null ids
          then pure []
          else vectorOf m $ do
                 s <- elements ids
                 d <- elements ids
                 r <- elements testRelations
                 pure (toyEdge s d r)
  let dedup = Map.elems (Map.fromList [ ((edgeSource e, edgeTarget e), e) | e <- es ])
  pure (buildGraph True (extractionFromLists nodes dedup))

-- | Node ids of a kind, computed directly from the graph.
kindNodes :: Graph -> Text -> [Text]
kindNodes g k =
  [ nodeId n | n <- Map.elems (gNodes g), fmap toText (nodeKind n) == Just k ]

-- | Reachability computed purely with FGL: map the projected graph's nodes to
-- consecutive Ints, keep only edges of the label, and take FGL's BFS frontier.
fglReachSet :: Graph -> Relation -> Text -> Set.Set Text
fglReachSet g rel start = Set.insert start (Set.fromList reachedTxt)
  where
    ids   = Map.keys (gNodes g)
    toInt = Map.fromList (zip ids [0 ..])
    toTxt = Map.fromList (zip [0 ..] ids)
    fg :: Gr () ()
    fg = mkGraph
           [ (i, ()) | i <- [0 .. length ids - 1] ]
           [ (toInt Map.! s, toInt Map.! d, ())
           | ((s, d), e) <- Map.toList (gEdges g), edgeRelation e == rel ]
    startI = Map.findWithDefault (-1) start toInt
    reachedTxt = [ toTxt Map.! j | j <- bfs startI fg ]

prop_reachable_agrees_with_fgl :: Property
prop_reachable_agrees_with_fgl =
  forAll genGraph $ \g ->
    conjoin
      [ counterexample (show rel <> " from " <> T.unpack s) $
          IC.reachable g rel s === fglReachSet g rel s
      | rel <- testRelations
      , s <- Map.keys (gNodes g)
      ]

prop_covered_agrees_with_fgl :: Property
prop_covered_agrees_with_fgl =
  forAll genGraph $ \g ->
    conjoin
      [ counterexample (show (k, rel, k')) $
          holdsOn g (IC.Covered k rel k') === fglCovered g rel k k'
      | k  <- kindTexts
      , rel <- [Calls, Tests, Maps]
      , k' <- kindTexts
      ]
  where
    fglCovered graph rel k k' =
      let us = kindNodes graph k
          ws = kindNodes graph k'
      in all (\u -> any (\w -> u `Set.member` fglReachSet graph rel w) ws) us

-- | Reverse-dependency predecessors, re-derived in the test from the exported
-- label set (mirrors the O3 closure statement).
revDeps :: Graph -> Text -> [Text]
revDeps g v =
  [ s | ((s, d), e) <- Map.toList (gEdges g), d == v, edgeRelation e `elem` depLabels ]

prop_impact_closed :: Property
prop_impact_closed =
  forAll genGraph $ \g ->
    forAll (sublistOf (Map.keys (gNodes g))) $ \t ->
      let i = Set.fromList (impact g t)
      in counterexample (show t <> " => " <> show (Set.toList i)) $
           conjoin
             [ counterexample (T.unpack v) $
                 property (all (`Set.member` i) (revDeps g v))
             | v <- Set.toList i ]

spec :: Spec
spec = describe "intent-graph-verification (unit 4: Domain checkers)" $ do
  intentCheckSpec
  impactSpec
  describe "properties" $ do
    it "reaches/reachable agree with FGL reachability" $
      property prop_reachable_agrees_with_fgl
    it "covered agrees with FGL reachability" $
      property prop_covered_agrees_with_fgl
    it "impact is closed under reverse dependency" $
      property prop_impact_closed
