module Graphos.UseCase.Query.RenderSpec where

import Test.Hspec
import Data.Aeson (Value(..), toJSON)
import qualified Data.List as L
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.UseCase.Query.Render
  ( renderCypherResultText
  , renderCypherResultJSON
  , renderMutationResultText
  , renderMutationResultJSON
  , enforceResponseBudget
  , encodeText
  )
import Graphos.UseCase.Query.Budget (BudgetCtl(..), defaultBudgetCtl, boundedNodes, capLabel)
import Graphos.Domain.Graph.Score
  ( ScoredNode(..)
  , QueryResponse(..)
  , MatchVerdict(..)
  , truncateLabel
  , scoredNodeBytes
  , enforceByteBudget
  , defaultMaxLabelChars
  )
import Graphos.Domain.Query.Cypher.Eval (CypherResult(..), MutationResult(..), MutationSummary(..))

mkResult :: CypherResult
mkResult = CypherResult
  { crColumns   = ["n"]
  , crRows      = [[String "fn-001"], [String "fn-002"]]
  , crTruncated = False
  }

spec :: Spec
spec = do
  mutationRenderSpec
  cypherRenderSpec
  renderBudgetSpec
  budgetRenderSpec

cypherRenderSpec :: Spec
cypherRenderSpec = describe "renderCypherResult" $ do
  describe "renderCypherResultText" $ do
    it "renders a count line, header, and one line per row" $ do
      let out = renderCypherResultText 2000 mkResult
      T.lines out `shouldBe`
        [ "Results (2 rows)"
        , "n"
        , "\"fn-001\""
        , "\"fn-002\""
        ]

    it "marks the count line when truncated" $ do
      let out = renderCypherResultText 2000 (mkResult { crTruncated = True })
      T.lines out `shouldBe`
        [ "Results (2 rows) [truncated]"
        , "n"
        , "\"fn-001\""
        , "\"fn-002\""
        ]

    it "renders (no rows) for an empty result" $ do
      let out = renderCypherResultText 2000 (CypherResult ["n"] [] False)
      T.lines out `shouldBe` [ "Results (0 rows)", "n", "(no rows)" ]

  describe "renderCypherResultJSON" $ do
    it "renders columns, rows, and truncated" $ do
      let out = renderCypherResultJSON mkResult
      out `shouldBe` "{\"columns\":[\"n\"],\"rows\":[[\"fn-001\"],[\"fn-002\"]],\"truncated\":false}"

    it "reflects the truncated flag" $ do
      let out = renderCypherResultJSON (mkResult { crTruncated = True })
      out `shouldBe` "{\"columns\":[\"n\"],\"rows\":[[\"fn-001\"],[\"fn-002\"]],\"truncated\":true}"

-- Mutation result renderers (opencypher-write-mutations).
mutationRenderSpec :: Spec
mutationRenderSpec = describe "renderMutationResult" $ do
  let mr = MutationResult
        { mrGraph = undefined -- not used by renderers
        , mrSummary = MutationSummary 1 0 2 3 1 0 0
        , mrResult = CypherResult ["n.status"] [[Number 7]] False
        }

  describe "renderMutationResultText" $ do
    it "renders a summary line of non-zero counts" $ do
      T.isInfixOf "nodes created: 1" (renderMutationResultText 2000 mr) `shouldBe` True
      T.isInfixOf "rels upserted: 2" (renderMutationResultText 2000 mr) `shouldBe` True
      T.isInfixOf "properties set: 3" (renderMutationResultText 2000 mr) `shouldBe` True
      T.isInfixOf "properties removed: 1" (renderMutationResultText 2000 mr) `shouldBe` True
      T.isPrefixOf "OK" (renderMutationResultText 2000 mr) `shouldBe` False

    it "renders OK (no changes) for a zero summary" $ do
      let zero = mr { mrSummary = MutationSummary 0 0 0 0 0 0 0 }
      T.isPrefixOf "OK (no changes)" (renderMutationResultText 2000 zero) `shouldBe` True

    it "mentions the re-extraction caveat" $ do
      T.isInfixOf "extraction" (renderMutationResultText 2000 mr) `shouldBe` True

  describe "renderMutationResultJSON" $ do
    it "renders summary, columns, rows, and truncated" $ do
      renderMutationResultJSON mr `shouldBe`
        "{\"columns\":[\"n.status\"],\"rows\":[[7]],\"summary\":{\"nodes_created\":1,\"nodes_deleted\":0,\"properties_removed\":1,\"properties_set\":3,\"rels_created\":0,\"rels_deleted\":0,\"rels_upserted\":2},\"truncated\":false}"

-- * Budget-aware serialization helpers
renderBudgetSpec :: Spec
renderBudgetSpec = describe "budget-aware serialization" $ do
  let mkScored :: Text -> Double -> ScoredNode
      mkScored lbl sc = ScoredNode
        { snNodeId = lbl
        , snLabel  = lbl
        , snScore  = sc
        , snSourceFile = "graphos-out/graph.json"
        , snCommunityId = Nothing
        , snKind = Just "Function"
        }
      ranked = [ mkScored (T.pack ("fn-" ++ show i)) (1.0 - 0.5 * fromIntegral i) | i <- ([0 :: Int .. 2]) ]
      generous = defaultBudgetCtl { bcByteBudget = 100000 }
      oneNodeBytes = case ranked of
        (h : _) -> T.length (encodeText (toJSON h))
        []      -> 0

  describe "capLabel" $ do
    it "leaves a label untouched when the cap is non-positive" $ do
      let out = capLabel defaultBudgetCtl { bcMaxLabelChars = 0 } (mkScored "short" 0.9)
          result = snLabel out
      result `shouldBe` "short"

    it "leaves a short label untouched under the default cap" $ do
      let out = capLabel defaultBudgetCtl (mkScored "short" 0.9)
          result = snLabel out
      result `shouldBe` "short"

    it "applies word-boundary truncation through the budget cap" $ do
      let ctl = defaultBudgetCtl { bcMaxLabelChars = 5 }
          out = capLabel ctl (mkScored "this-is-a-very-long-label" 0.9)
          result = snLabel out
      T.isSuffixOf "…" result `shouldBe` True
      T.length result `shouldSatisfy` (\l -> l < 23)

  describe "boundedNodes" $ do
    it "returns an empty result with zero omitted for empty input" $ do
      let (kept, dropped) = boundedNodes defaultBudgetCtl []
      kept `shouldBe` ([] :: [ScoredNode])
      dropped `shouldBe` 0

    it "keeps every node when the byte budget is generous" $ do
      let (kept, dropped) = boundedNodes generous ranked
      length kept `shouldBe` 3
      dropped `shouldBe` 0

    it "caps the returned node count via bcMaxNodes without inflating omitted" $ do
      let (kept, dropped) = boundedNodes (generous { bcMaxNodes = 2 }) ranked
      length kept `shouldBe` 2
      dropped `shouldBe` 0

    it "drops lowest-scoring nodes once the byte budget is exhausted" $ do
      let ctl = defaultBudgetCtl { bcByteBudget = oneNodeBytes }
          (kept, dropped) = boundedNodes ctl ranked
      length kept `shouldBe` 1
      dropped `shouldBe` 1

    it "retains the highest-scoring prefix when scores are already ranked" $ do
      let sizes = map (\n -> T.length (encodeText (toJSON n))) ranked
          ctl = defaultBudgetCtl { bcByteBudget = sum (take 2 sizes) }
          (kept, dropped) = boundedNodes ctl ranked
      map snScore kept `shouldBe` [1.0, 0.5]
      dropped `shouldBe` 0

  describe "compact node JSON shape" $ do
    it "emits exactly id, label, score, source_file, kind, preview" $ do
      case toJSON (mkScored "some-identifier" 0.9) of
        Object obj ->
          (L.sort (KM.keys obj)) `shouldBe` L.sort [Key.fromText "id", Key.fromText "label", Key.fromText "score", Key.fromText "source_file", Key.fromText "kind", Key.fromText "preview", Key.fromText "community"]
        _ -> expectationFailure "node is not a JSON object"

    it "never leaks the raw label when it is long (preview is truncated)" $ do
      case toJSON (mkScored (T.replicate 500 "x") 0.9) of
        Object obj ->
          KM.lookup (Key.fromText "preview") obj `shouldSatisfy` \case
            Just (String p) -> T.length p < 500
            _               -> False
        _ -> expectationFailure "node is not a JSON object"
-- | Focused tests for the byte-budget response controls (task group 5).
mkNode :: Text -> Text -> Text -> Double -> Text -> ScoredNode
mkNode nid lbl src score kind = ScoredNode
  { snNodeId = nid
  , snLabel = lbl
  , snScore = score
  , snSourceFile = src
  , snKind = if T.null kind then Nothing else Just kind
  , snCommunityId = Nothing
  }

node :: ScoredNode
node = mkNode "n0" "label" "src/0.hs" 1.0 "kind"

budgetRenderSpec :: Spec
budgetRenderSpec = describe "query response budget" $ do
  describe "truncateLabel" $ do
    it "never exceeds n characters and appends an ellipsis" $ do
      let out = truncateLabel 5 (T.pack "abcdefgh")
      T.length out `shouldBe` 5
      T.head (T.drop 4 out) `shouldBe` chr 0x2026

    it "returns labels already within n unchanged" $ do
      truncateLabel 5 (T.pack "abc") `shouldBe` T.pack "abc"

    it "returns the label unchanged when n <= 0" $ do
      truncateLabel 0 (T.pack "abc") `shouldBe` T.pack "abc"

  describe "scoredNodeBytes" $ do
    it "is strictly positive for a serialized node" $ do
      scoredNodeBytes defaultMaxLabelChars node `shouldSatisfy` (> (0 :: Int))

    it "grows with label length while below maxChars" $ do
      let shorter = scoredNodeBytes defaultMaxLabelChars (mkNode "m" (T.pack "abc") "f" 1.0 "k")
          longer  = scoredNodeBytes defaultMaxLabelChars (mkNode "m" (T.pack "abcdef") "f" 1.0 "k")
      shorter `shouldSatisfy` (< longer)

  describe "enforceByteBudget" $ do
    it "keeps every node when the budget is generous" $ do
      let (kept, dropped) = enforceByteBudget defaultMaxLabelChars 100000 [node, node]
      ((length kept == 2 && dropped == 0)) `shouldBe` True

    it "drops every node when the budget is below header overhead" $ do
      let (kept, dropped) = enforceByteBudget defaultMaxLabelChars 100 [node, node, node]
      ((length kept == 0 && dropped == 3)) `shouldBe` True

  describe "enforceResponseBudget" $ do
    it "elides long labels to maxChars" $ do
      let resp0 = QueryResponse { qrespVerdict = Strong, qrespBestScore = 1.0, qrespHash = "h",
                                  qrespNodes = [mkNode "m" (T.pack "abcdefghij") "f" 1.0 "k"],
                                  qrespEdges = [], qrespSuggestions = [],
                                  qrespOmittedNodes = 0, qrespOmittedEdges = 0 }
          out = enforceResponseBudget 5 100000 0 resp0
      case qrespNodes out of
        (n : _) -> T.length (snLabel n) `shouldBe` 5
        [] -> error "expected one node"

    it "reports omitted nodes when maxNodes caps results" $ do
      let mk3 = [mkNode "a" "x" "f" 1.0 "k", mkNode "b" "y" "f" 1.0 "k", mkNode "c" "z" "f" 1.0 "k"]
          resp0 = QueryResponse { qrespVerdict = Strong, qrespBestScore = 1.0, qrespHash = "h",
                                  qrespNodes = mk3, qrespEdges = [], qrespSuggestions = [],
                                  qrespOmittedNodes = 0, qrespOmittedEdges = 0 }
          out = enforceResponseBudget defaultMaxLabelChars 100000 2 resp0
      (qrespOmittedNodes out == 1 && length (qrespNodes out) == 2) `shouldBe` True

    it "drops edges whose endpoints were omitted" $ do
      let resp0 = QueryResponse { qrespVerdict = Strong, qrespBestScore = 1.0, qrespHash = "h",
                                  qrespNodes = [mkNode "a" "a" "f" 1.0 "k", mkNode "b" "b" "f" 1.0 "k"],
                                  qrespEdges = [("a", "b", "semantic", 1.0), ("b", "z", "semantic", 1.0)],
                                  qrespSuggestions = [], qrespOmittedNodes = 0, qrespOmittedEdges = 0 }
          out = enforceResponseBudget defaultMaxLabelChars 100000 1 resp0
      (qrespOmittedEdges out == 2 && length (qrespEdges out) == 0) `shouldBe` True
