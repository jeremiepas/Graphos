module Graphos.UseCase.Query.RenderSpec where

import Test.Hspec
import Data.Aeson (Value(..), toJSON)
import qualified Data.List as L
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Data.Text (Text)

import Graphos.UseCase.Query.Render (renderCypherResultText, renderCypherResultJSON, renderMutationResultText, renderMutationResultJSON, BudgetCtl(..), defaultBudgetCtl, boundedNodes, capLabel, encodeText)
import Graphos.Domain.Graph.Score (ScoredNode(..))
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
  let node :: Text -> Double -> ScoredNode
      node lbl sc = ScoredNode
        { snNodeId = lbl
        , snLabel  = lbl
        , snScore  = sc
        , snSourceFile = "graphos-out/graph.json"
        , snCommunityId = Nothing
        , snKind = Just "Function"
        }
      ranked = [ node (T.pack ("fn-" ++ show i)) (1.0 - 0.5 * fromIntegral i) | i <- ([0 :: Int .. 2]) ]
      generous = defaultBudgetCtl { bcByteBudget = 100000 }
      oneNodeBytes = case ranked of
        (h : _) -> T.length (encodeText (toJSON h))
        []      -> 0

  describe "capLabel" $ do
    it "leaves a label untouched when the cap is non-positive" $ do
      let out = capLabel defaultBudgetCtl { bcMaxLabelChars = 0 } (node "short" 0.9)
          result = snLabel out
      result `shouldBe` "short"

    it "leaves a short label untouched under the default cap" $ do
      let out = capLabel defaultBudgetCtl (node "short" 0.9)
          result = snLabel out
      result `shouldBe` "short"

    it "applies word-boundary truncation through the budget cap" $ do
      let ctl = defaultBudgetCtl { bcMaxLabelChars = 5 }
          out = capLabel ctl (node "this-is-a-very-long-label" 0.9)
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
      case toJSON (node "some-identifier" 0.9) of
        Object obj ->
          (L.sort (KM.keys obj)) `shouldBe` L.sort [Key.fromText "id", Key.fromText "label", Key.fromText "score", Key.fromText "source_file", Key.fromText "kind", Key.fromText "preview"]
        _ -> expectationFailure "node is not a JSON object"

    it "never leaks the raw label when it is long (preview is truncated)" $ do
      case toJSON (node (T.replicate 500 "x") 0.9) of
        Object obj ->
          KM.lookup (Key.fromText "preview") obj `shouldSatisfy` \case
            Just (String p) -> T.length p < 500
            _               -> False
        _ -> expectationFailure "node is not a JSON object"
