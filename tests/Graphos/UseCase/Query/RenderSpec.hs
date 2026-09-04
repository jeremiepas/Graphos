module Graphos.UseCase.Query.RenderSpec where

import Test.Hspec
import Data.Aeson (Value(..))
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.UseCase.Query.Render
  ( renderCypherResultText
  , renderCypherResultJSON
  , renderMutationResultText
  , renderMutationResultJSON
  , enforceResponseBudget
  )
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

-- | Focused tests for the byte-budget response controls (task group 5).
mkNode :: Text -> Text -> Text -> Double -> Text -> ScoredNode
mkNode nid lbl src score kind = ScoredNode
  { snNodeId = nid
  , snLabel = lbl
  , snScore = score
  , snSourceFile = src
  , snKind = kind
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
