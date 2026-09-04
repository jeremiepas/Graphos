module Graphos.UseCase.Query.RenderSpec where

import Test.Hspec
import Data.Aeson (Value(..))
import qualified Data.Text as T

import Graphos.UseCase.Query.Render (renderCypherResultText, renderCypherResultJSON, renderMutationResultText, renderMutationResultJSON)
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
