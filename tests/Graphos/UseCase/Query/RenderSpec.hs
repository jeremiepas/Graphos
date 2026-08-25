module Graphos.UseCase.Query.RenderSpec where

import Test.Hspec
import Data.Aeson (Value(..))
import qualified Data.Text as T

import Graphos.UseCase.Query.Render (renderCypherResultText, renderCypherResultJSON)
import Graphos.Domain.Query.Cypher.Eval (CypherResult(..))

mkResult :: CypherResult
mkResult = CypherResult
  { crColumns   = ["n"]
  , crRows      = [[String "fn-001"], [String "fn-002"]]
  , crTruncated = False
  }

spec :: Spec
spec = describe "renderCypherResult" $ do
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
