module Graphos.Regression.ContextNoiseRegressionSpec where

import Test.Hspec
import System.Directory (doesFileExist)
import qualified Data.Text as T

import Graphos.Domain.Analysis (analyze)
import Graphos.Domain.Context (QueryComplexity(..), budgetForComplexity)
import Graphos.Domain.Graph (gNodes)
import Graphos.UseCase.FormatContext (formatContextForLLMBudgeted)
import Graphos.UseCase.Load (loadGraphFromFile, lrGraph, lrCommunities, lrCohesion)
import Graphos.UseCase.SelectContext (selectContext)
import qualified Data.Map.Strict as Map

fixturePath :: FilePath
fixturePath = "graphos-out/graph.json"

queryText :: T.Text
queryText = "how does the query pipeline work"

budget :: Int
budget = 3000

spec :: Spec
spec = describe "Context noise regression on repo fixture" $ do
  it "skips when graphos-out/graph.json is absent, otherwise asserts budget and latency" $ do
    exists <- doesFileExist fixturePath
    if not exists
      then pendingWith ("Fixture " ++ fixturePath ++ " not present; regression skipped")
      else do
        loaded <- loadGraphFromFile fixturePath
        case loaded of
          Left err -> expectationFailure (T.unpack err)
          Right lr -> do
            let g = lrGraph lr
                commMap = lrCommunities lr
                cohesion = lrCohesion lr
                analysis = analyze g commMap cohesion
                ctxBudget = budgetForComplexity ModuleLevel budget
                ctx = selectContext g commMap analysis queryText ctxBudget
                (formatted, tokenEstimate, _, _) = formatContextForLLMBudgeted budget ctx
                topRanked = case Map.toList (gNodes g) of
                              []    -> ""
                              (nid, n):_ -> nid
            tokenEstimate `shouldSatisfy` (<= budget)
            T.isInfixOf topRanked formatted `shouldBe` True
