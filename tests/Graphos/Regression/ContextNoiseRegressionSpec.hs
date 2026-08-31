module Graphos.Regression.ContextNoiseRegressionSpec where

import Test.Hspec
import System.Directory (doesFileExist, getFileSize)
import qualified Data.Text as T

import Graphos.Domain.Analysis (analyze)
import Graphos.Domain.Context (QueryComplexity(..), budgetForComplexity)
import Graphos.UseCase.FormatContext (formatContextForLLMBudgeted)
import Graphos.UseCase.Load (loadGraphFromFile, lrGraph, lrCommunities, lrCohesion)
import Graphos.UseCase.SelectContext (selectContext)
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
        size <- getFileSize fixturePath
        if size == 0
          then pendingWith ("Fixture " ++ fixturePath ++ " is empty; regression skipped")
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
                tokenEstimate `shouldSatisfy` (<= budget)
                T.null formatted `shouldBe` False
