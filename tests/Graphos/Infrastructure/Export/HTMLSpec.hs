module Graphos.Infrastructure.Export.HTMLSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Infrastructure.Export.HTML (communityAggregatesToJSON, VisCommunityAggregate(..))

spec :: Spec
spec = describe "communityAggregatesToJSON" $ do
  let g = buildGraph False (extractionFromLists [] [])
      commMap = Map.fromList [(1, [T.pack "a", T.pack "b"]), (2, [T.pack "c"])]

  it "uses LLM label when present" $ do
    let labels = Just (Map.fromList [(1, T.pack "Auth Module"), (2, T.pack "Parser")])
        aggs = communityAggregatesToJSON g commMap labels
    length aggs `shouldBe` 2
    map vcaLabel aggs `shouldMatchList` [T.pack "Auth Module", T.pack "Parser"]

  it "falls back to Community <id> when labels are absent" $ do
    let aggs = communityAggregatesToJSON g commMap Nothing
    length aggs `shouldBe` 2
    map vcaLabel aggs `shouldMatchList` [T.pack "Community 1", T.pack "Community 2"]

  it "falls back for missing keys in partial label map" $ do
    let labels = Just (Map.fromList [(1, T.pack "Auth Module")])
        aggs = communityAggregatesToJSON g commMap labels
    length aggs `shouldBe` 2
    map vcaLabel aggs `shouldMatchList` [T.pack "Auth Module", T.pack "Community 2"]

  it "falls back for empty string labels" $ do
    let labels = Just (Map.fromList [(1, T.pack ""), (2, T.pack "Parser")])
        aggs = communityAggregatesToJSON g commMap labels
    length aggs `shouldBe` 2
    map vcaLabel aggs `shouldMatchList` [T.pack "Community 1", T.pack "Parser"]
