module Graphos.Domain.AnalysisSpec where

import Test.Hspec
import Data.Text (Text)
import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Community (detectCommunities, scoreAllCohesion)
import Graphos.Domain.Analysis (analyze)

spec :: Spec
spec = do
  describe "analyze" $ do
    it "produces analysis with god nodes" $ do
      let ext = extractionFromLists [testNode "hub", testNode "leaf1", testNode "leaf2"] [testEdge "hub" "leaf1", testEdge "hub" "leaf2"]
          g = buildGraph False ext
          commMap = detectCommunities g
          cohesion = scoreAllCohesion g commMap
          analysis = analyze g commMap cohesion
      length (analysisGodNodes analysis) `shouldSatisfy` (>= 1)

-- Helpers
edgeIdFrom :: Text -> Text -> EdgeId
edgeIdFrom src tgt = EdgeId (src <> "->" <> tgt)

testNode :: Text -> Node
testNode nid = Node nid nid CodeFile "test.hs" (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge (edgeIdFrom src tgt) src tgt Calls 1.0 (Confidence 1.0) Nothing