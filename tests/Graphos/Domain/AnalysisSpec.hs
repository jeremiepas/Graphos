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
      let ext = emptyExtraction
            { extractionNodes = [testNode "hub", testNode "leaf1", testNode "leaf2"]
            , extractionEdges = [testEdge "hub" "leaf1", testEdge "hub" "leaf2"]
            }
          g = buildGraph False ext
          commMap = detectCommunities g
          cohesion = scoreAllCohesion g commMap
          analysis = analyze g commMap cohesion
      length (analysisGodNodes analysis) `shouldSatisfy` (>= 1)

-- Helpers
testNode :: Text -> Node
testNode nid = Node nid nid CodeFile "test.hs" (Just "L1") Nothing Nothing Nothing Nothing Nothing Nothing Nothing

testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge src tgt Calls Extracted 1.0 "test.hs" (Just "L1") 1.0