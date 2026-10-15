-- | Report rendering tests (bounded-report-export): the report consumes the
-- precomputed Analysis values — cohesion, articulation points, bcc count —
-- and never recomputes them from the graph.
module Graphos.UseCase.ReportSpec where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.UseCase.Report (generateReport)

spec :: Spec
spec = do
  describe "generateReport (bounded-report-export)" $ do
    let members = ["a", "b", "c"]
        ext = extractionFromLists (map testNode members) [testEdge "a" "b", testEdge "b" "c"]
        g = buildGraph False ext
        detection = Detection 0 0 True Nothing Map.empty Map.empty emptyExclusionCounts
        baseAnalysis = Analysis
          { analysisCommunities  = Map.fromList [(0, members)]
          , analysisNullModel    = DefaultNullModel
          , analysisCohesion     = Map.fromList [(0, 0.42)]
          , analysisGodNodes     = []
          , analysisSurprises    = []
          , analysisQuestions    = []
          , analysisArticulation = ["b"]
          , analysisBccCount     = 7
          }
        report = generateReport g baseAnalysis defaultConfig detection Nothing

    it "prints the cohesion value from the analysis map (no recomputation)" $
      -- 0.42 is deliberately NOT the structural cohesion of the fixture
      -- (a chain of 3 has cohesion 2/3): the rendered value proves the
      -- report reads the clustering-time map.
      report `shouldSatisfy` T.isInfixOf "| 0 | 3 | 0.42 |"

    it "prints the articulation points from the analysis record" $ do
      report `shouldSatisfy` T.isInfixOf "Articulation points: 1"
      report `shouldSatisfy` T.isInfixOf "| b | 2 |"

    it "prints the biconnected-component count from the analysis record" $
      -- 7 is deliberately not the fixture's real count (2): the rendered
      -- figure proves the report reads the precomputed value.
      report `shouldSatisfy` T.isInfixOf "Biconnected components: 7"

    it "renders the no-articulation message when the shared list is empty" $ do
      let analysis' = baseAnalysis { analysisArticulation = [] }
          report' = generateReport g analysis' defaultConfig detection Nothing
      report' `shouldSatisfy` T.isInfixOf "No articulation points found"

-- Helpers
testNode :: Text -> Node
testNode nid = Node nid (fromText nid) CodeFile (fromText "test.hs") Nothing (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge (EdgeId (src <> "->" <> tgt)) src tgt Calls 1.0 (Confidence 1.0) Nothing
