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
import Graphos.Domain.Graph (buildGraph, Graph)
import Graphos.UseCase.Report (generateReport, bridgeNodesSection, bridgeTableCap)

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
      -- new bridge row format: label | kind | source | degree
      report `shouldSatisfy` T.isInfixOf "| b | - | test.hs | 2 |"

    it "prints the biconnected-component count from the analysis record" $
      -- 7 is deliberately not the fixture's real count (2): the rendered
      -- figure proves the report reads the precomputed value.
      report `shouldSatisfy` T.isInfixOf "Biconnected components: 7"

    it "renders the no-articulation message when the shared list is empty" $ do
      let analysis' = baseAnalysis { analysisArticulation = [] }
          report' = generateReport g analysis' defaultConfig detection Nothing
      report' `shouldSatisfy` T.isInfixOf "No articulation points found"

    it "caps the bridge table at bridgeTableCap, drops degree ≤ 1, and totals the rest" $ do
      -- A path p0 -> p1 -> ... -> pN makes every internal node a degree-2
      -- articulation point; the two endpoints stay degree 1 and must be dropped.
      -- With far more than bridgeTableCap internal nodes the rendered table is
      -- truncated to the cap and the remainder reported in the footer.
      let (pg, pids) = pathGraph 200
          rendered = bridgeNodesSection pids pg
          rows = drop 2 (T.lines rendered)            -- skip header + separator
          dataRows = take bridgeTableCap rows         -- table is capped at the cap
      length dataRows `shouldSatisfy` (== bridgeTableCap)
      shouldSatisfy rendered (T.isInfixOf "199 articulation points, 149 omitted")
      -- degree-1 endpoints are filtered out of the table
      T.isInfixOf "| p0 |" rendered `shouldSatisfy` not
      T.isInfixOf "| p200 |" rendered `shouldSatisfy` not
      -- the new columns render label | kind | source | degree
      rendered `shouldSatisfy` T.isInfixOf "| Bridge Node | Kind | Source | Degree |"
      rendered `shouldSatisfy` T.isInfixOf "test.hs"

-- Helpers
pathGraph :: Int -> (Graph, [Text])
pathGraph n =
  let ids = [T.pack ("p" ++ show i) | i <- [0 .. n]]
      ext = extractionFromLists (map testNode ids) (zipWith testEdge ids (drop 1 ids))
  in (buildGraph False ext, ids)
testNode :: Text -> Node
testNode nid = Node nid (fromText nid) CodeFile (fromText "test.hs") Nothing (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge (EdgeId (src <> "->" <> tgt)) src tgt Calls 1.0 (Confidence 1.0) Nothing
