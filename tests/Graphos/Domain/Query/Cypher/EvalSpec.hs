module Graphos.Domain.Query.Cypher.EvalSpec where

import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Graphos.Domain.Types
  ( Node(..), Edge(..), EdgeId(..)
  , FileType(CodeFile)
  , Relation(Calls, Imports)
  , Confidence(..)
  , extractionFromLists
  )
import Graphos.Domain.Graph (Graph, buildGraph)
import Graphos.Domain.Graph.Index (GraphIndex(..))
import Graphos.Domain.Query.Cypher.Parser (parseQuery)
import Graphos.Domain.Query.Cypher.Eval (evaluate, CypherResult(..))

-- ───────────────────────────────────────────────
-- Fixture
-- ───────────────────────────────────────────────

mkNode :: Text -> Text -> Text -> Node
mkNode nid kind srcFile = Node
  { nodeId          = nid
  , nodeLabel       = nid
  , nodeFileType    = CodeFile
  , nodeSourceFile  = srcFile
  , nodeLineStart   = Just 1
  , nodeLineEnd     = Just 10
  , nodeSignature   = Nothing
  , nodeCommunityId = Nothing
  , nodeKind        = Just kind
  , nodeDegree      = Nothing
  , nodeIsBridge    = Nothing
  , nodeExtra       = Nothing
  }

mkEdge :: Text -> Text -> Text -> Relation -> Edge
mkEdge eid src tgt rel = Edge
  { edgeId         = EdgeId eid
  , edgeSource     = src
  , edgeTarget     = tgt
  , edgeRelation   = rel
  , edgeWeight     = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra      = Nothing
  }

fixtureNodes :: [Node]
fixtureNodes =
  [ mkNode "n1" "Function" "src/services/foo.hs"
  , mkNode "n2" "Function" "src/services/bar.hs"
  , mkNode "n3" "Function" "src/core/baz.hs"
  , mkNode "n4" "File" "src/main.hs"
  , mkNode "n5" "Function" "src/services/qux.hs"
  ]

fixtureEdges :: [Edge]
fixtureEdges =
  [ mkEdge "e1" "n1" "n2" Calls
  , mkEdge "e2" "n2" "n3" Calls
  , mkEdge "e3" "n4" "n1" Imports
  , mkEdge "e4" "n5" "n1" Calls
  ]

fixtureGraph :: Graph
fixtureGraph = buildGraph True (extractionFromLists fixtureNodes fixtureEdges)

emptyIndex :: GraphIndex
emptyIndex = GraphIndex Map.empty Map.empty Map.empty Map.empty

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

runQuery :: Text -> Int -> CypherResult
runQuery q budget =
  case parseQuery q of
    Left err  -> error ("parse error: " ++ show err)
    Right ast -> evaluate budget ast fixtureGraph emptyIndex

nodeProp :: Value -> Text -> Maybe Value
nodeProp (Object _) _ = Nothing
nodeProp _ _ = Nothing

-- ───────────────────────────────────────────────
-- Tests
-- ───────────────────────────────────────────────

spec :: Spec
spec = describe "Cypher Evaluator" $ do
  describe "node patterns" $ do
    it "matches nodes by label" $ do
      let r = runQuery "MATCH (n:Function) RETURN n" 100
      length (crRows r) `shouldBe` 4
      crTruncated r `shouldBe` False

    it "matches unlabelled nodes" $ do
      let r = runQuery "MATCH (n) RETURN n" 100
      length (crRows r) `shouldBe` 5

    it "matches no nodes for unknown label" $ do
      let r = runQuery "MATCH (n:Nonexistent) RETURN n" 100
      length (crRows r) `shouldBe` 0

  describe "relationship patterns" $ do
    it "matches directed relationships" $ do
      let r = runQuery "MATCH (a:Function)-[:Calls]->(b:Function) RETURN a, b" 100
      length (crRows r) `shouldBe` 3

    it "matches undirected relationships" $ do
      let r = runQuery "MATCH (a)-[:Calls]-(b) RETURN a, b" 100
      length (crRows r) `shouldBe` 6

    it "matches left-directed relationships" $ do
      let r = runQuery "MATCH (a)<-[:Calls]-(b) RETURN a, b" 100
      length (crRows r) `shouldBe` 3

  describe "variable-length paths" $ do
    it "matches 1-hop paths" $ do
      let r = runQuery "MATCH (a)-[:Calls*1..1]->(b) RETURN b" 100
      length (crRows r) `shouldBe` 3

    it "matches multi-hop paths" $ do
      let r = runQuery "MATCH (a)-[:Calls*1..2]->(b) RETURN b" 100
      length (crRows r) `shouldBe` 5

  describe "WHERE predicates" $ do
    it "filters by property CONTAINS" $ do
      let r = runQuery "MATCH (n:Function) WHERE n.source_file CONTAINS 'services' RETURN n" 100
      length (crRows r) `shouldBe` 3

    it "filters by property STARTS WITH" $ do
      let r = runQuery "MATCH (n:Function) WHERE n.source_file STARTS WITH 'src/core' RETURN n" 100
      length (crRows r) `shouldBe` 1

    it "filters by regex" $ do
      let r = runQuery "MATCH (n:Function) WHERE n.source_file =~ 'src/services/.*' RETURN n" 100
      length (crRows r) `shouldBe` 3

    it "excludes rows for unknown property" $ do
      let r = runQuery "MATCH (n) WHERE n.nonexistent = 'foo' RETURN n" 100
      length (crRows r) `shouldBe` 0

    it "filters by equality" $ do
      let r = runQuery "MATCH (n:Function) WHERE n.label = 'n1' RETURN n" 100
      length (crRows r) `shouldBe` 1

  describe "projection" $ do
    it "returns node properties as an object" $ do
      let r = runQuery "MATCH (n:Function) WHERE n.label = 'n1' RETURN n" 100
          row = case crRows r of
            [firstRow] -> firstRow
            _          -> []
          isObject = case row of
            [Object _] -> True
            _          -> False
      isObject `shouldBe` True

    it "returns a single property" $ do
      let r = runQuery "MATCH (n:Function) WHERE n.label = 'n1' RETURN n.label" 100
          row = case crRows r of
            [firstRow] -> firstRow
            _          -> []
      row `shouldBe` [String "n1"]

  describe "count" $ do
    it "counts all rows" $ do
      let r = runQuery "MATCH (n:Function) RETURN count(*)" 100
      crRows r `shouldBe` [[Number 4.0]]

    it "counts a variable" $ do
      let r = runQuery "MATCH (n:Function) RETURN count(n)" 100
      crRows r `shouldBe` [[Number 4.0]]

  describe "DISTINCT" $ do
    it "deduplicates rows" $ do
      let r = runQuery "MATCH (n:Function) RETURN DISTINCT n.file_type" 100
      length (crRows r) `shouldBe` 1

  describe "ORDER BY" $ do
    it "sorts by property" $ do
      let r = runQuery "MATCH (n:Function) RETURN n.label ORDER BY n.label" 100
          labels = [ v | row <- crRows r, [v] <- [row] ]
      labels `shouldBe` [String "n1", String "n2", String "n3", String "n5"]

  describe "SKIP / LIMIT" $ do
    it "applies SKIP and LIMIT" $ do
      let r = runQuery "MATCH (n:Function) RETURN n SKIP 1 LIMIT 2" 100
      length (crRows r) `shouldBe` 2

  describe "budget" $ do
    it "truncates results to budget" $ do
      let r = runQuery "MATCH (n) RETURN n" 2
      length (crRows r) `shouldBe` 2
      crTruncated r `shouldBe` True

    it "does not truncate when within budget" $ do
      let r = runQuery "MATCH (n:Function) RETURN n" 100
      crTruncated r `shouldBe` False
