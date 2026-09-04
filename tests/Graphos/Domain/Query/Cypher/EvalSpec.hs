module Graphos.Domain.Query.Cypher.EvalSpec where

import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText, toText)
import Test.Hspec

import Graphos.Domain.Types
  ( Node(..), Edge(..), EdgeId(..)
  , FileType(CodeFile)
  , Relation(Calls, Imports)
  , Confidence(..)
  , extractionFromLists
  )
import Graphos.Domain.Graph (Graph, buildGraph, gNodes, gEdges)
import Graphos.Domain.Graph.Index (GraphIndex(..))
import Graphos.Domain.Graph.Mutation (nodeExtraLabels, MutationSummary(..))
import Graphos.Domain.Query.Cypher.Mapping (nodeProperty, nodeCypherLabel)
import Graphos.Domain.Query.Cypher.Parser (parseQuery, parseStatement)
import Graphos.Domain.Query.Cypher.Eval (evaluate, evaluateStatement, CypherResult(..), MutationResult(..))
import Graphos.Domain.Query.Cypher.AST (CypherStatement(..))

-- ───────────────────────────────────────────────
-- Fixture
-- ───────────────────────────────────────────────

mkNode :: Text -> Text -> Text -> Node
mkNode nid kind srcFile = Node
  { nodeId          = nid
  , nodeLabel       = fromText nid
  , nodeFileType    = CodeFile
  , nodeSourceFile  = fromText srcFile
  , nodeLineStart   = Just 1
  , nodeLineEnd     = Just 10
  , nodeSignature   = Nothing
  , nodeCommunityId = Nothing
  , nodeKind        = Just (fromText kind)
  , nodeDegree      = Nothing
  , nodeIsBridge    = Nothing
  , nodeExtra       = Nothing
  , nodePresentBits = 0
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
spec = do
  evalSpec
  mutationSpec

evalSpec :: Spec
evalSpec = describe "Cypher Evaluator" $ do
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

-- Mutation evaluation (opencypher-write-mutations).
mutationSpec :: Spec
mutationSpec = describe "Cypher Mutation Evaluator" $ do
  let runMutOk :: Text -> MutationResult
      runMutOk q = case parseStatement q of
        Left err -> error ("parse error: " ++ show err)
        Right st -> case evaluateStatement 100 st fixtureGraph emptyIndex of
          Left err -> error ("mutation error: " ++ T.unpack err)
          Right mr -> mr
      runMut :: Text -> Either Text MutationResult
      runMut = \q -> case parseStatement q of
        Left err -> error ("parse error: " ++ show err)
        Right st -> evaluateStatement 100 st fixtureGraph emptyIndex
  describe "CREATE" $ do
    it "creates a node and counts it" $ do
      let mr = runMutOk "CREATE (m:Module {id: 'm9', label: 'New'})"
      msNodesCreated (mrSummary mr) `shouldBe` 1
      Map.member "m9" (gNodes (mrGraph mr)) `shouldBe` True

    it "rejects a duplicate id" $ do
      let r = runMut "CREATE (m:Module {id: 'n1'})"
      r `shouldSatisfy` isLeftMr

    it "generates an id when absent" $ do
      let mr = runMutOk "CREATE (x:Temp)"
      Map.member "gen-x-5" (gNodes (mrGraph mr)) `shouldBe` True

    it "creates nodes and a relationship" $ do
      let mr = runMutOk "CREATE (a {id: 'x1'}), (b {id: 'x2'}), (a)-[:Calls]->(b)"
      msNodesCreated (mrSummary mr) `shouldBe` 2
      msRelsCreated (mrSummary mr) `shouldBe` 1
      Map.member ("x1", "x2") (gEdges (mrGraph mr)) `shouldBe` True

    it "upserts an existing pair edge" $ do
      let mr = runMutOk "MATCH (n1), (n2) WHERE n1.id = 'n1' AND n2.id = 'n2' CREATE (n1)-[:Calls {weight: 2}]->(n2)"
      msRelsCreated (mrSummary mr) `shouldBe` 0
      msRelsUpserted (mrSummary mr) `shouldBe` 1

  describe "MERGE" $ do
    it "matches an existing node and applies ON MATCH SET" $ do
      let mr = runMutOk "MERGE (m:Function {id: 'n1'}) ON MATCH SET m.label = 'Touched'"
      msNodesCreated (mrSummary mr) `shouldBe` 0
      Map.lookup "n1" (gNodes (mrGraph mr)) `shouldSatisfy`
        (\mn -> maybe False (\n -> toText (nodeLabel n) == "Touched") mn)

    it "creates a missing node with ON CREATE SET" $ do
      let mr = runMutOk "MERGE (m:Module {id: 'm9'}) ON CREATE SET m.label = 'Created'"
      msNodesCreated (mrSummary mr) `shouldBe` 1
      Map.lookup "m9" (gNodes (mrGraph mr)) `shouldSatisfy`
        (\mn -> maybe False (\n -> toText (nodeLabel n) == "Created") mn)

  describe "SET" $ do
    it "sets a model field" $ do
      let mr = runMutOk "MATCH (n) WHERE n.id = 'n1' SET n.label = 'Renamed'"
      msPropertiesSet (mrSummary mr) `shouldBe` 1
      Map.lookup "n1" (gNodes (mrGraph mr)) `shouldSatisfy`
        (\mn -> maybe False (\n -> toText (nodeLabel n) == "Renamed") mn)

    it "stores a non-model property in the extra object" $ do
      let mr = runMutOk "MATCH (n) WHERE n.id = 'n1' SET n.review_status = 'approved'"
      Map.lookup "n1" (gNodes (mrGraph mr)) `shouldSatisfy`
        (\mn -> maybe False (\n -> nodeProperty n "review_status" == Just (String "approved")) mn)

    it "adds an extra label queryable by MATCH" $ do
      let mr = runMutOk "MATCH (n) WHERE n.id = 'n1' SET n:Deprecated"
      nodeExtraLabels (mrGraph mr `at` "n1") `shouldBe` ["Deprecated"]
      let cr = case parseStatement "MATCH (n:Deprecated) RETURN n.id" of
            Right (MutStatement _) -> error "unexpected mutation"
            Right (ReadStatement q) -> evaluate 100 q (mrGraph mr) emptyIndex
            Left e -> error (show e)
      crRows cr `shouldBe` [[String "n1"]]

  describe "REMOVE" $ do
    it "removes the primary label leaving an unlabelled node" $ do
      let mr = runMutOk "MATCH (n) WHERE n.id = 'n1' REMOVE n:Function"
      nodeCypherLabel (mrGraph mr `at` "n1") `shouldBe` Nothing
      Map.member "n1" (gNodes (mrGraph mr)) `shouldBe` True

  describe "DELETE" $ do
    it "errors on delete with relationships without DETACH" $ do
      runMut "MATCH (n)-[r]->() WHERE n.id = 'n1' DELETE n" `shouldSatisfy`
        either (T.isInfixOf "DETACH") (const False)

    it "detach deletes nodes and incident edges" $ do
      let mr = runMutOk "MATCH (n) WHERE n.id = 'n4' DETACH DELETE n"
      msNodesDeleted (mrSummary mr) `shouldBe` 1
      Map.member "n4" (gNodes (mrGraph mr)) `shouldBe` False
      Map.member ("n4", "n1") (gEdges (mrGraph mr)) `shouldBe` False

  describe "RETURN after mutation" $ do
    it "projects post-mutation values" $ do
      let mr = runMutOk "MATCH (n) WHERE n.id = 'n1' SET n.status = 7 RETURN n.status"
      crColumns (mrResult mr) `shouldBe` ["n.status"]
      crRows (mrResult mr) `shouldBe` [[Number 7]]

    it "exposes summary counters" $ do
      let mr = runMutOk "CREATE (x:Temp) RETURN nodes_created"
      crRows (mrResult mr) `shouldBe` [[Number 1]]

  where
    at g nid = maybe (error "missing node") id (Map.lookup nid (gNodes g))
    isLeftMr :: Either Text MutationResult -> Bool
    isLeftMr (Left _)  = True
    isLeftMr (Right _) = False
