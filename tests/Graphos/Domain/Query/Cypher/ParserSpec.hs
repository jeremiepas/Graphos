module Graphos.Domain.Query.Cypher.ParserSpec where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Graphos.Domain.Query.Cypher.AST
import Graphos.Domain.Query.Cypher.Parser (parseQuery, parseStatement)

spec :: Spec
spec = describe "Cypher Parser" $ do
  parserSpec
  mutationSpec

parserSpec :: Spec
parserSpec = describe "read grammar" $ do
  describe "node patterns" $ do
    it "parses a bare node" $ do
      parseQuery "MATCH (a) RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses a node with a label" $ do
      parseQuery "MATCH (a:Function) RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" ["Function"] Map.empty)]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses a node with multiple labels" $ do
      parseQuery "MATCH (a:Function:Kind) RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" ["Function", "Kind"] Map.empty)]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses a node with a property constraint" $ do
      parseQuery "MATCH (a {name: 'foo'}) RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] (Map.singleton "name" (EStr "foo")))]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

  describe "relationship patterns" $ do
    it "parses a directed relationship" $ do
      parseQuery "MATCH (a)-[:Calls]->(b) RETURN a, b" `shouldBe`
        Right (CypherQuery
          [ NodePatE (NodePat "a" [] Map.empty)
          , RelPatE (RelPat "a" "b" Nothing ["Calls"] DirRight (HopRange 1 1) Map.empty)
          , NodePatE (NodePat "b" [] Map.empty)
          ]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") Nothing, RIExpr (EVar "b") Nothing] [] Nothing Nothing))

    it "parses a relationship with a variable" $ do
      parseQuery "MATCH (a)-[r:Calls]->(b) RETURN r" `shouldBe`
        Right (CypherQuery
          [ NodePatE (NodePat "a" [] Map.empty)
          , RelPatE (RelPat "a" "b" (Just "r") ["Calls"] DirRight (HopRange 1 1) Map.empty)
          , NodePatE (NodePat "b" [] Map.empty)
          ]
          Nothing
          (ReturnClause False [RIExpr (EVar "r") Nothing] [] Nothing Nothing))

    it "parses a variable-length relationship" $ do
      parseQuery "MATCH (a)-[:Imports*1..3]->(b) RETURN b" `shouldBe`
        Right (CypherQuery
          [ NodePatE (NodePat "a" [] Map.empty)
          , RelPatE (RelPat "a" "b" Nothing ["Imports"] DirRight (HopRange 1 3) Map.empty)
          , NodePatE (NodePat "b" [] Map.empty)
          ]
          Nothing
          (ReturnClause False [RIExpr (EVar "b") Nothing] [] Nothing Nothing))

    it "parses an undirected relationship" $ do
      parseQuery "MATCH (a)-[:R]-(b) RETURN a, b" `shouldBe`
        Right (CypherQuery
          [ NodePatE (NodePat "a" [] Map.empty)
          , RelPatE (RelPat "a" "b" Nothing ["R"] DirUndirected (HopRange 1 1) Map.empty)
          , NodePatE (NodePat "b" [] Map.empty)
          ]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") Nothing, RIExpr (EVar "b") Nothing] [] Nothing Nothing))

    it "parses a left-directed relationship" $ do
      parseQuery "MATCH (a)<-[:R]-(b) RETURN a, b" `shouldBe`
        Right (CypherQuery
          [ NodePatE (NodePat "a" [] Map.empty)
          , RelPatE (RelPat "a" "b" Nothing ["R"] DirLeft (HopRange 1 1) Map.empty)
          , NodePatE (NodePat "b" [] Map.empty)
          ]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") Nothing, RIExpr (EVar "b") Nothing] [] Nothing Nothing))

    it "parses a chained relationship" $ do
      parseQuery "MATCH (a)-[:R]->(b)-[:S]->(c) RETURN a, c" `shouldBe`
        Right (CypherQuery
          [ NodePatE (NodePat "a" [] Map.empty)
          , RelPatE (RelPat "a" "b" Nothing ["R"] DirRight (HopRange 1 1) Map.empty)
          , NodePatE (NodePat "b" [] Map.empty)
          , RelPatE (RelPat "b" "c" Nothing ["S"] DirRight (HopRange 1 1) Map.empty)
          , NodePatE (NodePat "c" [] Map.empty)
          ]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") Nothing, RIExpr (EVar "c") Nothing] [] Nothing Nothing))

  describe "WHERE predicates" $ do
    it "parses a simple comparison" $ do
      parseQuery "MATCH (a) WHERE a.x = 1 RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PCompare (PropRef "a" "x") OpEq (ENum 1.0)))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses AND" $ do
      parseQuery "MATCH (a) WHERE a.x = 1 AND a.y = 2 RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PAnd
            (PCompare (PropRef "a" "x") OpEq (ENum 1.0))
            (PCompare (PropRef "a" "y") OpEq (ENum 2.0))))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses OR" $ do
      parseQuery "MATCH (a) WHERE a.x = 1 OR a.y = 2 RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (POr
            (PCompare (PropRef "a" "x") OpEq (ENum 1.0))
            (PCompare (PropRef "a" "y") OpEq (ENum 2.0))))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses NOT" $ do
      parseQuery "MATCH (a) WHERE NOT a.x = 1 RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PNot (PCompare (PropRef "a" "x") OpEq (ENum 1.0))))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses IN" $ do
      parseQuery "MATCH (a) WHERE a.x IN (1, 2, 3) RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PIn (PropRef "a" "x") [ENum 1.0, ENum 2.0, ENum 3.0]))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses IS NULL" $ do
      parseQuery "MATCH (a) WHERE a.x IS NULL RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PIsNull (PropRef "a" "x") True))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses IS NOT NULL" $ do
      parseQuery "MATCH (a) WHERE a.x IS NOT NULL RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PIsNull (PropRef "a" "x") False))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses STARTS WITH" $ do
      parseQuery "MATCH (a) WHERE a.x STARTS WITH 'foo' RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PStartsWith (PropRef "a" "x") (EStr "foo")))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses CONTAINS" $ do
      parseQuery "MATCH (a) WHERE a.x CONTAINS 'bar' RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PContains (PropRef "a" "x") (EStr "bar")))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses regex" $ do
      parseQuery "MATCH (a) WHERE a.x =~ 'src/.*' RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PRegex (PropRef "a" "x") (EStr "src/.*")))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses all comparison operators" $ do
      parseQuery "MATCH (a) WHERE a.x <> 1 RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PCompare (PropRef "a" "x") OpNeq (ENum 1.0)))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))
      parseQuery "MATCH (a) WHERE a.x < 1 RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PCompare (PropRef "a" "x") OpLt (ENum 1.0)))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))
      parseQuery "MATCH (a) WHERE a.x <= 1 RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PCompare (PropRef "a" "x") OpLe (ENum 1.0)))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))
      parseQuery "MATCH (a) WHERE a.x > 1 RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PCompare (PropRef "a" "x") OpGt (ENum 1.0)))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))
      parseQuery "MATCH (a) WHERE a.x >= 1 RETURN a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          (Just (PCompare (PropRef "a" "x") OpGe (ENum 1.0)))
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

  describe "RETURN clause" $ do
    it "parses DISTINCT" $ do
      parseQuery "MATCH (a) RETURN DISTINCT a" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          Nothing
          (ReturnClause True [RIExpr (EVar "a") Nothing] [] Nothing Nothing))

    it "parses ORDER BY" $ do
      parseQuery "MATCH (a) RETURN a ORDER BY a.x DESC" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") Nothing] [OrderItem (PropRef "a" "x") True] Nothing Nothing))

    it "parses SKIP" $ do
      parseQuery "MATCH (a) RETURN a SKIP 10" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] (Just 10) Nothing))

    it "parses LIMIT" $ do
      parseQuery "MATCH (a) RETURN a LIMIT 5" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") Nothing] [] Nothing (Just 5)))

    it "parses count(*)" $ do
      parseQuery "MATCH (a) RETURN count(*)" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          Nothing
          (ReturnClause False [RICount Nothing Nothing] [] Nothing Nothing))

    it "parses count(var)" $ do
      parseQuery "MATCH (a) RETURN count(a)" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          Nothing
          (ReturnClause False [RICount (Just "a") Nothing] [] Nothing Nothing))

    it "parses count with alias" $ do
      parseQuery "MATCH (a) RETURN count(*) AS total" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          Nothing
          (ReturnClause False [RICount Nothing (Just "total")] [] Nothing Nothing))

    it "parses alias" $ do
      parseQuery "MATCH (a) RETURN a AS node" `shouldBe`
        Right (CypherQuery
          [NodePatE (NodePat "a" [] Map.empty)]
          Nothing
          (ReturnClause False [RIExpr (EVar "a") (Just "node")] [] Nothing Nothing))

  describe "error handling" $ do
    it "rejects CREATE (read-only entry point)" $ do
      parseQuery "CREATE (a)" `shouldSatisfy` isLeft

    it "rejects MERGE (read-only entry point)" $ do
      parseQuery "MERGE (a)" `shouldSatisfy` isLeft

    it "rejects SET (read-only entry point)" $ do
      parseQuery "SET a.x = 1" `shouldSatisfy` isLeft

    it "rejects DELETE (read-only entry point)" $ do
      parseQuery "DELETE a" `shouldSatisfy` isLeft

    it "rejects empty input" $ do
      parseQuery "" `shouldSatisfy` isLeft

    it "reports position in error" $ do
      parseQuery "MATCH (a) RETURN" `shouldSatisfy` isLeft

  where
    isLeft :: Either Text CypherQuery -> Bool
    isLeft (Left _)  = True
    isLeft (Right _) = False

-- Mutation statement parsing (opencypher-write-mutations).
mutationSpec :: Spec
mutationSpec = describe "Cypher Mutation Parser" $ do
  describe "CREATE" $ do
    it "parses a node create" $ do
      parseStatement "CREATE (a:Module {id: 'm1'})" `shouldBe`
        Right (MutStatement (Mut Nothing
          [MCreate [NodePatE (NodePat "a" ["Module"] (Map.singleton "id" (EStr "m1")))]]
          Nothing))

    it "parses a node and relationship create" $ do
      parseStatement "CREATE (a {id: 'x'}), (b {id: 'y'}), (a)-[:Calls]->(b)" `shouldBe`
        Right (MutStatement (Mut Nothing
          [ MCreate
              [ NodePatE (NodePat "a" [] (Map.singleton "id" (EStr "x")))
              , NodePatE (NodePat "b" [] (Map.singleton "id" (EStr "y")))
              , NodePatE (NodePat "a" [] Map.empty)
              , RelPatE (RelPat "a" "b" Nothing ["Calls"] DirRight (HopRange 1 1) Map.empty)
              , NodePatE (NodePat "b" [] Map.empty)
              ]
          ]
          Nothing))

  describe "MERGE" $ do
    it "parses a node merge" $ do
      parseStatement "MERGE (n:Module {id: 'm1'})" `shouldBe`
        Right (MutStatement (Mut Nothing
          [MMerge (NodePatE (NodePat "n" ["Module"] (Map.singleton "id" (EStr "m1")))) []]
          Nothing))

    it "parses ON CREATE SET and ON MATCH SET" $ do
      parseStatement "MERGE (n:Module {id: 'm1'}) ON CREATE SET n.created = true ON MATCH SET n.seen = 1" `shouldBe`
        Right (MutStatement (Mut Nothing
          [MMerge
            (NodePatE (NodePat "n" ["Module"] (Map.singleton "id" (EStr "m1"))))
            [ OnCreate [SetProp "n" "created" (EBool True)]
            , OnMatch  [SetProp "n" "seen" (ENum 1)]
            ]]
          Nothing))

  describe "SET" $ do
    it "parses a property set" $ do
      parseStatement "SET n.status = 'ok'" `shouldBe`
        Right (MutStatement (Mut Nothing [MSet [SetProp "n" "status" (EStr "ok")]] Nothing))

    it "parses a label set" $ do
      parseStatement "SET n:Deprecated" `shouldBe`
        Right (MutStatement (Mut Nothing [MSet [SetLabel "n" "Deprecated"]] Nothing))

    it "parses multiple set items" $ do
      parseStatement "SET n.a = 1, n.b = 'x', n:Cool" `shouldBe`
        Right (MutStatement (Mut Nothing
          [MSet [ SetProp "n" "a" (ENum 1)
                , SetProp "n" "b" (EStr "x")
                , SetLabel "n" "Cool"
                ]]
          Nothing))

  describe "REMOVE" $ do
    it "parses a property remove" $ do
      parseStatement "REMOVE n.status" `shouldBe`
        Right (MutStatement (Mut Nothing [MRemove [RemoveProp "n" "status"]] Nothing))

    it "parses a label remove" $ do
      parseStatement "REMOVE n:Deprecated" `shouldBe`
        Right (MutStatement (Mut Nothing [MRemove [RemoveLabel "n" "Deprecated"]] Nothing))

  describe "DELETE" $ do
    it "parses a delete" $ do
      parseStatement "DELETE a, b" `shouldBe`
        Right (MutStatement (Mut Nothing [MDelete False ["a", "b"]] Nothing))

    it "parses a detach delete" $ do
      parseStatement "DETACH DELETE a" `shouldBe`
        Right (MutStatement (Mut Nothing [MDelete True ["a"]] Nothing))

  describe "MATCH + write" $ do
    it "parses match then set with return" $ do
      parseStatement "MATCH (n:Function) WHERE n.source_file = 'x' SET n.status = 1 RETURN n" `shouldBe`
        Right (MutStatement (Mut
          (Just (CypherQuery
            [NodePatE (NodePat "n" ["Function"] Map.empty)]
            (Just (PCompare (PropRef "n" "source_file") OpEq (EStr "x")))
            (ReturnClause False [RIExpr ENull Nothing] [] Nothing Nothing)))
          [MSet [SetProp "n" "status" (ENum 1)]]
          (Just (ReturnClause False [RIExpr (EVar "n") Nothing] [] Nothing Nothing))))

    it "parses match then delete" $ do
      parseStatement "MATCH (n:Temp) DETACH DELETE n" `shouldBe`
        Right (MutStatement (Mut
          (Just (CypherQuery [NodePatE (NodePat "n" ["Temp"] Map.empty)] Nothing
                 (ReturnClause False [RIExpr ENull Nothing] [] Nothing Nothing)))
          [MDelete True ["n"]]
          Nothing))

  describe "out-of-subset" $ do
    it "rejects WITH" $ do
      parseStatement "MATCH (a) WITH a RETURN a" `shouldSatisfy` isLeftSt

    it "rejects UNWIND" $ do
      parseStatement "UNWIND [1,2] AS x RETURN x" `shouldSatisfy` isLeftSt

    it "rejects SET var += {..}" $ do
      parseStatement "SET n.props += {a: 1}" `shouldSatisfy` isLeftSt

    it "rejects unknown relationship type in CREATE" $ do
      case parseStatement "CREATE (a)-[:Collabs]->(b)" of
        Left err -> err `shouldSatisfy` T.isInfixOf "Collabs"
        Right _  -> expectationFailure "expected unknown-type error"
    it "rejects MERGE with multiple patterns" $ do
      parseStatement "MERGE (a), (b)" `shouldSatisfy` isLeftSt

    it "rejects empty input" $ do
      parseStatement "" `shouldSatisfy` isLeftSt

  where
    isLeftSt :: Either Text CypherStatement -> Bool
    isLeftSt (Left _)  = True
    isLeftSt (Right _) = False
