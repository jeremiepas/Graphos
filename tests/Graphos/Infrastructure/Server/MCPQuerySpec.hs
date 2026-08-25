module Graphos.Infrastructure.Server.MCPQuerySpec where

import Test.Hspec
import Data.Aeson (Value(..), Object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Graphos.Domain.Types (Node(..), FileType(..), Edge(..), EdgeId(..)
                            , Relation(..), Confidence(..)
                            , extractionFromLists)
import Graphos.Domain.Graph (Graph, buildGraph)
import Graphos.Domain.Graph.Index (buildIndex)
import Graphos.Infrastructure.Server.MCP

mkNode :: Int -> T.Text -> Node
mkNode i label = Node
  { nodeId           = T.pack ("n" ++ show i)
  , nodeLabel        = label
  , nodeFileType     = CodeFile
  , nodeSourceFile   = "src/Test.hs"
  , nodeLineStart    = Just i
  , nodeLineEnd      = Nothing
  , nodeCommunityId  = Just 1
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  }

mkEdge :: Int -> T.Text -> T.Text -> Relation -> Edge
mkEdge i src tgt rel = Edge
  { edgeId        = EdgeId (T.pack ("e" ++ show i))
  , edgeSource    = src
  , edgeTarget    = tgt
  , edgeRelation  = rel
  , edgeWeight    = 1.0
  , edgeConfidence = Confidence 0.9
  , edgeExtra     = Nothing
  }

queryGraph :: Graph
queryGraph =
  let nodes = [ mkNode 1 "AuthModule"
              , mkNode 2 "AuthLogin"
              , mkNode 3 "Database"
              ]
      edges = [ mkEdge 1 "n1" "n2" Calls
              , mkEdge 2 "n2" "n3" Calls
              ]
      ext = extractionFromLists nodes edges
  in buildGraph False ext

mkArgs :: [(T.Text, Value)] -> KM.KeyMap Value
mkArgs pairs = KM.fromList [(Key.fromText k, v) | (k, v) <- pairs]

keyLookup :: T.Text -> Object -> Bool
keyLookup k obj = KM.member (Key.fromText k) obj

spec :: Spec
spec = describe "MCP query handler JSON shape" $ do
  let idx = buildIndex queryGraph Map.empty
  describe "handleQueryGraph" $ do
    it "returns verdict, best_score, hash, nodes, edges, omitted" $ do
      let args = mkArgs [("question", String "Auth")]
      result <- handleQueryGraph queryGraph idx args
      case result of
        Left err -> expectationFailure (T.unpack err)
        Right (Object obj) -> do
          keyLookup "verdict" obj `shouldBe` True
          keyLookup "best_score" obj `shouldBe` True
          keyLookup "hash" obj `shouldBe` True
          keyLookup "nodes" obj `shouldBe` True
          keyLookup "edges" obj `shouldBe` True
          keyLookup "omitted" obj `shouldBe` True
        Right _ -> expectationFailure "expected JSON object"

    it "returns none verdict and empty nodes/edges for unmatched query" $ do
      let args = mkArgs [("question", String "zzzznonexistent")]
      result <- handleQueryGraph queryGraph idx args
      case result of
        Left err -> expectationFailure (T.unpack err)
        Right (Object obj) -> do
          KM.lookup (Key.fromText "verdict") obj `shouldBe` Just (String "none")
          KM.lookup (Key.fromText "nodes") obj `shouldBe` Just (Array mempty)
          KM.lookup (Key.fromText "edges") obj `shouldBe` Just (Array mempty)
        Right _ -> expectationFailure "expected JSON object"

  describe "handleCypherQuery" $ do
    it "returns columns, rows, truncated for a valid query" $ do
      let args = mkArgs [("query", String "MATCH (n) RETURN n")]
      result <- handleCypherQuery queryGraph idx args
      case result of
        Left err -> expectationFailure (T.unpack err)
        Right (Object obj) -> do
          keyLookup "columns" obj `shouldBe` True
          keyLookup "rows" obj `shouldBe` True
          keyLookup "truncated" obj `shouldBe` True
          KM.lookup (Key.fromText "truncated") obj `shouldBe` Just (Bool False)
        Right _ -> expectationFailure "expected JSON object"

    it "errors when query is missing" $ do
      let args = mkArgs []
      result <- handleCypherQuery queryGraph idx args
      case result of
        Left err -> err `shouldBe` "Missing required argument: query"
        Right _  -> expectationFailure "expected Left"

    it "reports a parse error for out-of-subset queries" $ do
      let args = mkArgs [("query", String "CREATE (a)")]
      result <- handleCypherQuery queryGraph idx args
      case result of
        Left err -> T.isInfixOf "Cypher parse error" err `shouldBe` True
        Right _  -> expectationFailure "expected Left"
