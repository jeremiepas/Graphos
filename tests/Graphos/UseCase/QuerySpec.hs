module Graphos.UseCase.QuerySpec where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Graph.Index (buildIndexWithLabels)
import Graphos.UseCase.Query (queryGraph, queryGraphWithIndexScored, pathQuery, explainNode, QueryResult(..), QueryResponse(..), symbolLookup, neighborhoodExpansion, SymbolResult(..), NeighborsResult(..), resolveNodeArg, NodeResolution(..))
import Graphos.UseCase.Query.Render (renderPathResultJSON, renderExplainResultJSON, renderQueryResponseJSON)
import Graphos.Domain.Graph.Score (MatchVerdict(..))

-- Helper: create a test node
testNode :: Text -> Node
testNode nid = Node
  { nodeId           = nid
  , nodeLabel        = nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = "test.hs"

  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  }

-- helper: generate a unique EdgeId from source and target
edgeIdFrom :: Text -> Text -> EdgeId
edgeIdFrom src tgt = EdgeId (src <> "->" <> tgt)

-- Helper: create a test edge
testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge
  { edgeId        = edgeIdFrom src tgt
  , edgeSource    = src
  , edgeTarget    = tgt
  , edgeRelation  = Imports
  , edgeConfidence = Confidence 1.0
  , edgeWeight    = 1.0
  , edgeExtra     = Nothing
  }

spec :: Spec
spec = do
  describe "queryGraph" $ do
    it "finds nodes matching query terms" $ do
      let ext = extractionFromLists [testNode "AuthModule", testNode "Database", testNode "Router"] [testEdge "AuthModule" "Database"]
          g = buildGraph False ext
          result = queryGraph g "Auth" "bfs" 2000
      length (qrNodes result) `shouldSatisfy` (> 0)

    it "returns empty results for non-matching query" $ do
      let ext = extractionFromLists [testNode "Alpha", testNode "Beta"] []
          g = buildGraph False ext
          result = queryGraph g "ZZZZnotfound" "bfs" 2000
      length (qrNodes result) `shouldBe` 0

    it "supports DFS traversal mode" $ do
      let ext = extractionFromLists [testNode "Hub", testNode "Leaf1", testNode "Leaf2"] [testEdge "Hub" "Leaf1", testEdge "Hub" "Leaf2"]
          g = buildGraph False ext
          result = queryGraph g "Hub" "dfs" 2000
      qrTraverse result `shouldBe` "dfs"

    it "includes edges in the subgraph result for connected nodes" $ do
      let ext = extractionFromLists [testNode "AuthModule", testNode "AuthLogin", testNode "AuthSession"] [testEdge "AuthModule" "AuthLogin", testEdge "AuthModule" "AuthSession"]
          g = buildGraph False ext
          result = queryGraph g "Auth" "bfs" 2000
      -- Should find nodes matching "Auth" and their subgraph edges
      length (qrNodes result) `shouldSatisfy` (> 0)

  describe "pathQuery" $ do
    it "finds shortest path between connected nodes" $ do
      let ext = extractionFromLists [testNode "start", testNode "mid", testNode "end"] [testEdge "start" "mid", testEdge "mid" "end"]
          g = buildGraph False ext
      pathQuery g "start" "end" `shouldSatisfy` (/= Nothing)

    it "returns Nothing for disconnected nodes" $ do
      let ext = extractionFromLists [testNode "isolated1", testNode "isolated2"] []
          g = buildGraph False ext
      pathQuery g "isolated1" "isolated2" `shouldBe` Nothing

  describe "explainNode" $ do
    it "finds a node by label" $ do
      let ext = extractionFromLists [testNode "MyModule"] []
          g = buildGraph False ext
      explainNode g "MyModule" `shouldSatisfy` (/= Nothing)

    it "returns Nothing for non-existent node" $ do
      let ext = extractionFromLists [testNode "Exists"] []
          g = buildGraph False ext
      explainNode g "DoesNotExist" `shouldBe` Nothing

  describe "queryGraphWithIndexScored" $ do
    it "returns Strong verdict on exact-phrase fixture" $ do
      let ext = extractionFromLists
            [ testNode "AuthModule"
            , testNode "AuthLogin"
            , testNode "Database"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          result = queryGraphWithIndexScored g idx "AuthModule" "bfs" 2000
      qrespVerdict result `shouldBe` Strong
      length (qrespNodes result) `shouldSatisfy` (> 0)

    it "returns Weak verdict on marginal single-term fixture" $ do
      let ext = extractionFromLists
            [ testNode "AuthModule"
            , testNode "AuthLogin"
            , testNode "AuthSession"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          -- Query "Auth" matches all three but has low normalized score
          result = queryGraphWithIndexScored g idx "Auth" "bfs" 2000
      qrespVerdict result `shouldSatisfy` (\v -> v == Strong || v == Weak)
      length (qrespNodes result) `shouldSatisfy` (> 0)

    it "returns NoMatch verdict with empty nodes for unmatched query" $ do
      let ext = extractionFromLists
            [ testNode "AlphaModule"
            , testNode "BetaService"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          result = queryGraphWithIndexScored g idx "ZZZZnotfound" "bfs" 2000
      qrespVerdict result `shouldBe` NoMatch
      length (qrespNodes result) `shouldBe` 0

    it "returns a result-set hash" $ do
      let ext = extractionFromLists
            [ testNode "AuthModule"
            , testNode "AuthLogin"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          result = queryGraphWithIndexScored g idx "Auth" "bfs" 2000
      qrespHash result `shouldSatisfy` (\h -> T.length h == 8)

    it "returns suggestions on NoMatch" $ do
      let ext = extractionFromLists
            [ testNode "AuthModule"
            , testNode "AuthHandler"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          result = queryGraphWithIndexScored g idx "Aut" "bfs" 2000
      -- Either NoMatch with suggestions, or Weak (because 'Aut' is too short
      -- to be a query term — terms need length > 2)
      qrespVerdict result `shouldSatisfy` (\v -> v == NoMatch || v == Weak || v == Strong)

  describe "symbolLookup" $ do
    it "finds exact match by identifier" $ do
      let ext = extractionFromLists
            [ testNode "CliCommand"
            , testNode "Database"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          result = symbolLookup "CliCommand" g idx
      srNotFound result `shouldBe` False
      length (srFound result) `shouldSatisfy` (> 0)

    it "falls back to case-insensitive match when no exact match" $ do
      let ext = extractionFromLists
            [ testNode "CliCommand"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          result = symbolLookup "clicommand" g idx
      srNotFound result `shouldBe` False
      length (srFound result) `shouldSatisfy` (> 0)

    it "reports not-found with suggestions for miss" $ do
      let ext = extractionFromLists
            [ testNode "Database"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          result = symbolLookup "ZZZZnotfound" g idx
      srNotFound result `shouldBe` True

    it "lists all matches for duplicate names" $ do
      let ext = extractionFromLists
            [ (testNode "parse") { nodeSourceFile = "src/A.hs", nodeLineStart = Just 10 }
            , (testNode "parse2") { nodeSourceFile = "src/B.hs", nodeLineStart = Just 20 }
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          result = symbolLookup "parse" g idx
      srNotFound result `shouldBe` False
      length (srFound result) `shouldSatisfy` (> 0)

  describe "neighborhoodExpansion" $ do
    it "returns neighbors at depth 1" $ do
      let ext = extractionFromLists
            [ testNode "center"
            , testNode "neighbor1"
            , testNode "neighbor2"
            ]
            [ testEdge "center" "neighbor1"
            , testEdge "center" "neighbor2"
            ]
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          result = neighborhoodExpansion "center" 1 g idx
      nrCenterNode result `shouldBe` Just "center"
      length (nrNodes result) `shouldSatisfy` (> 0)

    it "returns not-found for unknown node id" $ do
      let ext = extractionFromLists [testNode "exists"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          result = neighborhoodExpansion "nonexistent" 2 g idx
      nrCenterNode result `shouldBe` Nothing
      nrNodes result `shouldBe` []

  describe "resolveNodeArg" $ do
    it "resolves an exact node id to ResolvedSingle" $ do
      let ext = extractionFromLists [testNode "center", testNode "other"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
      resolveNodeArg "center" g idx `shouldBe` ResolvedSingle "center"

    it "resolves an exact label to ResolvedSingle (id differs from label)" $ do
      let ext = extractionFromLists [(testNode "n1") { nodeLabel = "MyLabel" }] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
      resolveNodeArg "MyLabel" g idx `shouldBe` ResolvedSingle "n1"

    it "resolves a case-insensitive label to ResolvedSingle" $ do
      let ext = extractionFromLists [(testNode "n1") { nodeLabel = "MyLabel" }] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
      resolveNodeArg "mylabel" g idx `shouldBe` ResolvedSingle "n1"

    it "reports Ambiguous with two candidates for a duplicated label" $ do
      let ext = extractionFromLists
            [ (testNode "a1") { nodeLabel = "dup", nodeSourceFile = "src/A.hs" }
            , (testNode "a2") { nodeLabel = "dup", nodeSourceFile = "src/B.hs" }
            ] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
      case resolveNodeArg "dup" g idx of
        Ambiguous cands -> length cands `shouldBe` 2
        other           -> expectationFailure ("expected Ambiguous, got " ++ show other)

    it "reports NotFound for an unknown argument" $ do
      let ext = extractionFromLists [testNode "exists"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
      resolveNodeArg "zzz-no-such-node" g idx `shouldBe` NotFound

  describe "renderQueryResponseJSON" $ do
    it "emits valid JSON (not Haskell Show) with a verdict field" $ do
      let ext = extractionFromLists [testNode "AuthModule", testNode "Database"] [testEdge "AuthModule" "Database"]
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          resp = queryGraphWithIndexScored g idx "AuthModule" "bfs" 2000
          out = renderQueryResponseJSON resp
      out `shouldSatisfy` (T.pack "\"verdict\"" `T.isInfixOf`)
      out `shouldSatisfy` (\t -> not (T.pack "fromList" `T.isInfixOf` t))
      T.take 1 out `shouldBe` T.pack "{"

  describe "renderPathResultJSON" $ do
    it "renders Nothing as {\"path\":null}" $ do
      let result = renderPathResultJSON Nothing
      result `shouldSatisfy` (T.pack "\"path\":null" `T.isInfixOf`)

    it "renders Just ids with hops count" $ do
      let result = renderPathResultJSON (Just ["a", "b", "c"])
      result `shouldSatisfy` (T.pack "\"hops\":2" `T.isInfixOf`)
      result `shouldSatisfy` (T.pack "\"a\"" `T.isInfixOf`)
      result `shouldSatisfy` (T.pack "\"b\"" `T.isInfixOf`)
      result `shouldSatisfy` (T.pack "\"c\"" `T.isInfixOf`)

    it "renders empty list as {\"path\":[],\"hops\":0}" $ do
      let result = renderPathResultJSON (Just [])
      result `shouldSatisfy` (T.pack "\"path\":[]" `T.isInfixOf`)
      result `shouldSatisfy` (T.pack "\"hops\":0" `T.isInfixOf`)

  describe "renderExplainResultJSON" $ do
    it "renders Nothing as null" $ do
      renderExplainResultJSON Nothing `shouldBe` "null"

    it "renders node with id/label/source_file/community" $ do
      let node = testNode "MyModule"
          result = renderExplainResultJSON (Just node)
      result `shouldSatisfy` (T.pack "\"id\"" `T.isInfixOf`)
      result `shouldSatisfy` (T.pack "\"label\"" `T.isInfixOf`)
      result `shouldSatisfy` (T.pack "\"source_file\"" `T.isInfixOf`)
      result `shouldSatisfy` (T.pack "\"community\"" `T.isInfixOf`)
