module Graphos.Domain.Graph.ScoreSpec where

import Test.Hspec
import Data.Aeson (Value(..), ToJSON(toJSON))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Graph.Index (buildIndexWithLabels)
import Graphos.Domain.Graph.Score
import Graphos.UseCase.Query (queryGraphWithIndexScored)

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

spec :: Spec
spec = do
  describe "findSuggestions" $ do
    it "returns nearby vocabulary token for a misspelled query term" $ do
      let ext = extractionFromLists
            [ testNode "Database"
            , testNode "Handler"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          suggestions = findSuggestions ["databas"] idx
      suggestions `shouldSatisfy` (not . null)

    it "returns empty suggestions when no indexed token is within edit distance bound" $ do
      let ext = extractionFromLists
            [ testNode "Alpha"
            , testNode "Beta"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          suggestions = findSuggestions ["zzzzzzzfaraway"] idx
      suggestions `shouldBe` []

    it "returns up to 10 suggestions" $ do
      let nodes = [ testNode (T.pack $ "Node" ++ show i) | i <- [(0::Int)..20] ]
          ext = extractionFromLists nodes []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          suggestions = findSuggestions ["node0"] idx
      length suggestions `shouldSatisfy` (<= 10)

  describe "resultHash" $ do
    it "returns identical hash for identical input lists" $ do
      let ids = ["node-a", "node-b", "node-c"] :: [NodeId]
      resultHash ids `shouldBe` resultHash ids

    it "returns different hashes for reordered input lists" $ do
      let hash1 = resultHash ["node-a", "node-b"]
          hash2 = resultHash ["node-b", "node-a"]
      hash1 `shouldNotBe` hash2

    it "returns different hashes for completely different node sets" $ do
      let hash1 = resultHash ["alpha", "beta"]
          hash2 = resultHash ["gamma", "delta"]
      hash1 `shouldNotBe` hash2

    it "returns different hash from empty list hash" $ do
      let hash1 = resultHash ["node-a"]
          hash2 = resultHash []
      hash1 `shouldNotBe` hash2

  describe "QueryResponse JSON shape" $ do
    it "emits verdict, best_score, hash, nodes, edges" $ do
      let ext = extractionFromLists [ testNode "AuthModule" ] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          r = queryGraphWithIndexScored g idx "AuthModule" "bfs" 2000
      case toJSON r of
        Object obj -> do
          KM.member "verdict" obj `shouldBe` True
          KM.member "best_score" obj `shouldBe` True
          KM.member "hash" obj `shouldBe` True
          KM.member "nodes" obj `shouldBe` True
          KM.member "edges" obj `shouldBe` True
        _ -> expectationFailure "expected JSON Object"

    it "none-verdict response has empty nodes and edges" $ do
      let ext = extractionFromLists [ testNode "AuthModule" ] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          r = queryGraphWithIndexScored g idx "zzzznonexistent" "bfs" 2000
      case toJSON r of
        Object _ -> pure ()
        _ -> expectationFailure "expected JSON Object"
      qrespVerdict r `shouldBe` NoMatch
      qrespNodes r `shouldBe` []
      qrespEdges r `shouldBe` []

  describe "queryGraphWithIndexScored (hash determinism)" $ do
    it "identical query on same graph yields identical hash" $ do
      let ext = extractionFromLists
            [ testNode "AuthModule"
            , testNode "AuthLogin"
            , testNode "Database"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          r1 = queryGraphWithIndexScored g idx "Auth" "bfs" 2000
          r2 = queryGraphWithIndexScored g idx "Auth" "bfs" 2000
      qrespHash r1 `shouldBe` qrespHash r2

    it "different queries yield different hashes" $ do
      let ext = extractionFromLists
            [ testNode "AuthModule"
            , testNode "Database"
            ]
            []
          g = buildGraph False ext
          idx = buildIndexWithLabels g Map.empty Map.empty
          r1 = queryGraphWithIndexScored g idx "AuthModule" "bfs" 2000
          r2 = queryGraphWithIndexScored g idx "Database" "bfs" 2000
      qrespHash r1 `shouldNotBe` qrespHash r2
