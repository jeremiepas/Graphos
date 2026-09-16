{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.InferBetweennessWiringSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Short (fromText)

import Graphos.Domain.Types
import Graphos.Domain.Graph
import Graphos.UseCase.Infer (classifyBridgeNodes, classifyBridgeNodesWith, BridgeClassification(..))

wireNode :: Text -> Node
wireNode nid = Node
  { nodeId = nid
  , nodeLabel = fromText nid
  , nodeFileType = CodeFile
  , nodeSourceFile = fromText "t.hs"
  , nodeCommunityId = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodeLineStart = Just 1
  , nodeLineEnd = Nothing
  , nodeKind = Nothing
  , nodeSignature = Nothing
  , nodePresentBits = 0
  }

wireEdge :: Text -> Text -> Edge
wireEdge s t = Edge
  { edgeId = EdgeId (s <> "->" <> t)
  , edgeSource = s
  , edgeTarget = t
  , edgeRelation = Calls
  , edgeConfidence = Confidence 1.0
  , edgeWeight = 1.0
  , edgeExtra = Nothing
  }

-- Undirected path a-b-c-d; articulation points {b, c}.
pathG :: Graph
pathG = buildGraph False $ extractionFromLists
  (map wireNode ["a", "b", "c", "d"])
  [ wireEdge "a" "b", wireEdge "b" "c", wireEdge "c" "d" ]

commMap :: CommunityMap
commMap = Map.fromList [(1, ["a", "b"]), (2, ["c", "d"])]

betweenByNode :: [BridgeClassification] -> Map.Map NodeId Double
betweenByNode cs = Map.fromList [(bcNodeId c, bcBetweenness c) | c <- cs]

incidentScore :: NodeId -> Map.Map (NodeId, NodeId) Double -> Double
incidentScore nid score = sum [sc | ((s, t), sc) <- Map.toList score, s == nid || t == nid]

spec :: Spec
spec = do
  describe "AVI-512 classifyBridgeNodes wires default PipelineConfig caps into edge betweenness" $ do
    it "aggregates bcBetweenness from edgeBetweennessWith(defaultMaxSampledSources, defaultExactBetweennessNodeCap)" $ do
      let cs      = classifyBridgeNodes pathG commMap
          between = edgeBetweennessWith defaultMaxSampledSources defaultExactBetweennessNodeCap pathG
      Map.findWithDefault 0 "b" (betweenByNode cs) `shouldBe` incidentScore "b" between
    it "only articulation points are classified with nonzero bridge scores" $ do
      let cs  = classifyBridgeNodes pathG commMap
          nids = map bcNodeId cs
          byN  = betweenByNode cs
      nids `shouldBe` ["b", "c"]
      Map.findWithDefault 0 "b" byN `shouldSatisfy` (> 0)

  describe "AVI-512 classifyBridgeNodesWith propagates PipelineConfig caps (wiring completeness)" $ do
    it "wires cfgMaxSampledSources / cfgExactBetweennessNodeCap from PipelineConfig into edgeBetweennessWith" $ do
      let cfg       = defaultConfig { cfgMaxSampledSources = 7, cfgExactBetweennessNodeCap = 3 }
          cs        = classifyBridgeNodesWith cfg pathG commMap
          between   = edgeBetweennessWith (cfgMaxSampledSources cfg) (cfgExactBetweennessNodeCap cfg) pathG
      Map.findWithDefault 0 "b" (betweenByNode cs) `shouldBe` incidentScore "b" between
    it "changes runtime betweenness when caps force sampled rather than exact mode" $ do
      let exactCfg  = defaultConfig { cfgExactBetweennessNodeCap = 10000 } -- n=4 <= cap => exact (s=4)
          sampleCfg = defaultConfig { cfgExactBetweennessNodeCap = 1, cfgMaxSampledSources = 2 } -- n=4 > cap => sampled (s=2)
          a         = betweenByNode (classifyBridgeNodesWith exactCfg pathG commMap)
          b         = betweenByNode (classifyBridgeNodesWith sampleCfg pathG commMap)
      a `shouldNotBe` b
