{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Graphos.UseCase.PipelineIncrementalSpec where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph
import Graphos.Domain.Config (SemanticEdgesConfig(..), defaultSemanticEdgesConfig)
import Graphos.Domain.Community (Resolution(..), defaultResolution)
import Graphos.Domain.Types.Pipeline (EdgeDensity(..))
import Graphos.UseCase.Pipeline.Incremental (clusterAndInfer, cleanInferred)

node :: Text -> Node
node nid = Node
  { nodeId           = nid
  , nodeLabel        = fromText (T.toUpper nid)
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "test.hs"
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodePresentBits  = 0
  }

edge :: Relation -> Text -> Text -> Edge
edge rel s t = Edge
  { edgeId        = EdgeId (s <> "--" <> t)
  , edgeSource    = s
  , edgeTarget    = t
  , edgeRelation  = rel
  , edgeWeight    = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra     = Nothing
  }

spec :: Spec
spec = do
  describe "incremental merge-before-build confluence (AVI-559 / G2)" $ do
    it "folding a changed-file batch into the retained graph equals a full build" $ do
      let dir = False
          a = node "a"; b = node "b"; c = node "c"
          eAB = edge Calls "a" "b"
          eBC = edge Calls "b" "c"

          fullSrc  = extractionFromLists [a, b, c] [eAB, eBC]
          fullBuild = buildGraph dir fullSrc

          oldSrc  = extractionFromLists [a, b] [eAB]
          baseSrc = extractionFromLists [b, c] [eBC]
          merged  = mergeGraphs (buildGraph dir oldSrc) (buildGraph dir baseSrc)

      putStrLn ("MERGED adjFwd: " ++ show (Map.elems (gAdjFwd merged)))
      putStrLn ("MERGED adjBack: " ++ show (Map.elems (gAdjBack merged)))
      putStrLn ("FULLB  adjFwd: " ++ show (Map.elems (gAdjFwd fullBuild)))
      putStrLn ("FULLB  adjBack: " ++ show (Map.elems (gAdjBack fullBuild)))
      putStrLn ("MERGED edges: " ++ show (Map.elems (gEdges merged)))
      putStrLn ("FULLB  edges: " ++ show (Map.elems (gEdges fullBuild)))
      gNodes merged `shouldBe` gNodes fullBuild
      gEdges merged `shouldBe` gEdges fullBuild
      gDirected merged `shouldBe` gDirected fullBuild

    it "cluster+infer on the merged graph equals cluster+infer on the full build" $ do
      let dir = False
          a = node "a"; b = node "b"; c = node "c"
          eAB = edge Calls "a" "b"
          eBC = edge Calls "b" "c"

          fullSrc  = extractionFromLists [a, b, c] [eAB, eBC]
          fullBuild = buildGraph dir fullSrc

          oldSrc  = extractionFromLists [a, b] [eAB]
          baseSrc = extractionFromLists [b, c] [eBC]
          merged  = mergeGraphs (buildGraph dir oldSrc) (buildGraph dir baseSrc)

          res     = defaultResolution { resMaxIterations = 1 }
          seCfg   = defaultSemanticEdgesConfig { seEnabled = False }
          force   = False
          enrichedMerged = clusterAndInfer res seCfg force Normal dir merged
          enrichedFull   = clusterAndInfer res seCfg force Normal dir fullBuild
      enrichedMerged `shouldBe` enrichedFull

    it "cleanInferred drops Inferred edges while keeping structural ones" $ do
      let dir = False
          a = node "a"; b = node "b"; c = node "c"
          g = buildGraph dir $ extractionFromLists [a, b, c]
                [edge Calls "a" "b", edge Inferred "b" "c"]
          kept = Map.elems (gEdges (cleanInferred g))
      map edgeRelation kept `shouldBe` [Calls]
