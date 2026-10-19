{-# LANGUAGE OverloadedStrings #-}
-- | Hspec suite for intent-graph-verification 3.2 (projection).
--
-- Domain assertions: ids pass through verbatim (\mu = identity), signature and
-- body hash are carried for code nodes, and the projection is well-formed by
-- construction. Export/load assertions exercise the Aeson instances and the
-- checkWellFormed gate (a dangling edge endpoint is rejected).
module Graphos.Domain.Intent.ProjectionSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Short (fromText)
import Test.Hspec

import Graphos.Domain.Intent.Classifier (defaultClassifierConfig)
import Graphos.Domain.Intent.Projection
  ( IntentNode(..), IntentProjection(..)
  , projectionFromGraph, projectionToGraph, checkWellFormed )
import Graphos.Domain.Types.Node (Node(..), NodeId, FileType(..))
import Graphos.Domain.Types.Edge (Edge(..), Relation(..), EdgeId(..), Confidence(..))

-- | A code node exposing only the fields we set.
mkNodeCode :: Text -> Text -> Maybe Text -> Maybe Text -> Node
mkNodeCode nid lbl sig bh = Node
  { nodeId = nid
  , nodeLabel = fromText lbl
  , nodeFileType = CodeFile
  , nodeSourceFile = fromText ("f_" <> nid <> ".hs")
  , nodeLineStart = Nothing
  , nodeLineEnd = Nothing
  , nodeSignature = fmap fromText sig
  , nodeCommunityId = Nothing
  , nodeKind = Just (fromText "Variable")
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodeBodyHash = bh
  , nodePresentBits = 0
  }

-- | A directed edge with a generated id, unit weight/confidence.
mkEdge :: Text -> Text -> Relation -> Edge
mkEdge a b r = Edge
  { edgeId        = EdgeId (a <> "->" <> b)
  , edgeSource    = a
  , edgeTarget    = b
  , edgeRelation  = r
  , edgeWeight    = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra       = Nothing
  }

-- | Build a small directed graph from node/edge lists.
graph :: Bool -> [Node] -> [Edge] -> Graph
graph directed nodes edges = buildGraph directed $ extractionFromLists nodes edges

spec :: Spec
spec = do
  describe "3.2 projection (domain)" $ do
    it "preserves NodeId verbatim across before/after (\mu = identity)" $ do
      let before = graph False
            [ mkNodeCode "n0" "old" (Just "def a") (Just "h0"),
              mkNodeCode "n1" "x" Nothing Nothing ]
            [ mkEdge "n0" "n1" Calls ]
          after  = graph False
            [ mkNodeCode "n0" "changed" (Just "def a2") (Just "h1"),
              mkNodeCode "n1" "y" Nothing Nothing ]
            [ mkEdge "n0" "n1" Calls ]
      let pb = sort (projectionNodeIsets (projectGraph defaultClassifierConfig before))
          pa = sort (projectionNodeIsets (projectGraph defaultClassifierConfig after))
      pb `shouldBe` pa
      pb `shouldBe` ["n0", "n1"]

    it "carries signature and body hash for a code node" $ do
      let proj = projectGraph defaultClassifierConfig
             (graph False [mkNodeCode "n0" "foo" (Just "def foo") (Just "deadbeef")] [])
          node = Map.lookup "n0" (intentNodes proj)
      node `shouldBe` Just (IntentNode
        { intentNodeId = "n0"
        , intentLabel = "foo"
        , intentKind = Other
        , intentSourceFile = "f_n0.hs"
        , intentSignature = Just "def foo"
        , intentBodyHash = Just "deadbeef"
        , intentLineStart = Nothing
        , intentLineEnd = Nothing
        })

    it "is well-formed by construction (every edge endpoint is declared)" $ do
      let proj = projectGraph defaultClassifierConfig
             (graph False
                [ mkNodeCode "n0" "a" Nothing Nothing
                , mkNodeCode "n1" "b" Nothing Nothing ]
                [ mkEdge "n0" "n1" Calls ])
      checkWellFormed proj `shouldBe` Right []

  describe "3.2 projection (export/load)" $ do
    it "round-trips the Aeson export/import for a well-formed projection" $ do
      let proj = projectGraph defaultClassifierConfig
             (graph False [mkNodeCode "n0" "foo" (Just "def foo") (Just "h1")] [])
      Aeson.decode (Aeson.encode proj) `shouldBe` Just proj

    it "loader rejects a dangling edge endpoint, naming it" $ do
      let illformed = Aeson.object
            [ "nodes" .= [ Aeson.object
              [ "id" .= "n0", "label" .= "a", "kind" .= "other",
                "source_file" .= "a.hs",
                "line_start" .= (Nothing :: Maybe Int),
                "line_end" .= (Nothing :: Maybe Int) ] ]
          , "edges" .= [ Aeson.object
              [ "source" .= "n0", "target" .= "ghost", "relation" .= "calls" ] ] ]
      case Aeson.decode (Aeson.encode illformed) :: Maybe IntentProjection of
        Just p -> case checkWellFormed p of
                   Left msg  -> T.isInfixOf "ghost" msg `shouldBe` True
                   Right _   -> expectationFailure "expected dangling endpoint to be rejected"
        Nothing -> expectationFailure "expected ill-formed projection to decode"
