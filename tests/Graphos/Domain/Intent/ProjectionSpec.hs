{-# LANGUAGE OverloadedStrings #-}
-- | QuickCheck well-formedness property tests for the intent projection (intent-graph-verification 3.2).
--
-- Verifies:
--   * every edge endpoint is declared in the projection (O2's hypothesis)
--   * NodeId is preserved verbatim (µ = identity across before/after)
--   * signature and body hash present for code nodes where LSP provided them
--   * loader rejects ill-formed input naming the dangling endpoint
module Graphos.Domain.Intent.ProjectionSpec where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText, toText)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)

import Graphos.Domain.Intent.Projection
import Graphos.Domain.Intent.Classifier (defaultClassifierConfig)
import Graphos.Domain.Types.Node
  ( Node(..), NodeId, FileType(..) )
import Graphos.Domain.Types.Edge
  ( Edge(..), EdgeId(..), Relation(..), Confidence(..) )
import Graphos.Domain.Graph.Core (Graph(..), buildGraph)
import Graphos.Domain.Types.Graph (extractionFromLists)

-- | Arbitrary NodeId generator
genNodeId :: Gen Text
genNodeId = T.pack <$> listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"))

-- | Arbitrary node generator
genNode :: Gen Node
genNode = do
  nid <- genNodeId
  lbl <- T.pack <$> listOf1 (elements ['a'..'z'])
  ft  <- elements [CodeFile, DocFile]
  src <- T.pack <$> listOf1 (elements ['a'..'z'])
  pure Node
    { nodeId           = nid
    , nodeLabel        = fromText lbl
    , nodeFileType     = ft
    , nodeSourceFile   = fromText src
    , nodeSource       = Nothing
    , nodeLineStart    = Nothing
    , nodeLineEnd      = Nothing
    , nodeSignature    = Nothing
    , nodeCommunityId  = Nothing
    , nodeKind         = Nothing
    , nodeDegree       = Nothing
    , nodeIsBridge     = Nothing
    , nodeExtra        = Nothing
    , nodeBodyHash     = Nothing
    , nodePresentBits  = 0
    }

-- | Arbitrary edge generator (may reference non-existent nodes for ill-formed tests)
genEdge :: [NodeId] -> Gen Edge
genEdge existingIds = do
  src <- frequency [ (3, elements existingIds), (1, genNodeId) ]
  tgt <- frequency [ (3, elements existingIds), (1, genNodeId) ]
  rel <- elements [Calls, Imports, References, Contains, DependsOn, Tests, Maps]
  pure Edge
    { edgeId        = EdgeId (src <> "->" <> tgt)
    , edgeSource    = src
    , edgeTarget    = tgt
    , edgeRelation  = rel
    , edgeWeight    = 1.0
    , edgeConfidence = Confidence 1.0
    , edgeExtra     = Nothing
    }

-- | Build a well-formed graph from nodes and edges
buildWellFormedGraph :: [Node] -> [Edge] -> Graph
buildWellFormedGraph nodes edges =
  buildGraph False $ extractionFromLists nodes edges

-- | Build an ill-formed graph with dangling edges
buildIllFormedGraph :: [Node] -> [Edge] -> Graph
buildIllFormedGraph nodes edges =
  let extraction = extractionFromLists nodes edges
  in Graph
       { gNodes         = extractionNodes extraction
       , gEdges         = extractionEdges extraction
       , gAdjFwd        = Map.empty
       , gAdjBack       = Map.empty
       , gDirected      = False
       , gCompositions  = Nothing
       , gHash          = ""
       , gEmbeddings    = Nothing
       , gEmbeddingsPath = Nothing
       }

spec :: Spec
spec = do
  describe "3.2 projection well-formedness" $ do
    it "well-formed projection: every edge endpoint declared" $
      property $ \nodes edges ->
        let nodeIds = map nodeId nodes
            validEdges = filter (\e -> edgeSource e `elem` nodeIds && edgeTarget e `elem` nodeIds) edges
            g = buildWellFormedGraph nodes validEdges
            proj = projectionFromGraph defaultClassifierConfig g
        in checkWellFormed proj === Right []

    it "NodeId preserved verbatim (µ = identity) after round-trip" $
      property $ \nodes edges ->
        let nodeIds = map nodeId nodes
            validEdges = filter (\e -> edgeSource e `elem` nodeIds && edgeTarget e `elem` nodeIds) edges
            g = buildWellFormedGraph nodes validEdges
            proj = projectionFromGraph defaultClassifierConfig g
            g' = projectionToGraph proj
        in all (\nid -> Map.member nid (gNodes g) == Map.member nid (gNodes g'))
               (Map.keys (gNodes g))

    it "signature and body hash present for code nodes where LSP provided them" $
      property $ \nodes ->
        let codeNodes = [ n | n <- nodes, nodeFileType n == CodeFile ]
            withSig = [ n | n <- codeNodes, isJust (nodeSignature n) ]
            withHash = [ n | n <- codeNodes, isJust (nodeBodyHash n) ]
            g = buildWellFormedGraph nodes []
            proj = projectionFromGraph defaultClassifierConfig g
        in all (\n ->
                 case Map.lookup (nodeId n) (intentNodes proj) of
                   Just pn ->
                     (isJust (nodeSignature n) == isJust (intentSignature pn)) &&
                     (isJust (nodeBodyHash n) == isJust (intentBodyHash pn))
                   Nothing -> False
               ) codeNodes

    it "loader rejects ill-formed input naming the dangling endpoint" $
      property $ \nodes edges ->
        let nodeIds = map nodeId nodes
            -- Create at least one dangling edge
            danglingEdge = Edge
              { edgeId        = EdgeId "dangling->target"
              , edgeSource    = "dangling"
              , edgeTarget    = "target"
              , edgeRelation  = Calls
              , edgeWeight    = 1.0
              , edgeConfidence = Confidence 1.0
              , edgeExtra     = Nothing
              }
            allEdges = danglingEdge : edges
            g = buildIllFormedGraph nodes allEdges
            proj = projectionFromGraph defaultClassifierConfig g
        in case checkWellFormed proj of
             Left err -> "dangling" `T.isInfixOf` err
             Right _  -> False

    it "projectionToGraph drops dangling edges from ill-formed projection" $
      property $ \nodes edges ->
        let nodeIds = map nodeId nodes
            danglingEdge = Edge
              { edgeId        = EdgeId "dangling->target"
              , edgeSource    = "dangling"
              , edgeTarget    = "target"
              , edgeRelation  = Calls
              , edgeWeight    = 1.0
              , edgeConfidence = Confidence 1.0
              , edgeExtra     = Nothing
              }
            allEdges = danglingEdge : edges
            g = buildIllFormedGraph nodes allEdges
            proj = projectionFromGraph defaultClassifierConfig g
            g' = projectionToGraph proj
        in not (any (\e -> edgeSource e == "dangling" || edgeTarget e == "dangling")
                      (Map.elems (gEdges g')))

    it "projection is deterministic: same input yields byte-stable output" $
      property $ \nodes edges ->
        let nodeIds = map nodeId nodes
            validEdges = filter (\e -> edgeSource e `elem` nodeIds && edgeTarget e `elem` nodeIds) edges
            g = buildWellFormedGraph nodes validEdges
            proj1 = projectionFromGraph defaultClassifierConfig g
            proj2 = projectionFromGraph defaultClassifierConfig g
        in proj1 === proj2

-- | QuickCheck instances for Hspec integration
instance Arbitrary Node where
  arbitrary = genNode
  shrink n = [ n { nodeLabel = fromText (T.pack s) } | s <- shrink (T.unpack (toText (nodeLabel n))) ]

instance Arbitrary Edge where
  arbitrary = genEdge []
  shrink e = [ e { edgeSource = s } | s <- shrink (edgeSource e) ]