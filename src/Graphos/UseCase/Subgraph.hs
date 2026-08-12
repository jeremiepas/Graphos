{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Graphos.UseCase.Subgraph
  ( extractSubgraph
  , SubgraphConfig(..)
  , SubsystemConfig(..)
  , SubgraphTier(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), withText)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Graphos.Domain.Graph.Index (matchGlob)
import Graphos.Domain.Types
import Graphos.Domain.Types.Graph (LabeledGraph(..))

-- | Tier classification for nodes in a subgraph.
data SubgraphTier = CoreTier | BoundaryTier | ExternalTier
  deriving (Eq, Show, Generic, Ord)

instance ToJSON SubgraphTier where
  toJSON = \case
    CoreTier     -> "core"
    BoundaryTier -> "boundary"
    ExternalTier -> "external"

instance FromJSON SubgraphTier where
  parseJSON = withText "SubgraphTier" $ \t -> case t of
    "core"     -> pure CoreTier
    "boundary" -> pure BoundaryTier
    "external" -> pure ExternalTier
    _          -> fail $ "Unknown SubgraphTier: " ++ T.unpack t

instance NFData SubgraphTier

-- | Configuration for a named subsystem.
data SubsystemConfig = SubsystemConfig
  { scSubsystemName     :: !Text
  , scSubsystemPatterns :: ![Text]
  } deriving (Eq, Show, Generic)

instance ToJSON SubsystemConfig
instance FromJSON SubsystemConfig

-- | Configuration for subgraph extraction.
data SubgraphConfig = SubgraphConfig
  { scSubsystems      :: ![SubsystemConfig]
  , scMaxHops         :: !Int
  , scIncludeDerived  :: !Bool
  } deriving (Eq, Show, Generic)

instance ToJSON SubgraphConfig
instance FromJSON SubgraphConfig

-- | Provenance for edges.
data EdgeProvenance = SourceGraph | Derived
  deriving (Eq, Show, Generic)

instance ToJSON EdgeProvenance where
  toJSON SourceGraph = "source"
  toJSON Derived     = "derived"

instance FromJSON EdgeProvenance where
  parseJSON = withText "EdgeProvenance" $ \t -> case t of
    "source" -> pure SourceGraph
    "derived" -> pure Derived
    _         -> fail $ "Unknown EdgeProvenance: " ++ T.unpack t

instance NFData EdgeProvenance

-- | Extracts a subgraph starting from the core subsystem nodes.
extractSubgraph :: LabeledGraph -> SubgraphConfig -> LabeledGraph
extractSubgraph g config =
  let
    allNodesMap = gNodes g
    allEdgesMap = gEdges g

    -- 1. Identify Core Nodes
    coreNodesSet = Set.fromList
      [ nid | (nid, n) <- Map.toList allNodesMap
      , any (\sub -> any (\pat -> matchGlob pat (T.toLower (nodeSourceFile n))) (scSubsystemPatterns sub)) (scSubsystems config)
      ]

    -- 2. Identify Boundary Nodes using BFS (up to maxHops)
    boundaryNodesSet =
      let
        bfs visited [] _ = visited
        bfs visited _ d | d >= scMaxHops config = visited
        bfs visited current d =
          let
            -- Get all neighbors (inbound or outbound) via 'Imports' edges
            nextNodes = Set.fromList
              [ target
              | src <- current
              , target <- getImportNeighbors src
              , not (target `Set.member` visited)
              ]
            -- Also need to check incoming imports
            incoming = Set.fromList
              [ source
              | target <- current
              , source <- getImportIncomingNeighbors target
              , not (source `Set.member` visited)
              ]
            allNext = nextNodes `Set.union` incoming
          in bfs (visited `Set.union` allNext) (Set.toList allNext) (d + 1)

        getImportNeighbors nid =
          [ edgeTarget edge
          | (_, edge) <- Map.toList allEdgesMap
          , edgeSource edge == nid
          , edgeRelation edge == Imports
          ]

        getImportIncomingNeighbors nid =
          [ edgeSource edge
          | (_, edge) <- Map.toList allEdgesMap
          , edgeTarget edge == nid
          , edgeRelation edge == Imports
          ]

        in bfs coreNodesSet (Set.toList coreNodesSet) 0

    -- 3. Identify External Nodes
    externalNodesSet =
      let
        boundaryOrCore = coreNodesSet `Set.union` boundaryNodesSet
        targets = Set.fromList
          [ edgeTarget edge
          | edge <- Map.elems allEdgesMap
          , edgeRelation edge == Imports
          , edgeSource edge `Set.member` boundaryOrCore
          ]
      in Set.difference targets boundaryOrCore

    -- 4. Collect all nodes and their metadata
    allSelectedNodes = coreNodesSet `Set.union` boundaryNodesSet `Set.union` externalNodesSet

    nodeMetadata nid =
      if nid `Set.member` coreNodesSet
      then let sub = findSubsystem nid in (CoreTier, sub)
      else if nid `Set.member` boundaryNodesSet
      then (BoundaryTier, Nothing)
      else (ExternalTier, Nothing)

    findSubsystem nid = case Map.lookup nid allNodesMap of
      Nothing -> Nothing
      Just n  ->
        let matches = [ scSubsystemName sub | sub <- scSubsystems config
                      , any (\pat -> matchGlob pat (T.toLower (nodeSourceFile n))) (scSubsystemPatterns sub)
                      ]
        in case matches of
             (m:_) -> Just m
             []    -> Nothing

    newNodesMap = Map.fromList
      [ (nid, n { nodeExtra = Just (object [ "tier" .= tier, "subsystem" .= sub ]) })
      | (nid, n) <- Map.toList allNodesMap
      , nid `Set.member` allSelectedNodes
      , let (tier, sub) = nodeMetadata nid
      ]

    newEdgesMap = Map.fromList
      [ (eid, edge { edgeExtra = Just (object [ "provenance" .= SourceGraph ]) })
      | (eid, edge) <- Map.toList allEdgesMap
      , edgeSource edge `Set.member` allSelectedNodes
      , edgeTarget edge `Set.member` allSelectedNodes
      ]

    newAdjFwd = Map.fromListWith Set.union
      [ (edgeSource e, Set.singleton (edgeTarget e))
      | e <- Map.elems newEdgesMap
      ]

    newAdjBack = Map.fromListWith Set.union
      [ (edgeTarget e, Set.singleton (edgeSource e))
      | e <- Map.elems newEdgesMap
      ]

  in LabeledGraph
    { gNodes = newNodesMap
    , gEdges = newEdgesMap
    , gAdjFwd = newAdjFwd
    , gAdjBack = newAdjBack
    }
