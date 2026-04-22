-- | Core domain types for Graphos (re-export hub).
-- All types are defined in focused sub-modules; this module re-exports
-- everything for backward compatibility.
module Graphos.Domain.Types
  ( -- * Node types
    NodeId
  , Node(..)
  , FileType(..)

    -- * Edge types
  , EdgeId
  , Edge(..)
  , Relation(..)
  , relationToText
  , textToRelation
  , Confidence(..)
  , confidenceScore

    -- * Hyperedge types
  , Hyperedge(..)

    -- * Extraction types
  , Extraction(..)
  , emptyExtraction

    -- * Graph types
  , LabeledGraph(..)

    -- * Community types
  , CommunityId
  , CommunityMap
  , CohesionMap

    -- * Analysis types
  , Analysis(..)
  , GodNode(..)
  , SurprisingConnection(..)
  , SuggestedQuestion(..)
  , GraphDiff(..)

    -- * Detection types
  , Detection(..)
  , FileCategory(..)

    -- * Configuration
  , PipelineConfig(..)
  , EdgeDensity(..)
  , defaultConfig
  , GraphosConfig(..)
  , defaultGraphosConfig
  ) where

import Graphos.Domain.Types.Node (NodeId, Node(..), FileType(..))
import Graphos.Domain.Types.Edge (EdgeId, Edge(..), Relation(..), relationToText, textToRelation, Confidence(..), confidenceScore)
import Graphos.Domain.Types.Graph (Hyperedge(..), Extraction(..), emptyExtraction, LabeledGraph(..), CommunityId, CommunityMap, CohesionMap, GraphDiff(..))
import Graphos.Domain.Types.Pipeline (PipelineConfig(..), EdgeDensity(..), defaultConfig, Detection(..), FileCategory(..))
import Graphos.Domain.Types.Analysis (Analysis(..), GodNode(..), SurprisingConnection(..), SuggestedQuestion(..))
import Graphos.Domain.Config (GraphosConfig(..), defaultGraphosConfig)