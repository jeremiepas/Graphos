-- | Core domain types for Graphos (re-export hub).
-- All types are defined in focused sub-modules; this module re-exports
-- everything for backward compatibility.
module Graphos.Domain.Types
  ( -- * Node types
    NodeId
  , Node(..)
  , FileType(..)

    -- * Edge types
  , EdgeId(..)
  , Edge(..)
  , Relation(..)
  , relationToText
  , textToRelation
  , Confidence(..)


    -- * Extraction types
  , Extraction(..)
  , emptyExtraction
  , extractionFromLists
  , extNodes
  , extEdges

      -- * Graph types
    , LabeledGraph


    -- * Community types
  , CommunityId
  , CommunityMap
  , CohesionMap

    -- * Push mode types
  , PushMode(..)

    -- * Analysis types
  , Analysis(..)
  , GodNode(..)
  , SurprisingConnection(..)
  , SuggestedQuestion(..)
  , GraphDiff(..)
  , CommunityAggregate(..)

    -- * Hyperedge types (legacy)
  , Hyperedge(..)

    -- * Detection types
  , Detection(..)
  , FileCategory(..)

    -- * Ingest types
  , IngestResult(..)
  , IngestEmbedding(..)
  , emptyIngestEmbedding
  , IngestIndex(..)
  , emptyIngestIndex
  , addToIndex
  , lookupEmbedding
  , mergeIndex
  , lookupIndex
  , isFileUpToDate
  , indexSize

    -- * Configuration
  , PipelineConfig(..)
  , EdgeDensity(..)
  , defaultConfig
  , GraphosConfig(..)
  , defaultGraphosConfig
  , Neo4jConfig(..)
  , defaultNeo4jConfig
  , MemgraphConfig(..)
  , defaultMemgraphConfig
  , LabelingConfig(..)
  , defaultLabelingConfig
  , ObservabilityConfig(..)
  , defaultObservabilityConfig
  , mergeGraphosConfig
  , mergeObservabilityConfig
  , ExtractorMode(..)
  , ExtractorConfig(..)
  , defaultExtractors
  , Granularity(..)
  , defaultGranularity
  , EmbeddingConfig(..)
  , defaultEmbeddingConfig
  , VisionConfig(..)
  , defaultVisionConfig

    -- * Incremental writer handle
  , IncrementalWriter(..)
  ) where

import Graphos.Domain.Types.Node (NodeId, Node(..), FileType(..))
import Graphos.Domain.Types.Writer (IncrementalWriter(..))
import Graphos.Domain.Types.Edge (EdgeId(..), Edge(..), Relation(..), relationToText, textToRelation, Confidence(..))
import Graphos.Domain.Types.Graph (Extraction(..), emptyExtraction, extractionFromLists, extNodes, extEdges, LabeledGraph, CommunityId, CommunityMap, CohesionMap, PushMode(..), GraphDiff(..), Hyperedge(..))
import Graphos.Domain.Types.Pipeline (PipelineConfig(..), EdgeDensity(..), defaultConfig, Detection(..), FileCategory(..))
import Graphos.Domain.Types.Analysis (Analysis(..), GodNode(..), SurprisingConnection(..), SuggestedQuestion(..), CommunityAggregate(..))
import Graphos.Domain.Types.Ingest (IngestResult(..), IngestEmbedding(..), emptyIngestEmbedding, IngestIndex(..), emptyIngestIndex, addToIndex, lookupEmbedding, mergeIndex, lookupIndex, indexSize, isFileUpToDate)
import Graphos.Domain.Config (GraphosConfig(..), defaultGraphosConfig, Neo4jConfig(..), defaultNeo4jConfig, MemgraphConfig(..), defaultMemgraphConfig, LabelingConfig(..), defaultLabelingConfig, ObservabilityConfig(..), defaultObservabilityConfig, mergeGraphosConfig, mergeObservabilityConfig, ExtractorMode(..), ExtractorConfig(..), defaultExtractors, Granularity(..), defaultGranularity, EmbeddingConfig(..), defaultEmbeddingConfig, VisionConfig(..), defaultVisionConfig)
