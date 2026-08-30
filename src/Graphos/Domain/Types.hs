-- | Core domain types for Graphos (re-export hub).
-- All types are defined in focused sub-modules; this module re-exports
-- everything for backward compatibility.
module Graphos.Domain.Types
  (     -- * Node types
    NodeId
  , Node(..)
  , FileType(..)
  , bitNodeLineStart, bitNodeLineEnd, bitNodeSignature
  , bitNodeCommunityId, bitNodeKind, bitNodeDegree
  , bitNodeIsBridge, bitNodeExtra
  , computePresentBits
  , isFieldPresent, setFieldPresent, clearFieldPresent

    -- * Edge types
  , EdgeId(..)
  , Edge(..)
  , Relation(..)
  , relationToText
  , textToRelation
  , Confidence(..)


    -- * graph.json contract
  , graphFileSchemaVersion
  , graphFileTopLevelKeys
  , graphFileRequiredKeys

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
  , ExclusionCounts(..)
  , emptyExclusionCounts
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

import Graphos.Domain.Types.Node
  ( NodeId, Node(..), FileType(..)
  , bitNodeLineStart, bitNodeLineEnd, bitNodeSignature
  , bitNodeCommunityId, bitNodeKind, bitNodeDegree
  , bitNodeIsBridge, bitNodeExtra
  , computePresentBits
  , isFieldPresent, setFieldPresent, clearFieldPresent
  )
import Graphos.Domain.Types.Writer (IncrementalWriter(..))
import Graphos.Domain.Types.Edge (EdgeId(..), Edge(..), Relation(..), relationToText, textToRelation, Confidence(..))
import Graphos.Domain.Types.GraphFile (graphFileSchemaVersion, graphFileTopLevelKeys, graphFileRequiredKeys)
import Graphos.Domain.Types.Graph (Extraction(..), emptyExtraction, extractionFromLists, extNodes, extEdges, LabeledGraph, CommunityId, CommunityMap, CohesionMap, PushMode(..), GraphDiff(..), Hyperedge(..))
import Graphos.Domain.Types.Pipeline (PipelineConfig(..), EdgeDensity(..), defaultConfig, Detection(..), ExclusionCounts(..), emptyExclusionCounts, FileCategory(..))
import Graphos.Domain.Types.Analysis (Analysis(..), GodNode(..), SurprisingConnection(..), SuggestedQuestion(..), CommunityAggregate(..))
import Graphos.Domain.Types.Ingest (IngestResult(..), IngestEmbedding(..), emptyIngestEmbedding, IngestIndex(..), emptyIngestIndex, addToIndex, lookupEmbedding, mergeIndex, lookupIndex, indexSize, isFileUpToDate)
import Graphos.Domain.Config (GraphosConfig(..), defaultGraphosConfig, Neo4jConfig(..), defaultNeo4jConfig, MemgraphConfig(..), defaultMemgraphConfig, LabelingConfig(..), defaultLabelingConfig, ObservabilityConfig(..), defaultObservabilityConfig, mergeGraphosConfig, mergeObservabilityConfig, ExtractorMode(..), ExtractorConfig(..), defaultExtractors, Granularity(..), defaultGranularity, EmbeddingConfig(..), defaultEmbeddingConfig, VisionConfig(..), defaultVisionConfig)
