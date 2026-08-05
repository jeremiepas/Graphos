-- | Domain configuration types for Graphos.
-- Re-export module — all types are defined in focused sub-modules.
-- Pure data types — no IO. Config file loading lives in Infrastructure.
module Graphos.Domain.Config
  ( -- * Extractor mode
    ExtractorMode(..)
  , ExtractorConfig(..)
  , defaultExtractors

    -- * Extraction granularity
  , Granularity(..)
  , defaultGranularity

    -- * LSP configuration
  , LSPServerConfig(..)
  , defaultLSPServers
  , defaultLanguageIds

    -- * File extension configuration
  , FileExtensionConfig(..)
  , defaultFileExtensions

     -- * Neo4j configuration
  , Neo4jConfig(..)
  , defaultNeo4jConfig

     -- * Memgraph configuration
  , MemgraphConfig(..)
  , defaultMemgraphConfig

     -- * LLM labeling configuration
  , LabelingConfig(..)
  , defaultLabelingConfig

     -- * Observability configuration
  , ObservabilityConfig(..)
  , defaultObservabilityConfig
  , OtelConfig(..)
  , defaultOtelConfig

      -- * Embedding configuration
  , EmbeddingConfig(..)
  , defaultEmbeddingConfig

     -- * Vision configuration
  , VisionConfig(..)
  , defaultVisionConfig

      -- * Top-level configuration
  , GraphosConfig(..)
  , defaultGraphosConfig

     -- * Config merging
  , mergeGraphosConfig
  , mergeObservabilityConfig
  ) where

import Graphos.Domain.Config.Core
import Graphos.Domain.Config.Extraction
import Graphos.Domain.Config.Export
import Graphos.Domain.Config.Observability
import Graphos.Domain.Config.Vision