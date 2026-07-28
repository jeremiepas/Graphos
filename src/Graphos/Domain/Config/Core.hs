-- | Top-level Graphos configuration and merging.
-- Pure data types — no IO. Config file loading lives in Infrastructure.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config.Core
  ( -- * Top-level configuration
    GraphosConfig(..)
  , defaultGraphosConfig

    -- * Config merging
  , mergeGraphosConfig
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

import Graphos.Domain.Config.Extraction
import Graphos.Domain.Config.Export
import Graphos.Domain.Config.Observability (ObservabilityConfig(..), defaultObservabilityConfig, mergeObservabilityConfig)
import Graphos.Domain.Config.Vision

-- ───────────────────────────────────────────────
-- Top-level Configuration
-- ───────────────────────────────────────────────

-- | Top-level Graphos configuration.
-- Loaded from graphos.yaml, with defaults for missing fields.
data GraphosConfig = GraphosConfig
  { gcLsp            :: Map String LSPServerConfig  -- ^ extension → LSP server config
  , gcLanguageIds    :: Map String Text              -- ^ extension → language ID
  , gcFileExtensions :: FileExtensionConfig          -- ^ file extension categories
  , gcExtractors     :: Map String ExtractorConfig  -- ^ extension → extractor config
  , gcGranularity    :: Granularity                  -- ^ global extraction granularity
  , gcNeo4j          :: Neo4jConfig                  -- ^ Neo4j connection settings
  , gcMemgraph       :: MemgraphConfig               -- ^ Memgraph connection settings
  , gcLabeling       :: LabelingConfig               -- ^ LLM labeling settings
  , gcObservability  :: ObservabilityConfig           -- ^ Tracing, metrics, debug settings
  , gcEmbedding      :: EmbeddingConfig               -- ^ Local embedding settings (Ollama)
  , gcVision         :: VisionConfig                  -- ^ Vision analysis settings
  } deriving (Eq, Show, Generic)

-- | Default Graphos configuration (used when no config file is found).
defaultGraphosConfig :: GraphosConfig
defaultGraphosConfig = GraphosConfig
  { gcLsp            = defaultLSPServers
  , gcLanguageIds    = defaultLanguageIds
  , gcFileExtensions = defaultFileExtensions
  , gcExtractors     = defaultExtractors
  , gcGranularity    = defaultGranularity
  , gcNeo4j          = defaultNeo4jConfig
  , gcMemgraph       = defaultMemgraphConfig
  , gcLabeling       = defaultLabelingConfig
  , gcObservability  = defaultObservabilityConfig
  , gcEmbedding      = defaultEmbeddingConfig
  , gcVision         = defaultVisionConfig
  }

-- ───────────────────────────────────────────────
-- Config merging (global + project + CLI)
-- ───────────────────────────────────────────────

-- | Merge two GraphosConfig values: project overrides global.
--
-- Merge rules:
--   * Maps (LSP, language IDs, extractors): 'Map.union', project wins on key collision
--   * Scalar sections (Neo4j, Labeling, Observability): project wins if it differs
--     from defaults; otherwise global wins
--   * File extensions: full override (project wins if set)
mergeGraphosConfig :: GraphosConfig -> GraphosConfig -> GraphosConfig
mergeGraphosConfig global project = GraphosConfig
  { gcLsp = Map.union (gcLsp project) (gcLsp global)
  , gcLanguageIds = Map.union (gcLanguageIds project) (gcLanguageIds global)
  , gcFileExtensions = if gcFileExtensions project == defaultFileExtensions
                          then gcFileExtensions global
                          else gcFileExtensions project
  , gcExtractors = Map.union (gcExtractors project) (gcExtractors global)
  , gcGranularity = if gcGranularity project == defaultGranularity
                       then gcGranularity global
                       else gcGranularity project
  , gcNeo4j = if gcNeo4j project == defaultNeo4jConfig
                 then gcNeo4j global
                 else gcNeo4j project
  , gcMemgraph = if gcMemgraph project == defaultMemgraphConfig
                   then gcMemgraph global
                   else gcMemgraph project
  , gcLabeling = if gcLabeling project == defaultLabelingConfig
                   then gcLabeling global
                   else gcLabeling project
  , gcObservability = mergeObservabilityConfig (gcObservability global)
                                                (gcObservability project)
  , gcEmbedding = if gcEmbedding project == defaultEmbeddingConfig
                     then gcEmbedding global
                     else gcEmbedding project
  , gcVision = if gcVision project == defaultVisionConfig
                  then gcVision global
                  else gcVision project
  }