-- | Top-level Graphos configuration and merging.
-- Pure data types — no IO. Config file loading lives in Infrastructure.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config.Core
  ( -- * Top-level configuration
    GraphosConfig(..)
  , defaultGraphosConfig

    -- * Config merging
  , mergeGraphosConfig

    -- * Output directory resolution
  , effectiveOutputDir
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

import Graphos.Domain.Config.Extraction ( PdfExtractionMode(..)
                                        , defaultPdfExtractionMode
                                        , ExtractorConfig(..)
                                        , Granularity(..)
                                        , LSPServerConfig(..)
                                        , FileExtensionConfig(..)
                                        , defaultLSPServers
                                        , defaultLanguageIds
                                        , defaultExtractors
                                        , defaultFileExtensions
                                        , defaultGranularity
                                        )
import Graphos.Domain.Config.Export
import Graphos.Domain.Config.Ingest (IngestConfig(..), defaultIngestConfig, mergeIngestConfig)
import Graphos.Domain.Config.Cache (CacheConfig(..), defaultCacheConfig)
import Graphos.Domain.Config.Memory (MemoryConfig(..), defaultMemoryConfig)
import Graphos.Domain.Config.Observability (ObservabilityConfig(..), defaultObservabilityConfig, mergeObservabilityConfig)
import qualified Graphos.Domain.Config.Output
import Graphos.Domain.Config.Vision
import Graphos.Domain.Config.Detection (DetectionConfig(..), defaultDetectionConfig)
import Graphos.Domain.Config.Source (SourceConfig(..))

-- ───────────────────────────────────────────────
-- Top-level Configuration
-- ───────────────────────────────────────────────

-- | Top-level Graphos configuration.
-- Loaded from graphos.yaml, with defaults for missing fields.
data GraphosConfig = GraphosConfig
  { gcLsp              :: Map String LSPServerConfig  -- ^ extension → LSP server config
  , gcLanguageIds      :: Map String Text              -- ^ extension → language ID
  , gcFileExtensions   :: FileExtensionConfig          -- ^ file extension categories
  , gcExtractors       :: Map String ExtractorConfig  -- ^ extension → extractor config
  , gcGranularity      :: Granularity                  -- ^ global extraction granularity
  , gcPdfExtraction    :: PdfExtractionMode            -- ^ PDF extraction aggressiveness
  , gcNeo4j            :: Neo4jConfig                  -- ^ Neo4j connection settings
  , gcMemgraph         :: MemgraphConfig               -- ^ Memgraph connection settings
  , gcLabeling         :: LabelingConfig               -- ^ LLM labeling settings
  , gcObservability    :: ObservabilityConfig           -- ^ Tracing, metrics, debug settings
  , gcEmbedding        :: EmbeddingConfig               -- ^ Local embedding settings (Ollama)
  , gcSemanticEdges    :: SemanticEdgesConfig           -- ^ Semantic code↔doc edge inference settings
  , gcVision           :: VisionConfig                  -- ^ Vision analysis settings
   , gcIngest           :: IngestConfig                  -- ^ Single-file ingest settings
   , gcDetection        :: DetectionConfig               -- ^ Generated/vendored/minified code detection settings
   , gcMemory           :: MemoryConfig                  -- ^ Memory budget settings (memory.budget: auto|off|<size>)
   , gcCache            :: CacheConfig                   -- ^ Content-cache eviction policy (cache.max_mb; 0 = unlimited)
   , gcSources          :: [SourceConfig]                -- ^ Named multi-source roots ([] = single positional path)
   , gcOutput           :: Maybe FilePath                -- ^ Graph output directory override (Nothing = "graphos-out")
   } deriving (Eq, Show, Generic)

-- | Default Graphos configuration (used when no config file is found).
defaultGraphosConfig :: GraphosConfig
defaultGraphosConfig = GraphosConfig
  { gcLsp              = defaultLSPServers
  , gcLanguageIds      = defaultLanguageIds
  , gcFileExtensions   = defaultFileExtensions
  , gcExtractors       = defaultExtractors
  , gcGranularity      = defaultGranularity
  , gcPdfExtraction    = defaultPdfExtractionMode
  , gcNeo4j            = defaultNeo4jConfig
  , gcMemgraph         = defaultMemgraphConfig
  , gcLabeling         = defaultLabelingConfig
  , gcObservability    = defaultObservabilityConfig
  , gcEmbedding        = defaultEmbeddingConfig
  , gcSemanticEdges    = defaultSemanticEdgesConfig
   , gcVision           = defaultVisionConfig
   , gcIngest           = defaultIngestConfig
   , gcDetection        = defaultDetectionConfig
   , gcMemory           = defaultMemoryConfig
   , gcCache            = defaultCacheConfig
   , gcSources          = []
   , gcOutput           = Nothing
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
  , gcPdfExtraction = if gcPdfExtraction project == defaultPdfExtractionMode
                         then gcPdfExtraction global
                         else gcPdfExtraction project
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
  , gcSemanticEdges = if gcSemanticEdges project == defaultSemanticEdgesConfig
                         then gcSemanticEdges global
                         else gcSemanticEdges project
  , gcVision = if gcVision project == defaultVisionConfig
                    then gcVision global
                    else gcVision project
   , gcIngest = mergeIngestConfig (gcIngest global) (gcIngest project)
   , gcDetection = if gcDetection project == defaultDetectionConfig
                     then gcDetection global
                     else gcDetection project
   , gcMemory = if gcMemory project == defaultMemoryConfig
                   then gcMemory global
                   else gcMemory project
   , gcCache = if gcCache project == defaultCacheConfig
                  then gcCache global
                  else gcCache project
   , gcSources = if null (gcSources project)
                   then gcSources global
                   else gcSources project
   , gcOutput = maybe (gcOutput global) Just (gcOutput project)
   }
-- ───────────────────────────────────────────────
-- Output directory resolution
-- ───────────────────────────────────────────────

-- | Effective graph output directory for a loaded config (multi-source-graphs
-- 2.2): the graphos.yaml @output:@ key when set, the canonical default
-- otherwise. CLI precedence is layered at the call site via
-- 'Graphos.Domain.Config.Output.resolveOutputDir' (an explicit @-o@ wins over
-- this value).
effectiveOutputDir :: GraphosConfig -> FilePath
effectiveOutputDir cfg = case gcOutput cfg of
  Just out -> out
  Nothing  -> Graphos.Domain.Config.Output.defaultOutputDirName

-- ───────────────────────────────────────────────
-- YAML serialization (graphos init write path)
-- ───────────────────────────────────────────────

-- | The config-as-YAML key layout for @graphos init@ (multi-source-graphs
-- 2.3). Every key matches what the FromJSON parsers read back (ConfigFile's
-- section keys in Infrastructure.Config; each section's own field keys), so a
-- written file reloads to an Equal config. 'gcDetection' is omitted: it has
-- no ToJSON instance and its FromJSON default-fills everything the loader
-- needs.
instance ToJSON GraphosConfig where
  toJSON cfg = object
    [ "lsp"             .= gcLsp cfg
    , "language_ids"    .= gcLanguageIds cfg
    , "file_extensions" .= gcFileExtensions cfg
    , "extractors"      .= gcExtractors cfg
    , "granularity"     .= gcGranularity cfg
    , "pdf_extraction"  .= gcPdfExtraction cfg
    , "neo4j"           .= gcNeo4j cfg
    , "memgraph"        .= gcMemgraph cfg
    , "labeling"        .= gcLabeling cfg
    , "observability"   .= gcObservability cfg
    , "embedding"       .= gcEmbedding cfg
    , "semantic_edges"  .= gcSemanticEdges cfg
    , "vision"          .= gcVision cfg
    , "ingest"          .= gcIngest cfg
    , "memory"          .= gcMemory cfg
    , "cache"           .= gcCache cfg
    , "sources"         .= gcSources cfg
    , "output"          .= gcOutput cfg
    ]
