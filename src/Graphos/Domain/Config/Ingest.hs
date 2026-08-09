-- | Ingest configuration types.
-- Pure data types — no IO. Config file loading lives in Infrastructure.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config.Ingest
  ( -- * Ingest configuration
    IngestConfig(..)
  , defaultIngestConfig

    -- * URL fetching configuration
  , IngestUrlConfig(..)
  , defaultIngestUrlConfig

    -- * Per-category overrides
  , IngestCategoryConfig(..)
  , defaultIngestCategoryConfig
  , IngestCategories(..)
  , defaultIngestCategories

    -- * File entry for deduplication index
  , FileEntry(..)

    -- * Merge helpers
  , mergeIngestConfig
  , mergeIngestCategories
  , mergeIngestCategoryConfig
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, withObject, (.:?), (.!=), object, (.=))
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import Data.Text (Text)
import GHC.Generics (Generic)

import Graphos.Domain.Config.Extraction (Granularity(..), lowerFirst)

-- ───────────────────────────────────────────────
-- Per-category configuration
-- ───────────────────────────────────────────────

-- | Per-category override for ingest settings.
-- Nothing fields inherit from the top-level ingest configuration.
data IngestCategoryConfig = IngestCategoryConfig
  { iccEmbed       :: Maybe Bool        -- ^ Override top-level embed for this category
  , iccGranularity :: Maybe Granularity -- ^ Override top-level granularity for this category
  } deriving (Eq, Show, Generic)

instance ToJSON IngestCategoryConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }

instance FromJSON IngestCategoryConfig where
  parseJSON = withObject "IngestCategoryConfig" $ \v -> IngestCategoryConfig
    <$> v .:? "embed"       .!= Nothing
    <*> v .:? "granularity" .!= Nothing

defaultIngestCategoryConfig :: IngestCategoryConfig
defaultIngestCategoryConfig = IngestCategoryConfig
  { iccEmbed       = Nothing
  , iccGranularity = Nothing
  }

-- | Optional per-category overrides. Absent categories inherit top-level values.
data IngestCategories = IngestCategories
  { icatCode   :: Maybe IngestCategoryConfig
  , icatDoc    :: Maybe IngestCategoryConfig
  , icatPaper  :: Maybe IngestCategoryConfig
  , icatImage  :: Maybe IngestCategoryConfig
  , icatVideo  :: Maybe IngestCategoryConfig
  , icatOffice :: Maybe IngestCategoryConfig
  } deriving (Eq, Show, Generic)

instance ToJSON IngestCategories where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 4 }

instance FromJSON IngestCategories where
  parseJSON = withObject "IngestCategories" $ \v -> IngestCategories
    <$> v .:? "code"   .!= Nothing
    <*> v .:? "doc"    .!= Nothing
    <*> v .:? "paper"  .!= Nothing
    <*> v .:? "image"  .!= Nothing
    <*> v .:? "video"  .!= Nothing
    <*> v .:? "office" .!= Nothing

defaultIngestCategories :: IngestCategories
defaultIngestCategories = IngestCategories
  { icatCode   = Nothing
  , icatDoc    = Nothing
  , icatPaper  = Nothing
  , icatImage  = Nothing
  , icatVideo  = Nothing
  , icatOffice = Nothing
  }

-- ───────────────────────────────────────────────
-- URL fetch configuration
-- ───────────────────────────────────────────────

-- | Settings for URL-based ingestion.
data IngestUrlConfig = IngestUrlConfig
  { iucTimeout   :: Int    -- ^ HTTP response timeout in seconds
  , iucUserAgent :: String -- ^ User-Agent header for HTTP requests
  , iucRetry     :: Int    -- ^ Number of retries on failure
  } deriving (Eq, Show, Generic)

instance ToJSON IngestUrlConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }

instance FromJSON IngestUrlConfig where
  parseJSON = withObject "IngestUrlConfig" $ \v -> IngestUrlConfig
    <$> v .:? "timeout"    .!= 30
    <*> v .:? "user_agent" .!= "graphos/0.1.0"
    <*> v .:? "retry"      .!= 1

defaultIngestUrlConfig :: IngestUrlConfig
defaultIngestUrlConfig = IngestUrlConfig
  { iucTimeout   = 30
  , iucUserAgent = "graphos/0.1.0"
  , iucRetry     = 1
  }

-- ───────────────────────────────────────────────
-- File entry for deduplication index
-- ───────────────────────────────────────────────

-- | Record of a previously ingested file for SHA256-based deduplication.
data FileEntry = FileEntry
  { feHash       :: !Text -- ^ SHA256 of source file content
  , feIngestedAt :: !Text -- ^ ISO 8601 timestamp
  } deriving (Eq, Show, Generic)

instance NFData FileEntry

instance ToJSON FileEntry where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 2 }

instance FromJSON FileEntry where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 2 }

-- ───────────────────────────────────────────────
-- Top-level ingest configuration
-- ───────────────────────────────────────────────

-- | Configuration for the single-file ingest workflow.
-- Built-in defaults are backward compatible: embedding is disabled unless
-- explicitly enabled in graphos.yaml or via the --embed CLI flag.
data IngestConfig = IngestConfig
  { icEmbed          :: Bool              -- ^ Default: False (backward compat)
  , icEmbedModel     :: Maybe String      -- ^ Nothing = inherit from embedding.model
  , icEmbedDimension :: Maybe Int        -- ^ Nothing = inherit from embedding.dimension
  , icMerge          :: Bool              -- ^ Merge into existing graph.json
  , icDeduplicate    :: Bool              -- ^ Skip unchanged files by SHA256
  , icResolution     :: Double            -- ^ Community resolution for ingest
  , icMinCommSize    :: Int               -- ^ Min community size for ingest
  , icMaxLeidenIter  :: Int               -- ^ Max Leiden iterations for ingest
  , icIndexPath      :: String            -- ^ Path to ingest index.json
  , icUrl            :: IngestUrlConfig   -- ^ URL fetch settings
  , icCategories     :: IngestCategories -- ^ Per-category overrides
  } deriving (Eq, Show, Generic)

instance ToJSON IngestConfig where
  toJSON cfg = object
    [ "embed" .= icEmbed cfg
    , "embed_model" .= icEmbedModel cfg
    , "embed_dimension" .= icEmbedDimension cfg
    , "merge" .= icMerge cfg
    , "deduplicate" .= icDeduplicate cfg
    , "resolution" .= icResolution cfg
    , "min_comm_size" .= icMinCommSize cfg
    , "max_leiden_iter" .= icMaxLeidenIter cfg
    , "index_path" .= icIndexPath cfg
    , "url" .= icUrl cfg
    , "categories" .= icCategories cfg
    ]

instance FromJSON IngestConfig where
  parseJSON = withObject "IngestConfig" $ \v -> IngestConfig
    <$> v .:? "embed"           .!= False
    <*> v .:? "embed_model"     .!= Nothing
    <*> v .:? "embed_dimension" .!= Nothing
    <*> v .:? "merge"           .!= True
    <*> v .:? "deduplicate"     .!= True
    <*> v .:? "resolution"      .!= 0.8
    <*> v .:? "min_comm_size"   .!= 2
    <*> v .:? "max_leiden_iter" .!= 20
    <*> v .:? "index_path"      .!= "graphos-out/index.json"
    <*> v .:? "url"             .!= defaultIngestUrlConfig
    <*> v .:? "categories"      .!= defaultIngestCategories

defaultIngestConfig :: IngestConfig
defaultIngestConfig = IngestConfig
  { icEmbed          = False
  , icEmbedModel     = Nothing
  , icEmbedDimension = Nothing
  , icMerge          = True
  , icDeduplicate    = True
  , icResolution     = 0.8
  , icMinCommSize    = 2
  , icMaxLeidenIter  = 20
  , icIndexPath      = "graphos-out/index.json"
  , icUrl            = defaultIngestUrlConfig
  , icCategories     = defaultIngestCategories
  }

-- ───────────────────────────────────────────────
-- Config merging
-- ───────────────────────────────────────────────

-- | Merge two IngestConfig values: project overrides global.
mergeIngestConfig :: IngestConfig -> IngestConfig -> IngestConfig
mergeIngestConfig global project = IngestConfig
  { icEmbed          = icEmbed project
  , icEmbedModel     = mergeMaybe (icEmbedModel global) (icEmbedModel project)
  , icEmbedDimension = mergeMaybe (icEmbedDimension global) (icEmbedDimension project)
  , icMerge          = icMerge project
  , icDeduplicate    = icDeduplicate project
  , icResolution     = icResolution project
  , icMinCommSize    = icMinCommSize project
  , icMaxLeidenIter  = icMaxLeidenIter project
  , icIndexPath      = icIndexPath project
  , icUrl            = mergeIngestUrlConfig (icUrl global) (icUrl project)
  , icCategories     = mergeIngestCategories (icCategories global) (icCategories project)
  }

mergeMaybe :: Maybe a -> Maybe a -> Maybe a
mergeMaybe _global (Just project) = Just project
mergeMaybe global Nothing         = global

mergeIngestUrlConfig :: IngestUrlConfig -> IngestUrlConfig -> IngestUrlConfig
mergeIngestUrlConfig global project = IngestUrlConfig
  { iucTimeout   = if iucTimeout project /= iucTimeout defaultIngestUrlConfig
                      then iucTimeout project
                      else iucTimeout global
  , iucUserAgent = if iucUserAgent project /= iucUserAgent defaultIngestUrlConfig
                      then iucUserAgent project
                      else iucUserAgent global
  , iucRetry     = if iucRetry project /= iucRetry defaultIngestUrlConfig
                      then iucRetry project
                      else iucRetry global
  }

mergeIngestCategories :: IngestCategories -> IngestCategories -> IngestCategories
mergeIngestCategories global project = IngestCategories
  { icatCode   = mergeIngestCategoryConfigMaybe (icatCode global) (icatCode project)
  , icatDoc    = mergeIngestCategoryConfigMaybe (icatDoc global) (icatDoc project)
  , icatPaper  = mergeIngestCategoryConfigMaybe (icatPaper global) (icatPaper project)
  , icatImage  = mergeIngestCategoryConfigMaybe (icatImage global) (icatImage project)
  , icatVideo  = mergeIngestCategoryConfigMaybe (icatVideo global) (icatVideo project)
  , icatOffice = mergeIngestCategoryConfigMaybe (icatOffice global) (icatOffice project)
  }

mergeIngestCategoryConfigMaybe :: Maybe IngestCategoryConfig -> Maybe IngestCategoryConfig -> Maybe IngestCategoryConfig
mergeIngestCategoryConfigMaybe _global (Just project) = Just project
mergeIngestCategoryConfigMaybe global Nothing         = global

mergeIngestCategoryConfig :: IngestCategoryConfig -> IngestCategoryConfig -> IngestCategoryConfig
mergeIngestCategoryConfig global project = IngestCategoryConfig
  { iccEmbed       = mergeMaybe (iccEmbed global) (iccEmbed project)
  , iccGranularity = mergeMaybe (iccGranularity global) (iccGranularity project)
  }
