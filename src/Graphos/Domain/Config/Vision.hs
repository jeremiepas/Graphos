-- | Vision and embedding configuration types.
-- VisionConfig, EmbeddingConfig, LabelingConfig, and their defaults.
-- Pure data types — no IO.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config.Vision
  ( VisionConfig(..)
  , defaultVisionConfig
  , EmbeddingConfig(..)
  , defaultEmbeddingConfig
  , LabelingConfig(..)
  , defaultLabelingConfig
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, withObject, (.:?), (.!=))
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import Graphos.Domain.Config.Extraction (lowerFirst)
import GHC.Generics (Generic)

-- ───────────────────────────────────────────────
-- LLM Labeling Configuration
-- ───────────────────────────────────────────────

-- | Configuration for LLM-based community labeling.
-- Supports any OpenAI-compatible API (OpenAI, Ollama, LiteLLM, etc.)
--
-- All fields are optional in graphos.yaml — missing values fall back to defaults.
data LabelingConfig = LabelingConfig
  { labelingProvider  :: String  -- ^ Provider: "openai" | "ollama" | "litellm"
  , labelingModel     :: String  -- ^ Model name: "gpt-4o-mini" | "llama3" etc.
  , labelingApiKey    :: String  -- ^ API key (env var ${VAR} resolved at runtime)
  , labelingBaseUrl   :: String  -- ^ API base URL (e.g. "https://api.openai.com/v1")
  , labelingBatchSize :: Int     -- ^ Communities per LLM call (default: 10)
  } deriving (Eq, Show, Generic)

instance ToJSON LabelingConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 8 }

-- | Custom FromJSON: all fields optional with sensible defaults for graphos.yaml.
instance FromJSON LabelingConfig where
  parseJSON = withObject "LabelingConfig" $ \v -> LabelingConfig
    <$> v .:? "provider"   .!= "openai"
    <*> v .:? "model"      .!= "gpt-4o-mini"
    <*> v .:? "apiKey"     .!= "${OPENAI_API_KEY}"
    <*> v .:? "baseUrl"    .!= "https://api.openai.com/v1"
    <*> v .:? "batchSize"  .!= 10

-- | Default labeling configuration (OpenAI gpt-4o-mini).
defaultLabelingConfig :: LabelingConfig
defaultLabelingConfig = LabelingConfig
  { labelingProvider  = "openai"
  , labelingModel     = "gpt-4o-mini"
  , labelingApiKey    = "${OPENAI_API_KEY}"
  , labelingBaseUrl   = "https://api.openai.com/v1"
  , labelingBatchSize = 10
  }

-- ───────────────────────────────────────────────
-- Embedding Configuration
-- ───────────────────────────────────────────────

-- | Configuration for local embedding generation via Ollama.
-- Disabled by default — only runs when --embed flag is passed or
-- embedding.enabled is set in graphos.yaml.
--
-- Targets small local models (nomic-embed-text, all-minilm) via
-- Ollama's OpenAI-compatible /embeddings endpoint.
data EmbeddingConfig = EmbeddingConfig
  { embEnabled   :: Bool     -- ^ Enable embedding generation (default: False)
  , embProvider  :: String   -- ^ Provider: "ollama" (only local for now)
  , embModel     :: String   -- ^ Model name (e.g. "nomic-embed-text")
  , embBaseUrl   :: String   -- ^ Ollama API base URL (e.g. "http://localhost:11434/v1")
  , embDimension :: Int      -- ^ Embedding vector dimension (0 = auto-detect from model)
  } deriving (Eq, Show, Generic)

instance ToJSON EmbeddingConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }

instance FromJSON EmbeddingConfig where
  parseJSON = withObject "EmbeddingConfig" $ \v -> EmbeddingConfig
    <$> v .:? "enabled"   .!= False
    <*> v .:? "provider"  .!= "ollama"
    <*> v .:? "model"     .!= "nomic-embed-text"
    <*> v .:? "baseUrl"   .!= "http://localhost:11434/v1"
    <*> v .:? "dimension" .!= 0

-- | Default embedding configuration (disabled, local Ollama).
defaultEmbeddingConfig :: EmbeddingConfig
defaultEmbeddingConfig = EmbeddingConfig
  { embEnabled   = False
  , embProvider  = "ollama"
  , embModel     = "nomic-embed-text"
  , embBaseUrl   = "http://localhost:11434/v1"
  , embDimension = 0
  }

-- ───────────────────────────────────────────────
-- Vision Configuration
-- ───────────────────────────────────────────────

-- | Configuration for multimodal LLM vision analysis.
-- Supports any OpenAI-compatible API (OpenAI, Ollama, LiteLLM, etc.)
-- with image_url content type.
--
-- When apiKey or baseUrl are not explicitly set, they inherit from
-- labeling config. Vision is disabled by default.
data VisionConfig = VisionConfig
  { vcEnabled   :: Bool     -- ^ Enable vision analysis (default: False)
  , vcModel     :: String   -- ^ Model name (e.g. "qwen3.6-moe", "gpt-4o")
  , vcApiKey    :: String   -- ^ API key (env var ${VAR} resolved at runtime)
  , vcBaseUrl   :: String   -- ^ API base URL (e.g. "http://localhost:11434/v1")
  , vcMaxTokens :: Int      -- ^ Max tokens for vision response (default: 1000)
  , vcBatchSize :: Int      -- ^ Images per batch with GC between (default: 5)
  } deriving (Eq, Show, Generic)

instance ToJSON VisionConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 2 }

instance FromJSON VisionConfig where
  parseJSON = withObject "VisionConfig" $ \v -> VisionConfig
    <$> v .:? "enabled"    .!= False
    <*> v .:? "model"       .!= "qwen3.6-moe"
    <*> v .:? "apiKey"      .!= "${OPENAI_API_KEY}"
    <*> v .:? "baseUrl"     .!= "http://localhost:11434/v1"
    <*> v .:? "maxTokens"   .!= 1000
    <*> v .:? "batchSize"   .!= 5

-- | Default vision configuration (disabled, local Ollama qwen3.6-moe).
defaultVisionConfig :: VisionConfig
defaultVisionConfig = VisionConfig
  { vcEnabled   = False
  , vcModel     = "qwen3.6-moe"
  , vcApiKey    = "${OPENAI_API_KEY}"
  , vcBaseUrl   = "http://localhost:11434/v1"
  , vcMaxTokens = 1000
  , vcBatchSize = 5
  }