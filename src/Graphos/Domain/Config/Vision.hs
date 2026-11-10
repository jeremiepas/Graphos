-- | Vision and embedding configuration types.
-- VisionConfig, EmbeddingConfig, LabelingConfig, and their defaults.
-- Pure data types — no IO.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config.Vision
  ( VisionConfig(..)
  , defaultVisionConfig
  , EmbeddingConfig(..)
  , defaultEmbeddingConfig
  , validateEmbeddingConfig
  , LabelingConfig(..)
  , defaultLabelingConfig
  , SemanticEdgesConfig(..)
  , defaultSemanticEdgesConfig
  , defaultPathExtensions
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, withObject, object, (.:?), (.!=), (.=))
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
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
  { labelingProvider  :: String            -- ^ Provider: "ollama" | "openai" | "litellm"
  , labelingModel     :: String            -- ^ Model name: "llama3.2" | "gpt-4o-mini" etc.
  , labelingApiKey    :: String            -- ^ API key (env var ${VAR} resolved at runtime; empty for Ollama)
  , labelingBaseUrl   :: String            -- ^ API base URL (e.g. "http://localhost:11434/v1")
  , labelingBatchSize :: Int               -- ^ Communities per LLM call (default: 20)
  , labelingHeaders   :: Map String String -- ^ Custom HTTP headers (env vars in values resolved at runtime)
  } deriving (Eq, Show, Generic)

instance ToJSON LabelingConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 8 }

-- | Custom FromJSON: all fields optional with sensible defaults for graphos.yaml.
instance FromJSON LabelingConfig where
  parseJSON = withObject "LabelingConfig" $ \v -> LabelingConfig
    <$> v .:? "provider"   .!= "ollama"
    <*> v .:? "model"      .!= "llama3.2"
    <*> v .:? "apiKey"     .!= ""
    <*> v .:? "baseUrl"    .!= "http://localhost:11434/v1"
    <*> v .:? "batchSize"  .!= 20
    <*> v .:? "headers"    .!= Map.empty

-- | Default labeling configuration (local Ollama llama3.2).
defaultLabelingConfig :: LabelingConfig
defaultLabelingConfig = LabelingConfig
  { labelingProvider  = "ollama"
  , labelingModel     = "llama3.2"
  , labelingApiKey    = ""
  , labelingBaseUrl   = "http://localhost:11434/v1"
  , labelingBatchSize = 20
  , labelingHeaders   = Map.empty
  }

-- ───────────────────────────────────────────────
-- Embedding Configuration
-- ───────────────────────────────────────────────

-- | Configuration for local embedding generation via Ollama.
-- Disabled by default — only runs when --embed flag is passed or
-- embedding.enabled is set in graphos.yaml.
--
-- Targets small local models (nomic-embed-text, all-minilm) via an
-- OpenAI-compatible /embeddings endpoint (Ollama or a standalone
-- llama-server).
--
-- 'embMaxTokens' bounds the input length: small encoders reject documents
-- past their context window (e.g. a 512-token model served by llama-server
-- returns HTTP 500 on oversized inputs), so each text is truncated to fit
-- before the request — see 'Graphos.Domain.Embedding.prepare' (model-table
-- default limit with @maxTokens@ override) — and
-- 'Graphos.Infrastructure.LLM.Embedding.truncateForEmbedding'.
--
-- 'embDocPrefix' / 'embQueryPrefix' support asymmetric bi-encoders
-- (e.g. LFM2.5 @document:@ / @query:@ prefixes): the doc prefix is prepended
-- to every embedded text before the API call, empty default = passthrough
-- (byte-identical to unprefixed behavior).
data EmbeddingConfig = EmbeddingConfig
  { embEnabled   :: Bool               -- ^ Enable embedding generation (default: False)
  , embProvider  :: String             -- ^ Provider: "ollama" (only local for now)
  , embModel     :: String             -- ^ Model name (e.g. "nomic-embed-text")
  , embBaseUrl   :: String             -- ^ Ollama API base URL (e.g. "http://localhost:11434/v1")
  , embDimension :: Int                -- ^ Embedding vector dimension (0 = auto-detect from model)
  , embHeaders   :: Map String String  -- ^ Custom HTTP headers for embedding API calls
  , embBatchSize :: Int                -- ^ Max texts per embedding API request (default: 64)
  , embConcurrency :: Int              -- ^ Max batches processed concurrently (default: 1 = sequential)
  , embMaxTokens :: Int                -- ^ Truncate each input to ~this many tokens before the API call (default: 512; 0 = no truncation)
  , embDocPrefix :: Text               -- ^ Prepended to every embedded text sent to the API (default: empty = passthrough)
  , embQueryPrefix :: Text             -- ^ Prepended to query-time text for search/similarity (default: empty = passthrough)
  , embStreaming :: Bool               -- ^ Append batches to a staged sidecar as they complete, atomically renamed at the end (default: True; False = write-at-end)
  } deriving (Eq, Show, Generic)

instance ToJSON EmbeddingConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }

instance FromJSON EmbeddingConfig where
  parseJSON = withObject "EmbeddingConfig" $ \v -> EmbeddingConfig
    <$> v .:? "enabled"   .!= False
    <*> v .:? "provider"  .!= "ollama"
    <*> v .:? "model"     .!= defaultEmbeddingModel
    <*> v .:? "baseUrl"   .!= "http://localhost:11434/v1"
    <*> v .:? "dimension" .!= 0
    <*> v .:? "headers"   .!= Map.empty
    <*> v .:? "batchSize"   .!= 64
    <*> v .:? "concurrency" .!= 1
    <*> v .:? "maxTokens"   .!= 512
    <*> v .:? "docPrefix"   .!= ""
    <*> v .:? "queryPrefix" .!= ""
    <*> v .:? "streaming"   .!= True

-- | Default embedding model: the LFM2.5-Embedding-350M GGUF reference.
-- Cited by the recorded benchmark (change lfm-embedding-optimization,
-- bench/BENCHMARK.md): 512-token fit, asymmetric @document:@/@query:@
-- prefixes, and llama-server proven throughput on the target hardware.
-- A config naming @nomic-embed-text@ explicitly continues to work unchanged
-- (and its cache keys are unchanged).
defaultEmbeddingModel :: String
defaultEmbeddingModel = "hf.co/LiquidAI/LFM2.5-Embedding-350M-GGUF:Q4_K_M"

-- | Default embedding configuration (disabled, LFM2.5 GGUF reference).
defaultEmbeddingConfig :: EmbeddingConfig
defaultEmbeddingConfig = EmbeddingConfig
  { embEnabled   = False
  , embProvider  = "ollama"
  , embModel     = defaultEmbeddingModel
  , embBaseUrl   = "http://localhost:11434/v1"
  , embDimension = 0
  , embHeaders   = Map.empty
  , embBatchSize = 64
  , embConcurrency = 1
  , embMaxTokens = 512
  , embDocPrefix = ""
  , embQueryPrefix = ""
  , embStreaming = True
  }

-- | Validate embedding batch/concurrency/token bounds.
-- Returns @Left (key, value)@ naming the offending key when a value is out
-- of range; the config loader turns this into a load-time error.
validateEmbeddingConfig :: EmbeddingConfig -> Either (String, Int) ()
validateEmbeddingConfig cfg
  | embBatchSize cfg < 1     = Left ("embedding.batchSize", embBatchSize cfg)
  | embConcurrency cfg < 1   = Left ("embedding.concurrency", embConcurrency cfg)
  | embMaxTokens cfg < 0     = Left ("embedding.maxTokens", embMaxTokens cfg)
  | otherwise                = Right ()

-- ───────────────────────────────────────────────
-- Semantic Edge Inference Configuration
-- ───────────────────────────────────────────────

-- | Configuration for semantic (embedding-based) code↔doc edge inference.
-- Enabled by default — only runs when the graph has embeddings AND is a
-- mixed corpus (code + docs). Override with --no-semantic-edges or
-- --force-semantic-edges.
--
-- Also carries the deterministic doc-link thresholds (doc-code-linking):
-- minimum whole-word identifier length and accepted source-path extensions.
data SemanticEdgesConfig = SemanticEdgesConfig
  { seEnabled   :: Bool       -- ^ Enable semantic edge inference (default: True)
  , seMaxFanOut :: Int        -- ^ Max semantic edges per doc node (default: 50)
  , seThreshold :: Double     -- ^ Min cosine similarity (default: 0.5)
  , seMinIdentLength :: Int   -- ^ Min whole-word identifier length for symbol-mention doc links (default: 4)
  , sePathExtensions :: [Text] -- ^ Accepted source-path extensions for path-reference doc links (default: Graphos defaults)
  } deriving (Eq, Show, Generic)

instance ToJSON SemanticEdgesConfig where
  toJSON cfg = object
    [ "enabled"     .= seEnabled cfg
    , "max_fan_out" .= seMaxFanOut cfg
    , "threshold"   .= seThreshold cfg
    , "min_ident_length" .= seMinIdentLength cfg
    , "path_extensions" .= sePathExtensions cfg
    ]

instance FromJSON SemanticEdgesConfig where
  parseJSON = withObject "SemanticEdgesConfig" $ \v -> SemanticEdgesConfig
    <$> v .:? "enabled"   .!= True
    <*> v .:? "max_fan_out" .!= 50
    <*> v .:? "threshold" .!= 0.5
    <*> v .:? "min_ident_length" .!= 4
    <*> v .:? "path_extensions" .!= defaultPathExtensions

-- | Default path extensions for path-reference doc links (shared with the
-- linker's own default list).
defaultPathExtensions :: [Text]
defaultPathExtensions =
  [ ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs"
  , ".hs", ".lhs"
  , ".rs", ".py", ".pyx"
  , ".go", ".java", ".kt", ".scala", ".sc", ".kts"
  , ".rb", ".php"
  , ".cpp", ".cc", ".cxx", ".c++", ".hpp", ".hh", ".h"
  , ".cs", ".swift", ".m", ".mm"
  , ".ml", ".mli", ".ex", ".exs", ".erl", ".clj"
  , ".sh", ".bash", ".yaml", ".yml", ".toml", ".json", ".md", ".markdown"
  ]

-- | Default semantic edges configuration (enabled, fan-out 50, threshold 0.5,
-- doc-link identifier length 4, default path extensions).
defaultSemanticEdgesConfig :: SemanticEdgesConfig
defaultSemanticEdgesConfig = SemanticEdgesConfig
  { seEnabled   = True
  , seMaxFanOut = 50
  , seThreshold = 0.5
  , seMinIdentLength = 4
  , sePathExtensions = defaultPathExtensions
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
  { vcEnabled   :: Bool                -- ^ Enable vision analysis (default: False)
  , vcModel     :: String              -- ^ Model name (e.g. "qwen3.6-moe", "gpt-4o")
  , vcApiKey    :: String              -- ^ API key (env var ${VAR} resolved at runtime; empty for Ollama)
  , vcBaseUrl   :: String              -- ^ API base URL (e.g. "http://localhost:11434/v1")
  , vcMaxTokens :: Int                 -- ^ Max tokens for vision response (default: 1000)
  , vcBatchSize :: Int                 -- ^ Images per batch with GC between (default: 5)
  , vcHeaders   :: Map String String   -- ^ Custom HTTP headers for vision API calls
  } deriving (Eq, Show, Generic)

instance ToJSON VisionConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 2 }

instance FromJSON VisionConfig where
  parseJSON = withObject "VisionConfig" $ \v -> VisionConfig
    <$> v .:? "enabled"    .!= False
    <*> v .:? "model"       .!= "qwen3.6-moe"
    <*> v .:? "apiKey"      .!= ""
    <*> v .:? "baseUrl"     .!= "http://localhost:11434/v1"
    <*> v .:? "maxTokens"   .!= 1000
    <*> v .:? "batchSize"   .!= 5
    <*> v .:? "headers"     .!= Map.empty

-- | Default vision configuration (disabled, local Ollama qwen3.6-moe).
defaultVisionConfig :: VisionConfig
defaultVisionConfig = VisionConfig
  { vcEnabled   = False
  , vcModel     = "qwen3.6-moe"
  , vcApiKey    = ""
  , vcBaseUrl   = "http://localhost:11434/v1"
  , vcMaxTokens = 1000
  , vcBatchSize = 5
  , vcHeaders   = Map.empty
  }