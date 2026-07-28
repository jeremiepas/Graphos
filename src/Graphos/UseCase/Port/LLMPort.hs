-- | Port interface for LLM operations (labeling, embeddings, vision, URL validation).
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Only Domain types appear in signatures.
module Graphos.UseCase.Port.LLMPort
  ( -- * LLM port
    LLMPort(..)
  , -- * Image analysis types (moved from Infrastructure to avoid cross-layer import)
    ImageAnalysis(..)
  , ImageKind(..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Graphos.Domain.Config (LabelingConfig, EmbeddingConfig, VisionConfig)

-- | Image kind classification
data ImageKind = PhotoKind | DiagramKind | ScreenshotKind | IconKind
  deriving (Eq, Show)

-- | Image analysis result
data ImageAnalysis = ImageAnalysis
  { iaDescription :: Text
  , iaEntities    :: [Text]
  , iaKind        :: ImageKind
  } deriving (Eq, Show)

-- | Record-of-functions port for LLM operations.
data LLMPort = LLMPort
  { -- | Call LLM for community labeling
    lpCallLLM              :: LabelingConfig -> Text -> IO (Either Text Text)
    -- | Parse labels from LLM response
  , lpParseLabelsFromResponse :: Text -> Map Text Text
    -- | Generate embeddings for text
  , lpGenerateEmbedding    :: EmbeddingConfig -> Text -> IO (Either Text [Double])
    -- | Analyze image with vision model
  , lpAnalyzeImage         :: VisionConfig -> LabelingConfig -> FilePath -> IO (Either Text ImageAnalysis)
    -- | Validate a URL string
  , lpValidateUrl          :: Text -> Either Text Text
    }