-- | Port interface for LLM operations (labeling, embeddings, vision, URL validation).
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Only Domain types appear in signatures.
module Graphos.UseCase.Port.LLMPort
  ( -- * LLM port
    LLMPort(..)
  , -- * Image analysis types (mirrors Infrastructure.LLM.Vision)
    ImageAnalysis(..)
  , ImageKind(..)
  , Entity(..)
  , -- * Embedding utilities
    cosineSimilarity
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Graphos.Domain.Config (LabelingConfig, EmbeddingConfig, VisionConfig)
import Graphos.Domain.Types (CommunityId)

-- | Image kind classification (mirrors Infrastructure.LLM.Vision.ImageKind)
data ImageKind = Photo | Screenshot | Diagram | Resume | Chart | OtherKind
  deriving (Eq, Show)

-- | Image analysis result (mirrors Infrastructure.LLM.Vision.ImageAnalysis)
data ImageAnalysis = ImageAnalysis
  { iaDescription :: Text
  , iaEntities    :: [Entity]
  , iaKind        :: ImageKind
  } deriving (Eq, Show)

-- | Entity extracted from image analysis
data Entity = Entity
  { entityLabel :: Text
  , entityType  :: Text
  } deriving (Eq, Show)

-- | Record-of-functions port for LLM operations.
data LLMPort = LLMPort
  { -- | Call LLM for community labeling
    lpCallLLM              :: LabelingConfig -> Text -> IO (Either Text Text)
    -- | Parse labels from LLM response
  , lpParseLabelsFromResponse :: Text -> Map CommunityId Text
    -- | Generate embeddings for text
  , lpGenerateEmbedding    :: EmbeddingConfig -> Text -> IO (Either Text [Double])
    -- | Analyze image with vision model
  , lpAnalyzeImage         :: VisionConfig -> LabelingConfig -> FilePath -> IO (Either Text ImageAnalysis)
    -- | Validate a URL string
  , lpValidateUrl          :: Text -> Either Text Text
  }

-- | Cosine similarity between two vectors (for IngestIndex).
cosineSimilarity :: [Double] -> [Double] -> Double
cosineSimilarity a b =
  let dotProd = sum (zipWith (*) a b)
      normA = sqrt (sum (zipWith (*) a a))
      normB = sqrt (sum (zipWith (*) b b))
  in if normA == 0 || normB == 0
       then 0.0
       else dotProd / (normA * normB)
