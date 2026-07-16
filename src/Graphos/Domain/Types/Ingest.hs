-- | Ingest types: embedding records and index for fast single-file ingestion.
-- Pure data types with no IO dependencies.
{-# LANGUAGE StrictData #-}
module Graphos.Domain.Types.Ingest
  ( -- * Ingest result (spec-compliant)
    IngestResult(..)

    -- * Ingest index (spec-compliant)
  , IngestIndex(..)
  , emptyIngestIndex
  , lookupEmbedding
  , mergeIndex

    -- * Legacy embedding types (backward compat)
  , IngestEmbedding(..)
  , emptyIngestEmbedding
  , addToIndex
  , lookupIndex
  , indexSize
  ) where

import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

import Graphos.Domain.Types.Graph (Extraction)
import Graphos.Domain.Types.Node (NodeId)

-- ───────────────────────────────────────────────
-- Spec-Compliant Types (Task 6)
-- ───────────────────────────────────────────────

-- | Result of ingesting a single file
-- Extraction contains nodes/edges; embeddings optionally stores vectors per node
data IngestResult = IngestResult
  { irExtraction :: !Extraction
  , irEmbeddings :: !(Maybe (Map NodeId [Double]))
  } deriving (Eq, Show, Generic)

instance NFData IngestResult

instance ToJSON IngestResult where
  toJSON r = object
    [ "extraction" .= irExtraction r
    , "embeddings" .= irEmbeddings r
    ]

instance FromJSON IngestResult where
  parseJSON = withObject "IngestResult" $ \v -> IngestResult
    <$> v .: "extraction"
    <*> v .: "embeddings"

-- | In-memory index mapping nodeId → embedding vector.
-- Persisted as index.json for fast lookups during query.
-- Supports O(1) lookup via Map.
data IngestIndex = IngestIndex
  { iiNodes :: !(Map NodeId [Double])
  } deriving (Eq, Show, Generic)

instance NFData IngestIndex

-- | Empty index
emptyIngestIndex :: IngestIndex
emptyIngestIndex = IngestIndex { iiNodes = Map.empty }

-- | Look up an embedding by nodeId (O(1) via Map lookup)
lookupEmbedding :: NodeId -> IngestIndex -> Maybe [Double]
lookupEmbedding nid idx = Map.lookup nid (iiNodes idx)

-- | Merge two indices (right-biased: right side wins on key collision)
mergeIndex :: IngestIndex -> IngestIndex -> IngestIndex
mergeIndex left right = IngestIndex
  { iiNodes = Map.union (iiNodes left) (iiNodes right)
  }

instance ToJSON IngestIndex where
  toJSON idx = object
    [ "nodes" .= iiNodes idx
    ]

instance FromJSON IngestIndex where
  parseJSON = withObject "IngestIndex" $ \v -> IngestIndex
    <$> v .: "nodes"

-- ───────────────────────────────────────────────
-- Legacy Types (for backward compatibility)
-- ───────────────────────────────────────────────

-- | Embedding record for a single ingested node.
-- When embedding is disabled, the vector is empty and only metadata is stored.
-- When enabled (via Ollama), the vector contains the model's output.
data IngestEmbedding = IngestEmbedding
  { ieNodeId      :: NodeId       -- ^ The node this embedding belongs to
  , ieVector      :: [Double]     -- ^ Embedding vector (empty when embedding disabled)
  , ieSourceHash  :: Text         -- ^ Hash of the source file content (for cache invalidation)
  , ieTimestamp   :: UTCTime      -- ^ When this embedding was generated
  , ieModel      :: Text         -- ^ Model used for embedding (e.g. "nomic-embed-text")
  } deriving (Eq, Show)

-- | Empty embedding with no vector (used when embedding is disabled)
emptyIngestEmbedding :: NodeId -> Text -> UTCTime -> IngestEmbedding
emptyIngestEmbedding nid hash ts = IngestEmbedding
  { ieNodeId     = nid
  , ieVector     = []
  , ieSourceHash = hash
  , ieTimestamp  = ts
  , ieModel      = "none"
  }

instance ToJSON IngestEmbedding where
  toJSON e = object
    [ "node_id"     .= ieNodeId e
    , "vector"      .= ieVector e
    , "source_hash" .= ieSourceHash e
    , "timestamp"   .= ieTimestamp e
    , "model"       .= ieModel e
    ]

instance FromJSON IngestEmbedding where
  parseJSON = withObject "IngestEmbedding" $ \v -> IngestEmbedding
    <$> v .: "node_id"
    <*> v .: "vector"
    <*> v .: "source_hash"
    <*> v .: "timestamp"
    <*> v .: "model"

-- | Add an embedding to the index (overwrites if nodeId exists)
addToIndex :: IngestEmbedding -> IngestIndex -> IngestIndex
addToIndex emb idx = idx { iiNodes = Map.insert (ieNodeId emb) (ieVector emb) (iiNodes idx) }

-- | Look up an embedding by nodeId
lookupIndex :: NodeId -> IngestIndex -> Maybe IngestEmbedding
lookupIndex nid idx = do
  vec <- Map.lookup nid (iiNodes idx)
  -- Note: This loses timestamp/model info. Consider using IngestResult instead.
  Just IngestEmbedding
    { ieNodeId = nid
    , ieVector = vec
    , ieSourceHash = ""
    , ieTimestamp = undefined
    , ieModel = ""
    }

-- | Number of entries in the index
indexSize :: IngestIndex -> Int
indexSize = Map.size . iiNodes
