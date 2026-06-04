-- | Ingest types: embedding records and index for fast single-file ingestion.
-- Pure data types with no IO dependencies.
module Graphos.Domain.Types.Ingest
  ( -- * Embedding record
    IngestEmbedding(..)
  , emptyIngestEmbedding

    -- * Ingest index
  , IngestIndex(..)
  , emptyIngestIndex
  , addToIndex
  , lookupIndex
  , indexSize
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Graphos.Domain.Types.Node (NodeId)

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

-- | In-memory index mapping nodeId → IngestEmbedding.
-- Persisted as index.json for fast lookups during query.
-- When embeddings exist, supports cosine-similarity search.
data IngestIndex = IngestIndex
  { iiEntries :: Map NodeId IngestEmbedding
  } deriving (Eq, Show)

-- | Empty index
emptyIngestIndex :: IngestIndex
emptyIngestIndex = IngestIndex { iiEntries = Map.empty }

-- | Add an embedding to the index (overwrites if nodeId exists)
addToIndex :: IngestEmbedding -> IngestIndex -> IngestIndex
addToIndex emb idx = idx { iiEntries = Map.insert (ieNodeId emb) emb (iiEntries idx) }

-- | Look up an embedding by nodeId
lookupIndex :: NodeId -> IngestIndex -> Maybe IngestEmbedding
lookupIndex nid idx = Map.lookup nid (iiEntries idx)

-- | Number of entries in the index
indexSize :: IngestIndex -> Int
indexSize = Map.size . iiEntries

instance ToJSON IngestIndex where
  toJSON idx = object
    [ "entries" .= Map.elems (iiEntries idx)
    ]

instance FromJSON IngestIndex where
  parseJSON = withObject "IngestIndex" $ \v -> do
    entries <- v .: "entries"
    let entryMap = Map.fromList [(ieNodeId e, e) | e <- entries]
    pure IngestIndex { iiEntries = entryMap }