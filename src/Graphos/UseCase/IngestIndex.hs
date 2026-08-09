-- | Fast ingest index — in-memory index for nodeId → IngestEmbedding lookups.
-- Supports O(1) exact lookup and cosine-similarity search when embeddings exist.
-- Persisted as index.json in the output directory.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.IngestIndex
  ( -- * Index operations
    loadIndex
  , saveIndex
  , mergeIndices
  , searchSimilar
  , searchSimilarThreshold
  ) where

import Control.Exception (catch, SomeException)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (Down(..))
import System.Directory (doesFileExist)

import Graphos.Domain.Types
  ( NodeId, IngestIndex(..), emptyIngestIndex
  )
import qualified Graphos.Infrastructure.LLM.Embedding as Emb

-- | Load an IngestIndex from a JSON file.
-- Returns empty index if file doesn't exist or can't be parsed.
loadIndex :: FilePath -> IO IngestIndex
loadIndex path = do
  exists <- doesFileExist path
  if not exists
    then pure emptyIngestIndex
    else (do
      contents <- BSL.readFile path
      case Aeson.decode contents of
        Just idx -> pure idx
        Nothing  -> pure emptyIngestIndex
      ) `catch` \(_ :: SomeException) -> pure emptyIngestIndex

-- | Save an IngestIndex to a JSON file.
saveIndex :: FilePath -> IngestIndex -> IO ()
saveIndex path idx = BSL.writeFile path (Aeson.encode idx)

-- | Merge two indices (right-biased: overwrites on nodeId collision).
mergeIndices :: IngestIndex -> IngestIndex -> IngestIndex
mergeIndices a b = IngestIndex
  { iiVersion = max (iiVersion a) (iiVersion b)
  , iiFiles   = iiFiles b <> iiFiles a
  , iiNodes   = iiNodes b <> iiNodes a
  }

-- | Search for nodes similar to a query vector by cosine similarity.
-- Returns results sorted by similarity (highest first), limited to top N.
-- Only considers entries that have non-empty embedding vectors.
searchSimilar :: [Double] -> IngestIndex -> Int -> [(NodeId, Double)]
searchSimilar queryVec idx topN =
  let scored = [ (nid, Emb.cosineSimilarity queryVec vec)
               | (nid, vec) <- Map.toList (iiNodes idx)
               , not (null vec)
               ]
      sorted = sortBy (\(_, a) (_, b) -> compare (Down a) (Down b)) scored
  in take topN sorted

-- | Search for nodes similar to a query vector, filtered by minimum similarity.
-- Only returns results above the given threshold (0.0 - 1.0).
searchSimilarThreshold :: [Double] -> IngestIndex -> Double -> Int -> [(NodeId, Double)]
searchSimilarThreshold queryVec idx threshold topN =
  filter (\(_, score) -> score >= threshold) (searchSimilar queryVec idx topN)