-- | Content-addressed embedding cache (design D4).
--
-- Stores embedding vectors under @<cacheRoot>/embeddings/<key>.json@ where
-- @key = sha256hex(model <> text)@. Keys are content-addressed: identical
-- (model, text) pairs hit the same slot, and a model change produces a
-- different key, invalidating stale entries for free. Writes are atomic
-- (temp file + rename), so a concurrent reader never observes a truncated
-- entry, and two writers producing the same key are idempotent under the
-- API-determinism assumption (soundness per @runCached_sound@ in the
-- change's Lean artifact).
--
-- Readers treat an undecodable or absent entry as a cache miss, so a
-- partially-written or corrupt file can never poison the pipeline.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.LLM.EmbeddingCache
  ( embeddingCacheDir
  , cacheKey
  , loadVector
  , saveVector
  , embeddingSourceHash
  ) where

import Control.Exception (SomeException, catch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import Crypto.Hash.SHA256 (hash)
import Numeric (showHex)
import Data.Word (Word8)

import Graphos.Infrastructure.FileSystem.AtomicWrite (writeFileAtomic)

-- | Directory holding embedding cache entries, under the output cache root.
-- A dedicated @embeddings/@ subdirectory keeps the flat extraction entries
-- in @cache/@ from colliding with (model, text) keys.
embeddingCacheDir :: FilePath -> FilePath
embeddingCacheDir cacheRoot = cacheRoot </> "embeddings"

-- | Content-addressed cache key: SHA-256 (hex) of the model name
-- concatenated with the embedded text.
--
-- Including the model gives free invalidation on a model change: the key
-- differs, forcing a re-embed rather than serving a stale vector. Hash
-- injectivity is an implementation assumption (identifying key with text),
-- noted in the Lean artifact.
cacheKey :: Text -> Text -> String
cacheKey model text =
  concatMap byteToHex (BS.unpack digest)
  where
    digest = hash (TE.encodeUtf8 (model <> text)) :: BS.ByteString

-- | Hex-encode a single byte, zero-padding to two digits.
byteToHex :: Word8 -> String
byteToHex w = case showHex (fromIntegral w :: Integer) "" of
  s -> if length s == 1 then '0' : s else s

-- | Load the cached vector for (model, text).
-- Returns 'Nothing' on a miss: absent key, undecodable file, or missing
-- @vector@ field (corrupt or truncated writes are misses, never errors).
-- Any filesystem exception is also a miss — the cache must never break a run.
loadVector :: FilePath -> Text -> Text -> IO (Maybe [Double])
loadVector cacheRoot model text = do
  let entry = embeddingCacheDir cacheRoot </> cacheKey model text ++ ".json"
  exists <- doesFileExist entry
  if not exists
    then pure Nothing
    else do
      r <- (Just <$> BSL.readFile entry) `catch` \(_ :: SomeException) -> pure Nothing
      pure $ case r >>= Aeson.decode of
        Just (Aeson.Object obj) ->
          case KeyMap.lookup "vector" obj of
            Just (Aeson.Array arr) ->
              Just [ realToFrac n | Aeson.Number n <- V.toList arr ]
            _ -> Nothing
        _ -> Nothing

-- | Save a vector for (model, text), atomically.
-- Idempotent: the same key is written with the same value under the
-- API-determinism assumption, so concurrent writers are benign. Best-effort:
-- a failing cache write is swallowed (the API vector is already in hand).
saveVector :: FilePath -> Text -> Text -> [Double] -> IO ()
saveVector cacheRoot model text vec =
  (do
    createDirectoryIfMissing True (embeddingCacheDir cacheRoot)
    writeFileAtomic entry (Aeson.encode (Aeson.object ["vector" Aeson..= vec]))
  ) `catch` \(_ :: SomeException) -> pure ()
  where
    entry = embeddingCacheDir cacheRoot </> cacheKey model text ++ ".json"

-- | The persisted source-hash for an embedding record: SHA-256 (hex) of the
-- model name concatenated with the embedded text (same digest as 'cacheKey').
-- Two records with the same text and model therefore carry equal hashes,
-- regardless of their source file paths.
embeddingSourceHash :: Text -> Text -> Text
embeddingSourceHash model text = T.pack (cacheKey model text)