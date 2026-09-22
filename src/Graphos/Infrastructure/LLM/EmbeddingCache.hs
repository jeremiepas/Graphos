-- | Content-addressed embedding cache (design D4, lfm-embedding-optimization):
--
-- Stores embedding vectors under @<cacheRoot>/embeddings/<key>.json@ where
-- @key = sha256hex(model <> docPrefix <> preparedText)@ — i.e. the digest of
-- everything that determines the API's output: the model, the effective
-- prefixes, and the prepared text actually submitted (hash what was sent).
-- Keys are content-addressed: identical (model, prefix, prepared text) triples
-- hit the same slot, and a model *or prefix* change — or a preparation change
-- that alters the prepared text — produces a different key, invalidating
-- stale entries for free. Because the key is over the *prepared* text, two
-- raw texts that prepare identically converge on one entry (truncation
-- convergence). Writes are atomic (temp file + rename), so a concurrent
-- reader never observes a truncated entry, and two writers producing the
-- same key are idempotent under the API-determinism assumption (soundness
-- per @runCached_sound@ in the change's Lean artifact).
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

-- | Content-addressed cache key: SHA-256 (hex) of the model name, the
-- effective document prefix, and the prepared text concatenated.
--
-- Including the model gives free invalidation on a model change; including
-- the prefix does the same for a prefix change (spec: "Prefix change
-- invalidates the cache"). Hashing the prepared text makes truncation
-- convergence definitional: two raw texts differing only past the truncation
-- point prepare to the same string and share one entry. Hash injectivity is
-- an implementation assumption (identifying key with content), noted in the
-- Lean artifact.
cacheKey :: Text -> Text -> Text -> String
cacheKey model docPrefix preparedText =
  concatMap byteToHex (BS.unpack digest)
  where
    digest = hash (TE.encodeUtf8 (model <> docPrefix <> preparedText)) :: BS.ByteString

-- | Hex-encode a single byte, zero-padding to two digits.
byteToHex :: Word8 -> String
byteToHex w = case showHex (fromIntegral w :: Integer) "" of
  s -> if length s == 1 then '0' : s else s

-- | Load the cached vector for (model, docPrefix, preparedText).
-- Returns 'Nothing' on a miss: absent key, undecodable file, or missing
-- @vector@ field (corrupt or truncated writes are misses, never errors).
-- Any filesystem exception is also a miss — the cache must never break a run.
loadVector :: FilePath -> Text -> Text -> Text -> IO (Maybe [Double])
loadVector cacheRoot model docPrefix preparedText = do
  let entry = embeddingCacheDir cacheRoot </> cacheKey model docPrefix preparedText ++ ".json"
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

-- | Save a vector for (model, docPrefix, preparedText), atomically.
-- Idempotent: the same key is written with the same value under the
-- API-determinism assumption, so concurrent writers are benign. Best-effort:
-- a failing cache write is swallowed (the API vector is already in hand).
saveVector :: FilePath -> Text -> Text -> Text -> [Double] -> IO ()
saveVector cacheRoot model docPrefix preparedText vec =
  (do
    createDirectoryIfMissing True (embeddingCacheDir cacheRoot)
    writeFileAtomic entry (Aeson.encode (Aeson.object ["vector" Aeson..= vec]))
  ) `catch` \(_ :: SomeException) -> pure ()
  where
    entry = embeddingCacheDir cacheRoot </> cacheKey model docPrefix preparedText ++ ".json"

-- | The persisted source-hash for an embedding record: SHA-256 (hex) of the
-- model name, the effective document prefix, and the prepared text actually
-- submitted (same digest as 'cacheKey' — hash what was sent, D4). Two
-- records whose texts prepare identically under the same model and prefix
-- therefore carry equal hashes, regardless of their source file paths.
embeddingSourceHash :: Text -> Text -> Text -> Text
embeddingSourceHash model docPrefix preparedText =
  T.pack (cacheKey model docPrefix preparedText)