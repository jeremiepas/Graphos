-- | Content-cache policy (wire-incremental-update): the size cap bounding the
-- persistent extraction and embedding caches under @graphos-out/cache/@.
-- Pure data — no IO. The eviction sweep lives in
-- 'Graphos.Infrastructure.FileSystem.Cache'.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphos.Domain.Config.Cache
  ( CacheConfig(..)
  , defaultCacheConfig
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), withObject, object, (.=), (.:?), (.!=))
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | Cache eviction policy. @ccMaxBytes = 0@ disables eviction (unbounded).
data CacheConfig = CacheConfig
  { ccMaxBytes :: !Word64  -- ^ combined extraction+embedding cache size cap in bytes
  } deriving (Eq, Show, Generic)

-- | Default cap: 512 MB.
defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig { ccMaxBytes = 512 * 1024 * 1024 }

instance ToJSON CacheConfig where
  toJSON c = object
    [ "max_mb" .= (fromIntegral (ccMaxBytes c) `div` (1024 * 1024) :: Int)
    ]

instance FromJSON CacheConfig where
  parseJSON = withObject "CacheConfig" $ \v -> do
    mb <- v .:? "max_mb" .!= (512 :: Int)
    pure (CacheConfig { ccMaxBytes = fromIntegral mb * 1024 * 1024 })