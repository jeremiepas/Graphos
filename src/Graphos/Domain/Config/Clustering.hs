-- | Community size-cap configuration.
--
-- This is a leaf module: it depends on no other Graphos modules, so it can be
-- imported freely by 'Graphos.Domain.Config', downstream use-cases, and
-- infrastructure without introducing a module cycle.
module Graphos.Domain.Config.Clustering
  ( ClusteringConfig(..)
  , defaultClusteringConfig
  , validClusteringConfig
  , mergeClusteringConfig
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), withObject, (.:?))
import GHC.Generics (Generic)

-- | Tunable parameters for the post-Leiden community-size-cap pass.
--
-- Any community whose member count exceeds @ccMaxCommunityFraction@ of the
-- total node count is split into smaller sub-communities; when the pass is
-- disabled (@ccCommunityCapEnabled = False@) the raw Leiden result is kept.
data ClusteringConfig
  = ClusteringConfig
    { ccMaxCommunityFraction :: Double   -- ^ fraction of total nodes above which a community is split
    , ccCommunityCapEnabled  :: Bool      -- ^ whether the size-cap splitting pass runs at all
    } deriving (Eq, Show, Generic)

instance NFData ClusteringConfig

-- | YAML decoding for the clustering configuration. Absent keys fall back to the
-- same values as 'defaultClusteringConfig' so a partial block still yields a
-- valid config instead of failing the whole load.
instance FromJSON ClusteringConfig where
  parseJSON = withObject "ClusteringConfig" $ \v -> do
    frac    <- v .:? "max_community_fraction" >>= pure . maybe 0.05 id
    enabled <- v .:? "cap_enabled"             >>= pure . maybe True id
    pure $ ClusteringConfig frac enabled

-- | Default clustering configuration: cap any community above 5% of total
-- nodes, with the cap enabled.
defaultClusteringConfig :: ClusteringConfig
defaultClusteringConfig = ClusteringConfig
  { ccMaxCommunityFraction = 0.05
  , ccCommunityCapEnabled  = True
  }

-- | Validating smart constructor: rejects a non-positive fraction (which would
-- degenerate the splitter into singletons) and a fraction greater than 1 (which
-- can never trigger a split). A mis-configured value fails loudly here rather
-- than silently disabling the guard.
validClusteringConfig :: ClusteringConfig -> Either String ClusteringConfig
validClusteringConfig c
  | ccMaxCommunityFraction c <= 0 =
      Left $ "ccMaxCommunityFraction must be > 0, got " ++ show (ccMaxCommunityFraction c)
  | ccMaxCommunityFraction c > 1 =
      Left $ "ccMaxCommunityFraction must be <= 1, got " ++ show (ccMaxCommunityFraction c)
  | otherwise = Right c

-- | Resolve the effective clustering configuration from a CLI-derived project
-- config layered on top of a base 'ClusteringConfig' (from graphos.yaml or
-- defaults). A field equal to its default value is treated as "unset" and takes
-- the global value; an explicit non-default field wins. Mirrors the
-- 'mergeGraphosConfig' handling of every other leaf config.
mergeClusteringConfig :: ClusteringConfig -> ClusteringConfig -> ClusteringConfig
mergeClusteringConfig project global =
  ClusteringConfig
    { ccMaxCommunityFraction = if ccMaxCommunityFraction project == ccMaxCommunityFraction defaultClusteringConfig
                                 then ccMaxCommunityFraction global
                                 else ccMaxCommunityFraction project
      , ccCommunityCapEnabled  = if ccCommunityCapEnabled project == ccCommunityCapEnabled defaultClusteringConfig
                                 then ccCommunityCapEnabled global
                                 else ccCommunityCapEnabled project }
