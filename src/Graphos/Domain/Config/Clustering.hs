-- | Clustering configuration: the post-Leiden community-size cap and the
-- selectable clustering algorithm (Leiden or Bunch).
--
-- This is a leaf module: it depends on no other Graphos modules, so it can be
-- imported freely by 'Graphos.Domain.Config', downstream use-cases, and
-- infrastructure without introducing a module cycle.
module Graphos.Domain.Config.Clustering
  ( ClusteringConfig(..)
  , defaultClusteringConfig
  , validClusteringConfig
  , mergeClusteringConfig
  , normalizeClusterAlgorithm
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | Tunable parameters for the post-Leiden community-size-cap pass and for
-- the selectable clustering algorithm (openspec change
-- @bunch-search-based-modularization@).
--
-- Any community whose member count exceeds @ccMaxCommunityFraction@ of the
-- total node count is split into smaller sub-communities; when the pass is
-- disabled (@ccCommunityCapEnabled = False@) the raw Leiden result is kept.
-- @ccAlgorithm@ selects @leiden@ (the default, byte-identical to the
-- pre-existing path) or @bunch@; the @ccBunch*@ fields configure the search
-- and are only consulted when Bunch runs.
data ClusteringConfig
  = ClusteringConfig
    { ccMaxCommunityFraction :: Double   -- ^ fraction of total nodes above which a community is split
    , ccCommunityCapEnabled  :: Bool      -- ^ whether the size-cap splitting pass runs at all
    , ccAlgorithm            :: Text      -- ^ @leiden@ or @bunch@ (default @leiden@)
    , ccSeed                 :: Word64    -- ^ first seed of the Bunch population (default 0)
    , ccBunchPopulation      :: Int       -- ^ independent Bunch searches (default 8)
    , ccBunchThreshold       :: Int       -- ^ η in percent, 0 (NAHC) .. 100 (SAHC) (default 0)
    , ccBunchAnnealing       :: Bool      -- ^ simulated annealing on/off (default off)
    , ccBunchT0              :: Double    -- ^ annealing initial temperature (default 100)
    , ccBunchAlpha           :: Double    -- ^ annealing cooling factor (default 0.99)
    , ccBunchMaxIterations   :: Int       -- ^ Bunch iteration cap (default 10,000)
    , ccBunchMaxFiles        :: Int       -- ^ file cap above which Leiden runs instead (default 5,000)
    , ccBunchWeighted        :: Bool      -- ^ fold the MDG with edge weights (default unit weights)
    } deriving (Eq, Show, Generic)

instance NFData ClusteringConfig

-- | Private shape of the @clustering.bunch@ YAML block.
data BunchYaml = BunchYaml
  { byPopulation      :: Int
  , byThreshold       :: Int
  , byAnnealing       :: AnnealingYaml
  , byMaxIterations   :: Int
  , byMaxFiles        :: Int
  , byWeighted        :: Bool
  }

data AnnealingYaml = AnnealingYaml
  { ayEnabled :: Bool
  , ayT0      :: Double
  , ayAlpha   :: Double
  }

defaultAnnealingYaml :: AnnealingYaml
defaultAnnealingYaml = AnnealingYaml False 100 0.99

defaultBunchYaml :: BunchYaml
defaultBunchYaml = BunchYaml
  { byPopulation    = 8
  , byThreshold     = 0
  , byAnnealing     = defaultAnnealingYaml
  , byMaxIterations = 10000
  , byMaxFiles      = 5000
  , byWeighted      = False
  }

instance FromJSON AnnealingYaml where
  parseJSON = withObject "clustering.bunch.annealing" $ \v ->
    AnnealingYaml
      <$> (v .:? "enabled" >>= pure . maybe False id)
      <*> (v .:? "t0" >>= pure . maybe (ayT0 defaultAnnealingYaml) id)
      <*> (v .:? "alpha" >>= pure . maybe (ayAlpha defaultAnnealingYaml) id)

instance FromJSON BunchYaml where
  parseJSON = withObject "clustering.bunch" $ \v -> do
    ann <- v .:? "annealing" >>= pure . maybe defaultAnnealingYaml id
    BunchYaml
      <$> (v .:? "population" >>= pure . maybe (byPopulation defaultBunchYaml) id)
      <*> (v .:? "threshold" >>= pure . maybe (byThreshold defaultBunchYaml) id)
      <*> pure ann
      <*> (v .:? "maxIterations" >>= pure . maybe (byMaxIterations defaultBunchYaml) id)
      <*> (v .:? "maxFiles" >>= pure . maybe (byMaxFiles defaultBunchYaml) id)
      <*> (v .:? "weighted" >>= pure . maybe (byWeighted defaultBunchYaml) id)

-- | YAML decoding for the clustering configuration. Absent keys fall back to the
-- same values as 'defaultClusteringConfig' so a partial block still yields a
-- valid config instead of failing the whole load.
instance FromJSON ClusteringConfig where
  parseJSON = withObject "ClusteringConfig" $ \v -> do
    frac    <- v .:? "max_community_fraction" >>= pure . maybe (ccMaxCommunityFraction defaultClusteringConfig) id
    enabled <- v .:? "cap_enabled"             >>= pure . maybe (ccCommunityCapEnabled defaultClusteringConfig) id
    alg     <- v .:? "algorithm"               >>= pure . maybe (ccAlgorithm defaultClusteringConfig) normalizeClusterAlgorithm
    seed    <- v .:? "seed"                    >>= pure . maybe (ccSeed defaultClusteringConfig) id
    bunch   <- v .:? "bunch"                   >>= pure . maybe defaultBunchYaml id
    pure ClusteringConfig
      { ccMaxCommunityFraction = frac
      , ccCommunityCapEnabled  = enabled
      , ccAlgorithm            = alg
      , ccSeed                 = seed
      , ccBunchPopulation      = byPopulation bunch
      , ccBunchThreshold       = byThreshold bunch
      , ccBunchAnnealing       = ayEnabled (byAnnealing bunch)
      , ccBunchT0              = ayT0 (byAnnealing bunch)
      , ccBunchAlpha           = ayAlpha (byAnnealing bunch)
      , ccBunchMaxIterations   = byMaxIterations bunch
      , ccBunchMaxFiles        = byMaxFiles bunch
      , ccBunchWeighted        = byWeighted bunch
      }

-- | Symmetric encoding, so a 'ClusteringConfig' round-trips through JSON
-- (@decode . encode == Just@) and can be reused by config writers.
instance ToJSON ClusteringConfig where
  toJSON c = object
    [ "max_community_fraction" .= ccMaxCommunityFraction c
    , "cap_enabled" .= ccCommunityCapEnabled c
    , "algorithm" .= ccAlgorithm c
    , "seed" .= ccSeed c
    , "bunch" .= object
        [ "population" .= ccBunchPopulation c
        , "threshold" .= ccBunchThreshold c
        , "annealing" .= object
            [ "enabled" .= ccBunchAnnealing c
            , "t0" .= ccBunchT0 c
            , "alpha" .= ccBunchAlpha c
            ]
        , "maxIterations" .= ccBunchMaxIterations c
        , "maxFiles" .= ccBunchMaxFiles c
        , "weighted" .= ccBunchWeighted c
        ]
    ]

-- | Default clustering configuration: cap any community above 5% of total
-- nodes, with the cap enabled, and Leiden as the algorithm.
defaultClusteringConfig :: ClusteringConfig
defaultClusteringConfig = ClusteringConfig
  { ccMaxCommunityFraction = 0.05
  , ccCommunityCapEnabled  = True
  , ccAlgorithm            = "leiden"
  , ccSeed                 = 0
  , ccBunchPopulation      = byPopulation defaultBunchYaml
  , ccBunchThreshold       = byThreshold defaultBunchYaml
  , ccBunchAnnealing       = ayEnabled defaultAnnealingYaml
  , ccBunchT0              = ayT0 defaultAnnealingYaml
  , ccBunchAlpha           = ayAlpha defaultAnnealingYaml
  , ccBunchMaxIterations   = byMaxIterations defaultBunchYaml
  , ccBunchMaxFiles        = byMaxFiles defaultBunchYaml
  , ccBunchWeighted        = byWeighted defaultBunchYaml
  }

-- | Canonical algorithm name: @bunch@ stays @bunch@, everything else is
-- treated as the default @leiden@. The CLI reader and
-- 'validClusteringConfig' reject unknown names; this normaliser keeps the
-- downstream dispatch total.
normalizeClusterAlgorithm :: Text -> Text
normalizeClusterAlgorithm t =
  case T.toLower (T.strip t) of
    "bunch" -> "bunch"
    _       -> "leiden"

-- | Validating smart constructor: rejects a non-positive fraction (which would
-- degenerate the splitter into singletons) and a fraction greater than 1 (which
-- can never trigger a split), plus out-of-range Bunch settings. A
-- mis-configured value fails loudly here rather than silently disabling the
-- guard.
validClusteringConfig :: ClusteringConfig -> Either String ClusteringConfig
validClusteringConfig c
  | ccMaxCommunityFraction c <= 0 =
      Left $ "ccMaxCommunityFraction must be > 0, got " ++ show (ccMaxCommunityFraction c)
  | ccMaxCommunityFraction c > 1 =
      Left $ "ccMaxCommunityFraction must be <= 1, got " ++ show (ccMaxCommunityFraction c)
  | normalizeClusterAlgorithm (ccAlgorithm c) /= "bunch"
      && T.toLower (T.strip (ccAlgorithm c)) /= "leiden" =
      Left $ "clustering.algorithm must be leiden or bunch, got " ++ T.unpack (ccAlgorithm c)
  | ccBunchPopulation c < 1 =
      Left $ "clustering.bunch.population must be >= 1, got " ++ show (ccBunchPopulation c)
  | ccBunchThreshold c < 0 || ccBunchThreshold c > 100 =
      Left $ "clustering.bunch.threshold must be in 0..100, got " ++ show (ccBunchThreshold c)
  | ccBunchMaxIterations c < 0 =
      Left $ "clustering.bunch.maxIterations must be >= 0, got " ++ show (ccBunchMaxIterations c)
  | ccBunchMaxFiles c < 0 =
      Left $ "clustering.bunch.maxFiles must be >= 0, got " ++ show (ccBunchMaxFiles c)
  | ccBunchAnnealing c && ccBunchT0 c <= 0 =
      Left $ "clustering.bunch.annealing.t0 must be > 0, got " ++ show (ccBunchT0 c)
  | ccBunchAnnealing c && (ccBunchAlpha c <= 0 || ccBunchAlpha c > 1) =
      Left $ "clustering.bunch.annealing.alpha must be in (0,1], got " ++ show (ccBunchAlpha c)
  | otherwise = Right c

-- | Resolve the effective clustering configuration from a CLI-derived project
-- config layered on top of a base 'ClusteringConfig' (from graphos.yaml or
-- defaults). A field equal to its default value is treated as "unset" and takes
-- the global value; an explicit non-default field wins. Mirrors the
-- 'mergeGraphosConfig' handling of every other leaf config.
mergeClusteringConfig :: ClusteringConfig -> ClusteringConfig -> ClusteringConfig
mergeClusteringConfig project global =
  ClusteringConfig
    { ccMaxCommunityFraction = pick ccMaxCommunityFraction
    , ccCommunityCapEnabled  = pick ccCommunityCapEnabled
    , ccAlgorithm            = pick ccAlgorithm
    , ccSeed                 = pick ccSeed
    , ccBunchPopulation      = pick ccBunchPopulation
    , ccBunchThreshold       = pick ccBunchThreshold
    , ccBunchAnnealing       = pick ccBunchAnnealing
    , ccBunchT0              = pick ccBunchT0
    , ccBunchAlpha           = pick ccBunchAlpha
    , ccBunchMaxIterations   = pick ccBunchMaxIterations
    , ccBunchMaxFiles        = pick ccBunchMaxFiles
    , ccBunchWeighted        = pick ccBunchWeighted
    }
  where
    pick f = if f project == f defaultClusteringConfig then f global else f project
