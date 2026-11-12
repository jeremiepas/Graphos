-- | Clustering configuration: the community size cap and the selectable
-- clustering algorithm (openspec change @bunch-search-based-modularization@,
-- design D7).
--
-- This is a leaf module: it depends on no other Graphos modules, so it can be
-- imported freely by 'Graphos.Domain.Config', downstream use-cases, and
-- infrastructure without introducing a module cycle.
module Graphos.Domain.Config.Clustering
  ( ClusteringConfig(..)
  , ClusteringAlgorithmName(..)
  , BunchFileConfig(..)
  , defaultClusteringConfig
  , defaultBunchFileConfig
  , defaultAnnealing
  , forbiddenBunchRelations
  , parseRelationName
  , validClusteringConfig
  , mergeClusteringConfig
    -- * CLI overrides
  , ClusteringOverrides(..)
  , noClusteringOverrides
  , applyClusteringOverrides
  , parseAlgorithmName
  , renderAlgorithmName
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), withObject, withText, (.:?), (.!=))
import qualified Data.Text as T
import GHC.Generics (Generic)

import Graphos.Domain.Types.Edge (Relation(..), relationToText)

-- | The clustering algorithm of the build's cluster stage.
data ClusteringAlgorithmName = AlgoLeiden | AlgoBunch
  deriving (Eq, Show, Generic)

instance NFData ClusteringAlgorithmName

instance FromJSON ClusteringAlgorithmName where
  parseJSON = withText "ClusteringAlgorithmName" $ \t ->
    either fail pure (parseAlgorithmName (T.unpack t))

parseAlgorithmName :: String -> Either String ClusteringAlgorithmName
parseAlgorithmName s = case map toLowerAscii s of
  "leiden" -> Right AlgoLeiden
  "bunch"  -> Right AlgoBunch
  _        -> Left ("unknown clustering algorithm: " ++ s ++ " (expected leiden or bunch)")
  where
    toLowerAscii c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise            = c

renderAlgorithmName :: ClusteringAlgorithmName -> String
renderAlgorithmName AlgoLeiden = "leiden"
renderAlgorithmName AlgoBunch  = "bunch"

-- | The @clustering.bunch.*@ block: the search parameters of the Bunch
-- algorithm plus the stage-level file cap and MDG weighting.
data BunchFileConfig = BunchFileConfig
  { bfPopulation    :: Int                    -- ^ independent searches (default 8)
  , bfThreshold     :: Int                    -- ^ η in percent, 0..100 (default 0, NAHC)
  , bfAnnealing     :: Maybe (Double, Double) -- ^ @(T0, α)@ when simulated annealing is on
  , bfMaxIterations :: Int                    -- ^ iteration cap (default 10,000)
  , bfMaxFiles      :: Int                    -- ^ hard ceiling: file count above which Leiden runs instead (default 50,000)
  , bfMaxFilesPerRun :: Int                   -- ^ files per search run; above it the MDG is split by directory sub-tree (default 500)
  , bfRelations     :: [Relation]             -- ^ relations folded into the MDG (default imports, calls, references, extends, implements)
  , bfWeighted      :: Bool                   -- ^ fold the MDG with edge weights
  } deriving (Eq, Show, Generic)

instance NFData BunchFileConfig

defaultBunchFileConfig :: BunchFileConfig
defaultBunchFileConfig = BunchFileConfig
  { bfPopulation    = 8
  , bfThreshold     = 0
  , bfAnnealing     = Nothing
  , bfMaxIterations = 10000
  , bfMaxFiles      = 50000
  , bfMaxFilesPerRun = 500
  , bfRelations     = [Imports, Calls, References, Extends, Implements]
  , bfWeighted      = False
  }

-- | Relations the Bunch MDG may never fold (bunch-real-projects D2): file
-- structure, documentation links and edge-inference output.
forbiddenBunchRelations :: [Relation]
forbiddenBunchRelations = [Contains, Documents, DependsOn, Inferred]

-- | Parse a relation by its @graph.json@ name (@imports@, @calls@, …).
parseRelationName :: String -> Either String Relation
parseRelationName s = case [ r | r <- [minBound .. maxBound], T.unpack (relationToText r) == s ] of
  (r : _) -> Right r
  []      -> Left ("unknown relation: " ++ s)

-- | Annealing schedule used when annealing is enabled without explicit
-- @t0@/@alpha@ (the paper's @T0 = 100, α = 0.99@).
defaultAnnealing :: (Double, Double)
defaultAnnealing = (100, 0.99)

-- | Multi-word keys are accepted in snake_case (the file's convention) and
-- camelCase (the design's spelling).
instance FromJSON BunchFileConfig where
  parseJSON = withObject "BunchFileConfig" $ \v -> do
    let d = defaultBunchFileConfig
    pop     <- v .:? "population" .!= bfPopulation d
    thr     <- v .:? "threshold"  .!= bfThreshold d
    mIter   <- (<|>) <$> v .:? "max_iterations" <*> v .:? "maxIterations"
    mFiles  <- (<|>) <$> v .:? "max_files"      <*> v .:? "maxFiles"
    mPerRun <- (<|>) <$> v .:? "max_files_per_run" <*> v .:? "maxFilesPerRun"
    mRels   <- v .:? "relations"
    rels    <- case mRels of
      Nothing -> pure (bfRelations d)
      Just names -> either fail pure (mapM parseRelationName names)
    wgt     <- v .:? "weighted"   .!= bfWeighted d
    ann     <- v .:? "annealing"
    pure BunchFileConfig
      { bfPopulation    = pop
      , bfThreshold     = thr
      , bfAnnealing     = ann >>= annealingOf
      , bfMaxIterations = maybe (bfMaxIterations d) id mIter
      , bfMaxFiles      = maybe (bfMaxFiles d) id mFiles
      , bfMaxFilesPerRun = maybe (bfMaxFilesPerRun d) id mPerRun
      , bfRelations     = rels
      , bfWeighted      = wgt
      }
    where
      annealingOf (AnnealingBlock enabled t0 alpha)
        | enabled   = Just (maybe (fst defaultAnnealing) id t0, maybe (snd defaultAnnealing) id alpha)
        | otherwise = Nothing

-- | @clustering.bunch.annealing: {enabled, t0, alpha}@.
data AnnealingBlock = AnnealingBlock Bool (Maybe Double) (Maybe Double)

instance FromJSON AnnealingBlock where
  parseJSON = withObject "AnnealingBlock" $ \v ->
    AnnealingBlock <$> v .:? "enabled" .!= False <*> v .:? "t0" <*> v .:? "alpha"

-- | Tunable parameters of the cluster stage.
--
-- Any community whose member count exceeds @ccMaxCommunityFraction@ of the
-- total node count is split into smaller sub-communities; when the pass is
-- disabled (@ccCommunityCapEnabled = False@) the raw partition is kept.
-- @ccAlgorithm@ selects Leiden (default) or Bunch; @ccSeed@ is the first seed
-- of the Bunch population.
data ClusteringConfig
  = ClusteringConfig
    { ccMaxCommunityFraction :: Double                   -- ^ fraction of total nodes above which a community is split
    , ccCommunityCapEnabled  :: Bool                     -- ^ whether the size-cap splitting pass runs at all
    , ccAlgorithm            :: ClusteringAlgorithmName  -- ^ @clustering.algorithm@ (default leiden)
    , ccSeed                 :: Int                      -- ^ @clustering.seed@ (default 0)
    , ccBunch                :: BunchFileConfig          -- ^ @clustering.bunch.*@
    } deriving (Eq, Show, Generic)

instance NFData ClusteringConfig

-- | YAML decoding for the clustering configuration. Absent keys fall back to the
-- same values as 'defaultClusteringConfig' so a partial block still yields a
-- valid config instead of failing the whole load.
instance FromJSON ClusteringConfig where
  parseJSON = withObject "ClusteringConfig" $ \v -> do
    let d = defaultClusteringConfig
    frac    <- v .:? "max_community_fraction" .!= ccMaxCommunityFraction d
    enabled <- v .:? "cap_enabled"            .!= ccCommunityCapEnabled d
    algo    <- v .:? "algorithm"              .!= ccAlgorithm d
    seed    <- v .:? "seed"                   .!= ccSeed d
    bunch   <- v .:? "bunch"                  .!= ccBunch d
    pure $ ClusteringConfig frac enabled algo seed bunch

-- | Default clustering configuration: Leiden, cap any community above 5% of
-- total nodes, with the cap enabled.
defaultClusteringConfig :: ClusteringConfig
defaultClusteringConfig = ClusteringConfig
  { ccMaxCommunityFraction = 0.05
  , ccCommunityCapEnabled  = True
  , ccAlgorithm            = AlgoLeiden
  , ccSeed                 = 0
  , ccBunch                = defaultBunchFileConfig
  }

-- | Validating smart constructor: rejects a non-positive fraction (which would
-- degenerate the splitter into singletons) and a fraction greater than 1 (which
-- can never trigger a split), and out-of-range Bunch parameters. A
-- mis-configured value fails loudly here rather than silently disabling the
-- guard.
validClusteringConfig :: ClusteringConfig -> Either String ClusteringConfig
validClusteringConfig c
  | ccMaxCommunityFraction c <= 0 =
      Left $ "ccMaxCommunityFraction must be > 0, got " ++ show (ccMaxCommunityFraction c)
  | ccMaxCommunityFraction c > 1 =
      Left $ "ccMaxCommunityFraction must be <= 1, got " ++ show (ccMaxCommunityFraction c)
  | ccSeed c < 0 =
      Left $ "clustering.seed must be >= 0, got " ++ show (ccSeed c)
  | bfPopulation b < 1 =
      Left $ "clustering.bunch.population must be >= 1, got " ++ show (bfPopulation b)
  | bfThreshold b < 0 || bfThreshold b > 100 =
      Left $ "clustering.bunch.threshold must be in 0..100, got " ++ show (bfThreshold b)
  | bfMaxIterations b < 1 =
      Left $ "clustering.bunch.max_iterations must be >= 1, got " ++ show (bfMaxIterations b)
  | bfMaxFiles b < 0 =
      Left $ "clustering.bunch.max_files must be >= 0, got " ++ show (bfMaxFiles b)
  | bfMaxFilesPerRun b < 2 =
      Left $ "clustering.bunch.max_files_per_run must be >= 2, got " ++ show (bfMaxFilesPerRun b)
  | null (bfRelations b) =
      Left "clustering.bunch.relations must name at least one relation"
  | (r : _) <- filter (`elem` forbiddenBunchRelations) (bfRelations b) =
      Left $ "clustering.bunch.relations may not include " ++ T.unpack (relationToText r)
          ++ " (contains, documents, depends_on and inferred are never folded)"
  | Just (t0, alpha) <- bfAnnealing b, t0 <= 0 || alpha <= 0 || alpha >= 1 =
      Left $ "clustering.bunch.annealing needs t0 > 0 and 0 < alpha < 1, got " ++ show (t0, alpha)
  | otherwise = Right c
  where b = ccBunch c

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
    , ccBunch                = BunchFileConfig
        { bfPopulation    = pickB bfPopulation
        , bfThreshold     = pickB bfThreshold
        , bfAnnealing     = pickB bfAnnealing
        , bfMaxIterations = pickB bfMaxIterations
        , bfMaxFiles      = pickB bfMaxFiles
        , bfMaxFilesPerRun = pickB bfMaxFilesPerRun
        , bfRelations     = pickB bfRelations
        , bfWeighted      = pickB bfWeighted
        }
    }
  where
    pick :: Eq a => (ClusteringConfig -> a) -> a
    pick f = if f project == f defaultClusteringConfig then f global else f project
    pickB :: Eq a => (BunchFileConfig -> a) -> a
    pickB f = pick (f . ccBunch)

-- | Build-command overrides of the clustering configuration. 'Nothing' (or
-- 'False' for the switch) leaves the config value in place; a given value
-- wins over @graphos.yaml@.
data ClusteringOverrides = ClusteringOverrides
  { covAlgorithm     :: Maybe ClusteringAlgorithmName  -- ^ @--cluster-algorithm@
  , covSeed          :: Maybe Int                      -- ^ @--seed@
  , covPopulation    :: Maybe Int                      -- ^ @--bunch-population@
  , covThreshold     :: Maybe Int                      -- ^ @--bunch-threshold@
  , covAnnealing     :: Maybe (Double, Double)         -- ^ @--bunch-sa T0,ALPHA@
  , covMaxIterations :: Maybe Int                      -- ^ @--bunch-max-iterations@
  , covMaxFiles      :: Maybe Int                      -- ^ @--bunch-max-files@
  , covMaxFilesPerRun :: Maybe Int                     -- ^ @--bunch-max-files-per-run@
  , covWeighted      :: Bool                           -- ^ @--bunch-weighted@
  } deriving (Eq, Show, Generic)

instance NFData ClusteringOverrides

noClusteringOverrides :: ClusteringOverrides
noClusteringOverrides =
  ClusteringOverrides Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False

-- | CLI values override the configuration.
applyClusteringOverrides :: ClusteringOverrides -> ClusteringConfig -> ClusteringConfig
applyClusteringOverrides o c = c
  { ccAlgorithm = maybe (ccAlgorithm c) id (covAlgorithm o)
  , ccSeed      = maybe (ccSeed c) id (covSeed o)
  , ccBunch     = b
      { bfPopulation    = maybe (bfPopulation b) id (covPopulation o)
      , bfThreshold     = maybe (bfThreshold b) id (covThreshold o)
      , bfAnnealing     = covAnnealing o <|> bfAnnealing b
      , bfMaxIterations = maybe (bfMaxIterations b) id (covMaxIterations o)
      , bfMaxFiles      = maybe (bfMaxFiles b) id (covMaxFiles o)
      , bfMaxFilesPerRun = maybe (bfMaxFilesPerRun b) id (covMaxFilesPerRun o)
      , bfWeighted      = covWeighted o || bfWeighted b
      }
  }
  where b = ccBunch c
