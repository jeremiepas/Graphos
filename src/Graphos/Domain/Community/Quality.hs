-- | Algorithm-independent quality of a partition actually written to disk.
--
-- These types live in the domain so both the clustering use case and the
-- export port can name them without importing use-case orchestration.
module Graphos.Domain.Community.Quality
  ( ClusterQuality(..)
  , BunchSkip(..)
  ) where

import Data.Text (Text)

-- | Why Bunch did not run: the MDG's file count against the cap.
data BunchSkip = BunchSkip
  { skipFiles :: Int
  , skipCap   :: Int
  } deriving (Eq, Show)

-- | Quality of a stage's final partition, algorithm-independent.
data ClusterQuality = ClusterQuality
  { cqAlgorithm     :: Text             -- ^ @"leiden"@ or @"bunch"@
  , cqMQ            :: Double           -- ^ modularization quality (unit weights)
  , cqModularity    :: Double           -- ^ global modularity Q
  , cqIntraFraction :: Double           -- ^ intra-edge fraction of the partition
  , cqClusters      :: Int              -- ^ cluster count
  , cqSkip          :: Maybe BunchSkip  -- ^ Bunch skipped above the file cap
  } deriving (Eq, Show)
