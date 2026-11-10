{-# LANGUAGE StrictData #-}
-- | Bunch modularization quality (TurboMQ), Mitchell & Mancoridis 2008 §2.2
-- (openspec change @bunch-search-based-modularization@, capability
-- @modularization-quality@).
--
-- For a partition of the graph into clusters @i = 1..k@:
--
-- > MQ   = Σ_i CF_i
-- > CF_i = 0                                  when µ_i = 0
-- > CF_i = 2µ_i / (2µ_i + Σ_{j≠i}(ε_ij + ε_ji)) otherwise
--
-- @µ_i@ is the weight of the edges with both endpoints in cluster @i@ and the
-- denominator's second term — @ext_i@ below — is the weight of the edges with
-- exactly one endpoint in @i@. @CF_i ∈ [0, 1]@ and @MQ ∈ [0, k]@; the Lean arm
-- @GraphosFormalization.Bunch.MQ@ proves both bounds exactly, and its
-- @twoTriangles@ fixture (@MQ = 84/49@) is pinned by @MQSpec@.
--
-- Pure and deterministic: edges are visited in ascending @gEdges@ key order
-- and cluster factors are summed in ascending 'CommunityId' order, so the
-- @Double@ result is a function of @(weighting, g, cm)@ alone (the same rule
-- as 'Graphos.Domain.Community.modularity', INV-Q4). Every edge record counts
-- once, in whichever direction it is stored; the directed flag is irrelevant
-- because @ε_ij + ε_ji@ only ever appears as a sum. An edge whose endpoint is
-- not covered by the partition is not part of the scored MDG and is skipped;
-- self-edges are skipped as well.
module Graphos.Domain.Community.MQ
  ( Weighting(..)
  , ClusterFactor(..)
  , MQResult(..)
  , clusterFactor
  , modularizationQuality
  , modularizationQualityWith
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Graphos.Domain.Types (CommunityId, CommunityMap, Edge(..))
import Graphos.Domain.Graph (Graph, gEdges)
import Graphos.Domain.Community (buildReverseIndex)

-- | How much an edge record weighs. The paper's default is one per edge;
-- 'EdgeWeights' uses the record's 'edgeWeight' instead.
data Weighting
  = UnitWeights
  | EdgeWeights
  deriving (Eq, Show)

-- | One cluster's intra weight, external weight and cluster factor.
data ClusterFactor = ClusterFactor
  { cfIntra    :: Double   -- ^ µ_i
  , cfExternal :: Double   -- ^ ext_i = Σ_{j≠i}(ε_ij + ε_ji)
  , cfValue    :: Double   -- ^ CF_i
  } deriving (Eq, Show)

-- | The scored partition.
data MQResult = MQResult
  { mqValue         :: Double                     -- ^ MQ = Σ_i CF_i
  , mqClusterCount  :: Int                        -- ^ k
  , mqIntraFraction :: Double                     -- ^ Σ_i µ_i / Σ_e w_e (0 when no edge)
  , mqTotalWeight   :: Double                     -- ^ Σ_e w_e over the scored edges
  , mqFactors       :: Map CommunityId ClusterFactor
  } deriving (Eq, Show)

-- | The cluster factor formula on its two aggregates.
clusterFactor :: Double -> Double -> Double
clusterFactor mu ext
  | mu <= 0   = 0
  | otherwise = (2 * mu) / (2 * mu + ext)

-- | MQ with unit edge weights (the paper's default).
modularizationQuality :: Graph -> CommunityMap -> MQResult
modularizationQuality = modularizationQualityWith UnitWeights

-- | MQ with the chosen weighting.
modularizationQualityWith :: Weighting -> Graph -> CommunityMap -> MQResult
modularizationQualityWith weighting g cm =
  let assign = buildReverseIndex cm

      weightOf e = case weighting of
        UnitWeights -> 1.0
        EdgeWeights -> edgeWeight e

      -- Accumulate (µ per cluster, ext per cluster, total weight).
      step (muAcc, extAcc, tot) e
        | edgeSource e == edgeTarget e = (muAcc, extAcc, tot)
        | otherwise =
            case (Map.lookup (edgeSource e) assign, Map.lookup (edgeTarget e) assign) of
              (Just cu, Just cv)
                | cu == cv  -> (Map.insertWith (+) cu w muAcc, extAcc, tot + w)
                | otherwise ->
                    ( muAcc
                    , Map.insertWith (+) cu w (Map.insertWith (+) cv w extAcc)
                    , tot + w )
              _ -> (muAcc, extAcc, tot)
        where w = weightOf e

      (muMap, extMap, total) =
        foldl' step (Map.empty, Map.empty, 0) (Map.elems (gEdges g))

      factors = Map.fromList
        [ (cid, ClusterFactor mu ext (clusterFactor mu ext))
        | cid <- Map.keys cm
        , let mu  = Map.findWithDefault 0 cid muMap
        , let ext = Map.findWithDefault 0 cid extMap
        ]

      -- Ascending CommunityId order (Map.elems), fixed for determinism.
      mq = foldl' (\acc cf -> acc + cfValue cf) 0 (Map.elems factors)

      intraTotal = foldl' (+) 0 (Map.elems muMap)
      intraFrac  = if total <= 0 then 0 else intraTotal / total
  in MQResult
       { mqValue         = mq
       , mqClusterCount  = Map.size cm
       , mqIntraFraction = intraFrac
       , mqTotalWeight   = total
       , mqFactors       = factors
       }
