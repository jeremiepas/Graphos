{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Conflict quantification for last-write-wins (LWW) views — the coequalizer
-- half of J2 (AVI-661, M4; design.md §3 theorem T4 / SHALL-M4).
--
-- Given two merge orders @σ@ and @τ@ over a family of views, 'mergeGraphs'
-- folds each order into a single graph using @Map.union@ with fixed right-wins
-- (a pure, deterministic function of the order). Where the two orders disagree
-- on a generator we can /measure/ that disagreement:
--
-- > @D_N(σ,τ)@ = { nid : nodePayload(merge_σ)(nid) ≠ nodePayload(merge_τ)(nid) }
-- > @D_E(σ,τ)@ = { (u,v) : A_uv^σ ≠ A_uv^τ }
-- > @δ(σ,τ)   = |D_N| + Σ_{(u,v) ∈ D_E} |A_uv^σ − A_uv^τ|@
-- > @Δ_cluster(σ,τ)@ = { v : c^{merge_σ}(v) ≠ c^{merge_τ}(v) }   (community flips)
--
-- These are the measurable quantities behind the claims in design.md §3 T4:
--
--   * @|D_N| ≤ |K_conflict|@ and @|D_E| ≤ |conflicting edges|@ — a coequalizer
--     identifies only the conflicting generators; consistent ids never diverge.
--   * For consistent views @K_conflict = ∅ ⇒ δ = 0@ (content confluence, T1/T3).
--   * Given a fixed order the merges (and hence @δ@ / @Δ_cluster@) are reproducible.
--
-- The metrics compare two already-merged graphs ('Graph' inputs); the merge
-- itself is 'Graphos.Domain.Graph.Core.mergeGraphs', so callers fold each view
-- order first and hand the two results here. This keeps the quantifier pure and
-- independent of how the views were produced.
module Graphos.Domain.Graph.Conflict
  ( -- * Node / edge divergence
    nodeDisagreement
  , edgeDisagreement
  , deltaDivergence
    -- * Community-label flips
  , clusterFlips
    -- * Conflict set from a view family
  , conflictingNodeIds
    -- * Combined record
  , ConflictMetrics(..)
  , quantifyConflict
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.List (nub)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph(..))
import Graphos.Domain.Community
  ( Resolution
  , detectCommunitiesWithResolution
  , buildReverseIndex
  )

-- ── node / edge divergence ───────────────────────────────────────────────

-- | The node disagreement set @D_N(σ,τ)@: every id whose payload differs
-- between the two merged views. A id present in exactly one of the two graphs
-- counts as a disagreement there too (its payload is absent on one side).
--
-- Because 'mergeGraphs' is right-wins, an id whose payload is identical across
-- every view it appears in resolves to the same record under both orders and
-- therefore never lands here; only ids carrying ≥ 2 distinct payloads can
-- diverge, which is exactly the claim @D_N ⊆ K_conflict@.
nodeDisagreement :: Graph -> Graph -> Set NodeId
nodeDisagreement a b =
  let keys = Map.keysSet (gNodes a) `Set.union` Map.keysSet (gNodes b)
  in Set.filter (\nid -> nodePayloadOf a nid /= nodePayloadOf b nid) keys
  where
    nodePayloadOf :: Graph -> NodeId -> Maybe Node
    nodePayloadOf g nid = Map.lookup nid (gNodes g)

-- | The edge disagreement set @D_E(σ,τ)@: every edge key whose weight differs
-- between the two merged views. An edge present in exactly one graph (no weight
-- on one side) counts as a disagreement. As with 'nodeDisagreement', an edge
-- whose weight is identical across all views resolves identically under both
-- orders and never lands here, giving @|D_E| ≤ |conflicting edges|@.
edgeDisagreement :: Graph -> Graph -> Set (NodeId, NodeId)
edgeDisagreement a b =
  let keys = Map.keysSet (gEdges a) `Set.union` Map.keysSet (gEdges b)
  in Set.filter (\k -> edgeWeightAt a k /= edgeWeightAt b k) keys

-- | Total divergence @δ(σ,τ) = |D_N| + Σ_{(u,v) ∈ D_E} |A_uv^σ − A_uv^τ|@.
--
-- The node term is a cardinality; the edge term sums absolute weight deltas
-- over @D_E@. For consistent views both terms vanish, so @δ = 0@ — the content
-- confluence claim (T1/T3) recovered as a special case.
deltaDivergence :: Graph -> Graph -> Double
deltaDivergence a b =
  fromIntegral (Set.size (nodeDisagreement a b))
    + sum [ abs (edgeWeightAt a k - edgeWeightAt b k)
          | k <- Set.toList (edgeDisagreement a b) ]

-- ── community-label flips ─────────────────────────────────────────────────

-- | Community-label flip set @Δ_cluster(σ,τ)@: nodes whose community label
-- differs between the two merged views when each is clustered with the given
-- 'Resolution'.
--
-- This is the measured non-commutativity of @merge × cluster@ — it is computed,
-- not assumed zero. For consistent views the two merges are identical (T1), so
-- '@Δ_cluster = ∅@'; for conflicting views it can be non-empty but is bounded by
-- the spread of @D_N@ (INV-MONOTONE-CLUSTER).
clusterFlips :: Resolution -> Graph -> Graph -> Set NodeId
clusterFlips res a b =
  let ca = buildReverseIndex (detectCommunitiesWithResolution a res)
      cb = buildReverseIndex (detectCommunitiesWithResolution b res)
      keys = Map.keysSet ca `Set.union` Map.keysSet cb
  in Set.filter (\nid -> Map.findWithDefault (-1) nid ca /= Map.findWithDefault (-1) nid cb) keys

-- ── conflict set from a view family ───────────────────────────────────────

-- | The conflict set @K_conflict@: node ids carrying ≥ 2 distinct payloads
-- across the supplied view family. A payload is the full 'Node' record; an id
-- present in only one view carries a single payload and is therefore not
-- conflicting.
--
-- This is the superset that bounds @D_N@ (and, by the same argument over edge
-- weights, @D_E@): an id with a single payload resolves identically under every
-- fold order, so it can never appear in either disagreement set.
conflictingNodeIds :: [Graph] -> Set NodeId
conflictingNodeIds views =
  Set.fromList
    [ nid
    | (nid, payloads) <- Map.toList (foldNodePayloads Map.empty views)
    , length (nub payloads) >= 2
    ]
  where
    -- Accumulate the node records seen per id across views; an id conflicts when
    -- its payloads contain two records that are not equal (Node has Eq, not Ord).
    foldNodePayloads :: Map NodeId [Node] -> [Graph] -> Map NodeId [Node]
    foldNodePayloads acc = foldl' (\acc' g -> foldNodeInto acc' g) acc
    foldNodeInto :: Map NodeId [Node] -> Graph -> Map NodeId [Node]
    foldNodeInto acc' g =
      Map.foldlWithKey'
        (\acc'' nid node -> Map.insertWith (++) nid [node] acc'')
        acc' (gNodes g)

-- ── combined record ───────────────────────────────────────────────────────

-- | All divergence measurements between two merged views under one resolution.
data ConflictMetrics = ConflictMetrics
  { cmNodeDisagreement  :: !(Set NodeId)                 -- ^ @D_N@
  , cmEdgeDisagreement  :: !(Set (NodeId, NodeId))       -- ^ @D_E@
  , cmEdgeWeightDiff    :: !(Map (NodeId, NodeId) Double) -- ^ |A^σ − A^τ| per edge in @D_E@
  , cmDivergence        :: !Double                       -- ^ @δ@
  , cmClusterFlips      :: !(Set NodeId)                 -- ^ @Δ_cluster@
  } deriving (Eq, Show)

-- | Quantify the full conflict between two merged views at one resolution: the
-- node/edge disagreement sets, the scalar @δ@, and the community-flip set.
--
-- Every field is a pure deterministic function of the two graphs and the
-- resolution, so 'quantifyConflict' is reproducible for a fixed pair of orders
-- (the determinism claim for @δ@ / @Δ_cluster@).
quantifyConflict :: Resolution -> Graph -> Graph -> ConflictMetrics
quantifyConflict res a b =
  let de   = edgeDisagreement a b
      diff = Map.fromList [ (k, abs (edgeWeightAt a k - edgeWeightAt b k)) | k <- Set.toList de ]
  in ConflictMetrics
       { cmNodeDisagreement  = nodeDisagreement a b
       , cmEdgeDisagreement  = de
       , cmEdgeWeightDiff    = diff
       , cmDivergence        = deltaDivergence a b
       , cmClusterFlips      = clusterFlips res a b
       }

-- | Edge weight at a key, or @0@ when the edge is absent on that side. Used to
-- compare weights uniformly across graphs that may hold the edge in only one.
edgeWeightAt :: Graph -> (NodeId, NodeId) -> Double
edgeWeightAt g k = case Map.lookup k (gEdges g) of
  Just e -> edgeWeight e
  Nothing -> 0.0
