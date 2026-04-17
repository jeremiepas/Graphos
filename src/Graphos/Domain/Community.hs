-- | Community detection using Leiden algorithm with resolution control.
-- Uses the Leiden modularity maximization approach for community detection.
module Graphos.Domain.Community
  ( -- * Community detection
    detectCommunities
  , detectCommunitiesWithResolution
  , cohesionScore
  , scoreAllCohesion
  
    -- * Resolution control
  , Resolution(..)
  , MergeStrategy(..)
  , defaultResolution
  , mergeSmallCommunities

    -- * Reverse index
  , buildReverseIndex
  , communityOf
  ) where

import Data.List (sortOn, groupBy, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, neighbors, degree, gNodes, gEdges)

-- ───────────────────────────────────────────────
-- Resolution parameter
-- ───────────────────────────────────────────────

-- | Resolution controls how aggressively communities merge.
--   Higher values → fewer, larger communities.
--   Lower values  → more, smaller communities.
--
--   This is the γ parameter in the modularity formula:
--     Q = Σ [ (e_ii - γ × a_i²) ]
--   where γ = 1 is the standard Louvain/Leiden resolution.
data Resolution = Resolution
  { resGamma         :: Double   -- ^ Modularity resolution parameter (default: 1.0)
  , resMinSize       :: Int      -- ^ Minimum community size — smaller ones get merged (default: 3)
  , resMergeInto     :: MergeStrategy  -- ^ How to merge small communities
  } deriving (Eq, Show)

-- | How to merge communities that are too small
data MergeStrategy
  = MergeToNeighbor   -- ^ Merge into the neighboring community with most connections
  | MergeToLargest    -- ^ Merge into the single largest community
  deriving (Eq, Show)

defaultResolution :: Resolution
defaultResolution = Resolution
  { resGamma     = 1.0
  , resMinSize   = 3
  , resMergeInto = MergeToNeighbor
  }

-- | Detect communities with default resolution (gamma=1.0, minSize=3)
detectCommunities :: Graph -> CommunityMap
detectCommunities g = detectCommunitiesWithResolution g defaultResolution

-- | Detect communities with a custom resolution parameter
detectCommunitiesWithResolution :: Graph -> Resolution -> CommunityMap
detectCommunitiesWithResolution g res =
  let raw = leidenPhase g res (initialAssignment g)
  in mergeSmallCommunities g res raw
  where
    initialAssignment gr = Map.fromList $ zip (Map.keys (gNodes gr)) [0..]

-- | Build a reverse index: Map NodeId CommunityId for O(log N) lookups
buildReverseIndex :: CommunityMap -> Map NodeId CommunityId
buildReverseIndex commMap = Map.fromList
  [(nid, cid) | (cid, members) <- Map.toList commMap, nid <- members]

-- | Look up which community a node belongs to (O(log N) via reverse index)
communityOf :: NodeId -> Map NodeId CommunityId -> Maybe CommunityId
communityOf nid reverseIdx = Map.lookup nid reverseIdx

-- ───────────────────────────────────────────────
-- Leiden algorithm
-- ───────────────────────────────────────────────

-- | Single phase of Leiden algorithm with resolution parameter.
-- Max 50 iterations to prevent infinite loops from oscillation.
leidenPhase :: Graph -> Resolution -> Map NodeId CommunityId -> CommunityMap
leidenPhase g res assignment = leidenPhase' g res assignment 50

leidenPhase' :: Graph -> Resolution -> Map NodeId CommunityId -> Int -> CommunityMap
leidenPhase' _ _ assignment 0 = buildCommunityMap assignment  -- safety: max iterations
leidenPhase' g res assignment remaining =
  let improved = localMoving g res assignment
      refined = refineCommunities g improved
  in if refined == improved
     then buildCommunityMap refined
     else leidenPhase' g res refined (remaining - 1)

-- | Local moving: move each node to the best community (with resolution γ)
localMoving :: Graph -> Resolution -> Map NodeId CommunityId -> Map NodeId CommunityId
localMoving g res assignment = go assignment (Map.keys (gNodes g))
  where
    go assign [] = assign
    go assign (nid:nids) =
      let currentComm = Map.findWithDefault 0 nid assign
          nbs = Set.toList (neighbors g nid)
          neighborComms = nub [Map.findWithDefault currentComm n assign | n <- nbs]
          bestComm = bestCommunity g res assign nid neighborComms currentComm
      in go (Map.insert nid bestComm assign) nids

    bestCommunity _ res_ assign nid comms current =
      let scores = [(c, deltaModularity g res_ assign nid c) | c <- comms]
      in case scores of
           [] -> current
           _  -> let (bestC, bestScore) = maximumBySnd scores
                 in if bestScore > 0 then bestC else current

-- | Refine communities: check for badly connected communities.
-- Each split gets a unique new community ID (threading a counter through the fold).
-- IMPORTANT: cohesion is checked against the *original* assignment for this pass,
-- not the partially-mutated accumulator. This prevents cascading splits in one pass.
refineCommunities :: Graph -> Map NodeId CommunityId -> Map NodeId CommunityId
refineCommunities g assignment =
  let commMap = buildCommunityMap assignment
      maxCid = maximum (0 : Map.elems assignment)
      -- Use foldlWithKey' with counter: (assignment, nextCid) accumulator
      -- Cohesion is checked against the original assignment, not acc
      (finalAssign, _nextCid) = Map.foldlWithKey' (\(acc, cidCounter) cid members ->
        let wellConnected = [nid | nid <- members
                                  , cohesionToCommunity g assignment nid cid > 0.5]
        in if length wellConnected < length members `div` 2
           then (foldlStrict (\a nid -> Map.insert nid cidCounter a) acc wellConnected, cidCounter + 1)
           else (acc, cidCounter)
        ) (assignment, maxCid + 1) commMap
  in finalAssign
  where
    foldlStrict f z xs = go z xs
      where
        go acc []     = acc
        go acc (x:xs') = let acc' = f acc x in acc' `seq` go acc' xs'

-- ───────────────────────────────────────────────
-- Merge small communities
-- ───────────────────────────────────────────────

-- | After Leiden, merge communities that are too small.
-- Communities with fewer members than resMinSize get merged into
-- a larger neighboring community.
-- Uses foldl' so MergeToLargest always sees the current accumulator state.
mergeSmallCommunities :: Graph -> Resolution -> CommunityMap -> CommunityMap
mergeSmallCommunities g res commMap =
  let minSize = resMinSize res
      strategy = resMergeInto res
      smallComms = [(cid, members) | (cid, members) <- Map.toList commMap
                                   , length members < minSize]
  in if null smallComms
     then commMap
     else foldlStrict (\acc small -> mergeOne g strategy acc small acc) commMap smallComms
  where
    foldlStrict _f z []     = z
    foldlStrict f z (x:xs) = let z' = f z x in z' `seq` foldlStrict f z' xs

-- | Merge a single small community into its best neighbor
-- allComms is now the current accumulator (not the original commMap)
mergeOne :: Graph -> MergeStrategy -> CommunityMap -> (CommunityId, [NodeId]) -> CommunityMap -> CommunityMap
mergeOne g strategy allComms (smallCid, smallMembers) _ =
  let -- Find the best target community using the current state
      targetCid = case strategy of
        MergeToLargest  -> largestCommunity allComms smallCid
        MergeToNeighbor -> bestNeighborCommunity g allComms smallMembers smallCid
      -- Remove old community and add members to target
      withoutSmall = Map.delete smallCid allComms
  in case Map.lookup targetCid withoutSmall of
       Just targetMembers -> Map.insert targetCid (targetMembers ++ smallMembers) withoutSmall
       Nothing            -> allComms  -- shouldn't happen, keep as-is

-- | Find the largest community (by member count), excluding the given one
largestCommunity :: CommunityMap -> CommunityId -> CommunityId
largestCommunity commMap exclude =
  let candidates = [(cid, length members) | (cid, members) <- Map.toList commMap
                                           , cid /= exclude]
  in case sortOn (\(_, n) -> negate n) candidates of
       ((cid, _):_) -> cid
       []           -> exclude  -- fallback

-- | Find the neighboring community with the most edge connections
-- Uses reverse index for O(log N) community lookups instead of O(C*M) scan
bestNeighborCommunity :: Graph -> CommunityMap -> [NodeId] -> CommunityId -> CommunityId
bestNeighborCommunity g commMap smallMembers excludeCid =
  let reverseIdx = buildReverseIndex commMap
      -- Count edges from small community members to each other community
      edgeCounts = Map.fromListWith (+)
        [ (targetCid, 1 :: Int)
        | nid <- smallMembers
        , nb <- Set.toList (neighbors g nid)
        , Just targetCid <- [Map.lookup nb reverseIdx]
        , targetCid /= excludeCid
        ]
      best = case sortOn (\(_, n) -> negate n) (Map.toList edgeCounts) of
               ((cid, _):_) -> cid
               []           -> largestCommunity commMap excludeCid
  in best

-- ───────────────────────────────────────────────
-- Modularity calculation (with resolution γ)
-- ───────────────────────────────────────────────

-- | Calculate cohesion of a node to its community
cohesionToCommunity :: Graph -> Map NodeId CommunityId -> NodeId -> CommunityId -> Double
cohesionToCommunity g assign nid cid =
  let nbs = Set.toList (neighbors g nid)
      sameCommunity = length [n | n <- nbs, Map.findWithDefault (-1) n assign == cid]
      totalNbs = max 1 (length nbs)
  in fromIntegral sameCommunity / fromIntegral totalNbs

-- | Calculate modularity change when moving a node to a community
-- Uses resolution parameter γ: higher γ makes merging harder (needs stronger connection)
deltaModularity :: Graph -> Resolution -> Map NodeId CommunityId -> NodeId -> CommunityId -> Double
deltaModularity g res assign nid targetComm =
  let gamma = resGamma res
      m = fromIntegral (Map.size (gEdges g))
      ki = fromIntegral (degree g nid)
      nbs = Set.toList (neighbors g nid)
      sigmaIn = fromIntegral (length [n | n <- nbs, Map.findWithDefault (-1) n assign == targetComm])
      sigmaTot = sumKi targetComm assign
      -- Standard modularity with resolution: dQ = (sigmaIn/m) - gamma * (sigmaTot * ki)/(2m²)
      dQ = sigmaIn / m - gamma * (sigmaTot * ki) / (2 * m * m)
  in dQ
  where
    sumKi cid assign' =
      let nodesInComm = [n | (n, c) <- Map.toList assign', c == cid]
      in sum (fromIntegral . degree g <$> nodesInComm)

-- ───────────────────────────────────────────────
-- Building and scoring
-- ───────────────────────────────────────────────

-- | Build community map from assignment
buildCommunityMap :: Map NodeId CommunityId -> CommunityMap
buildCommunityMap assignment =
  let pairs = Map.toList assignment
      grouped = groupBy (\a b -> snd a == snd b) (sortOn snd pairs)
  in Map.fromList [(cid, map fst group) | group@((_, cid):_) <- grouped]

-- | Calculate cohesion score for a single community
cohesionScore :: Graph -> [NodeId] -> Double
cohesionScore g members =
  let memberSet = Set.fromList members
      internalEdges = length [1 :: Int | nid <- members
                              , n <- Set.toList (neighbors g nid)
                              , n `Set.member` memberSet
                              , nid < n]  -- count each edge once
      totalPossible = max 1 (length members * (length members - 1) `div` 2)
  in fromIntegral internalEdges / fromIntegral totalPossible

-- | Score all communities
scoreAllCohesion :: Graph -> CommunityMap -> CohesionMap
scoreAllCohesion g commMap = fmap (cohesionScore g) commMap

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

maximumBySnd :: Ord a => [(b, a)] -> (b, a)
maximumBySnd [] = error "maximumBySnd: empty list"
maximumBySnd xs = foldl1 (\a@(_,sa) b@(_,sb) -> if sb > sa then b else a) xs