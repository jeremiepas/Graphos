{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Domain.Community
  ( detectCommunities
  , detectCommunitiesWithResolution
  , cohesionScore
  , scoreAllCohesion

  , Resolution(..)
  , MergeStrategy(..)
  , defaultResolution
  , mergeSmallCommunities

  , countMoves

  , buildReverseIndex
  , communityOf

  , selectRepresentatives
  , filterEdgesByNodeSet

  , CommunityStats(..)
  , computeCommunityStats
  ) where

import Control.DeepSeq (deepseq, NFData(..))
import Control.Monad.ST (ST, runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, neighbors, gNodes, gEdges)

data Resolution = Resolution
  { resGamma         :: Double
  , resMinSize       :: Int
  , resMergeInto     :: MergeStrategy
  , resMaxIterations :: Int
  } deriving (Eq, Show)

data MergeStrategy
  = MergeToNeighbor
  | MergeToLargest
  deriving (Eq, Show)

defaultResolution :: Resolution
defaultResolution = Resolution
  { resGamma     = 1.0
  , resMinSize   = 3
  , resMergeInto = MergeToNeighbor
  , resMaxIterations = 50
  }

detectCommunities :: Graph -> CommunityMap
detectCommunities g = detectCommunitiesWithResolution g defaultResolution

detectCommunitiesWithResolution :: Graph -> Resolution -> CommunityMap
detectCommunitiesWithResolution g res =
  let raw = leidenPhase g res
  in mergeSmallCommunities g res raw

buildReverseIndex :: CommunityMap -> Map NodeId CommunityId
buildReverseIndex commMap = Map.fromList
  [(nid, cid) | (cid, members) <- Map.toList commMap, nid <- members]

communityOf :: NodeId -> Map NodeId CommunityId -> Maybe CommunityId
communityOf nid reverseIdx = Map.lookup nid reverseIdx

data CommunityStats = CommunityStats
  { csSigmaIn  :: !(Map CommunityId Double)
  , csSigmaTot :: !(Map CommunityId Double)
  , csDegrees  :: !(Map NodeId Double)
  , csM        :: !Double
  }

computeCommunityStats :: Graph -> Map NodeId CommunityId -> CommunityStats
computeCommunityStats g assign =
  let degrees = Map.mapWithKey (\nid _n -> fromIntegral (Set.size (neighbors g nid))) (gNodes g)
      m = sum (Map.elems degrees) / 2.0
      sigmaTot = Map.fromListWith (+)
        [ (Map.findWithDefault 0 nid assign, Map.findWithDefault 0.0 nid degrees)
        | nid <- Map.keys (gNodes g)
        ]
      internalContribs = [ (Map.findWithDefault 0 (edgeSource e) assign, 2.0)
                         | e <- Map.elems (gEdges g)
                         , Map.findWithDefault 0 (edgeSource e) assign
                           == Map.findWithDefault 0 (edgeTarget e) assign
                         ]
      sigmaIn = Map.fromListWith (+) internalContribs
  in CommunityStats { csSigmaIn = sigmaIn, csSigmaTot = sigmaTot, csDegrees = degrees, csM = m }

-- ───────────────────────────────────────────────
-- Optimized Leiden algorithm using Int-indexed vectors
-- ───────────────────────────────────────────────

data LeidenState = LeidenState
  { lsNodeIds   :: !(V.Vector NodeId)
  , lsNeighbors  :: !(V.Vector (VU.Vector Int))
  , lsDegrees    :: !(VU.Vector Double)
  , lsAssignment :: !(VU.Vector Int)
  , lsSigmaTot   :: !(IntMap Double)
  , lsM          :: !Double
  , lsGamma      :: !Double
  , lsN          :: !Int
  }

-- | Fully forcing instance: unboxed vectors are in normal form at WHNF; the
-- boxed neighbor vector and the sigma-tot IntMap are forced structurally so
-- that 'deepseq' between Leiden iterations genuinely clears thunks.
instance NFData LeidenState where
  rnf st =
    rnf (lsNodeIds st) `seq`
    rnf (lsNeighbors st) `seq`
    lsDegrees st `seq`
    lsAssignment st `seq`
    rnf (lsSigmaTot st) `seq`
    lsM st `seq`
    lsGamma st `seq`
    lsN st `seq`
    ()

buildLeidenState :: Graph -> Resolution -> LeidenState
buildLeidenState g res =
  let nodeIds  = V.fromList (Map.keys (gNodes g))
      n       = V.length nodeIds
      nidToIdx = Map.fromList (zip (V.toList nodeIds) [0::Int ..])
      degrees = VU.generate n $ \i ->
        fromIntegral (Set.size (neighbors g (nodeIds V.! i)))
      m = VU.sum degrees / 2.0
      adj = V.generate n $ \i ->
        let nbs = neighbors g (nodeIds V.! i)
            -- Skip neighbors not in the index (dangling edges from
            -- cross-file references). Prevents Map.! crash.
        in VU.fromList [case Map.lookup nb nidToIdx of
                          Just idx -> idx
                          Nothing  -> i  -- self-loop fallback for dangling edge
                       | nb <- Set.toList nbs]
      assign0 = VU.generate n id
      sigTot0 = IntMap.fromListWith (+)
        [ (i, degrees VU.! i) | i <- [0..n-1] ]
  in LeidenState
       { lsNodeIds   = nodeIds
       , lsNeighbors  = adj
       , lsDegrees    = degrees
       , lsAssignment = assign0
       , lsSigmaTot   = sigTot0
       , lsM          = m
       , lsGamma      = resGamma res
       , lsN          = n
       }

-- | One local-moving pass over all nodes.
-- The assignment vector is thawed once, mutated in place per move (O(1) per
-- move instead of a full-vector copy), and frozen at the end. Visit order and
-- move decisions are identical to the previous immutable implementation.
localMovingPass :: LeidenState -> (LeidenState, Int)
localMovingPass st0 = runST $ do
  massign <- VU.thaw (lsAssignment st0)
  (moved, sigTotFinal) <- localMovingLoop massign st0
  assign' <- VU.unsafeFreeze massign
  pure (st0 { lsAssignment = assign', lsSigmaTot = sigTotFinal }, moved)

-- | Visit every node once, moving it to the best neighboring community.
-- Reads and writes go through the mutable assignment vector so that earlier
-- moves in the same pass are observed (Leiden semantics), at O(1) per move.
localMovingLoop :: forall s. VUM.MVector s Int -> LeidenState -> ST s (Int, IntMap Double)
localMovingLoop massign st0 = loop 0 0 (lsSigmaTot st0)
  where
    n     = lsN st0
    m     = lsM st0
    gamma = lsGamma st0
    loop :: Int -> Int -> IntMap Double -> ST s (Int, IntMap Double)
    loop !i !moved !sigTot
      | i >= n = pure (moved, sigTot)
      | otherwise = do
          currentComm <- VUM.unsafeRead massign i
          let ki  = lsDegrees st0 VU.! i
              nbs = lsNeighbors st0 V.! i
          commOfNb <- VU.mapM (VUM.unsafeRead massign) nbs
          let neighborComms = nubInt (VU.toList commOfNb)
              bestComm = bestCommunityFor m gamma sigTot ki currentComm commOfNb neighborComms
          if bestComm /= currentComm
            then do
              VUM.unsafeWrite massign i bestComm
              let edgesToOld = VU.length (VU.filter (== currentComm) commOfNb)
                  edgesToNew = VU.length (VU.filter (== bestComm) commOfNb)
                  oldST = IntMap.findWithDefault 0.0 currentComm sigTot
                  newST = IntMap.findWithDefault 0.0 bestComm sigTot
                  !sigTot' = IntMap.insert currentComm (oldST - ki + fromIntegral edgesToOld) $
                             IntMap.insert bestComm (newST + ki - fromIntegral edgesToNew) sigTot
              loop (i + 1) (moved + 1) sigTot'
            else loop (i + 1) moved sigTot

nubInt :: [Int] -> [Int]
nubInt = go IntMap.empty
  where
    go _ []     = []
    go !seen (x:xs) = if IntMap.member x seen then go seen xs else x : go (IntMap.insert x () seen) xs

-- | Pure modularity-gain decision: pick the best community for a node given
-- precomputed neighbor communities. Identical scoring to the previous
-- implementation; the assignment vector is no longer consulted directly
-- because callers precompute @commOfNb@ from the mutable vector.
bestCommunityFor :: Double -> Double -> IntMap Double -> Double -> Int -> VU.Vector Int -> [Int] -> Int
bestCommunityFor m gamma sigmaTotMap ki currentComm commOfNb comms =
  -- Edge case: graph with no edges (m = 0) → stay in current community
  if m <= 0
  then currentComm
  else
    let twoM2 = 2.0 * m * m
        deltaQ targetComm =
          let sigmaTot = IntMap.findWithDefault 0.0 targetComm sigmaTotMap
              sigmaIn   = fromIntegral (VU.length (VU.filter (== targetComm) commOfNb))
          in sigmaIn / m - gamma * (sigmaTot * ki) / twoM2
        scores = [(c, deltaQ c) | c <- comms]
    in case scores of
         [] -> currentComm
         _  -> let (bestC, bestScore) = maximumBySnd scores
               in if bestScore > 0 then bestC else currentComm

refineCommunitiesOpt :: LeidenState -> LeidenState
refineCommunitiesOpt st =
  let assign  = lsAssignment st
      n       = lsN st
  -- Edge case: empty or single-node graph → no refinement needed
  in if n == 0
     then st
     else
       let maxCid  = VU.maximum assign
           commMembers = IntMap.fromListWith (++) [(assign VU.! i, [i]) | i <- [0..n-1]]
           -- Reassignments for one community are applied in a single batched
           -- update (one vector copy per split community) instead of one full
           -- copy per reassigned node. Decision order and results are
           -- unchanged: each node belongs to exactly one community, and the
           -- accumulator still reflects all earlier communities' splits.
           (assign', _nextCid) = IntMap.foldlWithKey' (\(acc, cid) _ members ->
             let wellConnected = [i | i <- members
                                    , cohesionToCommunityIdx st acc i (acc VU.! i) > 0.5]
             in if length wellConnected < length members `div` 2
                then (VU.unsafeUpd acc [(i, cid) | i <- wellConnected], cid + 1)
                else (acc, cid)
             ) (assign, maxCid + 1) commMembers
       in st { lsAssignment = assign' }

cohesionToCommunityIdx :: LeidenState -> VU.Vector Int -> Int -> Int -> Double
cohesionToCommunityIdx st assign i cid =
  let nbs = lsNeighbors st V.! i
      commOfNb = VU.map (assign VU.!) nbs
      sameCommunity = VU.length (VU.filter (== cid) commOfNb)
      totalNbs = max 1 (VU.length nbs)
  in fromIntegral sameCommunity / fromIntegral totalNbs

leidenPhase :: Graph -> Resolution -> CommunityMap
leidenPhase g res =
  -- Edge case: empty graph (no nodes) → no communities
  if Map.null (gNodes g)
  then Map.empty
  else
    let st0     = buildLeidenState g res
        maxIter = if resMaxIterations res > 0 then resMaxIterations res else 50
        finalSt = leidenLoop st0 maxIter
    in leidenStateToCommunityMap finalSt

leidenLoop :: LeidenState -> Int -> LeidenState
leidenLoop st0 maxIter = go st0 maxIter (lsAssignment st0)
  where
    go :: LeidenState -> Int -> VU.Vector Int -> LeidenState
    go !st 0 _prev = st
    go !st remaining !prevAssign =
      let (st', moved) = localMovingPass st
      in if moved == 0
         then st'
         else let st'' = refineCommunitiesOpt st'
              in st'' `deepseq`
                 if lsAssignment st'' == prevAssign
                 then st''
                 else go st'' (remaining - 1) (lsAssignment st'')

leidenStateToCommunityMap :: LeidenState -> CommunityMap
leidenStateToCommunityMap st =
  let assign  = lsAssignment st
      n       = lsN st
      nodeIds = lsNodeIds st
      grouped = IntMap.fromListWith (++) [(assign VU.! i, [nodeIds V.! i]) | i <- [0..n-1]]
  in Map.fromList [(cid, members) | (cid, members) <- IntMap.toList grouped]

countMoves :: Map NodeId CommunityId -> Map NodeId CommunityId -> Int
countMoves old new = Map.size $ Map.filter id $ Map.intersectionWith (/=) old new

-- | Merge communities below the minimum size into a neighboring (or largest)
-- community.
--
-- The node→community reverse index is built once and updated incrementally per
-- merge (only the moved members change), instead of being rebuilt for every
-- small community. Members are looked up from the CURRENT map at merge time —
-- this fixes a bug where a small community that had received members from an
-- earlier merge was later merged using its stale initial member list, silently
-- dropping the received nodes from the result.
mergeSmallCommunities :: Graph -> Resolution -> CommunityMap -> CommunityMap
mergeSmallCommunities g res commMap =
  let minSize = resMinSize res
      strategy = resMergeInto res
      smallCids = [cid | (cid, members) <- Map.toList commMap
                       , length members < minSize]
  in if null smallCids
     then commMap
     else
       let revIdx0 = buildReverseIndex commMap
           step (!acc, !revIdx) cid = mergeOne g strategy acc revIdx minSize cid
       in fst (foldlStrict' step (commMap, revIdx0) smallCids)

-- | Merge one small community into its target, updating the reverse index for
-- the moved members. Skips communities that grew to the minimum size through
-- earlier merges (they are no longer small).
mergeOne :: Graph -> MergeStrategy -> CommunityMap -> Map NodeId CommunityId -> Int -> CommunityId
         -> (CommunityMap, Map NodeId CommunityId)
mergeOne g strategy allComms revIdx minSize smallCid =
  case Map.lookup smallCid allComms of
    Nothing -> (allComms, revIdx)
    Just members
      | length members >= minSize -> (allComms, revIdx)
      | otherwise ->
          let targetCid = case strategy of
                MergeToLargest  -> largestCommunity allComms smallCid
                MergeToNeighbor -> bestNeighborCommunity g revIdx allComms members smallCid
              withoutSmall = Map.delete smallCid allComms
          in case Map.lookup targetCid withoutSmall of
               Just targetMembers ->
                 ( Map.insert targetCid (targetMembers ++ members) withoutSmall
                 , foldl' (\ri nid -> Map.insert nid targetCid ri) revIdx members
                 )
               Nothing -> (allComms, revIdx)

largestCommunity :: CommunityMap -> CommunityId -> CommunityId
largestCommunity commMap exclude =
  let candidates = [(cid, length members) | (cid, members) <- Map.toList commMap
                                           , cid /= exclude]
  in case sortOn (\(_, n) -> negate n) candidates of
       ((cid, _):_) -> cid
       []           -> exclude

-- | Pick the neighboring community with the most edges to the small
-- community's members, using the maintained reverse index (no rebuild).
bestNeighborCommunity :: Graph -> Map NodeId CommunityId -> CommunityMap -> [NodeId] -> CommunityId -> CommunityId
bestNeighborCommunity g reverseIdx commMap smallMembers excludeCid =
  let edgeCounts = Map.fromListWith (+)
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

cohesionScore :: Graph -> [NodeId] -> Double
cohesionScore g members
  | length members <= 1 = 1.0  -- singleton is trivially cohesive
  | otherwise =
      let memberSet = Set.fromList members
          internalEdges = length [1 :: Int | nid <- members
                                  , n <- Set.toList (neighbors g nid)
                                  , n `Set.member` memberSet
                                  , nid < n]
          totalPossible = max 1 (length members * (length members - 1) `div` 2)
      in fromIntegral internalEdges / fromIntegral totalPossible

scoreAllCohesion :: Graph -> CommunityMap -> CohesionMap
scoreAllCohesion g commMap
  | Map.null commMap = Map.empty
  | otherwise = fmap (cohesionScore g) commMap

maximumBySnd :: Ord a => [(b, a)] -> (b, a)
maximumBySnd [] = error "maximumBySnd: empty list — this should never happen (all communities have at least one neighbor)"
maximumBySnd xs = foldl1 (\a@(_,sa) b@(_,sb) -> if sb > sa then b else a) xs

foldlStrict' :: (a -> b -> a) -> a -> [b] -> a
foldlStrict' _f z []     = z
foldlStrict' f z (x:xs) = let !z' = f z x in foldlStrict' f z' xs

-- ───────────────────────────────────────────────
-- Representative node selection (for sub-graph Neo4j push)
-- ───────────────────────────────────────────────

selectRepresentatives
  :: Graph
  -> CommunityMap
  -> Int
  -> [NodeId]
  -> Map CommunityId [NodeId]
selectRepresentatives g commMap topN artPoints =
  let reverseIdx  = buildReverseIndex commMap
      artCommMap  = Map.fromListWith (++)
        [ (case Map.lookup nid reverseIdx of
             Just cid -> cid
             Nothing  -> -1, [nid])
        | nid <- artPoints
        , Map.member nid reverseIdx
        ]
  in Map.mapWithKey (\cid members ->
        let sortedByDegree = sortOn (\nid -> negate (fromIntegral (Set.size (neighbors g nid)) :: Double)) members
            topNodes = take topN sortedByDegree
            artForComm = Map.findWithDefault [] cid artCommMap
            merged = Set.toList (Set.fromList (topNodes ++ artForComm))
       in merged
    ) commMap

filterEdgesByNodeSet
  :: Set.Set NodeId
  -> Map (NodeId, NodeId) Edge
  -> Map (NodeId, NodeId) Edge
filterEdgesByNodeSet nodeSet edges =
  Map.filterWithKey (\(src, tgt) _ -> src `Set.member` nodeSet && tgt `Set.member` nodeSet) edges