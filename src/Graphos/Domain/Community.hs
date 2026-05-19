{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
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
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU
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

instance NFData LeidenState where
  rnf LeidenState{} = ()

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
        in VU.fromList [nidToIdx Map.! nb | nb <- Set.toList nbs]
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

localMovingPass :: LeidenState -> (LeidenState, Int)
localMovingPass st0 = go st0 0 0
  where
    n = lsN st0
    go :: LeidenState -> Int -> Int -> (LeidenState, Int)
    go !st !i !moved
      | i >= n = (st, moved)
      | otherwise =
          let assign = lsAssignment st
              ki = lsDegrees st VU.! i
              currentComm = assign VU.! i
              nbs = lsNeighbors st V.! i
              neighborComms = nubInt (VU.toList (VU.map (assign VU.!) nbs))
              bestComm = findBestCommunity st ki currentComm nbs neighborComms
          in if bestComm /= currentComm
             then let !st' = moveNode st i currentComm bestComm ki assign nbs
                  in go st' (i + 1) (moved + 1)
             else go st (i + 1) moved

nubInt :: [Int] -> [Int]
nubInt = go IntMap.empty
  where
    go _ []     = []
    go !seen (x:xs) = if IntMap.member x seen then go seen xs else x : go (IntMap.insert x () seen) xs

findBestCommunity :: LeidenState -> Double -> Int -> VU.Vector Int -> [Int] -> Int
findBestCommunity st ki currentComm nbs comms =
  let m           = lsM st
      gamma       = lsGamma st
      assign      = lsAssignment st
      sigmaTotMap = lsSigmaTot st
      twoM2       = 2.0 * m * m
      commOfNb    = VU.map (assign VU.!) nbs
      deltaQ targetComm =
        let sigmaTot = IntMap.findWithDefault 0.0 targetComm sigmaTotMap
            sigmaIn   = fromIntegral (VU.length (VU.filter (== targetComm) commOfNb))
        in sigmaIn / m - gamma * (sigmaTot * ki) / twoM2
      scores = [(c, deltaQ c) | c <- comms]
  in case scores of
       [] -> currentComm
       _  -> let (bestC, bestScore) = maximumBySnd scores
             in if bestScore > 0 then bestC else currentComm

moveNode :: LeidenState -> Int -> Int -> Int -> Double -> VU.Vector Int -> VU.Vector Int -> LeidenState
moveNode st i oldComm newComm ki assign nbs =
  let commOfNb = VU.map (assign VU.!) nbs
      edgesToOld = VU.length (VU.filter (== oldComm) commOfNb)
      edgesToNew = VU.length (VU.filter (== newComm) commOfNb)
      assign' = VU.unsafeUpd assign [(i, newComm)]
      sigTot  = lsSigmaTot st
      oldST   = IntMap.findWithDefault 0.0 oldComm sigTot
      newST   = IntMap.findWithDefault 0.0 newComm sigTot
      sigTot' = IntMap.insert oldComm (oldST - ki + fromIntegral edgesToOld) $
                IntMap.insert newComm (newST + ki - fromIntegral edgesToNew) sigTot
  in st { lsAssignment = assign', lsSigmaTot = sigTot' }

refineCommunitiesOpt :: LeidenState -> LeidenState
refineCommunitiesOpt st =
  let assign  = lsAssignment st
      n       = lsN st
      maxCid  = VU.maximum assign
      commMembers = IntMap.fromListWith (++) [(assign VU.! i, [i]) | i <- [0..n-1]]
      (assign', _nextCid) = IntMap.foldlWithKey' (\(acc, cid) _ members ->
        let wellConnected = [i | i <- members
                               , cohesionToCommunityIdx st acc i (acc VU.! i) > 0.5]
        in if length wellConnected < length members `div` 2
           then (Data.List.foldl' (\a i -> VU.unsafeUpd a [(i, cid)]) acc wellConnected, cid + 1)
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

mergeSmallCommunities :: Graph -> Resolution -> CommunityMap -> CommunityMap
mergeSmallCommunities g res commMap =
  let minSize = resMinSize res
      strategy = resMergeInto res
      smallComms = [(cid, members) | (cid, members) <- Map.toList commMap
                                   , length members < minSize]
  in if null smallComms
     then commMap
     else foldlStrict' (\acc small -> mergeOne g strategy acc small) commMap smallComms

mergeOne :: Graph -> MergeStrategy -> CommunityMap -> (CommunityId, [NodeId]) -> CommunityMap
mergeOne g strategy allComms (smallCid, smallMembers) =
  let targetCid = case strategy of
        MergeToLargest  -> largestCommunity allComms smallCid
        MergeToNeighbor -> bestNeighborCommunity g allComms smallMembers smallCid
      withoutSmall = Map.delete smallCid allComms
  in case Map.lookup targetCid withoutSmall of
       Just targetMembers -> Map.insert targetCid (targetMembers ++ smallMembers) withoutSmall
       Nothing            -> allComms

largestCommunity :: CommunityMap -> CommunityId -> CommunityId
largestCommunity commMap exclude =
  let candidates = [(cid, length members) | (cid, members) <- Map.toList commMap
                                           , cid /= exclude]
  in case sortOn (\(_, n) -> negate n) candidates of
       ((cid, _):_) -> cid
       []           -> exclude

bestNeighborCommunity :: Graph -> CommunityMap -> [NodeId] -> CommunityId -> CommunityId
bestNeighborCommunity g commMap smallMembers excludeCid =
  let reverseIdx = buildReverseIndex commMap
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

cohesionScore :: Graph -> [NodeId] -> Double
cohesionScore g members =
  let memberSet = Set.fromList members
      internalEdges = length [1 :: Int | nid <- members
                              , n <- Set.toList (neighbors g nid)
                              , n `Set.member` memberSet
                              , nid < n]
      totalPossible = max 1 (length members * (length members - 1) `div` 2)
  in fromIntegral internalEdges / fromIntegral totalPossible

scoreAllCohesion :: Graph -> CommunityMap -> CohesionMap
scoreAllCohesion g commMap = fmap (cohesionScore g) commMap

maximumBySnd :: Ord a => [(b, a)] -> (b, a)
maximumBySnd [] = error "maximumBySnd: empty list"
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