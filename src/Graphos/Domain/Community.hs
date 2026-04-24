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

    -- * Stats (exposed for incremental writer)
  , CommunityStats(..)
  , computeCommunityStats
  ) where

import Data.List (sortOn, groupBy, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, neighbors, gNodes, gEdges)

data Resolution = Resolution
  { resGamma         :: Double
  , resMinSize       :: Int
  , resMergeInto     :: MergeStrategy
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
  }

detectCommunities :: Graph -> CommunityMap
detectCommunities g = detectCommunitiesWithResolution g defaultResolution

detectCommunitiesWithResolution :: Graph -> Resolution -> CommunityMap
detectCommunitiesWithResolution g res =
  let raw = leidenPhase g res (initialAssignment g)
  in mergeSmallCommunities g res raw
  where
    initialAssignment gr = Map.fromList $ zip (Map.keys (gNodes gr)) [0..]

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

leidenPhase :: Graph -> Resolution -> Map NodeId CommunityId -> CommunityMap
leidenPhase g res assignment = leidenPhase' g res assignment 50

leidenPhase' :: Graph -> Resolution -> Map NodeId CommunityId -> Int -> CommunityMap
leidenPhase' _ _ assignment 0 = buildCommunityMap assignment
leidenPhase' g res assignment remaining =
  let improved = localMoving g res assignment
      refined = refineCommunities g improved
  in if refined == improved
     then buildCommunityMap refined
     else leidenPhase' g res refined (remaining - 1)

localMoving :: Graph -> Resolution -> Map NodeId CommunityId -> Map NodeId CommunityId
localMoving g res assign0 = go assign0 allNodes sigmaTotMap0 degrees0 m0
  where
    allNodes = Map.keys (gNodes g)
    degrees0 = Map.mapWithKey (\nid _n -> fromIntegral (Set.size (neighbors g nid))) (gNodes g)
    m0 = sum (Map.elems degrees0) / 2.0
    sigmaTotMap0 = Map.fromListWith (+)
      [ (Map.findWithDefault 0 nid assign0, Map.findWithDefault 0.0 nid degrees0)
      | nid <- allNodes
      ]

    go assign [] _sigmaTotMap _degrees _m = assign
    go assign (nid:nids) sigmaTotMap degrees m =
      let currentComm = Map.findWithDefault 0 nid assign
          nbs = Set.toList (neighbors g nid)
          neighborComms = nub [Map.findWithDefault currentComm n assign | n <- nbs]
          ki = Map.findWithDefault 1.0 nid degrees
          bestComm = bestCommunity assign nid ki neighborComms currentComm sigmaTotMap m
          newAssign = Map.insert nid bestComm assign
      in go newAssign nids sigmaTotMap degrees m

    bestCommunity assign nid ki comms current sigmaTotMap m =
      let scores = [(c, deltaModFast m res assign g nid ki sigmaTotMap c) | c <- comms]
      in case scores of
           [] -> current
           _  -> let (bestC, bestScore) = maximumBySnd scores
                 in if bestScore > 0 then bestC else current

deltaModFast :: Double -> Resolution -> Map NodeId CommunityId -> Graph -> NodeId -> Double -> Map CommunityId Double -> CommunityId -> Double
deltaModFast m res assign g nid ki sigmaTotMap targetComm =
  let gamma = resGamma res
      sigmaTot = Map.findWithDefault 0.0 targetComm sigmaTotMap
      nbs = Set.toList (neighbors g nid)
      sigmaIn = fromIntegral (length [n | n <- nbs, Map.findWithDefault (-1) n assign == targetComm])
      dQ = sigmaIn / m - gamma * (sigmaTot * ki) / (2 * m * m)
  in dQ

refineCommunities :: Graph -> Map NodeId CommunityId -> Map NodeId CommunityId
refineCommunities g assignment =
  let commMap = buildCommunityMap assignment
      maxCid = maximum (0 : Map.elems assignment)
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

mergeOne :: Graph -> MergeStrategy -> CommunityMap -> (CommunityId, [NodeId]) -> CommunityMap -> CommunityMap
mergeOne g strategy allComms (smallCid, smallMembers) _ =
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

cohesionToCommunity :: Graph -> Map NodeId CommunityId -> NodeId -> CommunityId -> Double
cohesionToCommunity g assign nid cid =
  let nbs = Set.toList (neighbors g nid)
      sameCommunity = length [n | n <- nbs, Map.findWithDefault (-1) n assign == cid]
      totalNbs = max 1 (length nbs)
  in fromIntegral sameCommunity / fromIntegral totalNbs

buildCommunityMap :: Map NodeId CommunityId -> CommunityMap
buildCommunityMap assignment =
  let pairs = Map.toList assignment
      grouped = groupBy (\a b -> snd a == snd b) (sortOn snd pairs)
  in Map.fromList [(cid, map fst group) | group@((_, cid):_) <- grouped]

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