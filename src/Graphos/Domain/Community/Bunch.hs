{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Bunch search-based modularization (Mitchell & Mancoridis 2008), Domain
-- side (openspec change @bunch-search-based-modularization@, capability
-- @bunch-clustering@).
--
-- == The MDG (design D2)
--
-- A graph of any granularity folds to an MDG whose nodes are the distinct
-- 'nodeSourceFile' values, indexed in ascending path order so that the same
-- graph given in any node order yields the same MDG (permutation invariance,
-- T3). Every edge of a relation other than 'Contains' between two different
-- files contributes to the undirected pair; the pair weighs 1 under
-- 'UnitWeights' (the paper's default) or the sum of its edges' 'edgeWeight'
-- under 'EdgeWeights'. Self-pairs and edges with an endpoint outside the
-- graph's node map are dropped.
--
-- == The search (design D3–D5)
--
-- Hill climbing from a random partition over the paper's two candidate
-- kinds — /neighbouring partitions/ (one file moved to another cluster or to
-- a new singleton) and /building-block partitions/ (a new cluster of exactly
-- two files joined by an edge) — in canonical order (files ascending, target
-- clusters ascending, new singleton, then building blocks by ascending edge).
-- The threshold @η@ selects next-ascent (@η = 0@: first strict improvement),
-- steepest-ascent (@η = 100@: best of all) or anything between (best among at
-- least @⌈η·|C|⌉@ examined candidates). Simulated annealing accepts a worsening
-- candidate with probability @exp(ΔMQ / T)@, @T ← α·T@ after every draw.
--
-- MQ is maintained incrementally: a move touches only the source and
-- destination clusters (`GraphosFormalization.Bunch.MQ.cf_moveTo`), so each
-- candidate costs O(deg) and the running total is exact for integer weights.
-- Every random draw comes from a seeded 'Gen', and files are indexed before
-- any draw, so a run is a deterministic function of @(mdg, config, seed)@.
module Graphos.Domain.Community.Bunch
  ( -- * File-level MDG
    FileMDG(..)
  , foldToFileMDG
  , mdgFromPairs
  , mdgFileCount
  , mdgEdgeCount
  , mdgTotalWeight
    -- * Search configuration and results
  , BunchConfig(..)
  , defaultBunchConfig
  , BunchRun(..)
  , BunchResult(..)
    -- * Search
  , bunchCluster
  , bunchSearch
  , bunchSearchFrom
  , randomPartition
    -- * Scoring helpers (tests, landscape)
  , mqOfAssignment
  , intraOfAssignment
  , replayMoves
  , canonicalPartition
  , assignmentOf
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.STRef
import Data.Text.Short (ShortText)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word64)

import Graphos.Domain.Types (Edge(..), Node(..), Relation(..))
import Graphos.Domain.Graph (Graph, gNodes, gEdges)
import Graphos.Domain.Community.MQ (Weighting(..), clusterFactor)
import Graphos.Domain.Random (Gen, mkGen, uniformInt, uniformDouble)

-- ───────────────────────────────────────────────
-- File-level MDG
-- ───────────────────────────────────────────────

-- | The file-level MDG. Edges are stored once per unordered pair with
-- @mdgEdgeU < mdgEdgeV@, in ascending pair order; the adjacency lists are
-- ascending by neighbour index and parallel to their weights.
data FileMDG = FileMDG
  { mdgFiles :: V.Vector ShortText           -- ^ file paths, ascending; index = file id
  , mdgEdgeU :: VU.Vector Int                -- ^ smaller endpoint of edge e
  , mdgEdgeV :: VU.Vector Int                -- ^ larger endpoint of edge e
  , mdgEdgeW :: VU.Vector Double             -- ^ weight of edge e
  , mdgAdj   :: V.Vector (VU.Vector Int)     -- ^ neighbours of file i, ascending
  , mdgAdjW  :: V.Vector (VU.Vector Double)  -- ^ weights parallel to 'mdgAdj'
  } deriving (Eq, Show)

mdgFileCount :: FileMDG -> Int
mdgFileCount = V.length . mdgFiles

mdgEdgeCount :: FileMDG -> Int
mdgEdgeCount = VU.length . mdgEdgeU

mdgTotalWeight :: FileMDG -> Double
mdgTotalWeight = VU.sum . mdgEdgeW

-- | Build an MDG from file names (index = position) and weighted pairs.
-- Pairs are normalised to @u < v@, self-pairs and out-of-range endpoints are
-- dropped, and repeated pairs have their weights summed.
mdgFromPairs :: [ShortText] -> [((Int, Int), Double)] -> FileMDG
mdgFromPairs files rawPairs =
  let n = length files
      norm ((u, v), w)
        | u == v || u < 0 || v < 0 || u >= n || v >= n = Nothing
        | otherwise = Just ((min u v, max u v), w)
      pairs = Map.toAscList (Map.fromListWith (+) [ p | Just p <- map norm rawPairs ])
      adjMap = Map.fromListWith (++)
        (concat [ [ (i, [(j, w)]), (j, [(i, w)]) ] | ((i, j), w) <- pairs ])
      adjOf i = sortOn fst (Map.findWithDefault [] i adjMap)
      adjLists = [ adjOf i | i <- [0 .. n - 1] ]
  in FileMDG
       { mdgFiles = V.fromList files
       , mdgEdgeU = VU.fromList [ i | ((i, _), _) <- pairs ]
       , mdgEdgeV = VU.fromList [ j | ((_, j), _) <- pairs ]
       , mdgEdgeW = VU.fromList [ w | (_, w) <- pairs ]
       , mdgAdj   = V.fromList [ VU.fromList (map fst l) | l <- adjLists ]
       , mdgAdjW  = V.fromList [ VU.fromList (map snd l) | l <- adjLists ]
       }

-- | Fold a graph to its file-level MDG.
foldToFileMDG :: Weighting -> Graph -> FileMDG
foldToFileMDG weighting g =
  let fileOf = Map.map nodeSourceFile (gNodes g)
      files  = Map.keys (Map.fromList [ (f, ()) | f <- Map.elems fileOf ])
      idx    = Map.fromList (zip files [0 :: Int ..])

      fileIdx nid = Map.lookup nid fileOf >>= \f -> Map.lookup f idx

      -- Unit weighting: one per pair whatever the edge count, so collapse
      -- duplicates before 'mdgFromPairs' sums them.
      pairOf e
        | edgeRelation e == Contains = Nothing
        | otherwise =
            case (fileIdx (edgeSource e), fileIdx (edgeTarget e)) of
              (Just i, Just j) | i /= j ->
                Just ((min i j, max i j), case weighting of
                                            UnitWeights -> 1
                                            EdgeWeights -> edgeWeight e)
              _ -> Nothing

      contributions = [ p | Just p <- map pairOf (Map.elems (gEdges g)) ]
      pairs = case weighting of
        UnitWeights -> Map.toAscList (Map.fromList contributions)
        EdgeWeights -> contributions
  in mdgFromPairs files pairs

-- ───────────────────────────────────────────────
-- Configuration and results
-- ───────────────────────────────────────────────

data BunchConfig = BunchConfig
  { bcSeed          :: Word64                 -- ^ first seed of the population
  , bcPopulation    :: Int                    -- ^ independent searches, seeds @s..s+p-1@
  , bcThreshold     :: Int                    -- ^ η in percent, 0 (NAHC) .. 100 (SAHC)
  , bcAnnealing     :: Maybe (Double, Double) -- ^ @(T0, α)@ when simulated annealing is on
  , bcMaxIterations :: Int                    -- ^ termination guarantee
  } deriving (Eq, Show)

defaultBunchConfig :: BunchConfig
defaultBunchConfig = BunchConfig
  { bcSeed = 0
  , bcPopulation = 8
  , bcThreshold = 0
  , bcAnnealing = Nothing
  , bcMaxIterations = 10000
  }

-- | One search from one seed.
data BunchRun = BunchRun
  { brSeed             :: Word64
  , brStartMQ          :: Double
  , brStartClusters    :: Int
  , brFinalMQ          :: Double
  , brFinalClusters    :: Int
  , brIntraFraction    :: Double
  , brEvaluations      :: Int        -- ^ MQ evaluations (candidates examined)
  , brIterations       :: Int
  , brAnnealingAccepts :: Int        -- ^ non-improving candidates accepted
  , brMQTrace          :: [Double]   -- ^ MQ after every iteration, oldest first
  , brPartition        :: [[Int]]    -- ^ clusters as ascending file lists, ordered by smallest member
  } deriving (Eq, Show)

-- | The population's runs and its best run (highest MQ, lowest seed on ties).
data BunchResult = BunchResult
  { bresBest :: BunchRun
  , bresRuns :: [BunchRun]
  } deriving (Eq, Show)

-- ───────────────────────────────────────────────
-- Random partition and canonical output
-- ───────────────────────────────────────────────

-- | The paper's random start: a cluster count drawn uniformly from @1..n@,
-- then every file assigned uniformly to one of those clusters.
randomPartition :: Int -> Gen -> (VU.Vector Int, Gen)
randomPartition n g0
  | n <= 0 = (VU.empty, g0)
  | otherwise =
      let (k0, g1) = uniformInt n g0
          k = k0 + 1
          go 0 g acc = (VU.fromList (reverse acc), g)
          go i g acc = let (c, g') = uniformInt k g in go (i - 1 :: Int) g' (c : acc)
      in go n g1 []

-- | Clusters as ascending file lists, ordered by their smallest member.
canonicalPartition :: VU.Vector Int -> [[Int]]
canonicalPartition assign =
  let groups = Map.fromListWith (flip (++))
        [ (assign VU.! i, [i]) | i <- [0 .. VU.length assign - 1] ]
  in sortOn head' (Map.elems groups)
  where
    head' []      = maxBound
    head' (x : _) = x

-- ───────────────────────────────────────────────
-- Search state
-- ───────────────────────────────────────────────

-- | Mutable search state. Cluster ids live in @[0, n)@; a cluster is "live"
-- while its size is positive. 'ssLive' and 'ssFree' are the ascending sets of
-- live and empty ids, so "every other cluster" and "smallest free id" are
-- both O(k)/O(log n) and canonical.
data SearchState s = SearchState
  { ssAssign  :: VUM.MVector s Int
  , ssMu      :: VUM.MVector s Double
  , ssExt     :: VUM.MVector s Double
  , ssSize    :: VUM.MVector s Int
  , ssLive    :: STRef s (Set.Set Int)
  , ssFree    :: STRef s (Set.Set Int)
  , ssMQ      :: STRef s Double
  , ssEvals   :: STRef s Int
  , ssGen     :: STRef s Gen
  , ssTemp    :: STRef s Double
  , ssAccepts :: STRef s Int
  }

-- | Build the state for an assignment, compressing its cluster ids to
-- @[0, k)@ in ascending order of first appearance by id.
initState :: FileMDG -> VU.Vector Int -> Gen -> Double -> ST s (SearchState s)
initState m assign0 gen t0 = do
  let n = mdgFileCount m
      distinct = Map.fromList [ (c, ()) | c <- VU.toList assign0 ]
      relabel = Map.fromList (zip (Map.keys distinct) [0 :: Int ..])
      k = Map.size distinct
      assignL = [ Map.findWithDefault 0 c relabel | c <- VU.toList assign0 ]
  vA <- VUM.replicate n 0
  forM_ (zip [0 ..] assignL) $ \(i, c) -> VUM.write vA i c
  vMu <- VUM.replicate n 0
  vExt <- VUM.replicate n 0
  vSize <- VUM.replicate n 0
  forM_ assignL $ \c -> VUM.modify vSize (+ 1) c
  forM_ [0 .. mdgEdgeCount m - 1] $ \e -> do
    let u = mdgEdgeU m VU.! e
        v = mdgEdgeV m VU.! e
        w = mdgEdgeW m VU.! e
    cu <- VUM.read vA u
    cv <- VUM.read vA v
    if cu == cv
      then VUM.modify vMu (+ w) cu
      else do
        VUM.modify vExt (+ w) cu
        VUM.modify vExt (+ w) cv
  mq0 <- sumCF vMu vExt n
  live <- newSTRef (Set.fromList [0 .. k - 1])
  free <- newSTRef (Set.fromList [k .. n - 1])
  mqRef <- newSTRef mq0
  evals <- newSTRef 0
  genRef <- newSTRef gen
  tRef <- newSTRef t0
  accRef <- newSTRef 0
  pure SearchState
    { ssAssign = vA, ssMu = vMu, ssExt = vExt, ssSize = vSize
    , ssLive = live, ssFree = free, ssMQ = mqRef, ssEvals = evals
    , ssGen = genRef, ssTemp = tRef, ssAccepts = accRef }

-- | Σ CF over cluster ids ascending (an empty cluster has µ = 0, CF = 0).
sumCF :: VUM.MVector s Double -> VUM.MVector s Double -> Int -> ST s Double
sumCF vMu vExt n = go 0 0
  where
    go i acc
      | i >= n = pure acc
      | otherwise = do
          mu <- VUM.read vMu i
          ex <- VUM.read vExt i
          go (i + 1) (acc + clusterFactor mu ex)

-- | Weight of file @i@'s edges into cluster @a@, into cluster @b@, and
-- elsewhere.
neighbourWeights :: FileMDG -> SearchState s -> Int -> Int -> Int -> ST s (Double, Double, Double)
neighbourWeights m st i a b = go 0 0 0 0
  where
    nbs = mdgAdj m V.! i
    ws  = mdgAdjW m V.! i
    len = VU.length nbs
    go idx wa wb wo
      | idx >= len = pure (wa, wb, wo)
      | otherwise = do
          c <- VUM.read (ssAssign st) (nbs VU.! idx)
          let w = ws VU.! idx
          if c == a then go (idx + 1) (wa + w) wb wo
          else if c == b then go (idx + 1) wa (wb + w) wo
          else go (idx + 1) wa wb (wo + w)

-- | Move file @i@ to cluster @b@ (any id in @[0, n)@, empty or not) and
-- return ΔMQ. Only the source and destination clusters change.
moveFile :: FileMDG -> SearchState s -> Int -> Int -> ST s Double
moveFile m st i b = do
  a <- VUM.read (ssAssign st) i
  if a == b then pure 0 else do
    (wA, wB, wO) <- neighbourWeights m st i a b
    muA <- VUM.read (ssMu st) a
    exA <- VUM.read (ssExt st) a
    muB <- VUM.read (ssMu st) b
    exB <- VUM.read (ssExt st) b
    let muA' = muA - wA
        exA' = exA - (wB + wO) + wA
        muB' = muB + wB
        exB' = exB - wB + (wA + wO)
        delta = clusterFactor muA' exA' + clusterFactor muB' exB'
              - clusterFactor muA exA - clusterFactor muB exB
    VUM.write (ssMu st) a muA'
    VUM.write (ssExt st) a exA'
    VUM.write (ssMu st) b muB'
    VUM.write (ssExt st) b exB'
    VUM.write (ssAssign st) i b
    sizeA <- VUM.read (ssSize st) a
    sizeB <- VUM.read (ssSize st) b
    VUM.write (ssSize st) a (sizeA - 1)
    VUM.write (ssSize st) b (sizeB + 1)
    when (sizeA == 1) $ do
      modifySTRef' (ssLive st) (Set.delete a)
      modifySTRef' (ssFree st) (Set.insert a)
    when (sizeB == 0) $ do
      modifySTRef' (ssLive st) (Set.insert b)
      modifySTRef' (ssFree st) (Set.delete b)
    modifySTRef' (ssMQ st) (+ delta)
    pure delta

-- ───────────────────────────────────────────────
-- Candidates
-- ───────────────────────────────────────────────

data Candidate
  = MoveTo Int Int        -- ^ file, target live cluster
  | NewSingleton Int      -- ^ file to the smallest free id
  | Block Int Int         -- ^ edge (u, v) to a new two-file cluster
  deriving (Eq, Show)

-- | All candidates of the current partition in canonical order.
candidates :: FileMDG -> SearchState s -> ST s [Candidate]
candidates m st = do
  live <- readSTRef (ssLive st)
  free <- readSTRef (ssFree st)
  let n = mdgFileCount m
      liveL = Set.toAscList live
  perFile <- mapM (\i -> do
      a <- VUM.read (ssAssign st) i
      sz <- VUM.read (ssSize st) a
      let moves = [ MoveTo i c | c <- liveL, c /= a ]
          single = [ NewSingleton i | sz > 1, not (Set.null free) ]
      pure (moves ++ single)) [0 .. n - 1]
  blocks <- if Set.null free then pure [] else
    fmap concat $ mapM (\e -> do
      let u = mdgEdgeU m VU.! e
          v = mdgEdgeV m VU.! e
      au <- VUM.read (ssAssign st) u
      av <- VUM.read (ssAssign st) v
      szu <- VUM.read (ssSize st) au
      -- {u, v} already forming exactly that cluster is a no-op.
      pure [ Block u v | not (au == av && szu == 2) ]) [0 .. mdgEdgeCount m - 1]
  pure (concat perFile ++ blocks)

-- | Apply a candidate; return ΔMQ and the action that reverts it.
applyCandidate :: FileMDG -> SearchState s -> Candidate -> ST s (Double, ST s ())
applyCandidate m st cand = case cand of
  MoveTo i c -> do
    a <- VUM.read (ssAssign st) i
    d <- moveFile m st i c
    pure (d, () <$ moveFile m st i a)
  NewSingleton i -> do
    a <- VUM.read (ssAssign st) i
    f <- Set.findMin <$> readSTRef (ssFree st)
    d <- moveFile m st i f
    pure (d, () <$ moveFile m st i a)
  Block u v -> do
    au <- VUM.read (ssAssign st) u
    av <- VUM.read (ssAssign st) v
    f <- Set.findMin <$> readSTRef (ssFree st)
    d1 <- moveFile m st u f
    d2 <- moveFile m st v f
    pure (d1 + d2, do _ <- moveFile m st v av
                      _ <- moveFile m st u au
                      pure ())

-- | Strict improvement: above floating noise.
improves :: Double -> Bool
improves d = d > 1e-12

-- | One hill-climbing iteration. Returns whether the partition changed.
iteration :: FileMDG -> BunchConfig -> SearchState s -> ST s Bool
iteration m cfg st = do
  cands <- candidates m st
  let total = length cands
      -- ⌈η · |C| / 100⌉ candidates must be examined before the best is taken.
      minExamine = (bcThreshold cfg * total + 99) `div` 100
  go minExamine cands (0 :: Int) Nothing
  where
    bump = modifySTRef' (ssEvals st) (+ 1)

    -- Best candidate seen so far: keep the larger delta, earlier on ties.
    better c d Nothing = Just (c, d)
    better c d (Just (c0, d0)) = if d > d0 then Just (c, d) else Just (c0, d0)

    applyBest Nothing = pure False
    applyBest (Just (c, _)) = do
      _ <- applyCandidate m st c
      pure True

    -- Simulated annealing draw for a worsening candidate.
    anneal d = case bcAnnealing cfg of
      Nothing -> pure False
      Just (_, alpha) -> do
        t <- readSTRef (ssTemp st)
        g <- readSTRef (ssGen st)
        let (u, g') = uniformDouble g
            p = if t <= 0 then 0 else exp (d / t)
        writeSTRef (ssGen st) g'
        writeSTRef (ssTemp st) (alpha * t)
        if u < p
          then modifySTRef' (ssAccepts st) (+ 1) >> pure True
          else pure False

    go _ [] _ best = applyBest best
    go minEx (c : cs) idx best = do
      bump
      (d, revert) <- applyCandidate m st c
      if improves d
        then
          if bcThreshold cfg == 0
            then pure True                      -- NAHC: keep the first improvement
            else do
              revert
              let best' = better c d best
              if idx + 1 >= minEx
                then applyBest best'
                else go minEx cs (idx + 1) best'
        else if d < 0
          then do
            accepted <- anneal d
            if accepted then pure True else revert >> go minEx cs (idx + 1) best
          else revert >> go minEx cs (idx + 1) best

-- ───────────────────────────────────────────────
-- Runs
-- ───────────────────────────────────────────────

-- | Search from a given start assignment; the seed only feeds the annealing
-- draws. Cluster ids of @assign0@ may be arbitrary non-negative ints.
bunchSearchFrom :: BunchConfig -> Word64 -> VU.Vector Int -> FileMDG -> BunchRun
bunchSearchFrom cfg seed assign0 m = runST $ do
  let t0 = maybe 0 fst (bcAnnealing cfg)
  st <- initState m assign0 (mkGen seed) t0
  startMQ <- readSTRef (ssMQ st)
  startK <- Set.size <$> readSTRef (ssLive st)
  let loop iters trace
        | iters >= bcMaxIterations cfg = pure (iters, trace)
        | otherwise = do
            changed <- iteration m cfg st
            mq <- readSTRef (ssMQ st)
            if changed then loop (iters + 1) (mq : trace) else pure (iters, trace)
  (iters, trace) <- loop 0 []
  finalK <- Set.size <$> readSTRef (ssLive st)
  evals <- readSTRef (ssEvals st)
  accepts <- readSTRef (ssAccepts st)
  assign <- VU.freeze (ssAssign st)
  -- The running total carries the rounding of this run's move sequence, so
  -- two runs ending in the same partition could differ in the last bit and
  -- break "lowest seed on ties". Report the exact recompute over the
  -- canonical partition instead: equal partitions give bit-identical MQ.
  let part = canonicalPartition assign
      canon = assignmentOf (mdgFileCount m) part
      totalW = mdgTotalWeight m
      intra = intraOfAssignment m canon
  pure BunchRun
    { brSeed = seed
    , brStartMQ = startMQ
    , brStartClusters = startK
    , brFinalMQ = mqOfAssignment m canon
    , brFinalClusters = finalK
    , brIntraFraction = if totalW <= 0 then 0 else intra / totalW
    , brEvaluations = evals
    , brIterations = iters
    , brAnnealingAccepts = accepts
    , brMQTrace = reverse trace
    , brPartition = part
    }

-- | Assignment vector of a canonical partition: cluster j = its position.
assignmentOf :: Int -> [[Int]] -> VU.Vector Int
assignmentOf n part =
  VU.replicate n 0 VU.// [ (i, j) | (j, cl) <- zip [0 ..] part, i <- cl ]

intraWeight :: FileMDG -> SearchState s -> ST s Double
intraWeight m st = go 0 0
  where
    n = mdgFileCount m
    go i acc
      | i >= n = pure acc
      | otherwise = do
          mu <- VUM.read (ssMu st) i
          go (i + 1) (acc + mu)

-- | One search from one seed: the paper's random start, then hill climbing.
bunchSearch :: BunchConfig -> Word64 -> FileMDG -> BunchRun
bunchSearch cfg seed m =
  let (assign0, _) = randomPartition (mdgFileCount m) (mkGen seed)
  in bunchSearchFrom cfg seed assign0 m

-- | The population: seeds @bcSeed .. bcSeed + bcPopulation - 1@; the best run
-- has the highest MQ, the lowest seed on ties.
bunchCluster :: BunchConfig -> FileMDG -> BunchResult
bunchCluster cfg m =
  let p = max 1 (bcPopulation cfg)
      runs = [ bunchSearch cfg (bcSeed cfg + fromIntegral i) m | i <- [0 .. p - 1] ]
      pick b r = if brFinalMQ r > brFinalMQ b then r else b
  in case runs of
       [] -> error "bunchCluster: empty population"
       (r0 : rs) -> BunchResult { bresBest = foldl pick r0 rs, bresRuns = runs }

-- ───────────────────────────────────────────────
-- Scoring helpers
-- ───────────────────────────────────────────────

-- | MQ of an assignment recomputed from scratch (the oracle for the
-- incremental path).
mqOfAssignment :: FileMDG -> VU.Vector Int -> Double
mqOfAssignment m assign = runST $ do
  st <- initState m assign (mkGen 0) 0
  readSTRef (ssMQ st)

-- | Intra-edge weight of an assignment.
intraOfAssignment :: FileMDG -> VU.Vector Int -> Double
intraOfAssignment m assign = runST $ do
  st <- initState m assign (mkGen 0) 0
  intraWeight m st

-- | Apply single-file moves in order and report the incrementally maintained
-- MQ after each one. Target ids must lie in @[0, n)@.
replayMoves :: FileMDG -> VU.Vector Int -> [(Int, Int)] -> [Double]
replayMoves m assign0 moves = runST $ do
  st <- initState m assign0 (mkGen 0) 0
  mapM (\(i, b) -> moveFile m st i b >> readSTRef (ssMQ st)) moves
