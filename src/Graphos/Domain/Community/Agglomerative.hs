-- | Agglomerative hierarchical clustering of files
-- (clustering-evaluation-and-selection, design D4).
--
-- The feature vector of file @i@ is row @i@ of the file dependency matrix
-- @W@, which is the Bunch file-level MDG folded with edge weights
-- ('fileMatrix'): same files, same relation allowlist, no second fold. Rows
-- are sparse ('mdgAdj' / 'mdgAdjW').
--
-- Every linkage (single, average, complete) runs the generic algorithm with
-- a cached nearest neighbour per cluster and Lance–Williams updates
-- (min / weighted mean / max). It always merges the globally closest pair,
-- ties broken by the smallest @(cluster id, cluster id)@ — the naive O(F³)
-- rule — so the result equals the naive reference exactly, ties included.
-- (A minimum spanning tree cut, or the nearest-neighbour chain, can resolve a
-- tie at the cut differently.)
--
-- Cluster ids are the smallest file index of the cluster. Pure: 'runST' over
-- an unboxed upper-triangle distance matrix (O(F²) memory, hence the file cap
-- in the UseCase layer).
module Graphos.Domain.Community.Agglomerative
  ( Distance(..)
  , Linkage(..)
  , distanceName
  , linkageName
  , fileMatrix
  , rowDistance
  , featureRow
  , clusterCountFor
  , agglomerate
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Graphos.Domain.Types (Relation)
import Graphos.Domain.Graph (Graph)
import Graphos.Domain.Community.Bunch (FileMDG(..), foldToFileMDGWith, mdgFileCount)
import Graphos.Domain.Community.MQ (Weighting(..))

data Distance = Euclidean | Manhattan | Cosine
  deriving (Eq, Ord, Show, Enum, Bounded)

data Linkage = Single | Average | Complete
  deriving (Eq, Ord, Show, Enum, Bounded)

distanceName :: Distance -> String
distanceName d = case d of Euclidean -> "euclidean"; Manhattan -> "manhattan"; Cosine -> "cosine"

linkageName :: Linkage -> String
linkageName l = case l of Single -> "single"; Average -> "average"; Complete -> "complete"

-- | The file dependency matrix @W@: the Bunch MDG over @rels@ folded with
-- edge weights, so @W[i][j]@ is the summed weight of the dependency edges
-- between files @i@ and @j@.
fileMatrix :: [Relation] -> Graph -> FileMDG
fileMatrix rels = foldToFileMDGWith rels EdgeWeights

-- | Number of clusters for a divisor: @K = ⌈F / q⌉@, at least 1 when @F > 0@.
clusterCountFor :: Int -> Int -> Int
clusterCountFor f q
  | f <= 0 = 0
  | otherwise = max 1 ((f + max 1 q - 1) `div` max 1 q)

type Row = (VU.Vector Int, VU.Vector Double)

-- | Feature row of file @i@: row @i@ of @W@ with the self entry @W[i][i]@ set
-- to the row's largest weight, so two files that depend only on each other
-- get equal rows (without it their rows are orthogonal). A file with no
-- dependency keeps a zero row.
featureRow :: FileMDG -> Int -> Row
featureRow m i =
  let ix = mdgAdj m V.! i
      ws = mdgAdjW m V.! i
  in if VU.null ws then (ix, ws) else
       let self = VU.maximum ws
           (lo, hi) = VU.span (< i) ix
           k = VU.length lo
       in ( lo VU.++ VU.singleton i VU.++ hi
          , VU.take k ws VU.++ VU.singleton self VU.++ VU.drop k ws )

-- | Distance between two sparse rows (indices ascending). Cosine distance is
-- @1 − cos@, and 1 when either row is zero.
rowDistance :: Distance -> Row -> Row -> Double
rowDistance d (ia, wa) (ib, wb) = case d of
  Euclidean -> sqrt (max 0 (norm2 wa + norm2 wb - 2 * dot))
  Manhattan -> manhattan
  Cosine
    | na == 0 || nb == 0 -> 1
    | otherwise -> max 0 (1 - dot / (na * nb))
  where
    norm2 = VU.foldl' (\s x -> s + x * x) 0
    na = sqrt (norm2 wa)
    nb = sqrt (norm2 wb)
    -- (shared index weights) by a merge of the two ascending index lists
    shared = go 0 0 []
      where
        go i j acc
          | i >= VU.length ia || j >= VU.length ib = acc
          | ka < kb = go (i + 1) j acc
          | ka > kb = go i (j + 1) acc
          | otherwise = go (i + 1) (j + 1) ((wa VU.! i, wb VU.! j) : acc)
          where ka = ia VU.! i; kb = ib VU.! j
    dot = sum [ x * y | (x, y) <- shared ]
    manhattan =
      VU.sum (VU.map abs wa) + VU.sum (VU.map abs wb)
        - sum [ abs x + abs y - abs (x - y) | (x, y) <- shared ]

-- | Index of pair @i < j@ in a row-major upper triangle of size @n@.
tri :: Int -> Int -> Int -> Int
tri n i j = i * (2 * n - i - 1) `div` 2 + (j - i - 1)

-- | Cluster the MDG's files into exactly @min K F@ clusters (each a list of
-- file indices, ascending; clusters ordered by their smallest index).
agglomerate :: Distance -> Linkage -> Int -> FileMDG -> [[Int]]
agglomerate dist link k m
  | n == 0 = []
  | kk >= n = [ [i] | i <- [0 .. n - 1] ]
  | otherwise = genericLinkage
  where
    n = mdgFileCount m
    kk = max 1 k
    d i j = rowDistance dist (featureRow m i) (featureRow m j)

    genericLinkage = runST $ do
      dm <- VUM.new (n * (n - 1) `div` 2) :: ST s (VUM.MVector s Double)
      forM_ [0 .. n - 1] $ \i -> forM_ [i + 1 .. n - 1] $ \j ->
        VUM.write dm (tri n i j) (d i j)
      sizes <- VUM.replicate n (1 :: Int)
      alive <- VUM.replicate n True
      label <- VU.thaw (VU.generate n id)        -- file -> cluster id
      nn    <- VUM.replicate n (-1 :: Int)       -- cached nearest neighbour
      let dist' a b = VUM.read dm (tri n (min a b) (max a b))
          -- nearest live neighbour of a, ties to the smallest id
          nearest a = do
            let loop j best bestD
                  | j >= n = pure best
                  | j == a = loop (j + 1) best bestD
                  | otherwise = do
                      al <- VUM.read alive j
                      if not al then loop (j + 1) best bestD else do
                        dj <- dist' a j
                        if best < 0 || dj < bestD then loop (j + 1) j dj
                          else loop (j + 1) best bestD
            loop 0 (-1) 0
      forM_ [0 .. n - 1] $ \a -> nearest a >>= VUM.write nn a
      let step remaining
            | remaining <= kk = pure ()
            | otherwise = do
                -- global closest pair by (distance, a, b), a < b
                let scan a best
                      | a >= n = pure best
                      | otherwise = do
                          al <- VUM.read alive a
                          b <- VUM.read nn a
                          if not al || b < 0 then scan (a + 1) best else do
                            dab <- dist' a b
                            let cand = (dab, min a b, max a b)
                            scan (a + 1) (case best of
                                            Nothing -> Just cand
                                            Just c  -> Just (min c cand))
                mbest <- scan 0 Nothing
                case mbest of
                  Nothing -> pure ()
                  Just (_, a, b) -> do
                    -- merge b into a (a < b keeps the smallest id)
                    na <- VUM.read sizes a
                    nb <- VUM.read sizes b
                    forM_ [0 .. n - 1] $ \c -> when (c /= a && c /= b) $ do
                      al <- VUM.read alive c
                      when al $ do
                        dac <- dist' a c
                        dbc <- dist' b c
                        let new = case link of
                              Complete -> max dac dbc
                              Average  -> (fromIntegral na * dac + fromIntegral nb * dbc)
                                            / fromIntegral (na + nb)
                              Single   -> min dac dbc
                        VUM.write dm (tri n (min a c) (max a c)) new
                    VUM.write sizes a (na + nb)
                    VUM.write alive b False
                    forM_ [0 .. n - 1] $ \f -> do
                      l <- VUM.read label f
                      when (l == b) $ VUM.write label f a
                    -- refresh cached neighbours that pointed at a or b, and a's
                    forM_ [0 .. n - 1] $ \c -> do
                      al <- VUM.read alive c
                      when al $ do
                        nc <- VUM.read nn c
                        when (c == a || nc == a || nc == b) $ nearest c >>= VUM.write nn c
                        -- a's distances changed: a may now be c's nearest
                        when (c /= a && nc /= a && nc /= b) $ do
                          dca <- dist' c a
                          dcn <- if nc < 0 then pure (1 / 0) else dist' c nc
                          when (dca < dcn || (dca == dcn && a < nc)) $ VUM.write nn c a
                    step (remaining - 1)
      step n
      lab <- VU.freeze label
      pure (toClusters [ (i, lab VU.! i) | i <- [0 .. n - 1] ])

    toClusters assign =
      let byLabel = Map.fromListWith (flip (++)) [ (l, [i]) | (i, l) <- assign ]
      in sort (map sort (Map.elems byLabel))
