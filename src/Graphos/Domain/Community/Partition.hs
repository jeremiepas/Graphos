-- | Partitions of a node set and the measures that compare them
-- (clustering-evaluation-and-selection, design D2–D3): restriction to common
-- nodes, meet, directory ground truth, MoJo distance, MoJoFM and stability.
--
-- Pure: no IO, no graph. The UseCase layer folds a graph's communities to a
-- 'Partition' of files and calls these functions.
module Graphos.Domain.Community.Partition
  ( -- * Partitions
    Partition
  , fromAssignment
  , fromGroups
  , toAssignment
  , groups
  , elements
  , size
  , restrictTo
  , restrictCommon
  , meet
  , meetAll
  , refines
  , samePartition
    -- * Ground truth
  , directoryGroup
  , directoryTruth
    -- * MoJo
  , mno
  , maxMno
  , MoJoResult(..)
  , mojoFM
  , stability
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (sortOn, sortBy)
import Data.Ord (Down(..), comparing)
import Data.Text (Text)
import qualified Data.Text as T

-- ───────────────────────────────────────────────
-- Partitions
-- ───────────────────────────────────────────────

-- | A partition of a finite set: every element maps to the label of its
-- group. Labels are opaque; two partitions are equal when they group the same
-- elements together ('samePartition'), whatever their labels.
newtype Partition a = Partition (Map a Int)
  deriving (Eq, Show)

-- | Build a partition from any labelling; labels are renumbered densely in
-- ascending order of the original label.
fromAssignment :: Ord l => Map a l -> Partition a
fromAssignment m =
  let labels = Map.fromList (zip (Set.toAscList (Set.fromList (Map.elems m))) [0 ..])
  in Partition (Map.map (\l -> Map.findWithDefault 0 l labels) m)

-- | Build a partition from its groups. An element listed in several groups
-- keeps the last one; empty groups vanish.
fromGroups :: Ord a => [[a]] -> Partition a
fromGroups gs = Partition (Map.fromList [ (x, i) | (i, g) <- zip [0 ..] gs, x <- g ])

toAssignment :: Partition a -> Map a Int
toAssignment (Partition m) = m

-- | The groups, each ascending, ordered by their smallest element.
groups :: Ord a => Partition a -> [[a]]
groups (Partition m) =
  sortOn head' (Map.elems (Map.fromListWith (flip (++)) [ (l, [x]) | (x, l) <- Map.toAscList m ]))
  where head' xs = case xs of (x : _) -> Just x; [] -> Nothing

elements :: Partition a -> Set a
elements (Partition m) = Map.keysSet m

size :: Partition a -> Int
size (Partition m) = Map.size m

restrictTo :: Ord a => Set a -> Partition a -> Partition a
restrictTo s (Partition m) = Partition (Map.restrictKeys m s)

-- | Both partitions restricted to the elements they share, with the number of
-- elements dropped from each side.
restrictCommon :: Ord a => Partition a -> Partition a -> (Partition a, Partition a, Int, Int)
restrictCommon p q =
  let common = Set.intersection (elements p) (elements q)
  in ( restrictTo common p, restrictTo common q
     , size p - Set.size common, size q - Set.size common )

-- | The meet on the common elements: two elements share a group iff they
-- share one in both partitions.
meet :: Ord a => Partition a -> Partition a -> Partition a
meet p q =
  let (Partition p', Partition q', _, _) = restrictCommon p q
  in fromAssignment (Map.intersectionWith (,) p' q')

-- | The meet of several partitions (the multi-ref ground truth, design D2),
-- with the number of elements dropped because some partition lacks them.
-- The meet of no partition is empty.
meetAll :: Ord a => [Partition a] -> (Partition a, Int)
meetAll [] = (Partition Map.empty, 0)
meetAll ps@(p0 : rest) =
  let m = foldl meet p0 rest
      everything = Set.unions (map elements ps)
  in (m, Set.size everything - size m)

-- | @p `refines` q@: every group of @p@ lies inside one group of @q@ (on the
-- elements of @p@, which must all be in @q@).
refines :: Ord a => Partition a -> Partition a -> Bool
refines (Partition p) (Partition q) =
  all (\g -> Set.size g <= 1)
    (Map.elems (Map.fromListWith Set.union
       [ (lp, Set.singleton lq) | (x, lp) <- Map.toList p, Just lq <- [Map.lookup x q] ]))
  && Map.keysSet p `Set.isSubsetOf` Map.keysSet q

-- | Same elements, same grouping, labels ignored.
samePartition :: Ord a => Partition a -> Partition a -> Bool
samePartition p q = groups p == groups q

-- ───────────────────────────────────────────────
-- Ground truth from directories (design D2)
-- ───────────────────────────────────────────────

-- | The directory of a file path truncated to depth @d@: @src/a/b/x.ts@ at
-- @d = 2@ is @src/a@. A leading @./@ is ignored; a file shallower than @d@
-- keeps its whole directory (@.@ at the root).
directoryGroup :: Int -> Text -> Text
directoryGroup d path =
  let p = maybe path id (T.stripPrefix "./" path)
      dirs = case T.splitOn "/" p of
        [] -> []
        parts -> take (length parts - 1) parts
  in case take (max 0 d) dirs of
       [] -> "."
       ds -> T.intercalate "/" ds

-- | The directory ground truth of a file set at depth @d@.
directoryTruth :: Int -> [Text] -> Partition Text
directoryTruth d files = fromAssignment (Map.fromList [ (f, directoryGroup d f) | f <- files ])

-- ───────────────────────────────────────────────
-- MoJo and MoJoFM (design D3)
-- ───────────────────────────────────────────────

-- | @mno(A, B)@, the one-way MoJo distance of Wen & Tzerpos (2004): the
-- fewest Move and Join operations turning @A@ into @B@, on their common
-- elements. Each cluster of @A@ is tagged with a group of @B@ it shares the
-- most elements with; the elements outside their cluster's tag are moved
-- (@M@), and the clusters tagged with the same group are joined. Tagging
-- maximises the number @g@ of distinct groups used, by a maximum bipartite
-- matching over the maximal-overlap pairs, so
-- @mno = M + l − g@ with @l@ the number of clusters of @A@.
mno :: Ord a => Partition a -> Partition a -> Int
mno a0 b0 =
  let (Partition a, Partition b, _, _) = restrictCommon a0 b0
      clusters = Map.elems (Map.fromListWith (++) [ (la, [x]) | (x, la) <- Map.toList a ])
      overlap c = Map.fromListWith (+) [ (Map.findWithDefault (-1) x b, 1 :: Int) | x <- c ]
      tagged = [ (length c - best, [ grp | (grp, v) <- Map.toList ov, v == best ])
               | c <- clusters
               , let ov = overlap c
               , let best = maximum (0 : Map.elems ov) ]
      moves = sum (map fst tagged)
      g = maxMatching (map snd tagged)
  in moves + length clusters - g

-- | Size of a maximum bipartite matching; left vertex @i@ may match any right
-- vertex in its list (Kuhn's augmenting paths, deterministic order).
maxMatching :: [[Int]] -> Int
maxMatching adj = Map.size (foldl augment Map.empty [0 .. length adj - 1])
  where
    adjMap = Map.fromList (zip [0 :: Int ..] adj)
    -- One augmenting-path search per left vertex, from a fresh visited set.
    augment owner u = maybe owner id (fst (tryAug owner Set.empty u))
    -- @owner@ maps a right vertex to its left vertex. The visited set is
    -- threaded through every branch, failed ones included (Kuhn), so each
    -- search visits a right vertex at most once.
    tryAug :: Map Int Int -> Set Int -> Int -> (Maybe (Map Int Int), Set Int)
    tryAug owner seen u = go (Map.findWithDefault [] u adjMap) seen
      where
        go [] s = (Nothing, s)
        go (v : vs) s
          | v `Set.member` s = go vs s
          | otherwise =
              let s' = Set.insert v s
              in case Map.lookup v owner of
                   Nothing -> (Just (Map.insert v u owner), s')
                   Just u' -> case tryAug owner s' u' of
                     (Just owner', s'') -> (Just (Map.insert v u owner'), s'')
                     (Nothing, s'')     -> go vs s''

-- | @max(mno(∀A, B))@ for a reference @B@ of @n@ elements with group sizes
-- @g₁ ≥ g₂ ≥ …@: @n − h@, where @h@ is the largest @k@ with @g_i ≥ k − i + 1@
-- for every @i ≤ k@ (the longest "staircase" of group sizes). Checked against
-- the maximum over every partition @A@ by brute force for @n ≤ 6@ in the test
-- suite (and @n ≤ 7@ when it was derived).
maxMno :: Ord a => Partition a -> Int
maxMno b =
  let sizes = sortBy (comparing Down) (map length (groups b))
      ok k = and [ s >= k - i | (i, s) <- zip [0 ..] (take k sizes) ] && length sizes >= k
      h = last (0 : [ k | k <- [1 .. length sizes], ok k ])
  in size b - h

-- | A MoJoFM score with its ingredients.
data MoJoResult = MoJoResult
  { mjScore        :: Double  -- ^ MoJoFM in [0, 100]
  , mjMno          :: Int     -- ^ mno(A, B) on the common elements
  , mjMaxMno       :: Int     -- ^ max(mno(∀A, B)) on the common elements
  , mjCommon       :: Int     -- ^ elements in both partitions
  , mjDroppedA     :: Int     -- ^ elements of A not in B
  , mjDroppedB     :: Int     -- ^ elements of B not in A
  } deriving (Eq, Show)

-- | @MoJoFM(A, B) = (1 − mno(A, B) / max(mno(∀A, B))) × 100@, @B@ the
-- reference, on the common elements; 100 when the maximum is 0.
mojoFM :: Ord a => Partition a -> Partition a -> MoJoResult
mojoFM a b =
  let (a', b', da, db) = restrictCommon a b
      d = mno a' b'
      mx = maxMno b'
      score = if mx == 0 then 100 else (1 - fromIntegral d / fromIntegral mx) * 100
  in MoJoResult { mjScore = score, mjMno = d, mjMaxMno = mx
                , mjCommon = size a', mjDroppedA = da, mjDroppedB = db }

-- | Stability of two builds' partitions: MoJoFM in both directions on their
-- common elements, @(MoJoFM(old, new), MoJoFM(new, old))@.
stability :: Ord a => Partition a -> Partition a -> (MoJoResult, MoJoResult)
stability old new = (mojoFM old new, mojoFM new old)
