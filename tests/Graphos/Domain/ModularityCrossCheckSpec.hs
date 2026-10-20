-- | AVI-618 §6 — the Haskell arm of the modularity cross-check.
--
-- Requirements:
-- @docs/math-requirements/AVI-618-auditable-modularity-lean-formalization-requirements.md@
-- §5 (pinned fixtures) and §6 (the @|Q_lean − Q_hs| ≤ eps@ contract).
--
-- For every pinned §5 fixture @f@ this module:
--
--   1. builds @g_f@ through @extractionFromLists@ \/ @buildGraph@,
--   2. computes @Q_hs(f) = modularity g_f cm_f@ (the shipped @Double@ path),
--   3. reads @Q_lean(f)@ — the *exact* rational the Lean definition reduces to
--      — from the checked-in Lean harness table
--      (@formalization/lean/crosscheck/q-lean.tsv@), and
--   4. asserts @|Q_lean(f) − Q_hs(f)| ≤ 2⁻²⁴@, plus @Q_lean(f)@ equals the §5
--      table value exactly (as a 'Rational').
--
-- The Lean arm is @formalization/lean/GraphosFormalization/Harness/CrossCheck.lean@;
-- regenerate the table with:
--
-- > cd formalization/lean && lake exe modularity-crosscheck --tsv > crosscheck/q-lean.tsv
--
-- The Lean side is exact (@ℚ@) because kernel Lean cannot evaluate @Float@
-- arithmetic (see the module header of @Phase1/CrossCheckFixtures.lean@); the
-- Haskell side is @Double@, so the only difference the tolerance must absorb is
-- rounding. @lean.yml@ re-runs the Lean harness with @--check@ (report-only) so
-- a Lean-side change that moves a value surfaces as table drift.
module Graphos.Domain.ModularityCrossCheckSpec (spec) where

import Control.Monad (forM_)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Short (fromText)
import GHC.Float (castDoubleToWord64)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
import Test.Hspec
import Text.Read (readMaybe)

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph, Graph(..), degree)
import Graphos.Domain.Community
  ( buildReverseIndex, detectCommunities, localMovesFrom, modularity )

-- | The contract's tolerance (§6): @eps = 2⁻²⁴@.
eps :: Double
eps = 1 / 16777216

-- | One row of the Lean harness table (exact values only — no @Float@).
data LeanRow = LeanRow
  { rowFixture   :: String
  , rowPartition :: String
  , rowM         :: Int
  , rowQnum      :: Integer
  , rowNum       :: Integer
  , rowDen       :: Integer
  } deriving (Eq, Show)

rowLabel :: LeanRow -> String
rowLabel r = "fixture " ++ rowFixture r ++ " (" ++ rowPartition r ++ ")"

----------------------------------------
-- Lean table loading
----------------------------------------

-- | Locate the checked-in Lean table by walking up from the test's working
-- directory (so both @cabal test@ from the package root and an IDE run work).
findLeanTable :: IO (Maybe FilePath)
findLeanTable = do
  cwd <- getCurrentDirectory
  search 6 cwd
  where
    rel = "formalization" </> "lean" </> "crosscheck" </> "q-lean.tsv"
    search 0 _ = pure Nothing
    search n dir = do
      let candidate = dir </> rel
      ok <- doesFileExist candidate
      if ok then pure (Just candidate) else search (n - 1 :: Int) (takeDirectory dir)

splitTabs :: String -> [String]
splitTabs s = case break (== '\t') s of
  (a, [])     -> [a]
  (a, _:rest) -> a : splitTabs rest

parseRow :: String -> Maybe LeanRow
parseRow line = case splitTabs line of
  [f, p, m, q, n, d, _dec] -> do
    m' <- readMaybe m
    q' <- readMaybe q
    n' <- readMaybe n
    d' <- readMaybe d
    if d' <= 0 then Nothing else Just (LeanRow f p m' q' n' d')
  _ -> Nothing

loadLeanTable :: IO [LeanRow]
loadLeanTable = do
  mpath <- findLeanTable
  case mpath of
    Nothing ->
      ioError (userError ("AVI-618 §6: Lean table not found; expected " ++
        "formalization/lean/crosscheck/q-lean.tsv (run `lake exe modularity-crosscheck --tsv`)"))
    Just path -> do
      contents <- readFile path
      let ls = [l | l <- lines contents, not (null l)
                  , not ("#" `prefixOf` l), not ("fixture" `prefixOf` l)]
      pure (mapMaybeWithLine parseRow ls)

-- | @mapMaybe@ that names the offending line on a parse failure.
mapMaybeWithLine :: (String -> Maybe a) -> [String] -> [a]
mapMaybeWithLine f = go
  where
    go [] = []
    go (l:ls) = case f l of
      Just x  -> x : go ls
      Nothing -> error ("AVI-618 §6: cannot parse Lean table row: " ++ show l)

prefixOf :: String -> String -> Bool
prefixOf p s = take (length p) s == p

----------------------------------------
-- Pinned fixtures (§5)
----------------------------------------

-- | A pinned row: the graph, the partition and the §5 table's exact @Q@.
data Pinned = Pinned
  { pinGraph :: Graph
  , pinCm    :: CommunityMap
  , pinExact :: Rational
  }

-- | Build a @CommunityMap@ from @(node, community)@ assignments.
communityMap :: [(Text, Int)] -> CommunityMap
communityMap = Map.fromListWith (++) . map (\(n, c) -> (c, [n]))

triangleGraph :: Graph
triangleGraph = buildGraph False $
  extractionFromLists (map testNode ["a", "b", "c"]) (cliqueEdges ["a", "b", "c"])

-- | §5 fixture 3 graph: two 4-cliques joined by the single bridge @d–e@.
twoCliquesGraph :: Graph
twoCliquesGraph = buildGraph False $
  extractionFromLists
    (map testNode ["a", "b", "c", "d", "e", "f", "g", "h"])
    (cliqueEdges ["a", "b", "c", "d"] ++ cliqueEdges ["e", "f", "g", "h"] ++ [testEdge "d" "e"])

-- | §5 fixture 4 graph: the path @p1—p2—p3—p4—p5—p6@.
path6Graph :: Graph
path6Graph = buildGraph False $
  extractionFromLists
    (map testNode ["p1", "p2", "p3", "p4", "p5", "p6"])
    [ testEdge "p1" "p2", testEdge "p2" "p3", testEdge "p3" "p4"
    , testEdge "p4" "p5", testEdge "p5" "p6" ]

triangleSingle :: CommunityMap
triangleSingle = communityMap [("a", 0), ("b", 0), ("c", 0)]

triangleSingletons :: CommunityMap
triangleSingletons = communityMap [("a", 0), ("b", 1), ("c", 2)]

twoCliquesSplit :: CommunityMap
twoCliquesSplit = communityMap
  [("a", 0), ("b", 0), ("c", 0), ("d", 0), ("e", 1), ("f", 1), ("g", 1), ("h", 1)]

path6Singletons :: CommunityMap
path6Singletons = communityMap [("p1", 0), ("p2", 1), ("p3", 2), ("p4", 3), ("p5", 4), ("p6", 5)]

-- | The partition the pinned table records as @Q(after)@ for fixture 4. The
-- test below asserts it equals @localMovesFrom path6Graph path6Singletons@, so
-- the table tracks the shipped code instead of a hand-copied snapshot.
path6AfterLocalMove :: CommunityMap
path6AfterLocalMove = communityMap [("p1", 0), ("p2", 0), ("p3", 0), ("p4", 1), ("p5", 2), ("p6", 2)]

pinnedFixtures :: Map.Map (String, String) Pinned
pinnedFixtures = Map.fromList
  [ (("1", "single"), Pinned triangleGraph triangleSingle 0)
  , (("2", "singletons"), Pinned triangleGraph triangleSingletons (-1 % 3))
  , (("3", "split (leiden)"), Pinned twoCliquesGraph twoCliquesSplit (11 % 26))
  , (("4a", "path6 singletons"), Pinned path6Graph path6Singletons (-9 % 50))
  , (("4b", "path6 after move"), Pinned path6Graph path6AfterLocalMove (11 % 50))
  , (("5a", "single"), Pinned twoCliquesGraph (communityMap [(n, 0) | n <- ["a","b","c","d","e","f","g","h"]]) 0)
  , (("5b", "singletons"), Pinned twoCliquesGraph
        (communityMap (zip ["a","b","c","d","e","f","g","h"] [0 ..])) (-43 % 338))
  , (("5c", "cross"), Pinned twoCliquesGraph
        (communityMap [("a", 0), ("b", 0), ("c", 0), ("e", 0), ("d", 1), ("f", 1), ("g", 1), ("h", 1)])
        (-1 % 26))
  , (("5d", "split (leiden)"), Pinned twoCliquesGraph twoCliquesSplit (11 % 26))
  , (("6", "split (leiden)"), Pinned twoCliquesGraph twoCliquesSplit (11 % 26))
  , (("7", "split (leiden)"), Pinned twoCliquesGraph twoCliquesSplit (11 % 26))
  ]

----------------------------------------
-- Spec
----------------------------------------

spec :: Spec
spec = do
  rows <- runIO loadLeanTable

  describe "AVI-618 §6 — Lean Q vs Haskell modularity (pinned fixtures)" $ do
    it "the Lean harness table names exactly the 11 pinned (fixture, partition) rows" $ do
      sort (map (\r -> (rowFixture r, rowPartition r)) rows)
        `shouldBe` sort (Map.keys pinnedFixtures)

    forM_ rows $ \r -> it (rowLabel r ++ ": |Q_lean - Q_hs| <= 2^-24 and Q_lean exact") $
      case Map.lookup (rowFixture r, rowPartition r) pinnedFixtures of
        Nothing -> expectationFailure ("no Haskell fixture for " ++ rowLabel r)
        Just pin -> checkRow r pin

  describe "AVI-618 §5 fixture assertions (Haskell side)" $ do
    it "fixture 1 — triangle, single community: Q == 0" $
      modularity triangleGraph triangleSingle `shouldBe` 0.0

    it "fixture 2 — triangle, all singletons: Q == -1/3" $
      abs (modularity triangleGraph triangleSingletons + 1 / 3) `shouldSatisfy` (<= eps)

    it "fixture 3 — two 4-cliques + bridge: Leiden output is the pinned split, Q in [-1/2, 1]" $ do
      communityMembershipSets (detectCommunities twoCliquesGraph)
        `shouldBe` communityMembershipSets twoCliquesSplit
      let q = modularity twoCliquesGraph twoCliquesSplit
      q `shouldSatisfy` (\x -> x >= -0.5 && x <= 1)
      abs (q - 11 / 26) `shouldSatisfy` (<= eps)

    it "fixture 4 — path p1..p6: localMovesFrom g cm0 is the pinned partition and Q does not decrease" $ do
      let cmAfter = localMovesFrom path6Graph path6Singletons
          qBefore = modularity path6Graph path6Singletons
          qAfter  = modularity path6Graph cmAfter
      communityMembershipSets cmAfter `shouldBe` communityMembershipSets path6AfterLocalMove
      qAfter `shouldSatisfy` (>= qBefore - eps)
      abs (qAfter - 11 / 50) `shouldSatisfy` (<= eps)

    it "fixture 5 — every reachable partition on the fixture stays in [-1/2, 1]" $ do
      let reachable =
            [ communityMap [(n, 0) | n <- ["a","b","c","d","e","f","g","h"]]
            , communityMap (zip ["a","b","c","d","e","f","g","h"] [0 ..])
            , communityMap [("a",0),("b",0),("c",0),("e",0),("d",1),("f",1),("g",1),("h",1)]
            , twoCliquesSplit
            , detectCommunities twoCliquesGraph
            ]
      forM_ reachable $ \cm -> do
        let q = modularity twoCliquesGraph cm
        q `shouldSatisfy` (\x -> x >= -0.5 && x <= 1)

    it "fixture 6 — Q recomputed from gEdges equals computeCommunityStats-based modularity" $ do
      let q = modularity twoCliquesGraph twoCliquesSplit
      q `shouldBe` modularityDirect twoCliquesGraph twoCliquesSplit
      q `shouldSatisfy` (> 0)

    it "fixture 7 — two modularity calls are bit-equal" $ do
      let q1 = modularity twoCliquesGraph twoCliquesSplit
          q2 = modularity twoCliquesGraph twoCliquesSplit
      castDoubleToWord64 q1 `shouldBe` castDoubleToWord64 q2

----------------------------------------
-- Cross-check core
----------------------------------------

-- | The §6 assertion, plus the two exact checks that keep it honest:
--
--   * @Q_lean@ equals the §5 table's exact value as a 'Rational' (no rounding
--     smuggling on the Lean side), and
--   * @m@ and the graph match the fixture the Lean row names.
checkRow :: LeanRow -> Pinned -> Expectation
checkRow r pin = do
  let g       = pinGraph pin
      cm      = pinCm pin
      qHs     = modularity g cm
      qLean   = fromIntegral (rowNum r) / fromIntegral (rowDen r) :: Double
      leanExact = rowNum r % rowDen r
      mHs     = Map.size (gEdges g)
      -- The contract's own quantity, evaluated in exact rational arithmetic:
      -- @|Q_lean - toRational Q_hs|@.
      exactDiff = abs (toRational qHs - leanExact)

  mHs `shouldBe` rowM r
  leanExact `shouldBe` pinExact pin
  -- The Lean numerator is over the unreduced denominator (2m)², so this ties
  -- `qnum`, `m` and the exact rational together.
  rowQnum r `shouldBe` round (pinExact pin * fromIntegral ((2 * rowM r) ^ (2 :: Int)))
  -- (c)+(d): Lean value vs the Haskell Double, within the contract tolerance.
  abs (qLean - qHs) `shouldSatisfy` (<= eps)
  exactDiff `shouldSatisfy` (<= 1 % (2 ^ (24 :: Int)))
  -- The §5 table value itself, against the Haskell Double.
  abs (qHs - fromRational (pinExact pin)) `shouldSatisfy` (<= eps)

-- | Independent audit oracle (CTO note 1 §2.2 fallback, as in @CommunitySpec@):
-- recompute @Q@ straight from @gEdges@, bypassing 'computeCommunityStats'.
modularityDirect :: Graph -> CommunityMap -> Double
modularityDirect g cm =
  let assign   = buildReverseIndex cm
      degrees  = Map.fromList [(nid, fromIntegral (degree g nid)) | nid <- Map.keys (gNodes g)]
      m        = sum (Map.elems degrees) / 2.0
      twoM     = 2.0 * m
      sigmaTot = Map.fromListWith (+)
            [ (Map.findWithDefault 0 nid assign, fromIntegral (degree g nid))
            | nid <- Map.keys (gNodes g) ]
      sigmaIn  = Map.fromListWith (+)
            [ (Map.findWithDefault 0 (edgeSource e) assign, 2.0)
            | e <- Map.elems (gEdges g)
            , Map.findWithDefault 0 (edgeSource e) assign
              == Map.findWithDefault 0 (edgeTarget e) assign ]
  in if m == 0.0 then 0.0
     else foldl
            (\q cid -> q + (Map.findWithDefault 0.0 cid sigmaIn) / twoM
                         - (Map.findWithDefault 0.0 cid sigmaTot / twoM)
                           * (Map.findWithDefault 0.0 cid sigmaTot / twoM))
            0.0 (Map.keys cm)

----------------------------------------
-- Helpers (duplicated from CommunitySpec for test isolation)
----------------------------------------

testNode :: Text -> Node
testNode nid = Node
  { nodeId = nid
  , nodeLabel = fromText nid
  , nodeFileType = CodeFile
  , nodeSourceFile = fromText "test.hs"
  , nodeSource = Nothing
  , nodeCommunityId = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodeLineStart = Just 1
  , nodeLineEnd = Nothing
  , nodeKind = Nothing
  , nodeSignature = Nothing
  , nodePresentBits = 0
  }

testEdge :: Text -> Text -> Edge
testEdge src tgt = Edge (EdgeId (src <> "->" <> tgt)) src tgt Calls 1.0 (Confidence 1.0) Nothing

-- | All-pairs edges over a node list (undirected clique).
cliqueEdges :: [Text] -> [Edge]
cliqueEdges ns = [testEdge a b | a <- ns, b <- ns, a < b]

-- | Community structure as a set of member sets (ignores community IDs).
communityMembershipSets :: CommunityMap -> Set (Set NodeId)
communityMembershipSets = Set.fromList . map Set.fromList . Map.elems
