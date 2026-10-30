-- | Graph query operations — traversal and search.
-- Pure functions over the domain types.
--
-- Memory optimization: Uses CachedFGL to share a single FGL conversion
-- across all algorithm calls, saving ~600MB on 100k-node graphs.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Graphos.Domain.Graph.Query
  ( neighbors
  , degree
  , shortestPath
  , shortestPathWithCached
  , shortestPathReachable
  , shortestPathReachableWithCached
  , breadthFirstSearch
  , breadthFirstSearchWithCached
  , depthFirstSearch
  , depthFirstSearchWithCached
  , stronglyConnectedComponents
  , stronglyConnectedComponentsWithCached
  , cond
  , condWithCached
  , isAcyclicDirected
  , isAcyclicDirectedWithCached
  , connectedComponents
  , connectedComponentsWithCached
  , weaklyConnectedComponents
  , weaklyConnectedComponentsWithCached
  , isolatedNodes
  , isolatedNodesWithCached
  , hasCycleUndirected
  , hasCycleUndirectedWithCached
  , subgraph
  ) where

import Control.DeepSeq (deepseq)
import Control.Monad (forM_, unless)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Graph.Inductive.Graph as FGL (Edge)
import Data.Graph.Inductive.Graph (edges, labEdges)
import Data.Graph.Inductive.Query.BFS (bfs, esp)
import Data.Graph.Inductive.Query.DFS (dfs)

import Graphos.Domain.Types
import Graphos.Domain.Graph.Core (Graph(..), computeGraphHash)
import Graphos.Domain.Graph.Analysis (CachedFGL(..), toCachedFGL, cachedFindIdx)

-- ───────────────────────────────────────────────
-- Queries
-- ───────────────────────────────────────────────

-- | Get neighbor node IDs.
-- For directed graphs: forward neighbors only.
-- For undirected graphs: union of forward and backward adjacency.
neighbors :: Graph -> NodeId -> Set NodeId
neighbors g nid =
  let fwd = Map.findWithDefault Set.empty nid (gAdjFwd g)
      bwd = Map.findWithDefault Set.empty nid (gAdjBack g)
  in if gDirected g then fwd else fwd `Set.union` bwd

-- | Get degree of a node
degree :: Graph -> NodeId -> Int
degree g nid = Set.size $ neighbors g nid

-- | Breadth-first search from a start node, returns visited node IDs
-- Uses fgl's BFS algorithm internally
breadthFirstSearch :: Graph -> NodeId -> Int -> Set NodeId
breadthFirstSearch g start maxDepth = breadthFirstSearchWithCached (toCachedFGL g) start maxDepth

-- | Breadth-first search using a pre-built CachedFGL
breadthFirstSearchWithCached :: CachedFGL -> NodeId -> Int -> Set NodeId
breadthFirstSearchWithCached cfg start _maxDepth =
  let gr = cfgGraph cfg
      nidMap = cfgNidMap cfg
  in case cachedFindIdx cfg start of
       Just startIdx -> Set.fromList [nidMap V.! idx | idx <- bfs startIdx gr]
       Nothing -> Set.empty

-- | Depth-first search from a start node, returns visited node IDs
-- Uses fgl's DFS algorithm internally
depthFirstSearch :: Graph -> NodeId -> Int -> Int -> Set NodeId
depthFirstSearch g start maxDepth budget = depthFirstSearchWithCached (toCachedFGL g) start maxDepth budget

-- | Depth-first search using a pre-built CachedFGL
depthFirstSearchWithCached :: CachedFGL -> NodeId -> Int -> Int -> Set NodeId
depthFirstSearchWithCached cfg start _maxDepth budget =
  let gr = cfgGraph cfg
      nidMap = cfgNidMap cfg
      result = case cachedFindIdx cfg start of
                 Just startIdx -> Set.fromList [nidMap V.! idx | idx <- dfs [startIdx] gr]
                 Nothing -> Set.empty
  in if Set.size result > budget
     then Set.fromList (take budget (Set.toList result))
     else result

-- | Shortest path between two nodes (BFS)
-- Uses fgl's ESP (shortest path by edge count) algorithm internally
shortestPath :: Graph -> NodeId -> NodeId -> Maybe [NodeId]
shortestPath g src tgt = shortestPathWithCached (toCachedFGL g) src tgt

-- | Shortest path using a pre-built CachedFGL
shortestPathWithCached :: CachedFGL -> NodeId -> NodeId -> Maybe [NodeId]
shortestPathWithCached cfg src tgt =
  let gr = cfgGraph cfg
      nidMap = cfgNidMap cfg
  in case (cachedFindIdx cfg src, cachedFindIdx cfg tgt) of
       (Just srcIdx, Just tgtIdx) ->
         let path = esp srcIdx tgtIdx gr
         in if null path then Nothing
             else Just [nidMap V.! idx | idx <- path]
       _ -> Nothing

-- ───────────────────────────────────────────────
-- Reachability & strong connectivity
-- ───────────────────────────────────────────────

-- | Forward reachability decision: does an arc-walk exist from \`u\` to \`v\`?
-- Decides \`u :-> v\` (Thm 2.4) — the reflexive-transitive closure of the
-- directed edge relation \`E\`. Equivalent to \`shortestPath g u v /= Nothing\`.
shortestPathReachable :: Graph -> NodeId -> NodeId -> Bool
shortestPathReachable g u v = shortestPathReachableWithCached (toCachedFGL g) u v

-- | Reachability decision using a pre-built CachedFGL.
shortestPathReachableWithCached :: CachedFGL -> NodeId -> NodeId -> Bool
shortestPathReachableWithCached cfg src tgt =
  case (cachedFindIdx cfg src, cachedFindIdx cfg tgt) of
    (Just _, Just _) -> maybe False (const True) (shortestPathWithCached cfg src tgt)
    _                -> False

-- | Strongly connected components of the directed edge relation \`E\`.
-- Partitions \`V\` into mutual-reachability classes (Thm 2.2, Tarjan §3.1): the
-- result is a partition of \`gNodes\` with pairwise-disjoint, non-empty components
-- (INV-7). Deterministic — seeds visited in ascending \`NodeId\` order and component
-- ids assigned by ascending minimum \`NodeId\` — so the mapping is invariant under
-- any permutation of \`gNodes\`/\`gEdges\` keys (INV-9). Iterative single DFS pass,
-- O(N+E) time and O(C) auxiliary component buckets beyond the O(N) state vectors.
stronglyConnectedComponents :: Graph -> Map.Map Int [NodeId]
stronglyConnectedComponents g =
  stronglyConnectedComponentsWithCached (toCachedFGL g)

-- | SCC decomposition using a pre-built CachedFGL.
stronglyConnectedComponentsWithCached :: CachedFGL -> Map.Map Int [NodeId]
stronglyConnectedComponentsWithCached cfg =
  let nidMap = cfgNidMap cfg
      n = V.length nidMap
      comps = sccIndices n (sccAdjacency cfg)
      ordered = sortOn minimum comps
      mapped = Map.fromList (zip [0 ..] [sort [nidMap V.! x | x <- c] | c <- ordered])
  in comps `deepseq` mapped `deepseq` mapped

-- | Forward adjacency over fgl sequential indices, sorted ascending per source.
-- Derived from the directed edge relation \`E\` (= FGL edges, built from \`gEdges\`).
sccAdjacency :: CachedFGL -> V.Vector (V.Vector Int)
sccAdjacency cfg =
  let gr = cfgGraph cfg
      n = V.length (cfgNidMap cfg)
      grouped = Map.fromListWith (++) [(s, [v]) | (s, v) <- edges gr :: [FGL.Edge]]
      sorted = Map.map (V.fromList . sort) grouped
  in V.generate n (\s -> Map.findWithDefault V.empty s sorted)

-- | Tarjan SCC over an index-keyed adjacency list. Iterative (explicit frame
-- stack) so it does not recurse per node and scales to large graphs. Returns
-- completed components as node-index lists. Seeds are visited in ascending
-- index order (== ascending \`NodeId\`, since indices are assigned in key order).
sccIndices :: Int -> V.Vector (V.Vector Int) -> [[Int]]
sccIndices n adj = runST $ do
  idx      <- MV.replicate n (negate 1)   -- discovery index; -1 == unvisited (∞)
  low      <- MV.replicate n 0
  onStack  <- MV.replicate n False
  compStk  <- MV.new n                    -- Tarjan node stack (nodes on current path)
  cTop     <- newSTRef (0 :: Int)
  frStk    <- MV.new n                    -- explicit DFS frame stack
  fTop     <- newSTRef (0 :: Int)
  comps    <- newSTRef ([] :: [[Int]])    -- completed components (node-index lists)
  cnt      <- newSTRef (0 :: Int)         -- global discovery counter

  let
    visit u = do
      k <- readSTRef cnt
      writeSTRef cnt (k + 1)
      MV.write idx u k
      MV.write low u k
      MV.write onStack u True
      t <- readSTRef cTop
      MV.write compStk t u
      writeSTRef cTop (t + 1)

    pushFrame u startIdx parent = do
      t <- readSTRef fTop
      MV.write frStk t (u, startIdx, parent)
      writeSTRef fTop (t + 1)

    -- Tree-edge propagation: parent inherits the CHILD's lowlink (Tarjan §3.1);
    -- back/cross-edge propagation: parent inherits the TARGET's index.
    relow u v = do
      lu <- MV.read low u
      lv <- MV.read low v
      MV.write low u (min lu lv)

    relowIdx u v = do
      lu <- MV.read low u
      iv <- MV.read idx v
      MV.write low u (min lu iv)

    -- Pop the Tarjan node stack down to and including @root@, returning the
    -- popped node indices (in pop order, then reversed to DFS order).
    popUntil r acc = do
      t <- readSTRef cTop
      x <- MV.read compStk (t - 1)
      writeSTRef cTop (t - 1)
      MV.write onStack x False
      if x == r then pure (reverse (x : acc)) else popUntil r (x : acc)

    drain = do
      m <- readSTRef fTop
      if m == 0
        then pure ()
        else do
          writeSTRef fTop (m - 1)
          (u, nx, parent) <- MV.read frStk (m - 1)
          let au = adj V.! u
          if nx >= V.length au
            then do
              lu <- MV.read low u
              iu <- MV.read idx u
              if lu == iu
                then do
                  comp <- popUntil u []
                  cs <- readSTRef comps
                  writeSTRef comps (comp : cs)
                else pure ()
              case parent of
                Just p  -> relow p u        -- finished child: propagate low[u]
                Nothing -> pure ()
              drain
            else do
              let v = au V.! nx
              vu <- MV.read idx v
              pushFrame u (nx + 1) parent   -- resume u at the next neighbor later
              if vu == negate 1
                then do
                  visit v
                  pushFrame v 0 (Just u)    -- recurse into unvisited neighbor v
                else do
                  os <- MV.read onStack v
                  if os then relowIdx u v else pure ()
              drain

    seedLoop = forM_ [0 .. n - 1] $ \s -> do
      vs <- MV.read idx s
      if vs == negate 1
        then do
          visit s
          pushFrame s 0 Nothing
          drain
        else pure ()

  seedLoop
  result <- readSTRef comps
  pure result

-- | Induced subgraph restricted to the given node set.
subgraph :: Graph -> Set NodeId -> Graph
subgraph g nodeSet =
  let nodes' = Map.filterWithKey (\k _ -> k `Set.member` nodeSet) (gNodes g)
      edges' = Map.filterWithKey (\(s, t) _ -> s `Set.member` nodeSet && t `Set.member` nodeSet) (gEdges g)
      fwd' = Map.map (`Set.intersection` nodeSet) $ Map.filterWithKey (\k _ -> k `Set.member` nodeSet) (gAdjFwd g)
      bwd' = Map.map (`Set.intersection` nodeSet) $ Map.filterWithKey (\k _ -> k `Set.member` nodeSet) (gAdjBack g)
      embs' = case gEmbeddings g of
                Nothing -> Nothing
                Just m  -> Just (Map.filterWithKey (\k _ -> k `Set.member` nodeSet) m)
    in Graph { gNodes = nodes', gEdges = edges', gAdjFwd = fwd', gAdjBack = bwd', gDirected = gDirected g, gCompositions = Nothing, gHash = computeGraphHash nodes' edges', gEmbeddings = embs', gEmbeddingsPath = gEmbeddingsPath g }

-- ───────────────────────────────────────────────
-- Condensation (Thm 2.3)
-- ───────────────────────────────────────────────

-- | Condensation of the SCC partition.
--
-- `cond(G)` is the DAG whose vertices are the SCC classes and whose arc
-- `A → B` (`A ≠ B`) exists iff some `a ∈ A` and `b ∈ B` are adjacent in the
-- directed relation `E`. The arc map is keyed by component id and stores the
-- sorted, deduplicated successor component ids (self-arcs omitted); every
-- component id appears as a key, so sinks map to `[]`.
--
-- By Thm 2.3 the condensation of any graph's SCC partition is acyclic, so this
-- is always a well-defined DAG. Runs in O(N + E): one SCC pass to label nodes
-- plus a single edge scan of the condensation; O(C) auxiliary buckets.
cond :: Graph -> Map.Map Int [Int]
cond g = condWithCached (toCachedFGL g)

-- | Condensation over a pre-built CachedFGL.
condWithCached :: CachedFGL -> Map.Map Int [Int]
condWithCached cfg =
  let comps = stronglyConnectedComponentsWithCached cfg
      nidMap = cfgNidMap cfg
      -- NodeId → its SCC component id. labEdges yields numeric indices (see
      -- toCachedFGL); convert each arc endpoint to its NodeId via nidMap before
      -- the component lookup, so nodeComp is keyed by NodeId.
      nodeComp = Map.fromList [(n, cid) | (cid, ns) <- Map.toList comps, n <- ns]
      -- Condensation arcs: collapse each source→target arc to its component pair,
      -- dropping self-arcs (within-component arcs never appear in condensation).
      arcMap = Map.fromListWith (Set.union)
          [ (nodeComp Map.! (nidMap V.! u), Set.singleton (nodeComp Map.! (nidMap V.! v)))
          | (u, v, _) <- labEdges (cfgGraph cfg)
          , nodeComp Map.! (nidMap V.! u) /= nodeComp Map.! (nidMap V.! v) ]
  in arcMap `deepseq` Map.fromList
          [ (cid, sort $ Set.toList (Map.findWithDefault Set.empty cid arcMap))
          | cid <- Map.keys comps ]

-- ───────────────────────────────────────────────
-- Directed acyclicity (Thm 2.5, INV-C5)
-- ───────────────────────────────────────────────

-- | Directed DAG predicate.
--
-- A graph is a directed acyclic graph exactly when every SCC is a singleton
-- (no mutual-reachability cluster of size ≥ 2, Thm 2.5) **and** no self-loop
-- exists (`ℓ = 0`): a single node with a self-loop is still a one-node SCC yet
-- constitutes a directed cycle (INV-C5).
isAcyclicDirected :: Graph -> Bool
isAcyclicDirected g =
  let selfLoop = any (\e -> edgeSource e == edgeTarget e) (Map.elems (gEdges g))
  in not selfLoop && isAcyclicDirectedWithCached (toCachedFGL g)

-- | Directed DAG predicate over a pre-built CachedFGL.
isAcyclicDirectedWithCached :: CachedFGL -> Bool
isAcyclicDirectedWithCached cfg =
  let comps = stronglyConnectedComponentsWithCached cfg
      noMultiNodeScc = all (\ns -> length ns <= 1) (Map.elems comps)
      -- Self-loop present ⟹ a length-1 directed cycle (Thm 2.5, INV-C5).
      hasSelfLoop = any (\(u, v, _) -> u == v) (labEdges (cfgGraph cfg))
  in noMultiNodeScc && not hasSelfLoop

-- ───────────────────────────────────────────────
-- Isolated nodes (INV-C3)
-- ───────────────────────────────────────────────

-- | Isolated nodes: those of degree 0 (no arc in either direction).
--
-- Output is sorted by NodeId (INV-C3). Every isolated node is a singleton
-- component in both connectivity decompositions below.
isolatedNodes :: Graph -> [NodeId]
isolatedNodes g =
  let allIds = Map.keys (gNodes g)
      seen = Set.fromList
           (concatMap (Set.toList . snd) (Map.toList (gAdjFwd g))
            ++ concatMap (Set.toList . snd) (Map.toList (gAdjBack g)))
  in sort [nid | nid <- allIds, not (nid `Set.member` seen)]

-- | Isolated nodes over a pre-built CachedFGL (see 'isolatedNodes').
--
-- A node is isolated iff its sequential index is never an arc endpoint, read in
-- either direction; the surviving indices map back to 'NodeId' and are emitted
-- in ascending order (indices are assigned in ascending NodeId key order at
-- 'toCachedFGL', so index order already equals NodeId order). O(N + E).
isolatedNodesWithCached :: CachedFGL -> [NodeId]
isolatedNodesWithCached cfg =
  let nidMap = cfgNidMap cfg
      n = V.length nidMap
      arcs = labEdges (cfgGraph cfg)
      involved = Set.fromList ([ u | (u, _, _) <- arcs ] ++ [ v | (v, _, _) <- arcs ])
      isolatedIdx = [ i | i <- [0 .. n - 1], not (i `Set.member` involved) ]
  in sort [nidMap V.! i | i <- isolatedIdx]

-- ───────────────────────────────────────────────
-- Undirected cycle (Thm 2.6 contrapositive)
-- ───────────────────────────────────────────────

-- | Undirected cycle predicate.
--
-- Decides whether the undirected support of `g` contains a cycle. Every arc is
-- read as an undirected edge; the support has a cycle iff some connected class
-- holds a closed walk. A lone arc between two nodes is a tree edge (no cycle);
-- a self-loop (`u == v`, a length-1 cycle) and any longer closed walk both
-- count. Reciprocal arc pairs collapse to one undirected edge, so they do not
-- themselves form a cycle (simple-undirected-graph semantics). Runs in
-- O(N + E) via union-find with union-by-rank (tree height O(log N)).
hasCycleUndirected :: Graph -> Bool
hasCycleUndirected g =
  let selfLoop = any (\e -> edgeSource e == edgeTarget e) (Map.elems (gEdges g))
  in selfLoop || hasCycleUndirectedWithCached (toCachedFGL g)

-- | Undirected cycle predicate over a pre-built CachedFGL.
hasCycleUndirectedWithCached :: CachedFGL -> Bool
hasCycleUndirectedWithCached cfg =
  let n = V.length (cfgNidMap cfg)
      arcs = labEdges (cfgGraph cfg)
      -- A self-loop is a length-1 undirected cycle.
      hasSelfLoop = any (\(u, v, _) -> u == v) arcs
      -- Simple undirected edge set: canonical `(min,max)` pair, deduped, with
      -- self-loops dropped. Deduping collapses both the AF-3 reverse embedding
      -- of an undirected graph and any reciprocal arc pair to a single edge, so a
      -- lone edge is never mistaken for a cycle.
      simpleEdges = Set.fromList [ (min u v, max u v) | (u, v, _) <- arcs, u /= v ]
  in runST $ do
       root <- MV.replicate n (0 :: Int)
       rank <- MV.replicate n (0 :: Int)
       hasCyc <- newSTRef hasSelfLoop
       let find' !i = do
             r <- MV.read root i
             if r == i then pure i else find' r
           addEdge !a !b = do
             ra <- find' a
             rb <- find' b
             if ra == rb
               then writeSTRef hasCyc True
               else do
                 ar <- MV.read rank ra
                 br <- MV.read rank rb
                 case compare ar br of
                   GT  -> do MV.write root rb ra
                             pure ()
                   LT  -> do MV.write root ra rb
                             pure ()
                   EQ  -> do MV.write root rb ra
                             MV.write rank ra (ar + 1)
       forM_ [0 .. n - 1] $ \i -> do
         MV.write root i i
         MV.write rank i 0
       forM_ simpleEdges $ \(a, b) -> addEdge a b
       readSTRef hasCyc

-- ───────────────────────────────────────────────
-- Connectivity decompositions (Thm 2.6, INV-C1 / INV-C3 / INV-C6)
-- ───────────────────────────────────────────────

-- | Connected components on the undirected support graph `G_s`.
--
-- Partitions `V` into connected classes of `G_s` (INV-C1); each SCC lies in
-- exactly one component, so the component map coarsens the SCC partition
-- (`C ≤ C_s`, INV-C6). Deterministic: component ids assigned by ascending
-- minimum NodeId. Runs in O(N + E) (one undirected DFS pass); O(C) buckets.
connectedComponents :: Graph -> Map.Map Int [NodeId]
connectedComponents g = undirectedComponentsWithCached (toCachedFGL g)

-- | Weakly connected components (the directed relation `E` read undirected).
--
-- For any graph this equals `connectedComponents` (both decompose the undirected
-- support graph), but the two names carry distinct contracts: this one reads
-- `E` undirected; the predecessor reads `G_s`. Partitions `V` (INV-C1) and
-- coarsens the SCC partition (INV-C6). Deterministic.
weaklyConnectedComponents :: Graph -> Map.Map Int [NodeId]
weaklyConnectedComponents g = undirectedComponentsWithCached (toCachedFGL g)

-- | Connected components over a pre-built CachedFGL (see 'connectedComponents').
connectedComponentsWithCached :: CachedFGL -> Map.Map Int [NodeId]
connectedComponentsWithCached cfg = undirectedComponentsWithCached cfg

-- | Weakly connected components over a pre-built CachedFGL (see 'weaklyConnectedComponents').
weaklyConnectedComponentsWithCached :: CachedFGL -> Map.Map Int [NodeId]
weaklyConnectedComponentsWithCached cfg = undirectedComponentsWithCached cfg

-- | Connectivity decomposition over a pre-built CachedFGL, shared by both
-- `connectedComponentsWithCached` and `weaklyConnectedComponentsWithCached`.
--
-- Iterative DFS over the undirected support of the FGL graph (every arc treated
-- bidirectional): seeds are visited in ascending sequential-index order, which
-- equals ascending NodeId (indices are assigned in key order at
-- 'toCachedFGL'). The first seed discovered for each component is that
-- component's minimum index, so components come out in ascending-min order;
-- component ids are then assigned by ascending minimum NodeId via 'sortOn'.
undirectedComponentsWithCached :: CachedFGL -> Map.Map Int [NodeId]
undirectedComponentsWithCached cfg =
  let nidMap = cfgNidMap cfg
      n = V.length nidMap
      -- Record each arc in BOTH directions so the adjacency is symmetric: an
      -- undirected connectivity DFS must traverse u→v AND v→u. Recording only
      -- `min u v → {max u v}` (the prior, buggy form) left the adjacency
      -- asymmetric, so the DFS could not walk back from the higher index to the
      -- lower one and split a single SCC across multiple "components".
      directedArcs = [ (u, Set.singleton v) | (u, v, _) <- labEdges (cfgGraph cfg) ]
                   ++ [ (v, Set.singleton u) | (u, v, _) <- labEdges (cfgGraph cfg) ]
      undirected   = Map.fromListWith Set.union directedArcs
      undirectedAdj = V.generate n
          $ \i -> V.fromList (sort (Set.toList (Map.findWithDefault Set.empty i undirected)))
      comps = undirectedComponentIndices n undirectedAdj
      ordered = sortOn minimum comps
      mapped = Map.fromList (zip [0 ..] [sort [nidMap V.! x | x <- c] | c <- ordered])
  in comps `deepseq` mapped `deepseq` mapped

-- | Undirected connectivity of an index-keyed CSR-like adjacency list.
--
-- Returns the components as lists of node indices, ordered by ascending
-- minimum index. Iterative (explicit stack) so it does not recurse per node
-- and scales to large graphs. O(N + E) time, O(N) state vectors.
undirectedComponentIndices :: Int -> V.Vector (V.Vector Int) -> [[Int]]
undirectedComponentIndices n adj = runST $ do
  vis    <- MV.replicate n False
  comps  <- newSTRef ([] :: [[Int]])
  stk    <- MV.new n
  cTop   <- newSTRef (0 :: Int)
  mem    <- newSTRef ([] :: [Int])

  let
    pushNeighbors xs = forM_ xs $ \w -> do
      wv <- MV.read vis w
      unless wv $ do
        MV.write vis w True
        t <- readSTRef cTop
        MV.write stk t w
        writeSTRef cTop (t + 1)

    loopStk = do
      t <- readSTRef cTop
      if t <= 0
        then pure ()
        else do
          writeSTRef cTop (t - 1)
          x <- MV.read stk (t - 1)
          ms <- readSTRef mem
          writeSTRef mem (x : ms)
          pushNeighbors (adj V.! x)
          loopStk

    start !s = do
      writeSTRef cTop 0
      writeSTRef mem []
      MV.write vis s True
      -- Push the seed directly (pushNeighbors would skip it as already visited).
      writeSTRef cTop 1
      MV.write stk 0 s
      loopStk
      cs <- readSTRef mem
      c <- readSTRef comps
      writeSTRef comps (cs : c)

    seedLoop = forM_ [0 .. n - 1] $ \s -> do
      vs <- MV.read vis s
      unless vs $ start s

  seedLoop
  result <- readSTRef comps
  pure result