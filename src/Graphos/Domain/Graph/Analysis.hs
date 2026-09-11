-- | Advanced graph analysis — structural properties and centrality.
-- Pure functions over the domain types.
--
-- Memory optimization: FGL graph is computed ONCE and shared across all
-- algorithms (articulation points, biconnected components, dominators,
-- edge betweenness). Previously each algorithm created its own FGL copy
-- (~200MB each on 100k-node graphs), totaling ~800MB of duplicate data.
-- Now we use a CachedFGL record to compute once and reuse.
--
-- ═══════════════════════════════════════════════════════════════════
-- Complexity assertions (AVI-534 §2, math-requirements
-- docs/math-requirements/AVI-534-structural-analysis-complexity.md — AC-1)
-- ═══════════════════════════════════════════════════════════════════
--
-- All bounds are worst-case in the @(N, M)@ model, where adjacency is read
-- over FGL sequential indices (CSR-equivalent dense-free layout; 'Data.Map'
-- lookups appear only at the CachedFGL boundary, never inside an algorithm
-- loop).
--
-- * 'articulationPoints' / @ap@ (FGL Tarjan low-link, single DFS pass):
--   __O(N + M)__ (Theorem 2.1; Tarjan 1972).
-- * 'biconnectedComponents' / @bcc@ (stack-based DFS, each arc once):
--   __O(N + M)__ (Theorem 2.1).
-- * 'dominators' / @dom@ (Cooper–Harvey–Kennedy iterative reduction):
--   __O(N + M)__ on reducible graphs; __O(N·M)__ worst-case on irreducible
--   inputs (Theorem 2.2; CHW 1982). Graphos code-dependency graphs are
--   reducible (every edge traces a call/import/contains relation with
--   well-founded nesting), so the operative bound is O(N + M).
-- * 'edgeBetweenness' (Brandes 2001): __O(N·M)__ exact (one BFS +
--   one backward dependency accumulation per source × N sources);
--   __O(s·(N + M))__ sampled over @s ≤ maxSampledSources@ sources
--   (Theorem 2.3). Per-source working memory is O(N + M) over the shared
--   'CachedFGL' (SG-3): one mutable BFS queue + one mutable dependency
--   accumulation, no per-source Map/Set allocation scaling with N.
--
-- Undirected-graph handling (AVI-534 AF-3): 'toCachedFGL' reverse-embeds
-- every @(u,v)@ edge as @(v,u)@ when the graph is undirected
-- (@gDirected g = False@), so FGL 'ap'/'bcc' single-pass traversals and the
-- Brandes BFS see the undirected support graph @G_s@; Art/BCC/Dom results
-- match the classical @Art(G_s)@/@BCC(G_s)@/@Dom(G_s)@ definitions.
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Domain.Graph.Analysis
  (     -- * Cached FGL (shared across algorithms to save memory)
    CachedFGL(..)
  , toCachedFGL
  , cachedFindIdx

    -- * Scale guards (AVI-534 §4)
  , defaultMaxSampledSources
  , defaultExactBetweennessNodeCap

    -- * Analysis algorithms
  , godNodes
  , articulationPoints
  , articulationPointsWithCached
  , biconnectedComponents
  , biconnectedComponentsWithCached
  , dominators
  , dominatorsWithCached
  , edgeBetweenness
  , edgeBetweennessWith
  , edgeBetweennessWithCached
  , edgeBetweennessWithSources
  , brandesSource
  , fglIsUndirected
  ) where

import Control.DeepSeq (deepseq)
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.List (sortOn, nub, sort, group)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Graph.Inductive.Graph as FGL
import Data.Graph.Inductive.Query.ArtPoint (ap)
import Data.Graph.Inductive.Query.BCC (bcc)
import Data.Graph.Inductive.Query.Dominators (dom)

import Graphos.Domain.Types
import Graphos.Domain.Graph.Core (Graph(..), isFileNode, isConceptNode)
import Data.Text.Short (toText)
import Graphos.Domain.Graph.FGL (FGLGraph, FGLNodeLabel, FGLEdgeLabel)

-- ───────────────────────────────────────────────
-- Cached FGL computation (memory optimization)
-- ───────────────────────────────────────────────

-- | Cached FGL graph + lookup tables.
-- Computed once from a Graphos Graph, then shared across all algorithm calls.
-- Saves ~600MB on 100k-node graphs by avoiding redundant FGL conversions.
-- Uses bijective sequential indices (0..N-1) to avoid hash collisions.
data CachedFGL = CachedFGL
  { cfgGraph   :: !FGLGraph
  , cfgNidMap  :: !(V.Vector NodeId)
  , cfgIdxMap  :: !(Map NodeId Int)
  } deriving (Eq, Show)

-- | Build a cached FGL graph with bijective sequential indices.
-- Assigns each NodeId a distinct Int in 0..N-1, eliminating hash collisions.
-- Forces the result with deepseq to ensure the FGL structure is fully evaluated.
--
-- Undirected graphs (AVI-534 AF-3): when @gDirected g = False@ every edge
-- @(u,v)@ is additionally reverse-embedded as @(v,u)@, so FGL's single-pass
-- traversals (ap/bcc low-link DFS, dominator BFS, Brandes BFS) see the
-- undirected support graph @G_s@. With reverse edges embedded, FGL's
-- @lsuc@-driven DFS equals a bidirectional traversal, so
-- @Art@/@BCC@ match the classical @Art(G_s)@/@BCC(G_s)@.
toCachedFGL :: Graph -> CachedFGL
toCachedFGL g =
  let nodeList = Map.toList (gNodes g)
      n = length nodeList
      -- Bijective mapping: NodeId -> sequential Int (0..N-1)
      idxMap = Map.fromList (zip (fst <$> nodeList) [0..n-1 :: Int])
      nidMap = V.fromList (fst <$> nodeList)
      -- Build FGL nodes with sequential indices
      fglNodes :: [FGL.LNode FGLNodeLabel]
      fglNodes = [(idx, (nid, node)) | (nid, node) <- nodeList, let idx = idxMap Map.! nid]
      directed = gDirected g
      -- Build FGL edges with sequential indices; undirected graphs get the
      -- reverse edge embedded so every FGL traversal is bidirectional.
      mkLEdge e =
        let srcIdx = idxMap Map.! edgeSource e
            tgtIdx = idxMap Map.! edgeTarget e
            lbl = (edgeRelation e, edgeConfidence e, e)
        in if directed || srcIdx == tgtIdx
             then [(srcIdx, tgtIdx, lbl)]
             else [(srcIdx, tgtIdx, lbl), (tgtIdx, srcIdx, lbl)]
      fglEdges :: [FGL.LEdge FGLEdgeLabel]
      fglEdges = concatMap mkLEdge (Map.elems (gEdges g))
  in CachedFGL { cfgGraph = FGL.mkGraph fglNodes fglEdges, cfgNidMap = nidMap, cfgIdxMap = idxMap }

-- | Find the fgl Int index for a Graphos NodeId — O(log N) via Map lookup.
cachedFindIdx :: CachedFGL -> NodeId -> Maybe Int
cachedFindIdx cfg nid = Map.lookup nid (cfgIdxMap cfg)

-- ───────────────────────────────────────────────
-- Scale guards (AVI-534 §4)
-- ───────────────────────────────────────────────

-- | SG-1: sampled betweenness draws at most this many sources (default 500).
defaultMaxSampledSources :: Int
defaultMaxSampledSources = 500

-- | SG-2: exact O(N·M) all-pairs betweenness is bypassed above this node
-- count; the sampled estimator (SG-1) is used instead.
defaultExactBetweennessNodeCap :: Int
defaultExactBetweennessNodeCap = 10000

-- ───────────────────────────────────────────────
-- Analysis queries (all use shared CachedFGL)
-- ───────────────────────────────────────────────

-- | Find god nodes (highest-degree nodes, excluding file hubs and concepts)
godNodes :: Graph -> Int -> [GodNode]
godNodes g topN =
  let degrees = [(nid, Set.size (neighbors' g nid), n) | (nid, n) <- Map.toList (gNodes g)]
      filtered = filter (\(_, deg, n) -> not (isFileNode g n) && not (isConceptNode n) && deg > 0) degrees
      sorted = sortOn (\(_, deg, _) -> negate deg) filtered
  in take topN [GodNode { gnId = nid, gnLabel = toText (nodeLabel n), gnEdges = deg }
                | (nid, deg, n) <- sorted]
  where
    neighbors' g' nid =
      let fwd = Map.findWithDefault Set.empty nid (gAdjFwd g')
          bwd = Map.findWithDefault Set.empty nid (gAdjBack g')
      in if gDirected g' then fwd else fwd `Set.union` bwd

-- | Find articulation points (bridge nodes) whose removal would disconnect the graph.
articulationPoints :: Graph -> [NodeId]
articulationPoints g = articulationPointsWithCached (toCachedFGL g)

-- | Find articulation points using a pre-built CachedFGL
articulationPointsWithCached :: CachedFGL -> [NodeId]
articulationPointsWithCached cfg =
  let gr = cfgGraph cfg
      nidMap = cfgNidMap cfg
      artPointIdxs = ap gr
  in artPointIdxs `deepseq` [nidMap V.! idx | idx <- artPointIdxs]

-- | Find biconnected components of the graph.
biconnectedComponents :: Graph -> [[NodeId]]
biconnectedComponents g = biconnectedComponentsWithCached (toCachedFGL g)

-- | Find biconnected components using a pre-built CachedFGL
biconnectedComponentsWithCached :: CachedFGL -> [[NodeId]]
biconnectedComponentsWithCached cfg =
  let gr = cfgGraph cfg
      nidMap = cfgNidMap cfg
      components = bcc gr
  in components `deepseq` [nub [nidMap V.! idx | idx <- FGL.nodes comp] | comp <- components]

-- | Compute the dominator tree for a given start node.
dominators :: Graph -> NodeId -> Map NodeId (Maybe NodeId)
dominators g start = dominatorsWithCached (toCachedFGL g) start

-- | Compute the dominator tree using a pre-built CachedFGL
dominatorsWithCached :: CachedFGL -> NodeId -> Map NodeId (Maybe NodeId)
dominatorsWithCached cfg start =
  let gr = cfgGraph cfg
      nidMap = cfgNidMap cfg
      build domList =
        Map.fromList [(nidMap V.! idx, case nidMap V.!? idom of Just d -> Just d; Nothing -> Nothing)
                      | (idx, idomList) <- domList, idom <- idomList]
  in case cachedFindIdx cfg start of
       Just startIdx ->
         let domList = dom gr startIdx
             raw = build domList
             -- §1.3: idom(u) is the closest STRICT dominator (d ≠ u). The root
             -- is dominated only by itself, so its immediate dominator is None.
             -- FGL reports the start node's idom as itself; override to Nothing.
             doms = Map.insert (nidMap V.! startIdx) Nothing raw
         in doms `deepseq` doms
       Nothing -> Map.empty

-- | Compute edge betweenness centrality (Brandes 2001).
--
-- For a directed graph the sum runs over ordered source/target pairs; for an
-- undirected graph it runs over unordered pairs (@srcIdx < tgtIdx@).
-- Results are normalized by @2/(N(N-1))@ so that the value of an edge is the
-- fraction of all-pairs shortest paths routed through it (AVI-534 §1.4 (1),
-- §3.3). Complexity: O(N·M) exact, O(s·(N+M)) sampled (Theorem 2.3).
--
-- Scale guards (AVI-534 §4): SG-2 caps the exact pass at
-- 'defaultExactBetweennessNodeCap' nodes (sampled estimator above it); SG-1
-- caps the sampled source count at 'defaultMaxSampledSources' with an
-- @(N\/s)@ rescale that keeps the estimator unbiased (Lemma 3.1).
edgeBetweenness :: Graph -> Map (NodeId, NodeId) Double
edgeBetweenness g =
  edgeBetweennessWith defaultMaxSampledSources defaultExactBetweennessNodeCap g

-- | Compute edge betweenness centrality using a pre-built CachedFGL with the
-- default scale guards ('defaultMaxSampledSources', 'defaultExactBetweennessNodeCap').
edgeBetweennessWithCached :: CachedFGL -> Map (NodeId, NodeId) Double
edgeBetweennessWithCached cfg =
  edgeBetweennessWithSources defaultMaxSampledSources defaultExactBetweennessNodeCap cfg

-- | Edge betweenness with configurable scale guards (AVI-534 SG-1/SG-2).
--
-- * @maxSampledSources@: SG-1 cap on the number of sampled sources; sources
--   are drawn deterministically (ascending sequential index, cf. §9) and the
--   per-source contribution is rescaled by @N\/s@ so the estimator is
--   unbiased (Lemma 3.1). With @s = N@ the estimator is exactly the exact
--   all-pairs value (AC-4).
-- * @exactNodeCap@: SG-2 cap; when @N > cap@ the exact O(N·M) pass is
--   bypassed in favour of the sampled estimator.
edgeBetweennessWith :: Int -> Int -> Graph -> Map (NodeId, NodeId) Double
edgeBetweennessWith maxSampledSources exactNodeCap g =
  edgeBetweennessWithSources maxSampledSources exactNodeCap (toCachedFGL g)

-- | Core Brandes edge betweenness over a pre-built CachedFGL.
--
-- One forward BFS + one backward dependency accumulation per source
-- (Brandes 2001). Per-source working memory is O(N + M) over the shared
-- 'CachedFGL' (SG-3): two ST vectors + one stack + one queue, no per-source
-- allocation that grows with N beyond that. Deterministic: BFS visits
-- successors in ascending index order, and Map accumulation is order-agnostic.
--
-- Pair handling (AVI-534 §3.3): the undirected sum runs over unordered pairs
-- and the directed sum over ordered pairs. With AF-3 reverse-embedding every
-- undirected arc exists in both orientations, so 'brandesSource' canonicalizes
-- each flow to the @(min, max)@ slot; because Brandes runs over every source,
-- the two orientations of an undirected arc are both summed, yielding the
-- ORDERED value — fold a @1/2@ into the normalization for undirected graphs to
-- recover the unordered value. Directed graphs keep their ordered value. The
-- @2/(N(N-1))@ factor then normalizes over the (unordered) pair count, and the
-- SG-1 rescale @(N\/s)@ keeps the sampled estimator unbiased (Lemma 3.1); with
-- @s = N@ it equals the exact value (AC-4).
edgeBetweennessWithSources :: Int -> Int -> CachedFGL -> Map (NodeId, NodeId) Double
edgeBetweennessWithSources maxSampledSources exactNodeCap cfg =
  let gr = cfgGraph cfg
      nidMap = cfgNidMap cfg
      n = V.length nidMap
      allIndices = [0 .. n - 1]
      -- SG-2: exact all-pairs only at or below the node cap; sampled above.
      useExact = n <= exactNodeCap
      -- SG-1: deterministic sample — ascending index order, capped.
      sources
        | useExact || n <= maxSampledSources = allIndices
        | otherwise = take maxSampledSources allIndices
      s = length sources
      isUndirected = fglIsUndirected gr
      -- One BFS per source = O(N + M); summing over s sources gives the
      -- O(s·(N+M)) sampled / O(N·M) exact bound (Theorem 2.3). For an
      -- undirected graph 'brandesSource' canonicalizes every flow to the
      -- (min, max) slot, but across DIFFERENT sources both orientations of
      -- each undirected arc are summed (the arc (u,v) flows appear when u is
      -- a nearer source, the arc (v,u) when v is), so 'edgeTotals' holds the
      -- ORDERED value — exactly twice the unordered value (§3.3). The /2 below
      -- converts to unordered; it cancels identically on exact and sampled
      -- output, preserving AC-4 (s = N ⇒ estimator = exact BC) and AC-2.
      edgeTotals = Map.fromListWith (+)
         [ ((nidMap V.! u, nidMap V.! w), d)
         | v <- sources
         , (u, w, d) <- brandesSource gr isUndirected v
         ]
      nD = fromIntegral n :: Double
      sD = fromIntegral s :: Double
      -- undirectedHalf halves the ordered accumulation for undirected graphs
      -- so the result is the fraction of UNORDERED pairs routed through each
      -- edge (§3.3); directed graphs keep their ordered value unchanged.
      undirectedHalf = if isUndirected then 0.5 else 1.0
      -- 2/(N(N-1)) normalization over unordered pairs (§3.3); the sampled
      -- estimator rescales by N/s (Lemma 3.1: E[ŷ_S] = (N/s)·Σ δ_x = BC).
      normalization
         | n > 1 && s > 0 = (2.0 / (nD * (nD - 1))) * (nD / sD) * undirectedHalf
         | otherwise = undirectedHalf
    in edgeTotals `deepseq` fmap (* normalization) edgeTotals

-- | True when every arc of the FGL graph has its reverse present (the shape
-- 'toCachedFGL' produces for undirected Graphos graphs after AF-3
-- reverse-embedding). Read once per betweenness call, O(M).
fglIsUndirected :: FGLGraph -> Bool
fglIsUndirected gr =
  let arcs = [(u, w) | (u, w, _) <- FGL.labEdges gr]
      arcSet = Set.fromList arcs
  in all (\(u, w) -> u == w || (w, u) `Set.member` arcSet) arcs

-- | Brandes single-source dependency pass (one BFS + backward accumulation).
--
-- The forward BFS enqueues each discovered node once (queue order = BFS
-- order); the backward pass pops the same order from the stack and
-- accumulates Δ (node dependencies) and per-edge flow
-- @σ(w)\/σ(v) · (1 + Δ(w))@ over predecessor lists. Successor sets are read
-- via @suc'@, canonicalized to sorted distinct lists for determinism (§9)
-- and parallel-edge dedup. Per-source working memory: two Int vectors
-- (queue, stack) + two Double vectors (σ, Δ) + one Int vector (dist) +
-- one boxed predecessor vector = O(N + M) over the shared 'CachedFGL'
-- (SG-3). Unweighted graphs only: hop counts are the shortest-path
-- measure (§0).
--
-- @isUndirected@ canonicalizes each edge flow to the @(min, max)@ slot so
-- the two orientations of a reverse-embedded undirected arc accumulate into
-- one pair; directed graphs emit flows in traversal orientation.
brandesSource :: FGLGraph -> Bool -> Int -> [(Int, Int, Double)]
brandesSource gr isUndirected src = runST $ do
  let n = FGL.order gr
  sigmaV <- VUM.replicate n (0 :: Double)
  distV  <- VUM.replicate n (-1 :: Int)
  deltaV <- VUM.replicate n (0 :: Double)
  predV  <- VM.replicate n []
  queueV <- VUM.new n
  stackV <- VUM.new n
  VUM.write distV src 0
  VUM.write sigmaV src 1
  VUM.write queueV 0 src
  VUM.write stackV 0 src
  let -- Sorted distinct successors of v: deterministic BFS expansion order,
      -- parallel edges deduplicated (σ counts multiplicity once per arc).
      succsOf v = [g' | g <- group (sort (FGL.suc' (FGL.context gr v))), g' <- take 1 g]
      -- Forward BFS: queue head qi, stack top ti.
      bfsLoop !qi !ti
        | qi >= ti = pure ti
        | otherwise = do
            v <- VUM.read queueV qi
            dv <- VUM.read distV v
            sv <- VUM.read sigmaV v
            ti' <- foldM
              (\t w -> do
                 dw <- VUM.read distV w
                 if dw < 0
                   then do
                     VUM.write distV w (dv + 1)
                     VUM.modify sigmaV (+ sv) w
                     VM.modify predV (v :) w
                     VUM.write queueV t w
                     VUM.write stackV t w
                     pure (t + 1)
                   else if dw == dv + 1
                     then do
                       VUM.modify sigmaV (+ sv) w
                       VM.modify predV (v :) w
                       pure t
                     else pure t)
              ti
              (succsOf v)
            bfsLoop (qi + 1) ti'
  orderLen <- bfsLoop 0 1
  -- Backward accumulation: pop BFS order in reverse; orderLen == stack size.
  let backLoop !ti acc
        | ti < 0 = pure acc
        | otherwise = do
            w <- VUM.read stackV ti
            sw <- VUM.read sigmaV w
            dw <- VUM.read deltaV w
            let coeff = if sw == 0 then 0 else (1 + dw) / sw
            acc' <- foldM
              (\a v -> do
                 sv' <- VUM.read sigmaV v
                 let flow = coeff * sv'
                 VUM.modify deltaV (+ flow) v
                 pure (addEdgeFlow v w flow a))
              acc
              =<< VM.read predV w
            backLoop (ti - 1) acc'
  backLoop (orderLen - 1) []
  where
    -- Canonical (u, w) slot: (min, max) merges the two orientations of a
    -- reverse-embedded undirected arc into one unordered pair (§3.3);
    -- directed graphs keep the traversal orientation (v, w).
    addEdgeFlow v w flow acc
      | isUndirected, v > w = (w, v, flow) : acc
      | otherwise = (v, w, flow) : acc
