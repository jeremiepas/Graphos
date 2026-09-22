-- | Spec-graph verification checks (spec-graph-verification change).
--
-- Pure mirror of the Lean model in
-- openspec/changes/spec-graph-verification/lean/ — same names, same
-- certificate discipline: gating verdicts are only accepted after their
-- certificate re-checks ('isTopoOrder' for acyclicity, 'isPathTo' for
-- coverage). The sort and BFS may be optimized freely; the re-checkers are
-- the trusted components and must stay in lockstep with the Lean theorems
-- (topo_certifies_acyclic, findPathTo_certified, areCandidates_complete,
-- blocking_subset).
--
-- Pure — no IO, fully testable.
module Graphos.Domain.SpecCheck
  ( -- * Relation selections
    specRels
  , coverageRels
    -- * Kinds and status
  , isRequirementNode
  , isDecisionNode
  , isCodeNode
  , isActiveNode
  , activeRequirementIds
    -- * Steps and chains
  , stepRel
  , succsRel
  , isChain
  , lastFrom
  , isClosedChain
    -- * Acyclicity with certificates
  , posIn
  , respectsEdges
  , isTopoOrder
  , topoSort
  , checkAcyclic
  , findCycleWitness
    -- * Coverage with certificates
  , isPathTo
  , findPathTo
  , implementedWitness
  , unimplementedRequirements
    -- * Contradiction candidates
  , constrainsTargets
  , areCandidates
  , candidatePairs
    -- * Duplication and single-point-of-failure findings
  , cosineSimilarity
  , duplicationCandidates
  , spofDecisions
    -- * Stale supersession
  , staleSupersessions
    -- * Adjudication gate
  , Verdict(..)
  , gate
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.List (elemIndex, find)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Short (toText)

import Graphos.Domain.Types

-- ── Relation selections ──────────────────────────────────────────────────────

-- | Relations the cycle check traverses.
specRels :: [Relation]
specRels = [DependsOn, Refines, Supersedes]

-- | Relations coverage traverses (requirement → … → code).
coverageRels :: [Relation]
coverageRels = [Satisfies, References, Contains]

-- ── Kinds and status ─────────────────────────────────────────────────────────

hasKind :: Text -> Node -> Bool
hasKind k n = fmap toText (nodeKind n) == Just k

isRequirementNode :: Node -> Bool
isRequirementNode = hasKind "Requirement"

isDecisionNode :: Node -> Bool
isDecisionNode = hasKind "Decision"

isCodeNode :: Node -> Bool
isCodeNode n = nodeFileType n == CodeFile && not (isRequirementNode n || isDecisionNode n)

-- | Decisions carry @{"active": false}@ in extra once superseded; everything
-- else defaults to active.
isActiveNode :: Node -> Bool
isActiveNode n = case nodeExtra n of
  Just (A.Object o) -> case KM.lookup "active" o of
    Just (A.Bool b) -> b
    _               -> True
  _ -> True

activeRequirementIds :: [Node] -> [NodeId]
activeRequirementIds ns =
  [ nodeId n | n <- ns, isRequirementNode n, isActiveNode n ]

-- ── Steps and chains (the trusted walk checker) ──────────────────────────────

stepRel :: [Edge] -> [Relation] -> NodeId -> NodeId -> Bool
stepRel es rels a b =
  any (\e -> edgeSource e == a && edgeTarget e == b && edgeRelation e `elem` rels) es

succsRel :: [Edge] -> [Relation] -> NodeId -> [NodeId]
succsRel es rels a =
  [ edgeTarget e | e <- es, edgeSource e == a, edgeRelation e `elem` rels ]

-- | Non-empty sequence of pairwise-connected nodes (Lean 'isChain').
isChain :: (NodeId -> NodeId -> Bool) -> [NodeId] -> Bool
isChain _ [] = False
isChain _ [_] = True
isChain step (a : b : rest) = step a b && isChain step (b : rest)

-- | Last node of the chain @a :: rest@ (Lean 'lastFrom').
lastFrom :: NodeId -> [NodeId] -> NodeId
lastFrom a [] = a
lastFrom _ (x : xs) = lastFrom x xs

-- | Cycle witness shape: length ≥ 2, chain, ends where it starts.
isClosedChain :: (NodeId -> NodeId -> Bool) -> [NodeId] -> Bool
isClosedChain step (a : b : rest) =
  isChain step (a : b : rest) && lastFrom b rest == a
isClosedChain _ _ = False

-- ── Acyclicity with certificates ─────────────────────────────────────────────

-- | Position in the order; total (length if absent), like the Lean 'pos'.
posIn :: [NodeId] -> NodeId -> Int
posIn order n = fromMaybe (length order) (elemIndex n order)

-- | The re-checked half of the certificate: every selected edge goes strictly
-- forward in the order (Lean 'respectsEdges' — subject of
-- topo_certifies_acyclic).
respectsEdges :: [Edge] -> [Relation] -> [NodeId] -> Bool
respectsEdges es rels order =
  all
    (\e ->
       edgeRelation e `notElem` rels
         || posIn order (edgeSource e) < posIn order (edgeTarget e))
    es

-- | Full certificate check: covers all given nodes and respects all edges.
isTopoOrder :: [NodeId] -> [Edge] -> [Relation] -> [NodeId] -> Bool
isTopoOrder nodeIds es rels order =
  all (`elem` order) nodeIds && respectsEdges es rels order

-- | Kahn-style sort; Nothing when a cycle blocks progress. Untrusted — see
-- 'checkAcyclic'.
topoSort :: [NodeId] -> [Edge] -> [Relation] -> Maybe [NodeId]
topoSort nodeIds es rels = go (length nodeIds + 1) nodeIds []
  where
    go :: Int -> [NodeId] -> [NodeId] -> Maybe [NodeId]
    go fuel remaining acc
      | null remaining = Just (reverse acc)
      | fuel <= 0 = Nothing
      | otherwise =
          case find (\n -> not (any (\m -> stepRel es rels m n) remaining)) remaining of
            Nothing -> Nothing
            Just n  -> go (fuel - 1) (filter (/= n) remaining) (n : acc)

-- | Certificate-checked acyclicity verdict: whatever the sort produced is
-- accepted only if 'isTopoOrder' validates it — a sort bug can cause a false
-- cycle alarm, never a false certificate (Lean checkAcyclic_sound /
-- checkAcyclic_no_cycle).
checkAcyclic :: [NodeId] -> [Edge] -> [Relation] -> Maybe [NodeId]
checkAcyclic nodeIds es rels = do
  order <- topoSort nodeIds es rels
  if isTopoOrder nodeIds es rels order then Just order else Nothing

-- | On a cyclic verdict, produce a closed-walk witness (validated by
-- 'isClosedChain' before being returned — same discipline as the positive
-- certificates).
findCycleWitness :: [NodeId] -> [Edge] -> [Relation] -> Maybe [NodeId]
findCycleWitness nodeIds es rels =
  case mapMaybe walkFrom nodeIds of
    (w : _) -> if isClosedChain (stepRel es rels) w then Just w else Nothing
    []      -> Nothing
  where
    walkFrom start = go (length nodeIds + 1) [start] start
      where
        go :: Int -> [NodeId] -> NodeId -> Maybe [NodeId]
        go fuel path cur
          | fuel <= 0 = Nothing
          | otherwise =
              case succsRel es rels cur of
                [] -> Nothing
                (nxt : _)
                  | nxt `elem` path ->
                      -- close the loop at the first revisit
                      Just (dropWhile (/= nxt) (reverse path) ++ [nxt])
                  | otherwise -> go (fuel - 1) (nxt : path) nxt

-- ── Coverage with certificates ───────────────────────────────────────────────

-- | Path certificate: chain from @s@ to a node satisfying the target
-- predicate (Lean 'isPathTo').
isPathTo :: (NodeId -> NodeId -> Bool) -> NodeId -> (NodeId -> Bool) -> [NodeId] -> Bool
isPathTo _ _ _ [] = False
isPathTo step s isTgt p@(a : rest) =
  a == s && isChain step p && isTgt (lastFrom a rest)

-- | BFS with path tracking, re-checked before acceptance (Lean 'findPathTo';
-- soundness by construction — findPathTo_certified).
findPathTo :: [Edge] -> [Relation] -> NodeId -> (NodeId -> Bool) -> Maybe [NodeId]
findPathTo es rels s isTgt =
  case bfs (length es * 2 + 2) [[s]] (Set.singleton s) of
    Just p | isPathTo (stepRel es rels) s isTgt p -> Just p
    _ -> Nothing
  where
    bfs :: Int -> [[NodeId]] -> Set.Set NodeId -> Maybe [NodeId]
    bfs fuel queue visited
      | fuel <= 0 = Nothing
      | otherwise = case queue of
          [] -> Nothing
          ([] : rest) -> bfs (fuel - 1) rest visited
          (path@(cur : _) : rest)
            | isTgt cur -> Just (reverse path)
            | otherwise ->
                let fresh = [ n | n <- succsRel es rels cur, not (Set.member n visited) ]
                in bfs (fuel - 1)
                     (rest ++ map (: path) fresh)
                     (foldr Set.insert visited fresh)

-- | Implemented = certificate exists; the certificate is the report entry.
implementedWitness :: [Node] -> [Edge] -> NodeId -> Maybe [NodeId]
implementedWitness ns es r = findPathTo es coverageRels r isCode
  where
    isCode nid = maybe False isCodeNode (find ((== nid) . nodeId) ns)

-- | Negative claim: warns, never gates by default (extraction-completeness
-- dependent; Lean 'unimplemented').
unimplementedRequirements :: [Node] -> [Edge] -> [NodeId]
unimplementedRequirements ns es =
  filter (isNothing . implementedWitness ns es) (activeRequirementIds ns)

-- ── Contradiction candidates ─────────────────────────────────────────────────

constrainsTargets :: [Edge] -> NodeId -> [NodeId]
constrainsTargets es r =
  [ edgeTarget e | e <- es, edgeSource e == r, edgeRelation e == Constrains ]

-- | The decision procedure proved complete in Lean (areCandidates_complete):
-- two distinct active requirements constraining a common target.
areCandidates :: [Node] -> [Edge] -> NodeId -> NodeId -> Bool
areCandidates ns es r1 r2 =
  r1 /= r2
    && active r1
    && active r2
    && any (`elem` constrainsTargets es r2) (constrainsTargets es r1)
  where
    active r = r `elem` activeRequirementIds ns

-- | Enumeration over active requirements (presentation over the decision
-- procedure).
candidatePairs :: [Node] -> [Edge] -> [(NodeId, NodeId)]
candidatePairs ns es =
  [ (r1, r2)
  | r1 <- rs
  , r2 <- rs
  , r1 < r2
  , areCandidates ns es r1 r2
  ]
  where
    rs = activeRequirementIds ns

-- ── Duplication and single-point-of-failure findings ─────────────────────────

-- | Cosine similarity between two equal-length vectors; 0 when either is
-- empty or lengths differ. Mirrors LLMPort.cosineSimilarity (kept local to
-- keep Domain IO-free and dependency-minimal).
cosineSimilarity :: [Double] -> [Double] -> Double
cosineSimilarity a b
  | null a || null b || length a /= length b = 0
  | denom == 0 = 0
  | otherwise = dot / denom
  where
    dot = sum (zipWith (*) a b)
    na = sqrt (sum [ x * x | x <- a ])
    nb = sqrt (sum [ x * x | x <- b ])
    denom = na * nb

-- | Duplication candidates: pairs of distinct nodes in the same community
-- whose embedding similarity exceeds the configured threshold. Only
-- Requirement / Decision nodes are compared; requires embeddings for both
-- members and the same (present) community id. Pure.
duplicationCandidates :: Double -> [Node] -> Map.Map NodeId [Double] -> [(NodeId, NodeId, Double)]
duplicationCandidates threshold ns embs =
  [ (a, b, sim)
  | (a, b) <- pairs
  , Just va <- [Map.lookup a embs]
  , Just vb <- [Map.lookup b embs]
  , communityOf a == communityOf b
  , communityOf a /= Nothing
  , let sim = cosineSimilarity va vb
  , sim >= threshold
  ]
  where
    specNodes = [ n | n <- ns, isRequirementNode n || isDecisionNode n ]
    ids = [ nodeId n | n <- specNodes ]
    communityOf nid =
      case [ nodeCommunityId n | n <- specNodes, nodeId n == nid ] of
        (mc : _) -> mc
        []       -> Nothing
    pairs = [ (a, b) | (i, a) <- zip [0 :: Int ..] ids
                     , (j, b) <- zip [0 :: Int ..] ids
                     , i < j ]

-- | Single-point-of-failure decisions: Decision nodes that are articulation
-- points of the undirected spec graph (their removal disconnects it).
-- Pure — a small iterative DFS over the spec-node adjacency, restricted to
-- Decision nodes.
spofDecisions :: [Node] -> [Edge] -> [NodeId]
spofDecisions ns es =
  [ d | d <- decisionIds, isArticulation d ]
  where
    decisionIds = [ nodeId n | n <- ns, isDecisionNode n ]
    specIds = [ nodeId n | n <- ns, isRequirementNode n || isDecisionNode n ]
    adj = Map.fromListWith (++)
          [ (edgeSource e, [edgeTarget e]) | e <- es, inSpec (edgeSource e), inSpec (edgeTarget e) ]
          <> Map.fromListWith (++)
          [ (edgeTarget e, [edgeSource e]) | e <- es, inSpec (edgeSource e), inSpec (edgeTarget e) ]
    inSpec nid = nid `elem` specIds
    isArticulation root =
      let nbrs = [ c | c <- Map.findWithDefault [] root adj, c /= root, inSpec c ]
          -- DFS avoiding the root entirely: if the remaining graph is
          -- connected, every neighbour is reachable from the first one.
          reachable = case nbrs of
            (first : _) -> dfs root (Set.singleton first) (Set.fromList [first])
            []          -> Set.empty
          stranded = [ c | c <- nbrs, Set.notMember c reachable ]
      in length nbrs >= 2 && not (null stranded)
    dfs _root visited frontier = case Set.toList frontier of
      [] -> visited
      (x : _) ->
        let visited' = Set.insert x visited
            frontier' = Set.delete x frontier
                         `Set.union` Set.fromList
                              [ n | n <- Map.findWithDefault [] x adj
                                  , inSpec n, n /= _root
                                  , Set.notMember n visited' ]
        in dfs _root visited' frontier'

-- ── Stale supersession ───────────────────────────────────────────────────────

-- | (active source, superseded decision): an active artifact still references
-- an inactive decision.
staleSupersessions :: [Node] -> [Edge] -> [(NodeId, NodeId)]
staleSupersessions ns es =
  [ (edgeSource e, edgeTarget e)
  | e <- es
  , edgeRelation e == References
  , activeSrc (edgeSource e)
  , supersededDecision (edgeTarget e)
  ]
  where
    activeSrc nid = any (\n -> nodeId n == nid && isActiveNode n) ns
    supersededDecision nid =
      any (\n -> nodeId n == nid && isDecisionNode n && not (isActiveNode n)) ns

-- ── Adjudication gate ────────────────────────────────────────────────────────

data Verdict
  = VerdictConflict
  | VerdictCompatible
  | VerdictDuplicate
  | VerdictUnadjudicated
  deriving (Eq, Show)

-- | Pure filter: only confirmed conflicts block. The gate cannot exceed the
-- surfaced candidates (Lean blocking_subset / blocking_confirmed).
gate :: ((NodeId, NodeId) -> Verdict) -> [(NodeId, NodeId)] -> [(NodeId, NodeId)]
gate adjudicate = filter ((== VerdictConflict) . adjudicate)
