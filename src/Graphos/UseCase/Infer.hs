module Graphos.UseCase.Infer
  ( inferCommunityBridges
  , inferTransitiveDeps
  , inferSharedContextEdges
  , inferCodeDocEdges
   , inferSemanticCodeDocEdges
   , inferNonSemanticEdges
   , inferSemanticEdgesForMode
   , inferEdges
   , semanticModeName
   , classifyBridgeNodes
  , BridgeClassification(..)
  , SemanticMode(..)
  , semanticMode
  , isSingleCorpus
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Domain.Analysis (dedupOn)
import Graphos.Domain.Config (SemanticEdgesConfig(..))
import Graphos.Domain.Graph
  ( Graph, gNodes, gEdges, gEmbeddings, neighbors, degree
  , articulationPoints, biconnectedComponents
  , edgeBetweenness
  )
import Graphos.UseCase.Port.LLMPort (cosineSimilarity)

-- | Maximum number of inferred community-bridge edges per run.
-- Guards against pathological community structures; candidates beyond the
-- cap are dropped.
maxCommunityBridges :: Int
maxCommunityBridges = 10000

-- | Maximum number of code nodes a doc label may match to produce edges.
-- Labels matching more nodes than this ("Config", "Usage", ...) are ambient
-- noise, not references, and are skipped.
maxLabelFanOut :: Int
maxLabelFanOut = 20

-- | Maximum number of importers a module may have before its transitive-dep
-- expansion is skipped. A module imported by thousands of files is a god
-- module; connecting all its importers pairwise is O(inDeg^2) noise edges
-- (~9M for a 3000-importer hub) and exhausts memory.
maxTransitiveFanIn :: Int
maxTransitiveFanIn = 64

-- | Maximum number of inferred transitive-dep edges per run. Guards against
-- pathological import structures; candidates beyond the cap are dropped.
maxTransitiveDeps :: Int
maxTransitiveDeps = 50000

data BridgeClassification = BridgeClassification
  { bcNodeId        :: NodeId
  , bcIsArticulation :: Bool
  , bcBccCount      :: Int
  , bcBetweenness   :: Double
  , bcCommunities   :: [CommunityId]
  } deriving (Eq, Show)

classifyBridgeNodes :: Graph -> CommunityMap -> [BridgeClassification]
classifyBridgeNodes g commMap =
  let artPoints = articulationPoints g
      bccs = biconnectedComponents g
      between = edgeBetweenness g
      nodeComm = nodeCommunityMap commMap
      bccMembership = Map.fromListWith (+)
        [ (nid, 1)
        | comp <- bccs
        , nid <- comp
        , nid `elem` artPoints
        ]
  in [ BridgeClassification
        { bcNodeId        = nid
        , bcIsArticulation = True
        , bcBccCount      = Map.findWithDefault 1 nid bccMembership
        , bcBetweenness   = sum [score | ((s,t), score) <- Map.toList between
                                        , s == nid || t == nid]
        , bcCommunities   = case Map.lookup nid nodeComm of
                               Just cid -> [cid]
                               Nothing  -> []
        }
      | nid <- artPoints
      ]

-- | Bridge communities that are connected by at least one real
-- inter-community edge. Candidates are derived from the graph's edges
-- (O(E log C)) — NOT from all community pairs, which is O(C^2) and
-- materialized ~50M edges at 10k+ communities. Result is capped at
-- 'maxCommunityBridges'.
inferCommunityBridges :: Graph -> CommunityMap -> [Edge]
inferCommunityBridges g commMap =
  let centroids = communityCentroids g commMap
      nodeComm = nodeCommunityMap commMap
      adjacentPairs = Set.toList $ Set.fromList
        [ if c1 < c2 then (c1, c2) else (c2, c1)
        | (s, t) <- Map.keys (gEdges g)
        , Just c1 <- [Map.lookup s nodeComm]
        , Just c2 <- [Map.lookup t nodeComm]
        , c1 /= c2
        ]
  in take maxCommunityBridges
       [ makeInferredEdge srcNid tgtNid Inferred 0.5
       | (cid1, cid2) <- adjacentPairs
       , Just srcNid <- [Map.lookup cid1 centroids]
       , Just tgtNid <- [Map.lookup cid2 centroids]
       , srcNid /= tgtNid
       , notEdgeAlready g srcNid tgtNid
       ]

-- | Infer shared-dependency edges: if both @src@ and @tgt@ import @mid@,
-- link @src@ -> @tgt@. Hubs imported by more than 'maxTransitiveFanIn'
-- files are skipped (pairwise expansion is O(inDeg^2) and exhausts memory),
-- and the total is capped at 'maxTransitiveDeps'.
inferTransitiveDeps :: Graph -> [Edge]
inferTransitiveDeps g =
  let edges = Map.toList (gEdges g)
      depEdges = [((s, t), e) | ((s, t), e) <- edges
                                , edgeRelation e `elem` [Imports, DependsOn]]
      predMap = Map.fromListWith (++) [(t, [s]) | ((s, t), _) <- depEdges]
      -- Hubs with a bounded importer list only; god modules are skipped.
      boundedHubs = Set.fromList [t | (t, importers) <- Map.toList predMap
                                      , length importers <= maxTransitiveFanIn]
      boundedDepEdges = [((s, t), e) | ((s, t), e) <- depEdges
                                       , t `Set.member` boundedHubs]
      transitiveDeps = dedupOn (\e -> (edgeSource e, edgeTarget e))
        [makeInferredEdge src tgt DependsOn 0.4
        | ((src, mid), _) <- boundedDepEdges
        , Just targets <- [Map.lookup mid predMap]
        , tgt <- targets
        , tgt /= src
        , notEdgeAlready g src tgt
        ]
  in take maxTransitiveDeps transitiveDeps

inferSharedContextEdges :: Graph -> Int -> [Edge]
inferSharedContextEdges g minShared =
  let allNodes = Map.keys (gNodes g)
      coOccurrences :: Map (NodeId, NodeId) Int
      coOccurrences = Map.fromListWith (+)
        [ (orderPair n1 n2, 1)
        | nid <- allNodes
        , let nbs = Set.toList (neighbors g nid)
        , length nbs <= 64
        , (n1, n2) <- pairUp nbs
        ]
      pairUp [] = []
      pairUp (x:xs) = [(x, y) | y <- xs] ++ pairUp xs
      validPairs = [(n1, n2, count)
                   | ((n1, n2), count) <- Map.toList coOccurrences
                   , count >= minShared
                   , notEdgeAlready g n1 n2
                   ]
  in [makeInferredEdge n1 n2 Inferred (min 0.9 (0.2 * fromIntegral sharedCount)) | (n1, n2, sharedCount) <- validPairs]
  where
    orderPair a b = if a < b then (a, b) else (b, a)

inferCodeDocEdges :: Graph -> [Edge]
inferCodeDocEdges g =
  let allNodes = Map.toList (gNodes g)
      docNodes = [(nid, n) | (nid, n) <- allNodes, nodeFileType n == DocFile]
      codeNodes = [(nid, n) | (nid, n) <- allNodes, nodeFileType n == CodeFile]

      -- Labels matching more than 'maxLabelFanOut' code nodes are ambiguous
      -- names ("Config", "Usage") and carry no linking signal; dropping them
      -- bounds the candidate list on doc-heavy corpora.
      boundedIdx :: Map Text [NodeId] -> Map Text [NodeId]
      boundedIdx = Map.filter (\ns -> length ns <= maxLabelFanOut)

      codeLabelIdx :: Map Text [NodeId]
      codeLabelIdx = boundedIdx $ Map.fromListWith (++)
        [ (nodeLabel cn, [nid])
        | (nid, cn) <- codeNodes
        ]

      codeBaseIdx :: Map Text [NodeId]
      codeBaseIdx = boundedIdx $ Map.fromListWith (++)
        [ (fileBaseName (nodeSourceFile cn), [nid])
        | (nid, cn) <- codeNodes
        , not (T.null (nodeSourceFile cn))
        ]

      nameAlignEdges =
        [ makeInferredEdge codeNid docNid References 0.7
        | (docNid, dn) <- docNodes
        , codeNid <- Map.findWithDefault [] (nodeLabel dn) codeLabelIdx
        , notEdgeAlready g docNid codeNid
        ]

      pathAlignEdges =
        [ makeInferredEdge codeNid docNid References 0.7
        | (docNid, dn) <- docNodes
        , not (T.null (nodeSourceFile dn))
        , let docBase = fileBaseName (nodeSourceFile dn)
        , not (T.null docBase)
        , codeNid <- Map.findWithDefault [] docBase codeBaseIdx
        , notEdgeAlready g docNid codeNid
        ]

  in dedupOn (\e -> (edgeSource e, edgeTarget e))
       (nameAlignEdges ++ pathAlignEdges)

-- | Semantic edge inference mode, determined by config + force flag + graph shape.
data SemanticMode
  = SemanticEnabled    -- ^ Run semantic inference (mixed corpus, under scale cap)
  | SemanticForced     -- ^ Run semantic inference (forced, bypass scale cap + auto-skip)
  | SemanticDisabled   -- ^ Explicitly disabled (--no-semantic-edges or seEnabled = False)
  | SemanticAutoSkip   -- ^ Single-corpus graph, auto-skipped
  | SemanticFallback   -- ^ >10K code nodes, fell back to literal-name inference
  deriving (Eq, Show)

-- | Determine the semantic edge inference mode for a given config, force flag, and graph.
-- Gating order: explicit disable > force > single-corpus auto-skip > scale cap > enabled.
semanticMode :: SemanticEdgesConfig -> Bool -> Graph -> SemanticMode
semanticMode se force g
  | not (seEnabled se) = SemanticDisabled
  | force              = SemanticForced
  | isSingleCorpus g   = SemanticAutoSkip
  | codeNodeCount g > 10000 = SemanticFallback
  | otherwise            = SemanticEnabled
  where
    codeNodeCount g' = length [() | n <- Map.elems (gNodes g'), nodeFileType n == CodeFile]

-- | Returns True when all nodes share one FileType (trivially single-corpus).
isSingleCorpus :: Graph -> Bool
isSingleCorpus g =
  let fileTypes = [nodeFileType n | n <- Map.elems (gNodes g)]
  in case fileTypes of
       [] -> True
       (ft:rest) -> all (== ft) rest

-- | Infer code↔doc edges via cosine similarity on node embeddings.
-- For each doc node with an embedding, find the top-k code nodes (by similarity)
-- that exceed the threshold, and create a References edge.
inferSemanticCodeDocEdges :: SemanticEdgesConfig -> Graph -> Map NodeId [Double] -> [Edge]
inferSemanticCodeDocEdges se g embs
  | Map.null embs = []
  | otherwise =
  let docNodes = [(nid, n) | (nid, n) <- Map.toList (gNodes g), nodeFileType n == DocFile]
      codeNodes = [(nid, n) | (nid, n) <- Map.toList (gNodes g), nodeFileType n == CodeFile]

      docWithEmb = [(nid, e) | (nid, _) <- docNodes, Just e <- [Map.lookup nid embs], not (null e)]
      codeWithEmb = [(nid, e) | (nid, _) <- codeNodes, Just e <- [Map.lookup nid embs], not (null e)]

      semanticEdges = concat
        [ let docVec = docEmb
              candidates = [ (cosineSimilarity docVec codeVec, codeNid)
                            | (codeNid, codeVec) <- codeWithEmb
                            , notEdgeAlready g docNid codeNid
                            ]
              filtered = [(sim, codeNid) | (sim, codeNid) <- candidates, sim >= seThreshold se]
              top = take (seMaxFanOut se) $ sortOn (\(sim, _) -> negate sim) filtered
          in [ makeInferredEdge codeNid docNid References sim
             | (sim, codeNid) <- top
             ]
        | (docNid, docEmb) <- docWithEmb
        ]
  in dedupOn (\e -> (edgeSource e, edgeTarget e)) semanticEdges

fileBaseName :: Text -> Text
fileBaseName path =
  let filename = case T.breakOnEnd "/" path of
        (_, f) | not (T.null f) -> T.dropWhile (== '/') f
        _                        -> path
      base = case T.breakOnEnd "." filename of
        (_, ext) | not (T.null ext) && T.length ext <= 5 ->
          case T.breakOnEnd "." (T.dropEnd (T.length ext + 1) filename) of
            (_, b) | not (T.null b) -> b
            _ -> filename
        _ -> filename
  in base

-- | Infer all non-semantic edges (community bridges, transitive deps, shared context, code-doc).
inferNonSemanticEdges :: EdgeDensity -> Graph -> CommunityMap -> [Edge]
inferNonSemanticEdges density g cm = case density of
  Sparse  -> inferCodeDocEdges g
  Normal  -> inferCommunityBridges g cm ++ inferTransitiveDeps g ++ inferCodeDocEdges g
  Dense   -> inferCommunityBridges g cm ++ inferTransitiveDeps g ++ inferSharedContextEdges g 3 ++ inferCodeDocEdges g
  Maximum -> inferCommunityBridges g cm ++ inferTransitiveDeps g ++ inferSharedContextEdges g 2 ++ inferCodeDocEdges g

-- | Infer semantic code↔doc edges for the given mode (empty for disabled/skip/fallback modes).
inferSemanticEdgesForMode :: SemanticMode -> SemanticEdgesConfig -> Graph -> [Edge]
inferSemanticEdgesForMode mode se g = case mode of
  SemanticEnabled  -> inferSemanticCodeDocEdges se g (fromMaybe Map.empty (gEmbeddings g))
  SemanticForced   -> inferSemanticCodeDocEdges se g (fromMaybe Map.empty (gEmbeddings g))
  _                -> []

-- | Human-readable name of a semantic mode for log output.
semanticModeName :: SemanticMode -> Text
semanticModeName SemanticEnabled    = "enabled"
semanticModeName SemanticForced     = "forced"
semanticModeName SemanticDisabled   = "disabled"
semanticModeName SemanticAutoSkip   = "auto-skip"
semanticModeName SemanticFallback   = "fallback"

inferEdges :: EdgeDensity -> SemanticEdgesConfig -> Bool -> Graph -> CommunityMap -> [Edge]
inferEdges density se force g cm =
  let mode = semanticMode se force g
  in inferNonSemanticEdges density g cm ++ inferSemanticEdgesForMode mode se g

communityCentroids :: Graph -> CommunityMap -> Map CommunityId NodeId
communityCentroids g commMap = Map.fromList
  [ (cid, centroidOf g members)
  | (cid, members) <- Map.toList commMap
  , not (null members)
  ]
  where
    centroidOf g' members =
      let scored = sortOn (\n -> negate (fromIntegral (degree g' n) :: Double)) members
      in case scored of (x:_) -> x; [] -> error "centroidOf: empty community"

notEdgeAlready :: Graph -> NodeId -> NodeId -> Bool
notEdgeAlready g src tgt =
  Map.notMember (src, tgt) (gEdges g) && Map.notMember (tgt, src) (gEdges g)

nodeCommunityMap :: CommunityMap -> Map NodeId CommunityId
nodeCommunityMap commMap = Map.fromList
  [(nid, cid) | (cid, nids) <- Map.toList commMap, nid <- nids]

makeInferredEdge :: NodeId -> NodeId -> Relation -> Double -> Edge
makeInferredEdge src tgt rel w = Edge
  { edgeId        = EdgeId (src <> "->" <> tgt <> ":" <> relationToText rel)
  , edgeSource    = src
  , edgeTarget    = tgt
  , edgeRelation  = rel
  , edgeWeight    = w
  , edgeConfidence = Confidence w
  , edgeExtra     = Nothing
  }