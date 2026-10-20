{-# LANGUAGE BangPatterns #-}
module Graphos.Domain.Analysis
  ( analyze
  , surprisingConnections
  , suggestQuestions
  , dedupOn
  , selectTopNOn
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText, toText)

import Graphos.Domain.Types (NodeId, Node(..), Edge(..), Relation(..), Confidence(..),
                            FileType(..), CommunityId, CommunityMap, CohesionMap,
                            SurprisingConnection(..), SuggestedQuestion(..),
                            Analysis(..), NullModel(..), relationToText)
import Graphos.Domain.Graph (Graph, godNodes, isFileNode, isConceptNode, gNodes, gEdges, degree)
import Graphos.Domain.Graph.Analysis (toCachedFGL, connectivityWithCached)

-- | Order-preserving, first-occurrence-wins deduplication by key.
-- O(k log k) replacement for @nubBy (\\a b -> key a == key b)@, which is
-- O(k^2) and must never be used on lists that scale with graph size.
dedupOn :: Ord k => (a -> k) -> [a] -> [a]
dedupOn key = go Set.empty
  where
    go _ [] = []
    go !seen (x:xs)
      | k `Set.member` seen = go seen xs
      | otherwise           = x : go (Set.insert k seen) xs
      where k = key x

-- | Stable bounded top-N selection: identical output to
-- @take n . sortOn key@ (including tie order, which stable sort resolves by
-- original position), but with O(length · n) time and O(n) residency instead
-- of materializing and sorting the full candidate list
-- (bounded-report-export).
selectTopNOn :: Ord k => Int -> (a -> k) -> [a] -> [a]
selectTopNOn n key xs
  | n <= 0 = []
  | otherwise = [x | (_, _, x) <- foldl' step [] (zipWith (\i x -> (key x, i, x)) [0 :: Int ..] xs)]
  where
    step acc e = take n (insertAsc e acc)
    insertAsc e [] = [e]
    insertAsc e@(k, i, _) (y@(k', i', _) : rest)
      | (k, i) < (k', i') = e : y : rest
      | otherwise         = y : insertAsc e rest

-- | Analyze a graph. Articulation points and the biconnected-component count
-- are computed here, once, from a single shared FGL conversion, and carried
-- on the result so report/export consume them without recomputation
-- (bounded-report-export D1/D2).
analyze :: Graph -> CommunityMap -> CohesionMap -> Analysis
analyze g commMap cohesionMap =
  let (artPoints, bccCount) = connectivityWithCached (toCachedFGL g)
      gods = godNodes g 10
      surprises = surprisingConnections g commMap 5
      labels = Map.fromList [(cid, T.pack ("Community " ++ show cid)) | cid <- Map.keys commMap]
      questions = suggestQuestions g commMap cohesionMap labels
  in Analysis
    { analysisCommunities   = commMap
    , analysisNullModel     = DefaultNullModel
    , analysisCohesion      = cohesionMap
    , analysisGodNodes     = gods
    , analysisSurprises    = surprises
    , analysisQuestions    = questions
    , analysisArticulation = artPoints
    , analysisBccCount     = bccCount
    }

surprisingConnections :: Graph -> CommunityMap -> Int -> [SurprisingConnection]
surprisingConnections g commMap topN =
  let nodeComm = nodeCommunityMap commMap
      sourceFiles = Set.fromList [toText (nodeSourceFile n) | n <- Map.elems (gNodes g), not (T.null (toText (nodeSourceFile n)))]
      isMultiSource = Set.size sourceFiles > 1
  in if isMultiSource
     then crossFileSurprises g nodeComm topN
     else crossCommunitySurprises g nodeComm topN

-- | Suggested questions. Cohesion is read from the provided map (the values
-- computed during clustering), never recomputed per community
-- (bounded-report-export).
suggestQuestions :: Graph -> CommunityMap -> CohesionMap -> Map CommunityId Text -> [SuggestedQuestion]
suggestQuestions g commMap cohesionMap labels =
  let nodeComm = nodeCommunityMap commMap
      ambiguousEdges = [(u, v, d) | (u, v, d) <- allEdges g
                                   , let Confidence c = edgeConfidence d
                                   , c < 0.3]
      ambiguousQs = [SuggestedQuestion
        { sqType = "ambiguous_edge"
        , sqQuestion = Just $ "What is the exact relationship between `" <> nodeLabel' g u <> "` and `" <> nodeLabel' g v <> "`?"
        , sqWhy = "Low confidence edge (relation: " <> relationToText (edgeRelation d) <> ") - confidence is low."
        } | (u, v, d) <- take 3 ambiguousEdges]
      bridgeQs = bridgeNodeQuestions g nodeComm labels
      lowCohesionQs = lowCohesionQuestions commMap cohesionMap labels
  in take 7 (ambiguousQs ++ bridgeQs ++ lowCohesionQs)

nodeCommunityMap :: CommunityMap -> Map NodeId CommunityId
nodeCommunityMap commMap = Map.fromList [(nid, cid) | (cid, nids) <- Map.toList commMap, nid <- nids]

crossFileSurprises :: Graph -> Map NodeId CommunityId -> Int -> [SurprisingConnection]
crossFileSurprises g nodeComm topN =
  let candidates = [(u, v, d, score, reasons)
                    | (u, v, d) <- allEdges g
                    , let uSrc = toText (nodeSourceFile (nodeData g u))
                    , let vSrc = toText (nodeSourceFile (nodeData g v))
                    , not (T.null uSrc)
                    , not (T.null vSrc)
                    , uSrc /= vSrc
                   , edgeRelation d `notElem` [Imports, Contains]
                   , not (isConceptNode (nodeData g u))
                   , not (isConceptNode (nodeData g v))
                   , let (score, reasons) = surpriseScore g d nodeComm uSrc vSrc u v
                   ]
      -- Bounded selection: only topN candidates are ever resident, instead
      -- of sorting the full O(E) list (bounded-report-export D3).
      sorted = selectTopNOn topN (\(_, _, _, s, _) -> Down s) candidates
  in take topN [SurprisingConnection
    { scSource      = nodeLabel' g u
    , scTarget      = nodeLabel' g v
    , scSourceFiles = [toText (nodeSourceFile (nodeData g u)), toText (nodeSourceFile (nodeData g v))]
    , scConfidence  = edgeConfidence d
    , scRelation    = relationToText (edgeRelation d)
    , scWhy         = T.intercalate "; " (if null reasons then ["cross-file semantic connection"] else reasons)
    } | (u, v, d, _, reasons) <- sorted]

crossCommunitySurprises :: Graph -> Map NodeId CommunityId -> Int -> [SurprisingConnection]
crossCommunitySurprises g nodeComm topN =
  let candidates = [(u, v, d, cid_u, cid_v)
                   | (u, v, d) <- allEdges g
                   , let cid_u = Map.lookup u nodeComm
                   , let cid_v = Map.lookup v nodeComm
                   , cid_u /= cid_v
                   , not (isFileNode g (nodeData g u))
                   , not (isFileNode g (nodeData g v))
                   , edgeRelation d `notElem` [Imports, Contains]
                   ]
      -- Best candidate per community pair, then bounded top-N over those
      -- bests: output-equal to sorting all candidates by descending
      -- confidence (stable) and deduplicating first-occurrence-wins per
      -- (cid_u, cid_v) — the first sorted occurrence of a pair IS its
      -- max-confidence candidate, ties resolved by original position — but
      -- with O(pairs) residency instead of a full O(E) sort
      -- (bounded-report-export D3).
      conf (_, _, d, _, _) = edgeConfidence d
      better a@(i, ca) b@(j, cb)
        | (Down (conf ca), i) < (Down (conf cb), j) = a
        | otherwise = b
      bests = Map.elems (Map.fromListWith better
                [ ((cu, cv), (i, c))
                | (i, c@(_, _, _, cu, cv)) <- zip [0 :: Int ..] candidates
                ])
      deduped = map snd (selectTopNOn topN (\(i, c) -> (Down (conf c), i)) bests)
  in take topN [SurprisingConnection
    { scSource      = nodeLabel' g u
    , scTarget      = nodeLabel' g v
    , scSourceFiles = [toText (nodeSourceFile (nodeData g u)), toText (nodeSourceFile (nodeData g v))]
    , scConfidence  = edgeConfidence d
    , scRelation    = relationToText (edgeRelation d)
    , scWhy         = "Bridges community " <> maybe "" (T.pack . show) cid_u <> " → community " <> maybe "" (T.pack . show) cid_v
    } | (u, v, d, cid_u, cid_v) <- deduped]

surpriseScore :: Graph -> Edge -> Map NodeId CommunityId -> Text -> Text -> NodeId -> NodeId -> (Int, [Text])
surpriseScore _g edge nodeComm uSrc vSrc u v =
  let Confidence c = edgeConfidence edge
      confBonus = if c < 0.3 then 3 else if c < 0.8 then 2 else 1
      crossFiletype = if fileCategory uSrc /= fileCategory vSrc
                      then (2, ["crosses file types"])
                      else (0, [])
      crossComm = case (Map.lookup u nodeComm, Map.lookup v nodeComm) of
        (Just cu, Just cv) | cu /= cv -> (1, ["bridges separate communities"])
        _ -> (0, [])
      semBonus = if edgeRelation edge == Inferred
                 then (round @Double (fromIntegral confBonus * 1.5), ["inferred connection"])
                 else (0, [])
  in (confBonus + fst crossFiletype + fst crossComm + fst semBonus
     , snd crossFiletype ++ snd crossComm ++ snd semBonus)

bridgeNodeQuestions :: Graph -> Map NodeId CommunityId -> Map CommunityId Text -> [SuggestedQuestion]
bridgeNodeQuestions g _nodeComm _labels =
  let betweenness = nodeBetweenness g
      topBridges = take 3 (sortOn (Down . snd) [(nid, bScore) | (nid, bScore) <- Map.toList betweenness
                                                                , bScore > 0
                                                                , not (isFileNode g (nodeData g nid))])
  in [SuggestedQuestion
     { sqType = "bridge_node"
     , sqQuestion = Just $ "Why does `" <> nodeLabel' g nid <> "` connect across communities?"
     , sqWhy = "High betweenness centrality - this node is a cross-community bridge."
     } | (nid, _) <- topBridges]

-- | Low-cohesion questions read the clustering-time cohesion map; a
-- community absent from the map is treated as cohesive (no question) rather
-- than recomputed (bounded-report-export). Producers construct the map over
-- the same community keys, so the miss case does not arise in the pipeline.
lowCohesionQuestions :: CommunityMap -> CohesionMap -> Map CommunityId Text -> [SuggestedQuestion]
lowCohesionQuestions commMap cohesionMap labels =
  [SuggestedQuestion
   { sqType = "low_cohesion"
   , sqQuestion = Just $ "Should `" <> lbl <> "` be split into smaller, more focused modules?"
   , sqWhy = "Cohesion score is low - nodes in this community are weakly interconnected."
   } | (cid, members) <- Map.toList commMap
    , length members >= 5
    , let score = Map.findWithDefault 1.0 cid cohesionMap
    , score < 0.15
    , let lbl = Map.findWithDefault (T.pack ("Community " ++ show cid)) cid labels]

nodeBetweenness :: Graph -> Map NodeId Double
nodeBetweenness g = Map.fromList [(nid, 1.0 / fromIntegral (max 1 (degree g nid)))
                                  | nid <- Map.keys (gNodes g)]

fileCategory :: Text -> FileType
fileCategory path
  | any (`T.isSuffixOf` T.toLower path) codeExts = CodeFile
  | any (`T.isSuffixOf` T.toLower path) paperExts = PaperFile
  | otherwise = DocFile
  where
    codeExts = [".py", ".ts", ".js", ".go", ".rs", ".java", ".c", ".cpp", ".rb", ".cs", ".kt"
               ,".scala", ".php", ".swift", ".lua", ".zig", ".hs", ".ex", ".m", ".jl"]
    paperExts = [".pdf"]

nodeData :: Graph -> NodeId -> Node
nodeData g nid = Map.findWithDefault (Node
  { nodeId           = nid
  , nodeLabel        = fromText "unknown"
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText ""
  , nodeSource       = Nothing
  , nodeLineStart    = Nothing
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodePresentBits  = 0
  }) nid (gNodes g)

nodeLabel' :: Graph -> NodeId -> Text
nodeLabel' g nid = toText (nodeLabel (nodeData g nid))

allEdges :: Graph -> [(NodeId, NodeId, Edge)]
allEdges g = [(s,t,e) | ((s,t), e) <- Map.toList (gEdges g)]

data Down a = Down a deriving (Eq, Show)
instance Ord a => Ord (Down a) where
  compare (Down x) (Down y) = compare y x
