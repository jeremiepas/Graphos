-- | Analysis functions - god nodes, surprising connections, suggested questions.
-- Pure functions over domain types.
module Graphos.Domain.Analysis
  ( analyze
  , surprisingConnections
  , suggestQuestions
  ) where

import Data.List (sortOn, nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types (NodeId, Node(..), Edge(..), Relation(..), Confidence(..),
                            FileType(..), CommunityId, CommunityMap, CohesionMap,
                            SurprisingConnection(..), SuggestedQuestion(..),
                            Analysis(..), relationToText)
import Graphos.Domain.Graph (Graph, godNodes, isFileNode, isConceptNode, gNodes, gEdges, degree)
import Graphos.Domain.Community (cohesionScore)

-- | Run full analysis on a graph with communities
analyze :: Graph -> CommunityMap -> CohesionMap -> Analysis
analyze g commMap cohesionMap =
  let gods = godNodes g 10
      surprises = surprisingConnections g commMap 5
      labels = Map.fromList [(cid, T.pack ("Community " ++ show cid)) | cid <- Map.keys commMap]
      questions = suggestQuestions g commMap labels
  in Analysis
    { analysisCommunities = commMap
    , analysisCohesion     = cohesionMap
    , analysisGodNodes     = gods
    , analysisSurprises    = surprises
    , analysisQuestions    = questions
    }

-- | Find surprising connections (cross-community, cross-file edges)
surprisingConnections :: Graph -> CommunityMap -> Int -> [SurprisingConnection]
surprisingConnections g commMap topN =
  let nodeComm = nodeCommunityMap commMap
      sourceFiles = Set.fromList [nodeSourceFile n | n <- Map.elems (gNodes g), not (T.null (nodeSourceFile n))]
      isMultiSource = Set.size sourceFiles > 1
  in if isMultiSource
     then crossFileSurprises g nodeComm topN
     else crossCommunitySurprises g nodeComm topN

-- | Suggest questions the graph can answer
suggestQuestions :: Graph -> CommunityMap -> Map CommunityId Text -> [SuggestedQuestion]
suggestQuestions g commMap labels =
  let nodeComm = nodeCommunityMap commMap
      ambiguousEdges = [(u, v, d) | (u, v, d) <- allEdges g
                                   , edgeConfidence d == Ambiguous]
      ambiguousQs = [SuggestedQuestion
        { sqType = "ambiguous_edge"
        , sqQuestion = Just $ "What is the exact relationship between `" <> nodeLabel' g u <> "` and `" <> nodeLabel' g v <> "`?"
        , sqWhy = "Edge tagged AMBIGUOUS (relation: " <> relationToText (edgeRelation d) <> ") - confidence is low."
        } | (u, v, d) <- take 3 ambiguousEdges]
      bridgeQs = bridgeNodeQuestions g nodeComm labels
      lowCohesionQs = lowCohesionQuestions g commMap labels
  in take 7 (ambiguousQs ++ bridgeQs ++ lowCohesionQs)

-- ───────────────────────────────────────────────
-- Internal helpers
-- ───────────────────────────────────────────────

nodeCommunityMap :: CommunityMap -> Map NodeId CommunityId
nodeCommunityMap commMap = Map.fromList [(nid, cid) | (cid, nids) <- Map.toList commMap, nid <- nids]

crossFileSurprises :: Graph -> Map NodeId CommunityId -> Int -> [SurprisingConnection]
crossFileSurprises g nodeComm topN =
  let candidates = [(u, v, d, score, reasons)
                   | (u, v, d) <- allEdges g
                   , let uSrc = nodeSourceFile (nodeData g u)
                   , let vSrc = nodeSourceFile (nodeData g v)
                   , not (T.null uSrc)
                   , not (T.null vSrc)
                   , uSrc /= vSrc
                   , edgeRelation d `notElem` [Imports, ImportsFrom, Contains, Method]
                   , not (isConceptNode (nodeData g u))
                   , not (isConceptNode (nodeData g v))
                   , let (score, reasons) = surpriseScore g d nodeComm uSrc vSrc u v
                   ]
      sorted = sortOn (\(_, _, _, s, _) -> Down s) candidates
  in take topN [SurprisingConnection
    { scSource      = nodeLabel' g u
    , scTarget      = nodeLabel' g v
    , scSourceFiles = [nodeSourceFile (nodeData g u), nodeSourceFile (nodeData g v)]
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
                   , edgeRelation d `notElem` [Imports, ImportsFrom, Contains, Method]
                   ]
      sorted = sortOn (\(_, _, d, _, _) -> Down (edgeConfidence d)) candidates
      deduped = nubBy (\a b -> ((\(_,_,_,cu,cv) -> (cu,cv)) a == (\(_,_,_,cu,cv) -> (cu,cv)) b)) sorted
  in take topN [SurprisingConnection
    { scSource      = nodeLabel' g u
    , scTarget      = nodeLabel' g v
    , scSourceFiles = [nodeSourceFile (nodeData g u), nodeSourceFile (nodeData g v)]
    , scConfidence  = edgeConfidence d
    , scRelation    = relationToText (edgeRelation d)
    , scWhy         = "Bridges community " <> maybe "" (T.pack . show) cid_u <> " → community " <> maybe "" (T.pack . show) cid_v
    } | (u, v, d, cid_u, cid_v) <- deduped]

surpriseScore :: Graph -> Edge -> Map NodeId CommunityId -> Text -> Text -> NodeId -> NodeId -> (Int, [Text])
surpriseScore _g edge nodeComm uSrc vSrc u v =
  let confBonus = case edgeConfidence edge of
        Ambiguous  -> 3
        Inferred  -> 2
        Extracted -> 1
      crossFiletype = if fileCategory uSrc /= fileCategory vSrc
                      then (2, ["crosses file types"])
                      else (0, [])
      crossComm = case (Map.lookup u nodeComm, Map.lookup v nodeComm) of
        (Just cu, Just cv) | cu /= cv -> (1, ["bridges separate communities"])
        _ -> (0, [])
      semBonus = if edgeRelation edge == SemanticallySimilarTo
                 then (round @Double (fromIntegral confBonus * 1.5), ["semantically similar concepts"])
                 else (0, [])
  in (confBonus + fst crossFiletype + fst crossComm + fst semBonus
     , ["inferred connection" | confBonus == 2] ++ snd crossFiletype ++ snd crossComm ++ snd semBonus)

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

lowCohesionQuestions :: Graph -> CommunityMap -> Map CommunityId Text -> [SuggestedQuestion]
lowCohesionQuestions g commMap labels =
  [SuggestedQuestion
   { sqType = "low_cohesion"
   , sqQuestion = Just $ "Should `" <> lbl <> "` be split into smaller, more focused modules?"
   , sqWhy = "Cohesion score is low - nodes in this community are weakly interconnected."
   } | (cid, members) <- Map.toList commMap
    , length members >= 5
    , let score = cohesionScore g members
    , score < 0.15
    , let lbl = Map.findWithDefault (T.pack ("Community " ++ show cid)) cid labels]

nodeBetweenness :: Graph -> Map NodeId Double
nodeBetweenness g = Map.fromList [(nid, 1.0 / fromIntegral (max 1 (degree g nid)))
                                  | nid <- Map.keys (gNodes g)]

fileCategory :: Text -> FileType
fileCategory path
  | any (`T.isSuffixOf` T.toLower path) codeExts = CodeFile
  | any (`T.isSuffixOf` T.toLower path) paperExts = PaperFile
  | otherwise = DocumentFile
  where
    codeExts = [".py", ".ts", ".js", ".go", ".rs", ".java", ".c", ".cpp", ".rb", ".cs", ".kt"
               ,".scala", ".php", ".swift", ".lua", ".zig", ".hs", ".ex", ".m", ".jl"]
    paperExts = [".pdf"]

nodeData :: Graph -> NodeId -> Node
nodeData g nid = Map.findWithDefault (Node
  { nodeId           = nid
  , nodeLabel        = T.pack "unknown"
  , nodeFileType     = CodeFile
  , nodeSourceFile   = T.pack ""
  , nodeSourceLocation = Nothing
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodeSourceUrl    = Nothing
  , nodeCapturedAt   = Nothing
  , nodeAuthor       = Nothing
  , nodeContributor  = Nothing
  }) nid (gNodes g)

nodeLabel' :: Graph -> NodeId -> Text
nodeLabel' g nid = nodeLabel (nodeData g nid)

allEdges :: Graph -> [(NodeId, NodeId, Edge)]
allEdges g = [(s,t,e) | ((s,t), e) <- Map.toList (gEdges g)]

data Down a = Down a deriving (Eq, Show)
instance Ord a => Ord (Down a) where
  compare (Down x) (Down y) = compare y x