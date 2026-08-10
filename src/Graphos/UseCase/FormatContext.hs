-- | Compact context formatter for LLM consumption.
-- Produces minimal, high-signal text representation of a SelectedContext
-- suitable for inclusion in an LLM prompt.
--
-- Target: ~50 tokens/node, ~20 tokens/edge, ~100 tokens/community.
module Graphos.UseCase.FormatContext
  ( formatContextForLLM
  , formatContextForLLMBudgeted
  , formatNodeCompact
  , formatEdgeCompact
  , formatCommunityHeader
  , countContextTokens
  , omittedFooter
  , EdgeMode(..)
  , formatKeyEdgesFiltered
  , formatExpansionHintsBudgeted
  ) where

import Data.List (sortOn)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types (NodeId, Node(..), Edge(..), Confidence(..)
                            , FileType(..), relationToText, CommunityId)
import Graphos.Domain.Context (SelectedContext(..), SelectionStrategy(..))

-- ───────────────────────────────────────────────
-- Main formatter
-- ───────────────────────────────────────────────

-- | Format a SelectedContext into a compact markdown string for LLM consumption.
-- Produces sections: Community header → Key Nodes → Key Edges → Bridge Nodes → Expansion suggestions.
formatContextForLLM :: SelectedContext -> Text
formatContextForLLM sc =
  T.unlines $ concat
    [ [formatCommunityHeader sc]
    , [formatKeyNodes sc]
    , [formatKeyEdges sc]
    , if null (scBridgeNodes sc) then [] else [formatBridgeNodes sc]
    , if null (scGodNodes sc) then [] else [formatGodNodesSection sc]
    , [formatExpansionHints sc]
    ]

-- | Budget-aware variant of formatContextForLLM.
-- Renders greedily in relevance-rank order, stopping when the next item would
-- exceed the budget (measured in tokens via countContextTokens).
-- Always preserves the highest-ranked node.  Emits an omitted footer.
formatContextForLLMBudgeted :: Int -> SelectedContext -> (Text, Int, Int, Int)
formatContextForLLMBudgeted budget sc =
  let header = formatCommunityHeader sc
      headerToks = countContextTokens header
      -- Nodes sorted by descending relevance score (first element of tuple)
      sortedNodes = sortOn (\(score, _, _) -> Down score)
                       [(relevanceScore' nid n, nid, n) | (nid, n) <- scNodes sc]
      -- Edges sorted by descending endpoint relevance
      sortedEdges = sortOn (\(score, _) -> Down score)
                       [(edgeRelevanceScore e, e) | e <- scEdges sc]
      -- Greedy node rendering
      goNodes acc toksSoFar [] = (acc, toksSoFar, 0, 0)
      goNodes acc toksSoFar ((_score, nid, n):rest) =
        let line = "- " <> formatNodeCompact nid n
            lineToks = countContextTokens line
            newToks = toksSoFar + lineToks
        in if newToks > budget && not (T.null acc)
            then (acc, toksSoFar, length rest + 1, length sortedEdges)
            else goNodes (acc <> line <> "\n") newToks rest
      (nodeBody, nodeToks, omittedNodes, _) = goNodes "" headerToks sortedNodes
      -- Greedy edge rendering
      goEdges acc toksSoFar [] = (acc, toksSoFar, 0)
      goEdges acc toksSoFar ((_score, e):rest) =
        let line = formatEdgeCompact e
            lineToks = countContextTokens line
            newToks = toksSoFar + lineToks
        in if newToks > budget && not (T.null acc)
            then (acc, toksSoFar, length rest)
            else goEdges (acc <> line <> "\n") newToks rest
      (edgeBody, edgeToks, omittedEdges) = goEdges "" nodeToks sortedEdges
      -- Bridge section
      bridgeSection = if null (scBridgeNodes sc)
                      then (mempty, edgeToks, 0)
                      else let txt = formatBridgeNodes sc
                               toks = countContextTokens txt
                           in if edgeToks + toks > budget && not (T.null edgeBody)
                                then (mempty, edgeToks, length (scBridgeNodes sc))
                                else (txt <> "\n", edgeToks + toks, 0)
      (bridgeTxt, afterBridgeToks, omittedBridges) = bridgeSection
      -- Hub section
      hubSection = if null (scGodNodes sc)
                   then (mempty, afterBridgeToks, 0)
                   else let txt = formatGodNodesSection sc
                            toks = countContextTokens txt
                        in if afterBridgeToks + toks > budget && not (T.null (T.strip edgeBody))
                             then (mempty, afterBridgeToks, length (scGodNodes sc))
                             else (txt <> "\n", afterBridgeToks + toks, 0)
      (hubTxt, afterHubToks, omittedHubs) = hubSection
      -- Hints section
      hintsSection = if T.null (formatExpansionHints sc)
                     then (mempty, afterHubToks)
                     else let txt = formatExpansionHints sc
                              toks = countContextTokens txt
                          in if afterHubToks + toks > budget && not (T.null (T.strip edgeBody))
                               then (mempty, afterHubToks)
                               else (txt, afterHubToks + toks)
      (hintsTxt, _) = hintsSection
      -- Build output
      body = T.stripEnd $ T.unlines
               $ filter (not . T.null)
               $ [header
                 , "### Key Nodes"
                 , T.stripEnd nodeBody
                 , "### Key Edges"
                 , T.stripEnd edgeBody
                 , if T.null (T.strip edgeBody) then "" else T.stripEnd bridgeTxt
                 , if T.null (T.strip edgeBody) then "" else T.stripEnd hubTxt
                 , hintsTxt
                 ]
      totalOmitted = omittedNodes + omittedEdges + omittedBridges + omittedHubs
      footerTxt = if totalOmitted > 0
                  then "- _omitted: " <> T.pack (show omittedNodes) <> " nodes, "
                       <> T.pack (show omittedEdges) <> " edges_"
                  else ""
      final = if T.null footerTxt then body else body <> "\n" <> footerTxt
  in (final, countContextTokens final, totalOmitted, 0)

-- | Relevance score for a node (used for truncation ordering).
relevanceScore' :: NodeId -> Node -> Double
relevanceScore' _ n = fromIntegral (matchScoreNode n)
  where
    matchScoreNode :: Node -> Int
    matchScoreNode node =
      let lower = T.toLower (nodeLabel node)
      in length (filter (`T.isInfixOf` lower) (T.words lower))

-- | Edge relevance score: sum of endpoint relevance scores.
edgeRelevanceScore :: Edge -> Double
edgeRelevanceScore e =
  fromIntegral (matchScoreText (edgeSource e) + matchScoreText (edgeTarget e))
  where
    matchScoreText :: Text -> Int
    matchScoreText t = length (T.words (T.toLower t))

-- | Omitted footer text for a given count.
omittedFooter :: Int -> Int -> Text
omittedFooter nodes edges =
  "- _omitted: " <> T.pack (show nodes) <> " nodes, "
    <> T.pack (show edges) <> " edges_"

-- | Approximate token count for a text.
-- Uses a simple heuristic: ~0.75 tokens per word (subword tokenizers average).
countContextTokens :: Text -> Int
countContextTokens txt =
  let wordCount = length (T.words txt)
  in ceiling (fromIntegral wordCount * 1.33 :: Double)

-- ───────────────────────────────────────────────
-- Section formatters
-- ───────────────────────────────────────────────

-- | Community header with stats
formatCommunityHeader :: SelectedContext -> Text
formatCommunityHeader sc =
  let commCount = Map.size (scCommunities sc)
      nodeCount = length (scNodes sc)
      edgeCount = length (scEdges sc)
      stratName = strategyLabel (scStrategy sc)
  in T.concat
    [ "## Relevant Code Graph ("
    , stratName
    , ", "
    , T.pack (show nodeCount)
    , " nodes, "
    , T.pack (show edgeCount)
    , " edges, "
    , T.pack (show commCount)
    , " communities)"
    ]

-- | Trivia tokens: bare types and values that add no semantic signal to edges.
triviaTokens :: [Text]
triviaTokens = ["undefined", "unknown", "null", "void", "nil"
               ,"promise", "result", "option", "either"
               ,"string", "number", "boolean", "integer", "float", "double"
               ,"true", "false"
               ]

-- | Edge mode: semantic (drop trivia/AMBIGUOUS) or all (preserve everything).
data EdgeMode = Semantic | All
  deriving (Eq, Show)

-- | Check if an edge is trivia-targeting in semantic mode.
isTriviaEdge :: Edge -> Bool
isTriviaEdge e =
  let tgt = T.toLower (edgeTarget e)
  in any (\t -> T.isInfixOf t tgt) triviaTokens

-- | Check if an edge has ambiguous confidence.
isAmbiguousEdge :: Edge -> Bool
isAmbiguousEdge e = case edgeConfidence e of
  Confidence c -> c < 0.7

-- | Filter and rank edges by mode.
filterAndRankEdges :: EdgeMode -> [Edge] -> [Edge]
filterAndRankEdges mode edges = sorted
  where
    filtered = case mode of
      Semantic -> filter (\e -> not (isTriviaEdge e) && not (isAmbiguousEdge e)) edges
      All -> edges
    sorted = sortOn (Down . edgeRelevanceScore) filtered

-- | Key edges section with optional semantic filtering and relevance ranking.
formatKeyEdgesFiltered :: EdgeMode -> SelectedContext -> Text
formatKeyEdgesFiltered mode sc =
  let ranked = filterAndRankEdges mode (scEdges sc)
      edgeLines = map formatEdgeCompact (take 50 ranked)
  in T.unlines ("### Key Edges" : edgeLines)

-- | Expansion hints with bounded count, community size filter, and chat filter.
formatExpansionHintsBudgeted :: Int -> Int -> SelectedContext -> Text
formatExpansionHintsBudgeted maxHints maxCommSize sc =
  let nodeCommMap :: Map.Map NodeId CommunityId
      nodeCommMap = Map.fromList [(nid, cid) | (cid, nids) <- Map.toList (scCommunities sc), nid <- nids]
      -- Score communities by aggregate relevance of their members in scNodes
      ranked = take maxHints $ sortOn (Down . (\(_, _, _, s) -> s :: Double))
                       [(cid, label, commSize, score :: Double)
                       | (cid, label) <- Map.toList (scCommunityLabels sc)
                       , let commSize = length $ Map.findWithDefault [] cid (scCommunities sc)
                       , commSize <= maxCommSize
                       , let score = sum [relevanceScore' nid n | (nid, n) <- scNodes sc
                                                              , Map.lookup nid nodeCommMap == Just cid]
                       , score > 0]
      lines' = map (\(cid, label, size, _) ->
                       "- If reasoning about " <> label <> ": include community "
                          <> T.pack (show cid) <> " (" <> T.pack (show size) <> " nodes)")
                   ranked
   in if null lines'
      then ""
      else T.unlines ("### Suggested Context Expansion" : lines')

-- ───────────────────────────────────────────────
-- Original section formatters (kept for legacy callers)
-- ───────────────────────────────────────────────

-- | Key nodes section — compact: label + type + source location
formatKeyNodes :: SelectedContext -> Text
formatKeyNodes sc =
  let sorted = sortOn (\(_, n) -> T.toLower (nodeLabel n)) (scNodes sc)
      nodeLines = map (\(nid, n) -> "- " <> formatNodeCompact nid n) sorted
  in T.unlines ("### Key Nodes" : nodeLines)

-- | Key edges section (unfiltered, legacy).
formatKeyEdges :: SelectedContext -> Text
formatKeyEdges sc =
  let edgeLines = map formatEdgeCompact (take 50 (scEdges sc))
  in T.unlines ("### Key Edges" : edgeLines)

-- | Bridge nodes section
formatBridgeNodes :: SelectedContext -> Text
formatBridgeNodes sc =
  let lines' = map (\nid -> "- " <> nid <> " (connects communities)") (scBridgeNodes sc)
  in T.unlines ("### Bridge Nodes" : lines')

-- | God nodes section
formatGodNodesSection :: SelectedContext -> Text
formatGodNodesSection sc =
  let lines' = map (\(nid, degree) -> "- " <> nid <> " [degree=" <> T.pack (show degree) <> "]")
                   (scGodNodes sc)
  in T.unlines ("### Hub Nodes" : lines')

-- | Expansion hints (unbounded, legacy).
formatExpansionHints :: SelectedContext -> Text
formatExpansionHints sc =
  let hints = Map.toList (scCommunityLabels sc)
      lines' = map (\(cid, label) ->
                       let size = length $ Map.findWithDefault [] cid (scCommunities sc)
                       in "- If reasoning about " <> label <> ": include community "
                          <> T.pack (show cid) <> " (" <> T.pack (show size) <> " nodes)")
                  hints
  in if null lines'
     then ""
     else T.unlines ("### Suggested Context Expansion" : lines')

-- | Compact node representation: label [kind] — source:file:start-end | signature
formatNodeCompact :: Text -> Node -> Text
formatNodeCompact _nid n =
  let kind = maybe "" (\k -> "[" <> k <> "] ") (nodeKind n)
      base = nodeLabel n <> " " <> kind <> "[" <> showFileType (nodeFileType n) <> "]"
      src = if T.null (nodeSourceFile n) then ""
            else " — src:" <> nodeSourceFile n
                <> maybe "" (\start -> ":" <> T.pack (show start)) (nodeLineStart n)
                <> case nodeLineEnd n of
                     Just end -> "-" <> T.pack (show end)
                     Nothing -> ""
      sig = maybe "" (\s -> " | " <> s) (nodeSignature n)
  in base <> src <> sig

-- | Compact edge representation: source → target [relation, confidence]
formatEdgeCompact :: Edge -> Text
formatEdgeCompact e =
  T.concat
    [ edgeSource e
    , " → "
    , edgeTarget e
    , " ["
    , relationToText (edgeRelation e)
    , ", "
    , confidenceLabel (edgeConfidence e)
    , "]"
    ]

-- ───────────────────────────────────────────────
-- Token counting
-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

showFileType :: FileType -> Text
showFileType CodeFile   = "code"
showFileType DocFile    = "doc"
showFileType PaperFile  = "paper"
showFileType ImageFile  = "image"
showFileType VideoFile  = "video"
showFileType AudioFile  = "audio"
showFileType OfficeFile = "office"

confidenceLabel :: Confidence -> Text
confidenceLabel (Confidence c)
  | c >= 1.0  = "EXTRACTED"
  | c >= 0.7  = "INFERRED"
  | otherwise = "AMBIGUOUS"

strategyLabel :: SelectionStrategy -> Text
strategyLabel CommunityAware        = "community-aware"
strategyLabel RelevanceWeightedBFS = "relevance-weighted BFS"
strategyLabel PathBased             = "path-based"
strategyLabel DifferentialContext   = "differential"