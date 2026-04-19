-- | Compact context formatter for LLM consumption.
-- Produces minimal, high-signal text representation of a SelectedContext
-- suitable for inclusion in an LLM prompt.
--
-- Target: ~50 tokens/node, ~20 tokens/edge, ~100 tokens/community.
module Graphos.UseCase.FormatContext
  ( formatContextForLLM
  , formatNodeCompact
  , formatEdgeCompact
  , formatCommunityHeader
  , countContextTokens
  ) where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types (Node(..), Edge(..), Confidence(..), GodNode(..)
                            , FileType(..), relationToText)
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

-- | Key nodes section — compact: label + type + source location
formatKeyNodes :: SelectedContext -> Text
formatKeyNodes sc =
  let sorted = sortOn (\(_, n) -> T.toLower (nodeLabel n)) (scNodes sc)
      nodeLines = map (\(nid, n) -> "- " <> formatNodeCompact nid n) sorted
  in T.unlines ("### Key Nodes" : nodeLines)

-- | Key edges section — compact: source → target [relation, confidence]
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
  let lines' = map (\g -> "- " <> gnLabel g <> " [degree=" <> T.pack (show (gnEdges g)) <> "]")
                   (scGodNodes sc)
  in T.unlines ("### Hub Nodes" : lines')

-- | Expansion hints — suggest which communities could be included
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

-- ───────────────────────────────────────────────
-- Individual formatters
-- ───────────────────────────────────────────────

-- | Compact node representation: label [kind] — source:file:start-end | signature
--   Target: ~50-60 tokens
formatNodeCompact :: Text -> Node -> Text
formatNodeCompact _nid n =
  let kind = maybe "" (\k -> "[" <> k <> "] ") (nodeKind n)
      base = nodeLabel n <> " " <> kind <> "[" <> showFileType (nodeFileType n) <> "]"
      src = if T.null (nodeSourceFile n) then ""
            else " — src:" <> nodeSourceFile n
                <> maybe "" (\loc -> ":" <> loc) (nodeSourceLocation n)
                <> case nodeLineEnd n of
                     Just end -> "-" <> T.pack (show end)
                     Nothing -> ""
      sig = maybe "" (\s -> " | " <> s) (nodeSignature n)
  in base <> src <> sig

-- | Compact edge representation: source → target [relation, confidence]
--   Target: ~20 tokens
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

-- | Approximate token count for a text.
-- Uses a simple heuristic: ~0.75 tokens per word (subword tokenizers average).
countContextTokens :: Text -> Int
countContextTokens txt =
  let wordCount = length (T.words txt)
  -- Approximate: 1 word ≈ 1.33 tokens (GPT-style tokenization average)
  -- We use ceiling to be conservative
  in ceiling (fromIntegral wordCount * 1.33 :: Double)

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

showFileType :: FileType -> Text
showFileType CodeFile     = "code"
showFileType DocumentFile = "doc"
showFileType PaperFile    = "paper"
showFileType ImageFile    = "image"
showFileType VideoFile    = "video"

confidenceLabel :: Confidence -> Text
confidenceLabel Extracted = "EXTRACTED"
confidenceLabel Inferred  = "INFERRED"
confidenceLabel Ambiguous = "AMBIGUOUS"

strategyLabel :: SelectionStrategy -> Text
strategyLabel CommunityAware        = "community-aware"
strategyLabel RelevanceWeightedBFS = "relevance-weighted BFS"
strategyLabel PathBased             = "path-based"
strategyLabel DifferentialContext   = "differential"