{-# LANGUAGE StrictData #-}
module Graphos.UseCase.Query.Render
  ( -- * Types
    CommonQueryOpts(..)
  , defaultCommonQueryOpts

    -- * Rendering
  , renderQueryResponseText
  , renderQueryResponseJSON
  , renderSymbolResultText
  , renderSymbolResultJSON
  , renderNeighborsResultText
  , renderNeighborsResultJSON
  , renderPathResultJSON
  , renderExplainResultJSON
  , renderAmbiguousText
  , renderAmbiguousJSON
  , renderNotFoundText
  , renderNotFoundJSON

    -- * Truncation
  , truncateOutput
  , estimateTokens
  ) where

import Data.Aeson (toJSON, object, (.=), Value(..), encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8)

import Graphos.Domain.Graph.Score (ScoredNode(..), QueryResponse(..), showVerdict)
import Graphos.Domain.Types.Node (Node(..), NodeId)
import Graphos.UseCase.Query (SymbolResult(..), NeighborsResult(..))
import Graphos.UseCase.Query.Refine (EdgeMode(..))

data CommonQueryOpts = CommonQueryOpts
  { cqoGraphPath  :: !FilePath
  , cqoBudget     :: !Int
  , cqoJson       :: !Bool
  , cqoLabelWidth :: !Int
  , cqoEdges      :: !EdgeMode
  } deriving (Eq, Show)

defaultCommonQueryOpts :: CommonQueryOpts
defaultCommonQueryOpts = CommonQueryOpts
  { cqoGraphPath  = "graphos-out/graph.json"
  , cqoBudget     = 2000
  , cqoJson       = False
  , cqoLabelWidth = 120
  , cqoEdges      = Semantic
  }

-- | Estimate token count from character count (rough: chars / 4).
estimateTokens :: Text -> Int
estimateTokens t = max 1 (T.length t `div` 4)

-- | Render a QueryResponse as human-readable text with budget-aware truncation.
renderQueryResponseText :: Int -> QueryResponse -> Text
renderQueryResponseText budget resp =
  let verdictLine = "Verdict: " <> showVerdict (qrespVerdict resp)
                     <> " (best score: " <> T.pack (show (qrespBestScore resp)) <> ")"
                     <> " [hash: " <> qrespHash resp <> "]"
      nodes = qrespNodes resp
      edges = qrespEdges resp
      suggestions = qrespSuggestions resp
      nodeLines = map renderScoredNode nodes
      edgeLines = map renderEdge edges
      suggestionLine = if null suggestions then "" else "\nDid you mean: " <> T.intercalate ", " suggestions <> "?"
      header = verdictLine <> "\n\n"
      nodesHeader = if null nodes then "" else "Results (" <> T.pack (show (length nodes)) <> " nodes):\n"
      nodesText = T.unlines nodeLines
      edgesHeader = if null edges then "" else "\nConnections:\n"
      edgesText = T.unlines edgeLines
      fullText = header <> nodesHeader <> nodesText <> edgesHeader <> edgesText <> suggestionLine
  in truncateOutput budget fullText

-- | Render a QueryResponse as a single JSON document via Aeson.
renderQueryResponseJSON :: QueryResponse -> Text
renderQueryResponseJSON resp = encodeText (toJSON resp)

-- | Render a SymbolResult as human-readable text.
renderSymbolResultText :: Int -> SymbolResult -> Text
renderSymbolResultText budget sr =
  if srNotFound sr
    then "No symbol found." <> if null (srSuggestions sr) then "" else "\nDid you mean: " <> T.intercalate ", " (srSuggestions sr) <> "?"
    else let header = "Symbols found: " <> T.pack (show (length (srFound sr))) <> "\n\n"
             nodeLines = map renderScoredNode (srFound sr)
         in truncateOutput budget (header <> T.unlines nodeLines)

-- | Render a SymbolResult as a single JSON document.
renderSymbolResultJSON :: SymbolResult -> Text
renderSymbolResultJSON sr = encodeText (toJSON sr)

-- | Render a NeighborsResult as human-readable text.
renderNeighborsResultText :: Int -> NeighborsResult -> Text
renderNeighborsResultText budget nr =
  case nrCenterNode nr of
    Nothing -> "Node not found."
    Just cid ->
      let header = "Neighbors of " <> cid <> " (depth " <> T.pack (show (nrMaxDepth nr)) <> "):\n\n"
          nodeLines = map renderScoredNode (nrNodes nr)
          edgeLines = map renderEdge (nrEdges nr)
          nodesText = T.unlines nodeLines
          edgesHeader = if null (nrEdges nr) then "" else "Connections:\n"
          edgesText = T.unlines edgeLines
      in truncateOutput budget (header <> nodesText <> edgesHeader <> edgesText)

-- | Render a NeighborsResult as a single JSON document.
renderNeighborsResultJSON :: NeighborsResult -> Text
renderNeighborsResultJSON nr = encodeText (toJSON nr)

-- | Render a scored node as a text line.
renderScoredNode :: ScoredNode -> Text
renderScoredNode sn =
  T.pack (show (snScore sn)) <> "  " <> snLabel sn <> " [" <> snNodeId sn <> "] (" <> snSourceFile sn <> ")"

-- | Render an edge as a text line.
renderEdge :: (Text, Text, Text, Double) -> Text
renderEdge (src, tgt, rel, conf) =
  src <> " --" <> rel <> "--> " <> tgt <> " [" <> T.pack (show conf) <> "]"

-- | Truncate output to fit within a budget (estimated tokens).
-- Always keeps the header (first non-empty line); truncates from the end.
-- Appends a footer showing how many lines were omitted.
truncateOutput :: Int -> Text -> Text
truncateOutput budget text
  | budget <= 0 = text
  | otherwise =
      let textLines = T.lines text
          totalTokens = sum (map estimateTokens textLines)
      in if totalTokens <= budget
         then text
         else case textLines of
                []     -> text
                (h:tl) ->
                  let headerBudget = estimateTokens (h <> "\n")
                      remainingBudget = budget - headerBudget
                      keptLines = takeLinesFromTop remainingBudget tl
                      omitted = length tl - length keptLines
                      footer = if omitted > 0
                               then "\n... truncated: " <> T.pack (show omitted) <> " more lines (raise --budget)"
                               else ""
                  in T.unlines (h : keptLines) <> footer

takeLinesFromTop :: Int -> [Text] -> [Text]
takeLinesFromTop _ [] = []
takeLinesFromTop remaining (l:ls)
  | estimateTokens l > remaining = []
  | otherwise = l : takeLinesFromTop (remaining - estimateTokens l) ls

-- | Encode a JSON value to Text (compact, no spaces).
encodeText :: Value -> Text
encodeText = TL.toStrict . TL.decodeUtf8 . encode

-- | Render a path result as JSON.
-- Nothing yields {"path":null}; Just ids yields {"path":[...],"hops":n} where hops = length ids - 1.
renderPathResultJSON :: Maybe [NodeId] -> Text
renderPathResultJSON Nothing = encodeText (object ["path" .= (Null :: Value)])
renderPathResultJSON (Just ids) =
  let hops = if null ids then 0 else length ids - 1
  in encodeText (object ["path" .= ids, "hops" .= hops])

-- | Render an explain result as JSON.
-- Nothing yields null; Just node yields the node's id/label/source_file/community.
renderExplainResultJSON :: Maybe Node -> Text
renderExplainResultJSON Nothing = encodeText (Null :: Value)
renderExplainResultJSON (Just node) =
  encodeText (object
    [ "id"          .= nodeId node
    , "label"       .= nodeLabel node
    , "source_file" .= nodeSourceFile node
    , "community"   .= nodeCommunityId node
    ])

-- | Render an ambiguous node-argument resolution as text: list every candidate
-- with its id and source location so the caller can re-run with a node id.
renderAmbiguousText :: [ScoredNode] -> Text
renderAmbiguousText candidates =
  "Ambiguous: " <> T.pack (show (length candidates))
    <> " nodes match. Re-run with a node id:\n"
    <> T.unlines
         [ "  " <> snLabel c <> " [" <> snNodeId c <> "] (" <> snSourceFile c <> ")"
         | c <- candidates ]

-- | Render an ambiguous node-argument resolution as a single JSON document:
-- @{"ambiguous":true,"candidates":[{"id":..,"label":..,"source_file":..}]}@.
renderAmbiguousJSON :: [ScoredNode] -> Text
renderAmbiguousJSON candidates = encodeText $ object
  [ "ambiguous"  .= True
  , "candidates" .=
      [ object [ "id" .= snNodeId c, "label" .= snLabel c, "source_file" .= snSourceFile c ]
      | c <- candidates ]
  ]

-- | Render a not-found node argument as text.
renderNotFoundText :: Text -> Text
renderNotFoundText arg = "Not found: no node id or label matches '" <> arg <> "'."

-- | Render a not-found node argument as a single JSON document: @{"not_found":<arg>}@.
renderNotFoundJSON :: Text -> Text
renderNotFoundJSON arg = encodeText $ object [ "not_found" .= arg ]