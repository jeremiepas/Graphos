{-# LANGUAGE StrictData #-}
module Graphos.UseCase.Query.Refine
  ( -- * Types
    EdgeMode(..)
  , RefineConfig(..)
  , defaultRefineConfig

    -- * Refining
  , refineEdges
  , collapseSelfEdges
  , dedupDeclarations
  , elideLabel
  , refineNodes
  , refineResponse
  ) where

import Data.Char (isSpace)
import Data.List (sortOn, uncons)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types (NodeId, Node(..))
import Graphos.Domain.Graph.Score (ScoredNode(..), QueryResponse(..))

data EdgeMode = Semantic | All
  deriving (Eq, Show, Read)

data RefineConfig = RefineConfig
  { rcEdgeMode   :: !EdgeMode
  , rcLabelWidth :: !Int
  } deriving (Eq, Show)

defaultRefineConfig :: RefineConfig
defaultRefineConfig = RefineConfig
  { rcEdgeMode   = Semantic
  , rcLabelWidth = 120
  }

-- | Trivia tokens: labels that are noise when appearing as Contains-edge targets.
triviaTokens :: Set Text
triviaTokens = Set.fromList
  [ "undefined", "unknown", "null"
  , "void", "unit", "never", "any", "object"
  , "promise", "result", "option", "either"
  , "true", "false", "none", "some"
  ]

-- | Check if a label is trivia.
isTrivia :: Text -> Bool
isTrivia label =
  let lower = T.toLower (T.strip label)
  in lower `Set.member` triviaTokens
     || (T.length lower <= 3 && not (T.null lower))

-- | Check if a label is a single-token type parameter (e.g. "T", "A").
isSingleTokenTypeParam :: Text -> Bool
isSingleTokenTypeParam label =
  let trimmed = T.strip label
  in T.length trimmed == 1 || (T.length trimmed <= 3 && T.all (\c -> c >= 'A' && c <= 'Z') trimmed)

-- | Filter edges based on EdgeMode.
-- In Semantic mode, drops Contains edges whose target label is trivia
-- or whose target is a leaf with degree 1 and label length > 200.
refineEdges :: EdgeMode
            -> Map NodeId Node
            -> [(Text, Text, Text, Double)]
            -> [(Text, Text, Text, Double)]
refineEdges All _ edges = edges
refineEdges Semantic nodeMap edges = filter keepEdge edges
  where
    keepEdge (_src, tgt, rel, _conf) =
      case rel of
        "contains" ->
          let tgtNode = Map.lookup tgt nodeMap
              tgtLabel = maybe tgt nodeLabel tgtNode
          in not (isTrivia tgtLabel || isSingleTokenTypeParam tgtLabel)
                && case tgtNode of
                     Just n -> maybe False (\d -> d > 1 || T.length (nodeLabel n) <= 200) (nodeDegree n)
                     Nothing -> True
        _ -> True

-- | Collapse self-edges: remove edges where source == target.
collapseSelfEdges :: [(Text, Text, Text, Double)] -> [(Text, Text, Text, Double)]
collapseSelfEdges = filter (\(src, tgt, _, _) -> src /= tgt)

-- | Strip common declaration prefixes from a label, returning the base name.
stripDeclarationPrefix :: Text -> Text
stripDeclarationPrefix label =
  let prefixes = [ "export const ", "export let ", "export var ", "export function "
                 , "export class ", "export interface ", "export type ", "export enum "
                 , "const ", "let ", "var ", "function ", "class ", "interface "
                 , "type ", "enum ", "public ", "private ", "protected ", "static "
                 , "async ", "abstract ", "readonly "
                 ]
  in foldl (\acc prefix ->
              if prefix `T.isPrefixOf` T.toLower label
              then T.drop (T.length prefix) label
              else acc
           ) label prefixes

-- | Group key for deduplication: (sourceFile, lineStart, strippedLabel).
type DedupKey = (Text, Maybe Int, Text)

-- | Deduplicate nodes whose labels differ only by declaration prefix,
-- given that they share the same source file and line.
-- Merged nodes keep the shortest label and union their edges.
dedupDeclarations :: Map NodeId Node
                  -> [ScoredNode]
                  -> [(Text, Text, Text, Double)]
                  -> ([ScoredNode], [(Text, Text, Text, Double)])
dedupDeclarations nodeMap nodes edges =
  let grouped :: Map DedupKey [ScoredNode]
      grouped = Map.fromListWith (++) [(dedupKeyOf n, [n]) | n <- nodes]
      (mergedNodes, mergeMap) = mergeGroups grouped
      remapEdge (src, tgt, rel, conf) =
        let newSrc = Map.findWithDefault src src mergeMap
            newTgt = Map.findWithDefault tgt tgt mergeMap
        in (newSrc, newTgt, rel, conf)
      newEdges = map remapEdge edges
  in (mergedNodes, newEdges)
  where
    dedupKeyOf n = case Map.lookup (snNodeId n) nodeMap of
      Just orig -> (snSourceFile n, nodeLineStart orig, T.toLower (stripDeclarationPrefix (snLabel n)))
      Nothing   -> (snSourceFile n, Nothing, T.toLower (stripDeclarationPrefix (snLabel n)))
    mergeGroups groups = foldl mergeGroup ([], Map.empty) (Map.toList groups)
    mergeGroup (accNodes, accMap) (_, [])      = (accNodes, accMap)
    mergeGroup (accNodes, accMap) (_, [single]) = (single : accNodes, accMap)
    mergeGroup (accNodes, accMap) (_, dupes@(_:_:_)) =
      let sorted = sortOn (T.length . snLabel) dupes
          rep = case uncons sorted of Just (r, _) -> r; Nothing -> error "impossible: dupes is non-empty"
          repId = snNodeId rep
          ids = map snNodeId dupes
          newMap = foldl (\m nid -> Map.insert nid repId m) accMap ids
      in (rep : accNodes, newMap)

-- | Elide a label at a word boundary, keeping it under the given width.
-- If the label exceeds the width, truncate at the last word boundary before
-- the limit and append "…".
elideLabel :: Int -> Text -> Text
elideLabel width label
  | T.length label <= width = label
  | otherwise =
      let prefix = T.take (max 1 (width - 1)) label
          trimmed = T.dropWhileEnd isSpace prefix
          (beforeLastWord, _) = T.breakOnEnd " " (T.init trimmed)
          result = if T.null beforeLastWord || T.length beforeLastWord < 2
                   then T.take width label <> "…"
                   else T.stripEnd beforeLastWord <> "…"
      in result

-- | Refine scored nodes: apply label elision.
refineNodes :: Int -> [ScoredNode] -> [ScoredNode]
refineNodes labelWidth nodes = map elideNodeLabel nodes
  where
    elideNodeLabel n = n { snLabel = elideLabel labelWidth (snLabel n) }

-- | Full refine pipeline for a QueryResponse.
-- Applies: edge filtering, self-edge collapse, declaration dedup, label elision.
refineResponse :: RefineConfig
               -> Map NodeId Node
               -> QueryResponse
               -> QueryResponse
refineResponse cfg nodeMap resp =
  let edgeMode = rcEdgeMode cfg
      labelWidth = rcLabelWidth cfg
      edges1 = refineEdges edgeMode nodeMap (qrespEdges resp)
      edges2 = collapseSelfEdges edges1
      (dedupedNodes, edges3) = dedupDeclarations nodeMap (qrespNodes resp) edges2
      finalNodes = refineNodes labelWidth dedupedNodes
  in resp { qrespNodes = finalNodes, qrespEdges = edges3 }