{-# LANGUAGE StrictData #-}
-- | Byte-budget-aware, rank-then-serialize capping for query results.
--
-- These helpers share a single serialized-byte budget across the node and edge
-- lists of a query response. They operate purely on the already-score-ranked
-- node list and the subgraph edge list, retaining the highest-priority items
-- first until a running byte counter would exceed 'bcByteBudget'.
--
-- This module is intentionally a leaf: it depends only on
-- 'Graphos.Domain.Graph.Score' (for 'ScoredNode') and
-- 'Graphos.UseCase.Query.Refine' (for 'elideLabel'), never on
-- 'Graphos.UseCase.Query', so callers can use it without forming an import cycle.
module Graphos.UseCase.Query.Budget
  ( BudgetCtl(..)
  , defaultBudgetCtl
  , boundedNodes
  , boundedEdges
  , edgeJsonBytes
  , nodeJsonBytes
  , capLabel
  ) where

import Data.Aeson (toJSON, encode, Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8)

import Graphos.Domain.Graph.Score (ScoredNode(..))
import Graphos.UseCase.Query.Refine (elideLabel)

-- | Encode a JSON value to strict 'Text' (compact, no whitespace).
encodeText :: Value -> Text
encodeText = TL.toStrict . TL.decodeUtf8 . encode

-- | Byte length of a scored node's compact JSON encoding.
nodeJsonBytes :: ScoredNode -> Int
nodeJsonBytes n = T.length (encodeText (toJSON n))

-- | Byte length of an edge's compact JSON encoding.
edgeJsonBytes :: (Text, Text, Text, Double) -> Int
edgeJsonBytes e = T.length (encodeText (toJSON e))

-- | Controls for compact, byte-budget-aware serialization of query results.
--
-- 'bcByteBudget' caps the serialized node/edge list via a running byte counter;
-- 'bcMaxNodes' caps the number of returned nodes (-1 = unbounded);
-- 'bcMaxLabelChars' caps the label width before elision (0 = use default 120).
data BudgetCtl = BudgetCtl
  { bcByteBudget      :: !Int
  , bcMaxNodes        :: !Int
  , bcMaxLabelChars   :: !Int
  } deriving (Eq, Show)

defaultBudgetCtl :: BudgetCtl
defaultBudgetCtl = BudgetCtl
  { bcByteBudget      = 2000
  , bcMaxNodes        = -1
  , bcMaxLabelChars   = 120
  }

-- | Truncate a scored node's label to 'bcMaxLabelChars' (word-boundary aware).
-- A non-positive cap leaves the label untouched.
capLabel :: BudgetCtl -> ScoredNode -> ScoredNode
capLabel bc n
  | bcMaxLabelChars bc <= 0 = n
  | otherwise = n { snLabel = elideLabel (bcMaxLabelChars bc) (snLabel n) }

-- | Greedily retain scored nodes (already score-ranked) until their cumulative
-- serialized byte size would exceed 'bcByteBudget'. Returns the kept nodes and
-- the count of dropped (omitted) nodes. Highest scores are retained first, so a
-- response that fits always keeps the most relevant nodes.
boundedNodes :: BudgetCtl -> [ScoredNode] -> ([ScoredNode], Int)
boundedNodes bc nodes =
  let cap = if bcMaxNodes bc < 0
              then length nodes
              else min (bcMaxNodes bc) (length nodes)
      capped = take cap nodes
      go _ acc [] = (reverse acc, 0)
      go remaining acc (n : ns)
        | not (null acc) && nodeJsonBytes n > remaining = (reverse acc, length ns)
        | otherwise = go (remaining - nodeJsonBytes n) (n : acc) ns
      (kept, dropped) = go (max 0 (bcByteBudget bc)) [] capped
   in (kept, dropped)

-- | Greedily retain edges (already in score/insertion order) until their cumulative
-- serialized byte size would exceed 'bcByteBudget'. Returns the kept edges and the
-- count of dropped (omitted) edges. Highest-priority edges are retained first, so a
-- response that fits always keeps the most relevant connections.
boundedEdges :: BudgetCtl -> [(Text, Text, Text, Double)] -> ([(Text, Text, Text, Double)], Int)
boundedEdges bc edges =
  let go _ acc [] = (reverse acc, 0)
      go budgetLeft acc (e : es)
        | not (null acc) && edgeJsonBytes e > budgetLeft = (reverse acc, length es)
        | otherwise = go (budgetLeft - edgeJsonBytes e) (e : acc) es
      (kept, dropped) = go (max 0 (bcByteBudget bc)) [] edges
   in (kept, dropped)
