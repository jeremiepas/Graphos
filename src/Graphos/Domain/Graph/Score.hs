-- | Scoring and verdict types for query results.
--
-- Normalized scoring = matched-terms / query-terms with exact full-label boost.
-- Verdict thresholds are named Domain constants so they can be retuned from field data.
--
-- Pure — no IO, fully testable.
{-# LANGUAGE StrictData #-}
module Graphos.Domain.Graph.Score
  ( -- * Verdict
    MatchVerdict(..)
  , verdictThreshold
  , computeVerdict
  , showVerdict

    -- * Scored node
  , ScoredNode(..)
  , scoredNodeLabel

    -- * Query response
  , QueryResponse(..)

    -- * Scoring helpers
  , normalizeScore
  , computeScore
  , fullLabelBoost

    -- * Hash
  , resultHash

    -- * Suggestions
  , findSuggestions
  , boundedDL
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withText)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))
import Data.Word (Word32)
import Data.Bits (xor, shiftR, (.&.))
import Data.List (sortOn)

import Graphos.Domain.Types (NodeId)
import Graphos.Domain.Graph.Index (GraphIndex(..), lookupTerm)

-- | Match verdict — self-reports query quality.
--
-- Strong: best score >= threshold (0.5). The query likely found what it was looking for.
-- Weak: best score > 0 but < threshold. Some match but uncertain.
-- NoMatch: no score > 0. Zero traversal — deleted the degenerate-fallback path.
data MatchVerdict
  = Strong
  | Weak
  | NoMatch
  deriving (Eq, Show, Generic, Ord)

instance NFData MatchVerdict

instance ToJSON MatchVerdict where
  toJSON Strong   = "strong"
  toJSON Weak     = "weak"
  toJSON NoMatch  = "none"

instance FromJSON MatchVerdict where
  parseJSON = withText "MatchVerdict" $ \t -> case t of
    "strong"  -> pure Strong
    "weak"    -> pure Weak
    "none"    -> pure NoMatch
    _         -> fail $ "Unknown verdict: " ++ T.unpack t

-- | Normalized score threshold for Strong verdict.
-- Named constant so it can be retuned from field data.
-- 0.5 means the query matched at least half of its terms in a single node.
verdictThreshold :: Double
verdictThreshold = 0.5

-- | Compute verdict from the best normalized score.
computeVerdict :: Double -> MatchVerdict
computeVerdict score
  | score >= verdictThreshold = Strong
  | score > 0                 = Weak
  | otherwise                 = NoMatch

-- | Render verdict as a short text label for CLI output.
showVerdict :: MatchVerdict -> Text
showVerdict Strong   = "strong"
showVerdict Weak     = "weak"
showVerdict NoMatch  = "none"

-- | A node scored by query relevance.
-- Score is normalized 0-1: matched-terms / query-terms with full-label boost.
data ScoredNode = ScoredNode
  { snNodeId      :: !NodeId
  , snLabel       :: !Text
  , snScore       :: !Double
  , snSourceFile  :: !Text
  , snCommunityId :: !(Maybe Int)
  } deriving (Eq, Show, Generic)

instance NFData ScoredNode

instance ToJSON ScoredNode where
  toJSON n = object
    [ "id"         .= snNodeId n
    , "label"      .= snLabel n
    , "score"      .= snScore n
    , "source_file" .= snSourceFile n
    , "community"  .= snCommunityId n
    ]

-- | Get the label text of a scored node.
scoredNodeLabel :: ScoredNode -> Text
scoredNodeLabel = snLabel

-- | Query response — carries verdict, scored results, and suggestions.
--
-- This replaces QueryResult end-to-end. The renderer consumes this type,
-- so verdict/hash/suggestions are always available to the caller.
data QueryResponse = QueryResponse
  { qrespVerdict     :: !MatchVerdict
  , qrespBestScore   :: !Double
  , qrespHash        :: !Text           -- ^ FNV-1a hex over ordered result node ids
  , qrespNodes       :: ![ScoredNode]   -- ^ Ranked score-descending
  , qrespEdges       :: ![(Text, Text, Text, Double)]
  , qrespSuggestions :: ![Text]         -- ^ Did-you-mean suggestions (always on NoMatch, alongside Weak)
  } deriving (Eq, Show, Generic)

instance NFData QueryResponse

instance ToJSON QueryResponse where
  toJSON r = object
    [ "verdict"      .= qrespVerdict r
    , "best_score"   .= qrespBestScore r
    , "hash"         .= qrespHash r
    , "nodes"        .= qrespNodes r
    , "edges"        .= qrespEdges r
    , "suggestions"  .= qrespSuggestions r
    ]

-- | Normalize a raw match count to a 0-1 score.
-- normalized = matchedTerms / queryTerms
normalizeScore :: Int -> Int -> Double
normalizeScore matched queryTotal
  | queryTotal == 0 = 0
  | otherwise       = fromIntegral matched / fromIntegral queryTotal

-- | Compute a raw match count from the index lookup results.
-- Counts how many of the query terms matched this node.
computeScore :: [Text] -> GraphIndex -> NodeId -> Int
computeScore terms idx nid =
  length $ filter (\t -> nid `elem` lookupTerm t idx) terms

-- | Full-label exact match boost.
--
-- When a query term exactly matches the lowercased full label of a node,
-- give it a small boost (+0.1) to rank it higher.
fullLabelBoost :: Text -> Text -> Double
fullLabelBoost queryTerm label
  | T.toLower queryTerm == T.toLower label = 0.1
  | otherwise = 0

-- | FNV-1a hash over ordered node ids, returned as 8-char hex.
--
-- Same graph + query ⇒ identical hash; different result id lists ⇒ different hashes.
-- Used for "no new information" detection by callers.
resultHash :: [NodeId] -> Text
resultHash ids =
  let raw = fnv1a32List (map T.unpack ids)
  in T.pack (printf "%08x" raw)

printf :: String -> Word32 -> String
printf fmt w = case fmt of
  "%08x" -> printf8 w
  _      -> show w

printf8 :: Word32 -> String
printf8 w =
  let hexDigits = "0123456789abcdef"
      toHex :: Int -> Char
      toHex d = hexDigits !! d
      d0 = fromIntegral ((w `shiftR` 28) .&. 0xF)
      d1 = fromIntegral ((w `shiftR` 24) .&. 0xF)
      d2 = fromIntegral ((w `shiftR` 20) .&. 0xF)
      d3 = fromIntegral ((w `shiftR` 16) .&. 0xF)
      d4 = fromIntegral ((w `shiftR` 12) .&. 0xF)
      d5 = fromIntegral ((w `shiftR`  8) .&. 0xF)
      d6 = fromIntegral ((w `shiftR`  4) .&. 0xF)
      d7 = fromIntegral ( w         .&. 0xF)
  in [toHex d0, toHex d1, toHex d2, toHex d3, toHex d4, toHex d5, toHex d6, toHex d7]

fnv1a32List :: [String] -> Word32
fnv1a32List = foldl' fnv1a32ByteWord fnv1a32Init
  where
    fnv1a32Init :: Word32
    fnv1a32Init = 2166136261

    fnv1a32ByteWord :: Word32 -> String -> Word32
    fnv1a32ByteWord h = foldl' fnv1a32Byte h
      where
        fnv1a32Byte :: Word32 -> Char -> Word32
        fnv1a32Byte h' c = (h' `xor` fromIntegral (fromEnum c)) * 16777619

-- | Find did-you-mean suggestions from the index vocabulary.
--
-- Pure function: nearest tokens from giLabelIndex by bounded Damerau-Levenshtein
-- distance (≤ 2) with first-char + length-window (±2) candidate pruning.
-- Up to 10 suggestions, ranked by distance then shared prefix.
--
-- Returns empty list when no indexed token is within distance bound.
findSuggestions :: [Text] -> GraphIndex -> [Text]
findSuggestions queryTerms idx =
  let queryWords = map T.toLower queryTerms
      vocab = Map.keys (giLabelIndex idx)
      -- Collect all misspelled query terms to suggest corrections for
      misspelled = filter (\w -> null (lookupTerm w idx)) queryWords
      -- For each misspelled word, find nearest vocabulary tokens
      allSuggestions :: [(Text, (Int, Int))]
      allSuggestions = concatMap (findNearest vocab) misspelled
      -- Deduplicate by token name, keeping best (distance, sharedPrefix)
      deduped :: Map Text (Int, Int)
      deduped = Map.fromListWith bestSuggestion allSuggestions
      bestSuggestion :: (Int, Int) -> (Int, Int) -> (Int, Int)
      bestSuggestion (d1, p1) (d2, p2)
        | d1 < d2   = (d1, p1)
        | d1 > d2   = (d2, p2)
        | p1 > p2   = (d1, p1)
        | otherwise = (d2, p2)
      -- Sort by distance asc, then shared prefix desc, take top 10
      ranked :: [(Text, (Int, Int))]
      ranked = sortOn (\(_, (d, p)) -> (d, negate p)) (Map.toList deduped)
  in map fst (take 10 ranked)

-- | Find nearest vocabulary tokens to a misspelled word.
-- Returns (token, (distance, sharedPrefix)) tuples.
-- Uses first-char + length-window (±2) pruning before computing distances.
findNearest :: [Text] -> Text -> [(Text, (Int, Int))]
findNearest vocab word =
  let wl = T.length word
      fc = case T.unpack word of
        (c:_) -> c
        _     -> '\NUL'
      -- Prune: length within ±2 and same first character
      candidates = filter (\v -> inRange v) vocab
      inRange v = let vl = T.length v
                  in abs (vl - wl) <= 2 && T.head v == fc
      -- Compute bounded DL distance (≤ 2) once per candidate
      results = [ (v, (d, sharedPrefixCount word v))
                | v <- candidates
                , let d = boundedDL word v
                , d <= 2 ]
  in results

-- | Compute Damerau-Levenshtein distance, bounded at 2.
-- Returns 3 if distance exceeds 2 (for pruning).
--
-- Two-row DP: O(m*n) time, O(n) space. The previous un-memoized
-- recursion recomputed each (i,j) subproblem exponentially many times
-- (work ~ 1.84^(m+n)), hanging for minutes on ~24-char query terms.
boundedDL :: Text -> Text -> Int
boundedDL a b
  | abs (T.length a - T.length b) > 2 = 3
  | otherwise = dl (T.unpack a) (T.unpack b)
  where
    dl :: String -> String -> Int
    dl xs ys =
      let n = length ys
          row0 = map (\j -> if j > 2 then 3 else j) [0..n] :: [Int]
          buildRow :: [Int] -> [Int] -> Int -> [Int]
          buildRow pp p i =
            let x  = xs !! (i - 1)
                xp = if i >= 2 then xs !! (i - 2) else '\NUL'
                cell j prevCell
                  | j == 0    = i
                  | otherwise =
                      let subCost = if x == ys !! (j - 1) then 0 else 1
                          del   = p !! j + 1
                          ins   = prevCell + 1
                          sub   = p !! (j - 1) + subCost
                          trans = if j >= 2 && x == ys !! (j - 2) && xp == ys !! (j - 1)
                                    then pp !! (j - 2) + 1
                                    else 3
                          m = min (min (min del ins) sub) trans
                      in if m >= 3 then 3 else m
                step acc j =
                  let (prevCell, accList) = acc
                      c = cell j prevCell
                  in (c, c : accList)
                (_, rev) = foldl' step (i, []) [1..n]
            in i : reverse rev
          rows = foldl' (\(pp, p) i -> let r = buildRow pp p i in (p, r))
                        (row0, row0) [1..length xs]
      in snd rows !! n

-- | Count of shared leading characters between two texts.
sharedPrefixCount :: Text -> Text -> Int
sharedPrefixCount a b =
  let go "" "" = 0
      go (x:xs) (y:ys)
        | x == y    = 1 + go xs ys
        | otherwise = 0
      go _ _ = 0
  in go (T.unpack a) (T.unpack b)
