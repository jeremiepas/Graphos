-- | Community labelling - generate human-readable names for communities.
-- Uses degree-weighted TF-IDF-like scoring to find the most distinctive
-- words that characterize each community.
module Graphos.Domain.Community.Label
  ( suggestCommunityLabels
  , labelFromNodes
  ) where

import Data.List (sortOn, nub, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Function (on)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, degree)
import qualified Data.Set as Set

-- | Suggest community labels based on member node labels.
-- Uses degree-weighted TF-IDF-like scoring to find the most
-- distinctive words for each community.
suggestCommunityLabels :: Graph -> CommunityMap -> Map CommunityId Text
suggestCommunityLabels g commMap =
  -- Compute document frequency: how many communities contain each word
  let allCommWords = [Map.keysSet (wordFreqs g members) | (_, members) <- Map.toList commMap]
      docFreq = Map.fromListWith (+) [(w, 1) | wordSet <- allCommWords, w <- Set.toList wordSet]
      numComms = max 1 (length allCommWords)
  in Map.fromList [(cid, labelFromNodesWithIDF g members docFreq numComms) 
                   | (cid, members) <- Map.toList commMap]

-- | Generate a 2-3 word label from a list of node IDs (simple version)
labelFromNodes :: Graph -> [NodeId] -> Text
labelFromNodes g memberIds =
  let nodes = [n | nid <- memberIds, Just n <- [Map.lookup nid (gNodes g)]]
      labels = map nodeLabel nodes
      allWords = concatMap (T.words . T.toLower) labels
      filtered = filter (not . isStopWord) allWords
      wordCounts = Map.fromListWith (+) [(w, 1 :: Int) | w <- filtered]
      topWords = take 3 $ map fst $ sortOn (\(_, c) -> negate c) $ Map.toList wordCounts
  in if null topWords
     then "Unnamed Community"
     else T.intercalate " " (map capitalize topWords)

-- | Generate a label using TF-IDF-like scoring for distinctiveness
labelFromNodesWithIDF :: Graph -> [NodeId] -> Map Text Int -> Int -> Text
labelFromNodesWithIDF g memberIds docFreq numComms =
  let -- Get word frequencies weighted by node degree (high-degree nodes matter more)
      tf = wordFreqs g memberIds
      -- Compute TF-IDF score: (tf * degreeWeight) * log(numComms / df)
      scored = Map.toList $ Map.mapWithKey (\w tfVal ->
        let df = fromIntegral (Map.findWithDefault 1 w docFreq)
            idf = log (fromIntegral numComms / df)
        in tfVal * idf) tf
      -- Take top 2-3 words by TF-IDF score
      topWords = take 3 $ map fst $ sortOn (\(_, s) -> negate s) scored
  in if null topWords
     then "Unnamed Community"
     else T.intercalate " " (map capitalize topWords)

-- | Get word frequencies weighted by node degree
wordFreqs :: Graph -> [NodeId] -> Map Text Double
wordFreqs g memberIds =
  let nodes = [(n, degree g nid) | nid <- memberIds, Just n <- [Map.lookup nid (gNodes g)]]
      -- Split each label into words, weight by node degree
      wordEntries = [(w, max 1 (fromIntegral deg / maxAvgDeg))
                    | (n, deg) <- nodes
                    , w <- filter (not . isStopWord) (T.words (T.toLower (nodeLabel n)))
                    ]
      maxAvgDeg = if null nodes then 1.0 else max 1.0 (fromIntegral (sum (map snd nodes)) / fromIntegral (length nodes))
  in Map.fromListWith (+) wordEntries

-- ───────────────────────────────────────────────
-- Stop words (extended for code/graph contexts)
-- ───────────────────────────────────────────────

-- | Check if a word is a stop word (too common to be informative)
isStopWord :: Text -> Bool
isStopWord w = w `elem` stopWords || T.length w <= 1

stopWords :: [Text]
stopWords =
  -- English stop words
  [ "the", "a", "an", "is", "are", "was", "were", "be", "been"
  , "have", "has", "had", "do", "does", "did", "will", "would"
  , "can", "could", "may", "might", "shall", "should"
  , "of", "in", "to", "for", "with", "on", "at", "from", "by"
  , "and", "or", "not", "no", "but", "if", "then", "else"
  , "this", "that", "these", "those", "it", "its"
  , "as", "into", "through", "during", "before", "after"
  , "which", "who", "what", "when", "where", "how", "why"
  , "all", "each", "every", "both", "few", "more", "most"
  , "other", "some", "such", "than", "too", "very"
  , "also", "just", "about", "up", "out", "over", "only"
  -- Code stop words (common but uninformative)
  , "class", "module", "function", "def", "type", "data", "new"
  , "get", "set", "make", "create", "return", "import"
  , "void", "null", "true", "false", "const", "let", "var"
  , "pub", "fn", "func", "self", "super", "init", "main"
  , "error", "result", "ok", "err", "some", "none", "just"
  , "test", "spec", "mock", "stub", "helper", "util", "utils"
  , "config", "settings", "options", "params", "args"
  , "default", "base", "abstract", "interface", "impl"
  , "handler", "manager", "builder", "factory", "adapter"
  , "info", "detail", "item", "entry", "record", "value"
  , "list", "map", "set", "array", "vec", "pair"
  , "run", "exec", "call", "apply", "compute", "process"
  , "start", "stop", "begin", "end", "open", "close"
  , "add", "remove", "update", "delete", "insert", "append"
  -- Graph-specific stop words (uninformative when everything is a graph)
  , "node", "edge", "graph", "community", "cluster", "group"
  , "source", "target", "weight", "score", "index"
  ]

-- | Capitalize first letter of a word
capitalize :: Text -> Text
capitalize w = case T.uncons w of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> w
  where
    toUpper c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise = c