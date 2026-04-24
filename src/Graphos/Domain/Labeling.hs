-- | LLM labeling domain types — pure data, no IO.
-- Configuration for community labeling via OpenAI-compatible APIs.
module Graphos.Domain.Labeling
  ( LabelingResult(..)
  , labelPrompt
  , batchCommunities
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types (CommunityId, Node(..), CommunityMap)
import Graphos.Domain.Graph (Graph, gNodes, degree)

-- | Result of LLM community labeling.
data LabelingResult = LabelingResult
  { lrLabels   :: Map CommunityId Text    -- ^ Community ID → LLM-generated label
  , lrTokensIn :: Int                     -- ^ Total input tokens used
  , lrTokensOut :: Int                    -- ^ Total output tokens used
  , llmRawResponses :: [Text]            -- ^ Raw LLM responses for debugging
  } deriving (Eq, Show)

-- | Build a labeling prompt for a batch of communities.
-- Includes top member nodes (by degree), internal edge types, and cohesion.
labelPrompt :: Graph -> CommunityMap -> Map CommunityId Double -> [CommunityId] -> Text
labelPrompt g commMap cohesion cids =
  let communitySections = map (formatCommunity g commMap cohesion) cids
  in T.unlines
     [ "You are a code architecture analyst. Given these communities of related code nodes,"
     , "assign a concise 2-4 word label that describes each community's purpose."
     , ""
     , T.unlines communitySections
     , "Respond ONLY with a JSON object mapping community IDs to labels."
     , "Example: {\"483\": \"Export Module\", \"484\": \"Config Parsing\"}"
     ]

-- | Format a single community for the labeling prompt.
formatCommunity :: Graph -> CommunityMap -> Map CommunityId Double -> CommunityId -> Text
formatCommunity g commMap cohesion cid =
  case Map.lookup cid commMap of
    Nothing -> ""
    Just members ->
      let topNodes = take 10 $ map snd $ take 10 $ reverse $ sortOn snd
            [(degree g nid, nid) | nid <- members, Map.member nid (gNodes g)]
          labels = map (\nid -> case Map.lookup nid (gNodes g) of
                              Just n -> nodeLabel n
                              Nothing -> "unknown") topNodes
          coh = case Map.lookup cid cohesion of
                  Just c -> T.pack $ show c
                  Nothing -> "N/A"
      in T.concat
           [ "Community ", T.pack (show cid), " (cohesion: ", coh
           , ", size: ", T.pack (show (length members)), "):"
           , "\n  Top nodes: ", T.intercalate ", " labels
           , "\n"
           ]

-- | Split community IDs into batches of given size.
batchCommunities :: [CommunityId] -> Int -> [[CommunityId]]
batchCommunities _ 0 = []
batchCommunities [] _ = []
batchCommunities cids size = take size cids : batchCommunities (drop size cids) size

-- | Sort a list by descending second element.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . Map.toDescList . Map.fromList . map (\x -> (f x, x))