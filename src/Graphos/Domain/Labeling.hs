-- | LLM labeling domain types — pure data, no IO.
-- Configuration for community labeling via OpenAI-compatible APIs.
module Graphos.Domain.Labeling
  ( LabelingResult(..)
  , labelPrompt
  , batchCommunities
  ) where

import Data.List (sortOn, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (toText)
import Data.Aeson (Value, eitherDecode, encode)

import Graphos.Domain.Types (CommunityId, Node(..), CommunityMap, FileType(..))
import Graphos.Domain.Types.Node (NodeId)
import Graphos.Domain.Graph (Graph, gNodes, degree, gCompositions)
import Graphos.Domain.Community (CommunityComposition(..))

-- | Result of LLM community labeling.
data LabelingResult = LabelingResult
  { lrLabels        :: Map CommunityId Text    -- ^ Community ID → LLM-generated label
  , lrTokensIn     :: Int                     -- ^ Total input tokens used
  , lrTokensOut    :: Int                     -- ^ Total output tokens used
  , llmRawResponses :: [Text]                 -- ^ Raw LLM responses for debugging
  } deriving (Eq, Show)

-- | Build a labeling prompt for a batch of communities.
-- Includes top member nodes (by degree), internal edge types, and cohesion.
labelPrompt :: Graph -> CommunityMap -> Map CommunityId Double -> [CommunityId] -> Text
labelPrompt g commMap cohesion cids =
  let compMap = case gCompositions g of
        Just cv -> case parseComps cv of
          Right comps -> comps
          Left _ -> Map.empty
        Nothing -> Map.empty
      communitySections = map (formatCommunity g commMap cohesion compMap) cids
  in T.unlines
      [ "You are a code-and-knowledge architecture analyst. Given these communities of related nodes"
      , "(code and documentation), assign a concise 2-4 word label that names the CONCEPT that"
      , "unifies each community — not the most frequent word."
      , ""
      , T.intercalate "\n" communitySections
      , "Respond ONLY with a JSON object mapping community IDs to labels."
      , "Example: {\"483\": \"Export Module\", \"484\": \"Config Parsing\"}"
      ]
  where
    parseComps :: Value -> Either String (Map CommunityId CommunityComposition)
    parseComps v = case eitherDecode (encode v) of
      Right comps -> Right (Map.fromList [(cid, comp) | (cid, comp) <- Map.toList comps])
      Left err -> Left err

-- | Format a single community for the labeling prompt.
formatCommunity :: Graph -> CommunityMap -> Map CommunityId Double -> Map CommunityId CommunityComposition -> CommunityId -> Text
formatCommunity g commMap cohesion compMap cid =
  case Map.lookup cid commMap of
    Nothing -> ""
    Just members ->
      let comp = Map.lookup cid compMap
          (codeNodes, docNodes) = partitionNodes members
          coh = case Map.lookup cid cohesion of
                  Just c -> T.pack $ show c
                  Nothing -> "N/A"
          size = T.pack (show (length members))
      in case comp of
        Just cc ->
          let compLine = "composition: "
                       <> T.pack (show (ccCodeCount cc)) <> " code + "
                       <> T.pack (show (ccDocCount cc)) <> " docs, "
                       <> T.pack (show (ccCodeDocEdges cc)) <> " code↔doc links"
          in T.concat
                [ "Community ", T.pack (show cid), " (cohesion: ", coh
                , ", size: ", size
                , ", " <> compLine <> "):"
                , "\n  Top code nodes: ", T.intercalate ", " codeNodes
                , case docNodes of
                    [] -> ""
                    ds -> "\n  Top doc nodes: " <> T.intercalate ", " ds
                , "\n"
                ]
        Nothing ->
          let labels = map (\nid -> case Map.lookup nid (gNodes g) of
                               Just n -> toText (nodeLabel n)
                               Nothing -> "unknown") $ take 10 codeNodes
          in T.concat
                [ "Community ", T.pack (show cid), " (cohesion: ", coh
                , ", size: ", size, "):"
                , "\n  Top nodes: ", T.intercalate ", " labels
                , "\n"
                ]
  where
    partitionNodes :: [NodeId] -> ([Text], [Text])
    partitionNodes members =
      let topNodes = take 10 $ map snd $ reverse $ sortOn fst
            [(degree g nid, nid) | nid <- members, Map.member nid (gNodes g)]
          (codeNodes, docNodes) = partition (\nid -> case Map.lookup nid (gNodes g) of
                                                      Just n -> nodeFileType n `elem` [CodeFile, PaperFile]
                                                      Nothing -> False) topNodes
       in ( map (\nid -> case Map.lookup nid (gNodes g) of
                          Just n -> toText (nodeLabel n) <> " (code)"
                          Nothing -> "unknown (code)") codeNodes
          , map (\nid -> case Map.lookup nid (gNodes g) of
                          Just n -> toText (nodeLabel n) <> " (doc)"
                          Nothing -> "unknown (doc)") docNodes
          )

-- | Split community IDs into batches of given size.
batchCommunities :: [CommunityId] -> Int -> [[CommunityId]]
batchCommunities _ 0 = []
batchCommunities [] _ = []
batchCommunities cids size = take size cids : batchCommunities (drop size cids) size