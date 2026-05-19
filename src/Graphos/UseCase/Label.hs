-- | LLM-based community labeling use case.
-- Batches communities, sends to LLM, parses labels, updates Neo4j.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.Label
  ( labelCommunities
  , pushLabelsToNeo4j
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (removeFile)
import Control.Exception (catch, SomeException)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Graphos.Domain.Types (CommunityId, CommunityMap, CohesionMap)
import Graphos.Domain.Graph (Graph)
import Graphos.Domain.Config (LabelingConfig(..), Neo4jConfig(..))
import Graphos.Domain.Labeling (LabelingResult(..), labelPrompt, batchCommunities)
import Graphos.Infrastructure.LLM.OpenAI (callLLM, parseLabelsFromResponse)

-- | Label communities using an LLM.
-- Batches communities, sends prompts, parses responses.
labelCommunities :: Graph -> CommunityMap -> CohesionMap -> LabelingConfig -> IO LabelingResult
labelCommunities g commMap cohesion cfg = do
  let cids = Map.keys commMap
      batchSize = labelingBatchSize cfg
      batches = batchCommunities cids batchSize

  TIO.putStrLn $ "[label] Labeling " <> T.pack (show (length cids)) <> " communities in "
              <> T.pack (show (length batches)) <> " batch(es)"

  allLabels <- mapM (labelBatch g commMap cohesion cfg) batches

  let mergedLabels = Map.unions allLabels
  pure LabelingResult
    { lrLabels = mergedLabels
    , lrTokensIn = 0
    , lrTokensOut = 0
    , llmRawResponses = []
    }

-- | Label a single batch of communities.
labelBatch :: Graph -> CommunityMap -> CohesionMap -> LabelingConfig -> [CommunityId] -> IO (Map CommunityId Text)
labelBatch g commMap cohesion cfg cids = do
  let prompt = labelPrompt g commMap cohesion cids
  result <- callLLM cfg prompt
  case result of
    Left err -> do
      TIO.putStrLn $ "[label] Error in batch: " <> err
      pure Map.empty
    Right response -> do
      let labels = parseLabelsFromResponse response
      if Map.null labels
        then TIO.putStrLn "[label] Warning: no labels parsed from response"
        else TIO.putStrLn $ "[label] Labeled " <> T.pack (show (Map.size labels)) <> " communities"
      pure labels

-- | Push updated community labels to Neo4j.
-- Sends MERGE statements to update the `label` property on Community nodes.
pushLabelsToNeo4j :: Neo4jConfig -> Map CommunityId Text -> IO (Text, Int, Int)
pushLabelsToNeo4j neoCfg labels
  | Map.null labels = pure ("No labels to push", 0, 0)
  | otherwise = catch (do
      let uri = neo4jUri neoCfg
          user = neo4jUser neoCfg
          password = neo4jPassword neoCfg
          stmts = map mkUpdateStmt (Map.toList labels)
          payload = Aeson.encode $ Aeson.object ["statements" Aeson..= stmts]
          payloadPath = "/tmp/graphos-neo4j-labels.json"
      BSL8.writeFile payloadPath payload

      let endpoint = uri ++ "/db/neo4j/tx/commit"
          userPass = user ++ ":" ++ password

      (exitCode, _stdout, stderr) <- readProcessWithExitCode "curl"
        [ "-s", "--max-time", "60"
        , "-X", "POST"
        , "-H", "Content-Type: application/json"
        , "-H", "Accept: application/json"
        , "-u", userPass
        , "--data-binary", "@" ++ payloadPath
        , endpoint
        ]
        ""

      removeFile payloadPath `catch` \(_ :: SomeException) -> pure ()

      case exitCode of
        ExitSuccess -> pure (T.pack $ "Updated " ++ show (Map.size labels) ++ " community labels in Neo4j", Map.size labels, 1)
        ExitFailure code -> pure (T.pack $ "Label push failed (curl exit " ++ show code ++ "): " ++ take 200 stderr, 0, 0)
  ) $ \(e :: SomeException) -> pure (T.pack $ "Label push error: " ++ show e, 0, 0)

-- | Build a parameterized Cypher statement to update a community label.
mkUpdateStmt :: (CommunityId, Text) -> Aeson.Value
mkUpdateStmt (cid, label) = Aeson.object
  [ "statement" Aeson..= ("MATCH (c:Community {id: $community_id}) SET c.label = $new_label, c.llm_labeled = true" :: Text)
  , "parameters" Aeson..= Aeson.object
      [ ("community_id", Aeson.String (T.pack ("community_" ++ show cid)))
      , ("new_label", Aeson.String label)
      ]
  ]