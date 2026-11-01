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
import Data.List (sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (removeFile)
import Control.Exception (catch, SomeException)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Data.Time.Clock (getCurrentTime)

import Graphos.Domain.Types (CommunityId, CommunityMap, CohesionMap)
import Graphos.Domain.Graph (Graph)
import Graphos.Domain.Config (LabelingConfig(..), Neo4jConfig(..))
import Graphos.Domain.Labeling
  ( LabelingResult(..)
  , labelPrompt, batchCommunities
  , matchCommunity, fingerprintOf
  , LabelCacheEntry(..)
  , MatchKind(..)
  )
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.LLMPort (LLMPort(..))
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..))
import Graphos.UseCase.Port.LoggingPort (lpLogInfo, lpLogWarn)

-- | Label communities using an LLM, consulting the persisted label cache first.
--
-- Communities whose member set already has a cached label under the current
-- model are reused without an LLM call; only misses are batched and sent.
-- Exact and fuzzy hits are counted separately for the cost-accounting line.
--
-- On completion the cache is REWRITTEN as a last-run snapshot: it holds
-- exactly this run's communities — hit entries carried with their ORIGINAL
-- @labeledAt@, misses stamped fresh — and entries for communities not in this
-- run are discarded.
labelCommunities :: AppEnv -> Graph -> CommunityMap -> CohesionMap -> LabelingConfig -> IO LabelingResult
labelCommunities appEnv g commMap cohesion cfg = do
  let lp         = llmPort appEnv
      fs         = fileSystemPort appEnv
      outDir     = appEnvOutputDir appEnv
      model      = T.pack (labelingModel cfg)
      cids       = Map.keys commMap
      batchSize  = labelingBatchSize cfg
      memberList = \cid -> Map.findWithDefault [] cid commMap
      membersOf  = \cid -> Set.fromList (memberList cid)
      lpWarn     = lpLogWarn (loggingPort appEnv)
      lpInfo     = lpLogInfo (loggingPort appEnv)

  loadResult <- fspLoadLabelCache fs outDir
  cached <- case loadResult of
    Right entries -> pure entries
    Left warnMsg  -> do
      lpWarn warnMsg
      pure []

  let candidates = filter (\e -> lceModel e == model) cached
      resolved   = map (\cid -> (cid, matchCommunity candidates model (membersOf cid))) cids
      hits       = [(cid, e, k) | (cid, Just (e, k)) <- resolved]
      missCids   = [cid | (cid, Nothing) <- resolved]
      nExact     = length [() | (_, _, ExactMatch) <- hits]
      nFuzzy     = length [() | (_, _, FuzzyMatch) <- hits]
      hitLabels  = Map.fromList [(cid, lceLabel e) | (cid, e, _) <- hits]

  TIO.putStrLn $ "[label] Labeling " <> T.pack (show (length cids)) <> " communities: "
               <> T.pack (show (nExact + nFuzzy)) <> " cache hits, " <> T.pack (show (length missCids)) <> " misses"

  missLabels <- if null missCids
    then pure Map.empty
    else do
      results <- mapM (\b -> labelBatch lp g commMap cohesion cfg b)
                 (batchCommunities (sort missCids) batchSize)
      pure (Map.unions results)

  now <- getCurrentTime
  let hitSnap  = [ LabelCacheEntry (fingerprintOf (membersOf cid)) (memberList cid)
                   (lceLabel e) model (lceLabeledAt e)
                 | (cid, e, _) <- hits ]
      missSnap = [ LabelCacheEntry (fingerprintOf (membersOf cid)) (memberList cid)
                   lbl model now
                 | (cid, lbl) <- Map.toList (Map.restrictKeys missLabels (Set.fromList missCids)) ]
  fspSaveLabelCache fs outDir (hitSnap ++ missSnap)
  lpInfo $ "[label] cache: " <> T.pack (show nExact) <> " exact hits, "
        <> T.pack (show nFuzzy) <> " fuzzy hits, "
        <> T.pack (show (length missCids)) <> " LLM-labeled"

  let mergedLabels = Map.union hitLabels missLabels
  pure LabelingResult
    { lrLabels        = mergedLabels
    , lrTokensIn      = 0
    , lrTokensOut     = 0
    , llmRawResponses = []
    }

-- | Label a single batch of communities.
labelBatch :: LLMPort -> Graph -> CommunityMap -> CohesionMap -> LabelingConfig -> [CommunityId] -> IO (Map CommunityId Text)
labelBatch lp g commMap cohesion cfg cids = do
  let prompt = labelPrompt g commMap cohesion cids
  result <- lpCallLLM lp cfg prompt
  case result of
    Left err -> do
      TIO.putStrLn $ "[label] Error in batch: " <> err
      pure Map.empty
    Right response -> do
      let labels = lpParseLabelsFromResponse lp response
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
