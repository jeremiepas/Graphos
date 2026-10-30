-- | LLM-based community labeling use case.
-- Batches communities, sends to LLM, parses labels, updates Neo4j.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.Label
  ( labelCommunities
  , pushLabelsToNeo4j
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import Control.Exception (catch, SomeException)
import Data.Time.Clock (getCurrentTime)

import Graphos.Infrastructure.Http (runCurl, CurlResult(..), withSecureTempFile)
import Graphos.Domain.Types (CommunityId, CommunityMap, CohesionMap)
import Graphos.Domain.Graph (Graph)
import Graphos.Domain.Config (LabelingConfig(..), Neo4jConfig(..))
import Graphos.Domain.Labeling
  ( LabelingResult(..)
  , labelPrompt
  , batchCommunities
  , LabelCacheEntry(..)
  , LabelCacheFile(..)
  , MatchKind(..)
  , fingerprintOf
  , matchCommunity
  )
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.LLMPort (LLMPort(..))
import Graphos.UseCase.Port.FileSystemPort (fspLoadLabelCache, fspSaveLabelCache)

-- | Label communities using an LLM, consulting and updating the label cache.
--
-- Before any LLM call, consults the persisted label cache (scoped to the
-- current model) via 'matchCommunity': exact + fuzzy hits are served from the
-- cache, and only misses are batched and sent to the LLM. After the pass the
-- cache is rewritten as a last-run snapshot containing exactly the current
-- run's successfully-labeled communities (hit entries carried with their
-- original 'lceLabeledAt', misses with a fresh timestamp). Logs a
-- cost-accounting line so the token savings are visible.
labelCommunities :: AppEnv -> Graph -> CommunityMap -> CohesionMap -> LabelingConfig -> FilePath -> IO LabelingResult
labelCommunities appEnv g commMap cohesion cfg outputDir = do
  let lp = llmPort appEnv
      fsp = fileSystemPort appEnv
      model = T.pack (labelingModel cfg)
      cids = Map.keys commMap
  cached <- fspLoadLabelCache fsp outputDir
  let entries = lcfEntries cached
      -- (community id, member set) pairs in ascending id order.
      memberSets = [ (cid, Set.fromList mems) | (cid, mems) <- Map.toList commMap ]
      decided = [ (cid, matchCommunity entries model members) | (cid, members) <- memberSets ]
      hits = [ (cid, kind, entry) | (cid, Just (entry, kind)) <- decided ]
      misses = [ cid | (cid, Nothing) <- decided ]
      exactHits = length [ () | (_, ExactMatch, _) <- hits ]
      fuzzyHits = length [ () | (_, FuzzyMatch, _) <- hits ]
      decMap = Map.fromList [ (cid, (kind, entry)) | (cid, kind, entry) <- hits ]

  TIO.putStrLn $ "[label] Labeling " <> T.pack (show (length cids))
               <> " communities: " <> T.pack (show exactHits) <> " exact, "
               <> T.pack (show fuzzyHits) <> " fuzzy, " <> T.pack (show (length misses))
               <> " new (LLM)"

  -- Only misses go to the LLM, batched.
  let missBatches = batchCommunities misses (labelingBatchSize cfg)
  freshLabels <- mapM (labelBatch lp g commMap cohesion cfg) missBatches
  let unlabeled = Map.unions freshLabels

  -- Labels map: hits carry the cached label, misses carry the fresh LLM label.
  let mergedLabels = Map.union
                         (Map.fromList [ (cid, lceLabel entry) | (cid, _, entry) <- hits ])
                         unlabeled

  -- Rewrite the cache as a last-run snapshot containing exactly the current
  -- run's communities. Hits carry their original entry verbatim (preserving
  -- lceLabeledAt). Misses that produced a label get a fresh entry; misses that
  -- failed to produce one are omitted so they are retried next run (never a
  -- crash, never a bogus cached label).
  now <- getCurrentTime
  let nowTxt = T.pack (show now)
      freshEntry cid mems =
        case unlabeled Map.!? cid of
          Just lbl -> Just (LabelCacheEntry
                { lceFingerprint = fingerprintOf (Set.fromList mems)
                , lceMembers     = mems
                , lceLabel       = lbl
                , lceModel       = model
                , lceLabeledAt   = nowTxt })
          Nothing -> Nothing
      snapshotEntries =
        [ case Map.lookup cid decMap of
            Just (_, entry) -> Just entry          -- hit: carried verbatim
            Nothing         -> freshEntry cid (Set.toList mems) -- miss: fresh entry from member set
          | (cid, mems) <- memberSets ]
  fspSaveLabelCache fsp outputDir (LabelCacheFile [ e | Just e <- snapshotEntries ])

  pure LabelingResult
    { lrLabels = mergedLabels
    , lrTokensIn = 0
    , lrTokensOut = 0
    , llmRawResponses = []
    , lrExactHits = exactHits
    , lrFuzzyHits = fuzzyHits
    , lrLlmLabeled = length misses
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
      withSecureTempFile "payload" payload $ \payloadPath -> do
        let endpoint = uri ++ "/db/neo4j/tx/commit"
            secretOpts = [("--header", "Content-Type: application/json")
                          , ("--header", "Accept: application/json")
                          , ("-u", user ++ ":" ++ password) ]
            firstArgs = [ "-s", "--max-time", "60", "-X", "POST" ]
            lastArgs = [ "--data-binary", "@" ++ payloadPath, endpoint ]
        res <- runCurl firstArgs secretOpts lastArgs
        case res of
          CurlSuccess _stdout -> pure (T.pack $ "Updated " ++ show (Map.size labels) ++ " community labels in Neo4j", Map.size labels, 1)
          CurlFailed n err -> pure (T.pack $ "Label push failed (curl exit " ++ show n ++ "): " ++ T.unpack (T.take 200 err), 0, 0)
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
