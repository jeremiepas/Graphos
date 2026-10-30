{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Specs for UseCase.Label.labelCommunities label-cache behavior
-- (multi-source-graphs 4.1/4.2): consult the persisted cache before calling the
-- LLM, serve exact/fuzzy hits verbatim, and rewrite the cache as a last-run
-- snapshot. The LLM is faked so labels always correspond to the batch's cids.
module Graphos.UseCase.LabelCacheSpec where

import Data.IORef
import Control.Exception (SomeException, catch)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Test.Hspec
import qualified System.Directory as Dir
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))

import Graphos.Domain.Types (CommunityId)
import Graphos.Domain.Config (LabelingConfig(..), defaultLabelingConfig)
import Graphos.Domain.Graph (Graph(..))
import Graphos.Domain.Labeling
  ( LabelCacheFile(..)
  , LabelCacheEntry(..)
  , fingerprintOf
  , LabelingResult(lrLabels, lrExactHits, lrFuzzyHits, lrLlmLabeled)
  )
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.LLMPort (LLMPort(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..))
import Graphos.Infrastructure.FileSystem.Cache (loadLabelCache, saveLabelCache)
import Graphos.UseCase.Label (labelCommunities)

-- ───────────────── helpers ─────────────────

type Member = Text

comm :: CommunityId -> [Member] -> (CommunityId, [Member])
comm = (,)

-- | A Graph whose nodes/compositions are empty; 'formatCommunity' and
-- 'labelPrompt' degrade to "unknown"/bare community lines without crashing,
-- which is all the (fake) labeling path needs here.
stubGraph :: Graph
stubGraph =
  Graph
    { gNodes = Map.empty
    , gEdges = Map.empty
    , gAdjFwd = Map.empty
    , gAdjBack = Map.empty
    , gDirected = False
    , gCompositions = Nothing
    , gHash = ""
    , gEmbeddings = Nothing
    , gEmbeddingsPath = Nothing
    }

-- | Build an AppEnv backed by a real FileSystemPort (for the label cache) and a
-- fake LLMPort that never reaches the network.
mkEnv :: LLMPort -> AppEnv
mkEnv llm = AppEnv
  { extractionPort = error "not used"
  , exportPort = error "not used"
  , fileSystemPort = FileSystemPort
      { fspShouldIgnore = \_ _ _ -> False
      , fspLoadCachedExtraction = \_ _ _ -> pure Nothing
      , fspSaveCachedExtraction = \_ _ _ _ -> pure ()
      , fspLoadLabelCache = loadLabelCache
      , fspSaveLabelCache = saveLabelCache
      , fspLoadCheckpoint = \_ -> pure Nothing
      , fspSaveCheckpoint = \_ _ -> pure ()
      , fspClearCheckpoint = \_ -> pure ()
      , fspLoadIgnorePatterns = \_ -> pure []
      }
  , loggingPort = LoggingPort
      { lpLogTrace = const (pure ())
      , lpLogDebug = const (pure ())
      , lpLogInfo = const (pure ())
      , lpLogWarn = const (pure ())
      , lpLogError = const (pure ())
      }
  , observabilityPort = error "not used"
  , llmPort = llm
  }

-- | Fake LLMPort: records submitted batches and answers each with a canned
-- label derived from the community IDs in the prompt. Because the prompt lists
-- exactly the batch's communities as @Community <cid>@ lines, the returned
-- labels always correspond to those cids.
fakeLLM :: IORef [[CommunityId]] -> LLMPort
fakeLLM callsRef = LLMPort
  { lpCallLLM = \_ prompt ->
        let cids = parseCids prompt
        in modifyIORef' callsRef (cids :)
             >> pure (Right (encodeLabels (Map.fromList [ (cid, "Label " <> T.pack (show cid)) | cid <- cids ])))
  , lpParseLabelsFromResponse = decodeLabels
  , lpGenerateEmbedding = \_ _ -> pure (Left "not used")
  , lpGenerateEmbeddings = \_ _ -> pure (Left "not used")
  , lpAnalyzeImage = \_ _ _ -> pure (Left "not used")
  , lpValidateUrl = \_ -> Right ""
  }

-- | Pull the @Community <cid>@ ids out of a labeling prompt.
parseCids :: Text -> [CommunityId]
parseCids txt =
  [ cid
    | ln <- T.lines txt
    , "Community " `T.isPrefixOf` ln
    , let rest = T.words (T.drop (T.length "Community ") ln)
    , (tok : _) <- [rest]
    , Just cid <- [readMaybe (T.unpack tok) :: Maybe CommunityId]
  ]

-- | Encode a labels map as newline-separated @<cid>\t<label>@ rows.
-- | Internal fake-LLM bridge helper; only round-trip fidelity is required, so
-- | we own the format rather than routing through aeson's @Key@ machinery.
encodeLabels :: Map.Map CommunityId Text -> Text
encodeLabels m =
  T.intercalate "\n"
    [ T.pack (show cid) <> "\t" <> label
    | (cid, label) <- Map.toList m
    ]

-- | Decode the row format produced by 'encodeLabels' (round-trips exactly).
decodeLabels :: Text -> Map.Map CommunityId Text
decodeLabels t =
  Map.fromList
    [ (cid, label)
      | line <- T.lines t
      , let parts = T.split (== '\t') line :: [Text]
      , cidTxt:_ <- [parts]
      , let label = T.intercalate "\t" (drop 1 parts)
      , T.all isDigit cidTxt
      , Just cid <- [readMaybe (T.unpack cidTxt)]
    ]

-- | Write a cache to disk through the real port (last-run snapshot form).
writeCache :: AppEnv -> FilePath -> LabelCacheFile -> IO ()
writeCache env out lcf = fspSaveLabelCache (fileSystemPort env) out lcf

-- | Remove a temp output dir, ignoring "already gone" errors.
removeDir :: FilePath -> IO ()
removeDir p = removeDirectoryRecursive p `catch` \(_ :: SomeException) -> pure ()

-- | Locate a cache entry by its member set.
findEntry :: [Member] -> LabelCacheFile -> Maybe LabelCacheEntry
findEntry mems (LabelCacheFile es) = lookup mems [(lceMembers e, e) | e <- es]

-- | Default labeling config keyed to a fixed model so cache entries are scoped.
labelCfg :: LabelingConfig
labelCfg = defaultLabelingConfig
  { labelingModel = "llama3.2"
  , labelingBatchSize = 20
  }

-- ───────────────── spec ─────────────────

-- | Create a temp output dir under the system temp dir.
-- 'System.Directory.createTempDirectory' is unavailable in this GHC, so we
-- build the path by hand. We clear any pre-existing directory first so each
-- test starts from a clean slate regardless of whether a prior test cleaned up.
tempOutDir :: IO FilePath
tempOutDir = do
  base <- Dir.getTemporaryDirectory
  let p = base </> "graphos-labelcache"
  removeDir p
  createDirectoryIfMissing True p
  pure p

spec :: Spec
spec = do
  describe "labelCommunities — label cache (multi-source-graphs 4.1/4.2)" $ do

    it "first run: all communities miss, LLM is called, cache is written" $ do
      out <- tempOutDir
      callsRef <- newIORef []
      let env = mkEnv (fakeLLM callsRef)
          commMap = Map.fromList [ comm 1 ["a", "b"], comm 2 ["c", "d"], comm 3 ["e", "f"] ]
          cohesion = Map.fromList [(1, 0.9), (2, 0.8), (3, 0.7)]
      res <- labelCommunities env stubGraph commMap cohesion labelCfg out
      submitted <- readIORef callsRef
      lrExactHits res `shouldBe` 0
      lrFuzzyHits res `shouldBe` 0
      lrLlmLabeled res `shouldBe` 3
      Map.size (lrLabels res) `shouldBe` 3
      concat submitted `shouldSatisfy` (\cs -> all (`elem` cs) [1, 2, 3])
      cached <- fspLoadLabelCache (fileSystemPort env) out
      length (lcfEntries cached) `shouldBe` 3
      lceLabel (fromJust (findEntry ["a", "b"] cached)) `shouldBe` "Label 1"
      lceModel (fromJust (findEntry ["a", "b"] cached)) `shouldBe` "llama3.2"
      lceFingerprint (fromJust (findEntry ["a", "b"] cached))
        `shouldBe` fingerprintOf (Set.fromList ["a", "b"])
      removeDir out

    it "serves an exact cache hit, skipping the LLM for that community" $ do
      out <- tempOutDir
      callsRef <- newIORef []
      let env = mkEnv (fakeLLM callsRef)
          commMap = Map.fromList [ comm 1 ["a", "b"], comm 2 ["c", "d"], comm 3 ["e", "f"] ]
          cohesion = Map.fromList [(1, 0.9), (2, 0.8), (3, 0.7)]
          preseed = LabelCacheFile
            [ LabelCacheEntry
                { lceFingerprint = fingerprintOf (Set.fromList ["a", "b"])
                , lceMembers = ["a", "b"]
                , lceLabel = "Cached One"
                , lceModel = "llama3.2"
                , lceLabeledAt = "2026-01-01T00:00:00Z"
                }
            ]
      writeCache env out preseed
      res <- labelCommunities env stubGraph commMap cohesion labelCfg out
      submitted <- readIORef callsRef
      lrExactHits res `shouldBe` 1
      lrFuzzyHits res `shouldBe` 0
      lrLlmLabeled res `shouldBe` 2
      (lrLabels res Map.!? 1) `shouldBe` Just "Cached One"
      (lrLabels res Map.!? 2) `shouldBe` Just "Label 2"
      -- LLM was asked only for the misses (2 and 3), never 1
      concat submitted `shouldSatisfy` (\cs -> not (1 `elem` cs) && all (`elem` cs) [2, 3])
      cached <- fspLoadLabelCache (fileSystemPort env) out
      length (lcfEntries cached) `shouldBe` 3
      -- the hit entry kept its original label AND timestamp verbatim
      lceLabel (fromJust (findEntry ["a", "b"] cached)) `shouldBe` "Cached One"
      lceLabeledAt (fromJust (findEntry ["a", "b"] cached)) `shouldBe` "2026-01-01T00:00:00Z"
      removeDir out

    it "serves a fuzzy cache hit (subset) and never calls the LLM" $ do
      out <- tempOutDir
      callsRef <- newIORef []
      let env = mkEnv (fakeLLM callsRef)
          commMap = Map.fromList [ comm 1 ["a", "b", "c", "d"] ]
          cohesion = Map.fromList [(1, 0.9)]
          preseed = LabelCacheFile
            [ LabelCacheEntry
                { lceFingerprint = fingerprintOf (Set.fromList ["a", "b", "c", "d", "e"])
                , lceMembers = ["a", "b", "c", "d", "e"]
                , lceLabel = "Cached Fuzzy"
                , lceModel = "llama3.2"
                , lceLabeledAt = "2026-02-02T00:00:00Z"
                }
            ]
      writeCache env out preseed
      res <- labelCommunities env stubGraph commMap cohesion labelCfg out
      submitted <- readIORef callsRef
      lrExactHits res `shouldBe` 0
      lrFuzzyHits res `shouldBe` 1
      lrLlmLabeled res `shouldBe` 0
      (lrLabels res Map.!? 1) `shouldBe` Just "Cached Fuzzy"
      submitted `shouldBe` ([] :: [[CommunityId]])
      cached <- fspLoadLabelCache (fileSystemPort env) out
      length (lcfEntries cached) `shouldBe` 1
      lceLabeledAt (fromJust (findEntry ["a", "b", "c", "d", "e"] cached))
        `shouldBe` "2026-02-02T00:00:00Z"
      removeDir out

    it "ignores a cache entry recorded under a different model" $ do
      out <- tempOutDir
      callsRef <- newIORef []
      let env = mkEnv (fakeLLM callsRef)
          commMap = Map.fromList [ comm 1 ["a", "b"] ]
          cohesion = Map.fromList [(1, 0.9)]
          preseed = LabelCacheFile
            [ LabelCacheEntry
                { lceFingerprint = fingerprintOf (Set.fromList ["a", "b"])
                , lceMembers = ["a", "b"]
                , lceLabel = "Old model label"
                , lceModel = "gpt-4o-mini"
                , lceLabeledAt = "2026-01-01T00:00:00Z"
                }
            ]
      writeCache env out preseed
      res <- labelCommunities env stubGraph commMap cohesion labelCfg out
      submitted <- readIORef callsRef
      lrExactHits res `shouldBe` 0
      lrLlmLabeled res `shouldBe` 1
      (lrLabels res Map.!? 1) `shouldBe` Just "Label 1"
      concat submitted `shouldSatisfy` (== [1])
      cached <- fspLoadLabelCache (fileSystemPort env) out
      length (lcfEntries cached) `shouldBe` 1
      lceModel (fromJust (findEntry ["a", "b"] cached)) `shouldBe` "llama3.2"
      removeDir out

    it "treats a corrupt cache file as empty (re-labels everything)" $ do
      out <- tempOutDir
      createDirectoryIfMissing True (out </> "cache")
      writeFile (out </> "cache" </> "label-cache.json") "{ this is not json"
      callsRef <- newIORef []
      let env = mkEnv (fakeLLM callsRef)
          commMap = Map.fromList [ comm 1 ["a", "b"] ]
          cohesion = Map.fromList [(1, 0.9)]
      res <- labelCommunities env stubGraph commMap cohesion labelCfg out
      submitted <- readIORef callsRef
      lrExactHits res `shouldBe` 0
      lrLlmLabeled res `shouldBe` 1
      (lrLabels res Map.!? 1) `shouldBe` Just "Label 1"
      submitted `shouldSatisfy` (== [[1]])
      cached <- fspLoadLabelCache (fileSystemPort env) out
      length (lcfEntries cached) `shouldBe` 1
      removeDir out
