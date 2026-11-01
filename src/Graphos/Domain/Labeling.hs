-- | LLM labeling domain types — pure data, no IO.
-- Configuration for community labeling via OpenAI-compatible APIs.
module Graphos.Domain.Labeling
  ( LabelingResult(..)
  , labelPrompt
  , batchCommunities

    -- * Label cache
  , LabelCacheEntry(..)
  , MatchKind(..)
  , fingerprintOf
  , containmentRatio
  , matchCommunity
  ) where

import Data.List (sortOn, partition, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Short (toText)
import Data.Aeson (Value, ToJSON(..), FromJSON(..), eitherDecode, encode, object, withObject, (.=), (.:?))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.ByteString as BS
import Crypto.Hash.SHA256 (hash)
import Numeric (showHex)
import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)

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

-- | A persisted label-cache entry for a single community.
--
-- The cache is a last-run snapshot keyed by model: only entries whose
-- 'lceModel' equals the current labeling model are consulted, so a model
-- change invalidates every entry. 'lceFingerprint' and 'lceMembers' together
-- address the community's member set; 'lceLabeledAt' records when the label
-- was produced (ISO-8601 text).
data LabelCacheEntry = LabelCacheEntry
  { lceFingerprint :: !Text          -- ^ SHA-256 (hex) of sorted member NodeIds
  , lceMembers     :: ![NodeId]      -- ^ The community's member NodeIds
  , lceLabel       :: !Text          -- ^ The LLM-generated label
  , lceModel       :: !Text          -- ^ The labeling model that produced the label
   , lceLabeledAt   :: !UTCTime       -- ^ Timestamp of labeling (UTC).
   } deriving (Eq, Show, Generic)

instance NFData LabelCacheEntry

-- | How a cached entry matched a new community.
data MatchKind
  = ExactMatch   -- ^ Sorted member set equals a cached entry exactly.
  | FuzzyMatch   -- ^ Bidirectional containment >= 0.8 against a cached entry.
  deriving (Eq, Show, Generic)

instance NFData MatchKind

-- | Default labeling timestamp used when the persisted cache omits or corrupts
-- the @labeledAt@ field (cache-tolerant decode). Never itself written by a run.
defaultLabeledAt :: UTCTime
defaultLabeledAt = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

-- | JSON encoding of a cache entry. Key names match the persisted format so the
-- file round-trips through disk unchanged.
instance ToJSON LabelCacheEntry where
  toJSON (LabelCacheEntry fp mem lbl model at) = object
    [ "fingerprint" .= fp
    , "members"     .= mem
    , "label"       .= lbl
    , "model"       .= model
    , "labeledAt"   .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" at
    ]

-- | Cache-tolerant decode: any missing field falls back to a safe default, so a
-- partially written or older cache file never fails to load. The @labeledAt@
-- field is decoded as text and parsed leniently (unparseable → 'defaultLabeledAt').
instance FromJSON LabelCacheEntry where
   parseJSON = withObject "LabelCacheEntry" $ \v -> do
     fp    <- fromMaybe ""       <$> v .:? "fingerprint"
     mem   <- fromMaybe []       <$> v .:? "members"
     lbl   <- fromMaybe ""       <$> v .:? "label"
     model <- fromMaybe ""       <$> v .:? "model"
     at    <- maybe defaultLabeledAt parseOpt <$> v .:? "labeledAt"
     pure (LabelCacheEntry fp mem lbl model at)
    where
       parseOpt t = case parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" (T.unpack t) of
         Just u -> u
         Nothing -> defaultLabeledAt

-- | SHA-256 (hex) of a community's sorted member NodeIds.
--
-- 'Set.toList' returns elements in ascending order, so the members are hashed
-- deterministically regardless of insertion order. A newline delimiter keeps
-- two members from merging across their boundary.
fingerprintOf :: Set NodeId -> Text
fingerprintOf members =
  T.pack (concatMap byteToHex (BS.unpack digest))
  where
    digest = hash (TE.encodeUtf8 (T.intercalate "\n" (Set.toList members))) :: BS.ByteString

-- | Hex-encode a single byte, zero-padding to two digits.
byteToHex :: Word8 -> String
byteToHex w =
  let s = showHex (fromIntegral w :: Integer) ""
  in if length s == 1 then '0' : s else s

-- | Containment ratio @|new ∩ old| / denom@, guarding divide-by-zero.
containmentRatio :: Int -> Int -> Double
containmentRatio _ 0 = 0.0
containmentRatio n d = fromIntegral n / fromIntegral d

-- | Size of the intersection between the new community and a cached entry's
-- members.
intersectionSizeOf :: Set NodeId -> LabelCacheEntry -> Int
intersectionSizeOf newMembers e =
  Set.size (Set.intersection newMembers (Set.fromList (lceMembers e)))

-- | Resolve a cached label for a new community.
--
-- The pipeline consults only entries recorded under the current model. An exact
-- fingerprint match short-circuits and wins over any fuzzy match. Otherwise the
-- best fuzzy candidate is chosen: the entry whose members satisfy the
-- bidirectional containment rule (both ratios >= 0.8) with the largest
-- intersection, tie-broken by the lowest community id.
--
-- 'LabelCacheEntry' carries no community id, so the caller passes entries in
-- ascending community-id order and this function treats the earliest matching
-- entry (lowest list position) as the lowest community id for the final
-- tie-break.
matchCommunity :: [LabelCacheEntry] -> Text -> Set NodeId -> Maybe (LabelCacheEntry, MatchKind)
matchCommunity entries model newMembers =
  let sameModel = filter (\e -> lceModel e == model) entries
      newFp     = fingerprintOf newMembers
  in case lookup newFp [(fingerprintOf (Set.fromList (lceMembers e)), e) | e <- sameModel] of
        Just e -> Just (e, ExactMatch)
        Nothing ->
          let indexed = zip [0 ..] sameModel
              scored  = map toScored indexed
              kept    = filter keep scored
          in case kept of
                [] -> Nothing
                _  -> case sortBy cmp kept of
                        ((_, _, e):_) -> Just (e, FuzzyMatch)
                        [] -> Nothing
          where
            toScored :: (Int, LabelCacheEntry) -> (Int, Int, LabelCacheEntry)
            toScored (position, e) =
              let inter = intersectionSizeOf newMembers e
              in (inter, position, e)

            keep :: (Int, Int, LabelCacheEntry) -> Bool
            keep (inter, _, e) =
              let newSize = Set.size newMembers
                  oldSize = Set.size (Set.fromList (lceMembers e))
              in containmentRatio inter newSize >= 0.8
              && containmentRatio inter oldSize >= 0.8

            cmp :: (Int, Int, LabelCacheEntry) -> (Int, Int, LabelCacheEntry) -> Ordering
            cmp (i1, p1, _) (i2, p2, _)
              | i1 /= i2 = compare i2 i1
              | otherwise = compare p1 p2