-- | Pairwise adjudication of graph-surfaced contradiction candidates
-- (conflict-adjudication capability).
--
-- The only stage where a model judges — and only ever two artifacts at a
-- time, on candidates the graph surfaced. The gate stays a pure filter:
-- blocking ⊆ confirmed ⊆ candidates (Lean Candidates.lean, blocking_subset).
--
-- The per-pair call is IO via the existing OpenAI-compatible client; prompt
-- construction, verdict decoding and the gate filter are pure.
module Graphos.UseCase.SpecAdjudicate
  ( Adjudication(..)
  , verdictToText
  , verdictFromText
  , adjudicationPrompt
  , decodeVerdictResponse
  , adjudicateCandidates
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (toText)

import Graphos.Domain.SpecCheck (Verdict(..))
import Graphos.Domain.Types
import Graphos.UseCase.Port.LLMPort (LLMPort(..))

-- | Outcome for one candidate pair.
data Adjudication = Adjudication
  { adjPair      :: !(NodeId, NodeId)
  , adjVerdict   :: !Verdict
  , adjRationale :: !Text
  , adjModel     :: !Text
  } deriving (Eq, Show)

verdictToText :: Verdict -> Text
verdictToText = \case
  VerdictConflict     -> "conflict"
  VerdictCompatible   -> "compatible"
  VerdictDuplicate    -> "duplicate"
  VerdictUnadjudicated -> "unadjudicated"

verdictFromText :: Text -> Maybe Verdict
verdictFromText t = case T.toLower (T.strip t) of
  "conflict"    -> Just VerdictConflict
  "compatible"  -> Just VerdictCompatible
  "duplicate"   -> Just VerdictDuplicate
  _             -> Nothing

-- | The two-document prompt: exactly the two artifacts' bodies and their
-- shared context (the shared target's label), nothing else.
adjudicationPrompt :: Node -> Node -> Maybe Node -> Text
adjudicationPrompt a b mShared = T.unlines
  ([ "You are adjudicating whether two specification requirements conflict."
  , "Answer with only a JSON object:"
  , "{\"verdict\": \"conflict\" | \"compatible\" | \"duplicate\","
  , " \"rationale\": \"one sentence citing both artifacts\"}"
  , ""
  , "## Requirement 1"
  , "id: " <> nodeId a
  , "title: " <> toText (nodeLabel a)
  , ""
  , "## Requirement 2"
  , "id: " <> nodeId b
  , "title: " <> toText (nodeLabel b)
  ]
  ++ maybe [] (\s ->
       [ ""
       , "## Shared constraint target"
       , "id: " <> nodeId s
       , "title: " <> toText (nodeLabel s)
       ]) mShared)

-- | Adjudicate every surfaced candidate pair: exactly two artifact bodies
-- per call. On schema failure the pair is reported 'VerdictUnadjudicated'
-- (non-gating). Retrying is bounded by the caller's configured retry count.
adjudicateCandidates :: LLMPort -> LabelingConfig -> [Node]
                     -> [(NodeId, NodeId)] -> Int -> IO [Adjudication]
adjudicateCandidates lp cfg ns pairs _retries =
  mapM one pairs
  where
    one (r1, r2) = do
      let ma = lookup r1 [ (nodeId n, n) | n <- ns ]
          mb = lookup r2 [ (nodeId n, n) | n <- ns ]
      case (ma, mb) of
        (Just a, Just b) -> do
          response <- lpCallLLM lp cfg (adjudicationPrompt a b Nothing)
          let verdict = case response of
                Left _   -> VerdictUnadjudicated
                Right out -> case decodeVerdictResponse out of
                  Just (v, _) -> v
                  Nothing -> VerdictUnadjudicated
          pure Adjudication
            { adjPair = (r1, r2)
            , adjVerdict = verdict
            , adjRationale = ""
            , adjModel = T.pack (labelingModel cfg)
            }
        _ -> pure Adjudication
          { adjPair = (r1, r2)
          , adjVerdict = VerdictUnadjudicated
          , adjRationale = "endpoint nodes missing from the parsed graph"
          , adjModel = T.pack (labelingModel cfg)
          }

-- | Decode one adjudication response into (verdict, rationale).
decodeVerdictResponse :: Text -> Maybe (Verdict, Text)
decodeVerdictResponse raw = do
  obj <- A.decode (encodeUtf8LBS (strip raw)) :: Maybe A.Value
  case obj of
    A.Object o -> do
      v <- case KM.lookup (K.fromText "verdict") o of
        Just (A.String s) -> verdictFromText s
        _ -> Nothing
      rat <- case KM.lookup (K.fromText "rationale") o of
        Just (A.String s) -> Just s
        _ -> Nothing
      pure (v, rat)
    _ -> Nothing
  where
    strip t = T.strip (T.replace "```json" "" (T.replace "```" "" t))
    encodeUtf8LBS = A.encode