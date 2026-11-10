{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Per-document semantic edge extraction for spec artifacts
-- (spec-artifact-schema capability, requirement "Hybrid extraction").
--
-- Trust discipline: the model may only emit edges between ids the
-- deterministic parser already produced. One document per call, plus the
-- titles of the artifacts it may reference. Emissions naming unknown ids are
-- dropped and counted, never invented into nodes.
--
-- The prompt construction and hypothesis application are pure; only the
-- single 'lpCallLLM' call per document is IO, riding the existing
-- OpenAI-compatible client.
module Graphos.UseCase.SpecExtract
  ( SpecEdgeHypothesis(..)
  , semanticRels
  , SpecExtractCall(..)
  , specEdgeCallPrompt
  , decodeSpecEdgeHypotheses
  , applyHypotheses
  , extractEdgesForDoc
  , extractSemanticEdges
  , ExtractionReport(..)
  ) where

import Data.List (nub)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Graphos.Domain.Types
import Graphos.UseCase.Port.LLMPort (LLMPort(..))

-- | Relations the semantic pass may emit (schema-restricted output).
semanticRels :: [Relation]
semanticRels = [Refines, Satisfies, Constrains, ConflictsWith, DependsOn]

-- | One model hypothesis: source id, relation, target id.
data SpecEdgeHypothesis = SpecEdgeHypothesis
  { sehSource   :: !Text
  , sehRelation :: !Relation
  , sehTarget   :: !Text
  } deriving (Eq, Show)

-- | Per-document extraction report: accepted edges and dropped hypotheses.
data ExtractionReport = ExtractionReport
  { erDocId    :: !(Maybe NodeId)
  , erEdges    :: ![Edge]
  , erDropped  :: !Int -- ^ hypotheses naming unknown ids or bad relations
  , erAccepted :: !Int
  } deriving (Eq, Show)

instance A.ToJSON ExtractionReport where
  toJSON r = A.object
    [ "doc"      A..= erDocId r
    , "accepted" A..= erAccepted r
    , "dropped"  A..= erDropped r
    ]

-- | One per-document call: exactly one document body goes in, plus titles of
-- already-parsed artifacts the model may connect to.
data SpecExtractCall = SpecExtractCall
  { scDocId    :: !Text             -- ^ id of the artifact node for this doc
  , scDocTitle :: !Text
  , scDocBody  :: !Text
  , scKnownIds :: ![(Text, Text)]   -- ^ (id, title) of other artifacts
  } deriving (Eq, Show)

-- | The prompt: exactly one document body plus reference titles
-- (spec scenario "One document per call").
specEdgeCallPrompt :: SpecExtractCall -> Text
specEdgeCallPrompt c = T.unlines
  [ "You are extracting semantic relationships between specification artifacts."
  , "Below is ONE document. You may connect it only to the listed artifact ids."
  , "Emit edges from this document's id to other listed ids only."
  , "Relations: refines | satisfies | constrains | conflicts_with | depends_on."
  , "Respond with only a JSON array of {\"source\",\"relation\",\"target\"} objects."
  , ""
  , "## This document"
  , "id: " <> scDocId c
  , "title: " <> scDocTitle c
  , ""
  , scDocBody c
  , ""
  , "## Other artifacts you may reference (id — title)"
  , T.unlines [ i <> " — " <> t | (i, t) <- scKnownIds c ]
  ]

-- | Parse the model's response into hypotheses. Unparseable output → [].
-- Malformed entries inside a parseable array are skipped. Relations not in
-- the allowed vocabulary are dropped by 'applyHypotheses'.
decodeSpecEdgeHypotheses :: Text -> [SpecEdgeHypothesis]
decodeSpecEdgeHypotheses raw =
  case A.decode (BSL.fromStrict (TE.encodeUtf8 (stripFences raw))) of
    Just (items :: [A.Value]) -> catMaybes
      [ case item of
          A.Object o -> do
            s <- lookupText "source" o
            t <- lookupText "target" o
            rel <- case KM.lookup (K.fromText "relation") o of
              Just (A.String k) -> textToRelation k
              _                 -> Nothing
            pure (SpecEdgeHypothesis s rel t)
          _ -> Nothing
      | item <- items ]
    _ -> []
  where
    lookupText k o = case KM.lookup (K.fromText k) o of
      Just (A.String s) -> Just s
      _ -> Nothing
    stripFences t = T.strip (T.replace "```" "" (T.replace "```json" "" t))

-- | Drop hypotheses whose endpoints are unknown or ill-formed, count them.
-- Pure — the deterministic half of the unknown-id drop rule.
applyHypotheses :: [NodeId] -> NodeId -> [SpecEdgeHypothesis] -> (Int, [Edge])
applyHypotheses knownIds docId hs =
  let valid =
        [ h | h <- hs
            , sehRelation h `elem` semanticRels
            , sehSource h `elem` knownIds
            , sehTarget h `elem` knownIds
            , sehSource h == docId || sehTarget h == docId
            ]
      accepted = nub valid
      edges =
        [ mkSemanticEdge (sehSource h) (sehRelation h) (sehTarget h)
        | h <- accepted ]
      dropped = length hs - length accepted
  in (dropped, edges)

mkSemanticEdge :: NodeId -> Relation -> NodeId -> Edge
mkSemanticEdge src rel tgt = Edge
  { edgeId = EdgeId (src <> "|" <> relationToText rel <> "|" <> tgt)
  , edgeSource = src
  , edgeTarget = tgt
  , edgeRelation = rel
  , edgeWeight = 1.0
  , edgeConfidence = Confidence 0.8
  , edgeExtra = Nothing
  }

-- | One structured-output call for one document. The prompt contains exactly
-- one document body (plus reference titles). A failed call is not an error:
-- it contributes zero edges (structure survives without the model).
extractEdgesForDoc :: LLMPort -> LabelingConfig -> [NodeId] -> SpecExtractCall
                   -> IO (Int, [Edge])
extractEdgesForDoc lp cfg knownIds c = do
  response <- lpCallLLM lp cfg (specEdgeCallPrompt c)
  let hs = case response of
        Left _    -> []
        Right out -> decodeSpecEdgeHypotheses out
  pure (applyHypotheses knownIds (scDocId c) hs)

-- | Run the per-document semantic pass over a corpus: one call per document,
-- each prompt holding exactly that document. Dropped hypotheses are reported
-- per document.
extractSemanticEdges :: LLMPort -> LabelingConfig -> [Node] -> [SpecExtractCall]
                     -> IO ([Edge], [ExtractionReport])
extractSemanticEdges lp cfg ns calls = do
  let knownIds = [ nodeId n | n <- ns ]
  results <- mapM (extractEdgesForDoc lp cfg knownIds) calls
  let reports =
        [ ExtractionReport { erDocId = Just (scDocId c)
                           , erEdges = es
                           , erDropped = drp
                           , erAccepted = length es
                           }
        | (c, (drp, es)) <- zip calls results
        ]
  pure (concat [ es | (_, es) <- results ], reports)