-- | Edge types for the knowledge graph.
-- Pure data types with no IO dependencies.
{-# LANGUAGE LambdaCase #-}
module Graphos.Domain.Types.Edge
  ( -- * Edge types
    EdgeId
  , Edge(..)
  , Relation(..)
  , relationToText
  , textToRelation
  , Confidence(..)
  , confidenceScore
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), (.:?), withObject, withText)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Graphos.Domain.Types.Node (NodeId)

-- | Unique identifier for an edge
type EdgeId = Text

-- | Relation types for edges
data Relation
  = Calls
  | Implements
  | References
  | Cites
  | ConceptuallyRelatedTo
  | SharesDataWith
  | SemanticallySimilarTo
  | RationaleFor
  | Imports
  | ImportsFrom
  | Contains
  | Method
  | Extends
  | Overrides
  | DependsOn
  deriving (Eq, Show, Generic, Ord)

instance ToJSON Relation where
  toJSON = toJSON . relationToText

instance FromJSON Relation where
  parseJSON = withText "Relation" $ \t ->
    case textToRelation t of
      Just r  -> pure r
      Nothing -> fail $ "Unknown relation: " ++ T.unpack t

-- | Convert a relation to its text representation
relationToText :: Relation -> Text
relationToText = \case
  Calls                    -> "calls"
  Implements                -> "implements"
  References                -> "references"
  Cites                    -> "cites"
  ConceptuallyRelatedTo    -> "conceptually_related_to"
  SharesDataWith           -> "shares_data_with"
  SemanticallySimilarTo    -> "semantically_similar_to"
  RationaleFor             -> "rationale_for"
  Imports                  -> "imports"
  ImportsFrom              -> "imports_from"
  Contains                  -> "contains"
  Method                   -> "method"
  Extends                  -> "extends"
  Overrides                -> "overrides"
  DependsOn                -> "depends_on"

-- | Parse a relation from its text representation
textToRelation :: Text -> Maybe Relation
textToRelation = \case
  "calls"                    -> Just Calls
  "implements"               -> Just Implements
  "references"               -> Just References
  "cites"                    -> Just Cites
  "conceptually_related_to"  -> Just ConceptuallyRelatedTo
  "shares_data_with"         -> Just SharesDataWith
  "semantically_similar_to"  -> Just SemanticallySimilarTo
  "rationale_for"            -> Just RationaleFor
  "imports"                  -> Just Imports
  "imports_from"             -> Just ImportsFrom
  "contains"                 -> Just Contains
  "method"                   -> Just Method
  "extends"                  -> Just Extends
  "overrides"                -> Just Overrides
  "depends_on"               -> Just DependsOn
  _                          -> Nothing

-- | Confidence level for an edge
data Confidence = Extracted | Inferred | Ambiguous
  deriving (Eq, Show, Generic, Ord)

instance ToJSON Confidence where
  toJSON Extracted = "EXTRACTED"
  toJSON Inferred  = "INFERRED"
  toJSON Ambiguous = "AMBIGUOUS"

instance FromJSON Confidence where
  parseJSON = withText "Confidence" $ \case
    "EXTRACTED" -> pure Extracted
    "INFERRED"  -> pure Inferred
    "AMBIGUOUS" -> pure Ambiguous
    t           -> fail $ "Unknown confidence: " ++ T.unpack t

-- | Convert confidence to a numeric score
confidenceScore :: Confidence -> Double
confidenceScore Extracted = 1.0
confidenceScore Inferred  = 0.7
confidenceScore Ambiguous  = 0.2

-- | An edge in the knowledge graph
data Edge = Edge
  { edgeSource        :: NodeId
  , edgeTarget        :: NodeId
  , edgeRelation      :: Relation
  , edgeConfidence    :: Confidence
  , edgeConfidenceScore :: Double
  , edgeSourceFile    :: Text
  , edgeSourceLocation :: Maybe Text
  , edgeWeight        :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON Edge where
  toJSON e = object
    [ "source"           .= edgeSource e
    , "target"           .= edgeTarget e
    , "relation"         .= edgeRelation e
    , "confidence"       .= edgeConfidence e
    , "confidence_score" .= edgeConfidenceScore e
    , "source_file"      .= edgeSourceFile e
    , "source_location"  .= edgeSourceLocation e
    , "weight"           .= edgeWeight e
    ]

instance FromJSON Edge where
  parseJSON = withObject "Edge" $ \v -> Edge
    <$> v .: "source"
    <*> v .: "target"
    <*> v .: "relation"
    <*> v .: "confidence"
    <*> v .: "confidence_score"
    <*> v .: "source_file"
    <*> v .:? "source_location"
    <*> v .: "weight"