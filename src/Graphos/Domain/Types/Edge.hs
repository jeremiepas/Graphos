{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphos.Domain.Types.Edge
  ( -- * Edge types
    EdgeId(..)
  , Edge(..)
  , Relation(..)
  , relationToText
  , textToRelation
  , Confidence(..)
  ) where

import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), FromJSON(..), ToJSONKey, FromJSONKey, Value, object, (.=), (.:), (.:?), withObject, withText, withScientific)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Graphos.Domain.Types.Node (NodeId)

newtype EdgeId = EdgeId Text
  deriving (Eq, Show, Generic, Ord, ToJSONKey, FromJSONKey)

instance NFData EdgeId

instance ToJSON EdgeId where
  toJSON (EdgeId t) = toJSON t

instance FromJSON EdgeId where
  parseJSON = withText "EdgeId" (pure . EdgeId)

data Relation
  = Calls
  | Imports
  | Extends
  | Implements
  | References
  | Contains
  | DependsOn
  | Inferred
  deriving (Eq, Show, Generic, Ord, Bounded, Enum)

instance NFData Relation

instance ToJSON Relation where
  toJSON = toJSON . relationToText

instance FromJSON Relation where
  parseJSON = withText "Relation" $ \t ->
    case textToRelation t of
      Just r  -> pure r
      Nothing -> fail $ "Unknown relation: " ++ T.unpack t

relationToText :: Relation -> Text
relationToText = \case
  Calls      -> "calls"
  Imports    -> "imports"
  Extends    -> "extends"
  Implements -> "implements"
  References -> "references"
  Contains   -> "contains"
  DependsOn  -> "depends_on"
  Inferred   -> "inferred"

textToRelation :: Text -> Maybe Relation
textToRelation = \case
  "calls"       -> Just Calls
  "imports"     -> Just Imports
  "extends"     -> Just Extends
  "implements"  -> Just Implements
  "references"  -> Just References
  "contains"    -> Just Contains
  "depends_on"  -> Just DependsOn
  "inferred"   -> Just Inferred
  _            -> Nothing

newtype Confidence = Confidence Double
  deriving (Eq, Show, Generic, Ord)

instance NFData Confidence

instance ToJSON Confidence where
  toJSON (Confidence d) = toJSON d

instance FromJSON Confidence where
  parseJSON = withScientific "Confidence" $ \n -> pure (Confidence (realToFrac n))

data Edge = Edge
  { edgeId        :: !EdgeId
  , edgeSource    :: !NodeId
  , edgeTarget    :: !NodeId
  , edgeRelation  :: !Relation
  , edgeWeight    :: !Double
  , edgeConfidence :: !Confidence
  , edgeExtra       :: !(Maybe Value)
  } deriving (Eq, Ord, Show, Generic)

instance NFData Edge

instance ToJSON Edge where
  toJSON e = object
    [ "id"         .= edgeId e
    , "source"     .= edgeSource e
    , "target"     .= edgeTarget e
    , "relation"   .= edgeRelation e
    , "weight"     .= edgeWeight e
    , "confidence" .= edgeConfidence e
    , "extra"      .= edgeExtra e
    ]

instance FromJSON Edge where
  parseJSON = withObject "Edge" $ \v -> Edge
    <$> v .:  "id"
    <*> v .:  "source"
    <*> v .:  "target"
    <*> v .:  "relation"
    <*> v .:  "weight"
    <*> v .:  "confidence"
    <*> v .:? "extra"