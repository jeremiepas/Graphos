-- | Node types for the knowledge graph.
-- Pure data types with no IO dependencies.
--
-- All fields are strict (!) to prevent thunk accumulation.
-- On large codebases (100k+ nodes), lazy fields create massive heap waste
-- (each unevaluated thunk = 16-24 bytes overhead + pointer indirection).
-- Bang patterns force immediate evaluation, reducing memory by 3-4×.
--
{-# LANGUAGE StrictData #-}
module Graphos.Domain.Types.Node
  ( -- * Node types
    NodeId
  , Node(..)
  , FileType(..)

    -- * Extra helpers
  , nodeExtraCapturedAt
  , setNodeExtraCapturedAt
  ) where

import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), (.:), (.:?), (.!=), withObject, withText)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Unique identifier for a node (derived from file + entity name)
-- TODO: Migrate to newtype NodeId = NodeId Text (dedicated migration task after Domain types stablized)
type NodeId = Text

-- | File type classification
data FileType
  = CodeFile
  | DocFile
  | PaperFile
  | ImageFile
  | VideoFile
  | AudioFile
  | OfficeFile
  deriving (Eq, Show, Generic)

instance ToJSON FileType where
  toJSON CodeFile   = "code"
  toJSON DocFile    = "doc"
  toJSON PaperFile  = "paper"
  toJSON ImageFile  = "image"
  toJSON VideoFile  = "video"
  toJSON AudioFile  = "audio"
  toJSON OfficeFile = "office"

instance FromJSON FileType where
  parseJSON = withText "FileType" $ \t -> case t of
    "code"   -> pure CodeFile
    "doc"    -> pure DocFile
    "paper"  -> pure PaperFile
    "image"  -> pure ImageFile
    "video"  -> pure VideoFile
    "audio"  -> pure AudioFile
    "office" -> pure OfficeFile
    _        -> fail $ "Unknown file type: " ++ T.unpack t

-- | A node in the knowledge graph
--

data Node = Node
  { -- Spec-required fields
    nodeId           :: !NodeId
  , nodeLabel        :: !Text
  , nodeFileType     :: !FileType
  , nodeSourceFile   :: !Text
  , nodeLineStart    :: !(Maybe Int)       -- ^ Start line number (spec field)
  , nodeLineEnd     :: !(Maybe Int)
  , nodeSignature    :: !(Maybe Text)
  , nodeCommunityId  :: !(Maybe Int)       -- ^ CommunityId from Leiden (spec field)
  , nodeKind         :: !(Maybe Text)
  , nodeDegree       :: !(Maybe Int)       -- ^ Node degree in graph (spec field)
  , nodeIsBridge     :: !(Maybe Bool)      -- ^ Is articulation point (spec field)
  , nodeExtra        :: !(Maybe Value)     -- ^ Extensible metadata (spec field)
  } deriving (Eq, Show, Generic)

-- | Read the conversation timestamp stored under @capturedAt@ in 'nodeExtra'.
-- Returns 'Nothing' when the key is absent or not a JSON string.
nodeExtraCapturedAt :: Node -> Maybe Text
nodeExtraCapturedAt n =
  case nodeExtra n of
    Just (Object km) ->
      case KM.lookup (Key.fromText "capturedAt") km of
        Just (String t) -> Just t
        _               -> Nothing
    _                -> Nothing

-- | Store a conversation timestamp under @capturedAt@ in 'nodeExtra',
-- merging with any existing JSON object values.
setNodeExtraCapturedAt :: Text -> Node -> Node
setNodeExtraCapturedAt ts n =
  let key = Key.fromText "capturedAt"
      base = case nodeExtra n of
             Just (Object km) -> km
             _                -> KM.empty
  in n { nodeExtra = Just (Object (KM.insert key (String ts) base)) }

instance NFData FileType
instance NFData Node

instance ToJSON Node where
  toJSON n = object
    [ "id"            .= nodeId n
    , "label"         .= nodeLabel n
    , "file_type"     .= nodeFileType n
    , "source_file"   .= nodeSourceFile n
    , "line_start"   .= nodeLineStart n
    , "line_end"     .= nodeLineEnd n
    , "signature"    .= nodeSignature n
    , "community_id" .= nodeCommunityId n
    , "kind"         .= nodeKind n
    , "degree"       .= nodeDegree n
    , "is_bridge"    .= nodeIsBridge n
    , "extra"        .= nodeExtra n
    ]

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v -> Node
    <$> v .:  "id"
    <*> v .:  "label"
    <*> v .:  "file_type"
    <*> v .:? "source_file" .!= ""
    <*> v .:? "line_start"
    <*> v .:? "line_end"
    <*> v .:? "signature"
    <*> v .:? "community_id"
    <*> v .:? "kind"
    <*> v .:? "degree"
    <*> v .:? "is_bridge"
    <*> v .:? "extra"