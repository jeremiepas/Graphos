-- | Node types for the knowledge graph.
-- Pure data types with no IO dependencies.
--
-- All fields are strict (!) to prevent thunk accumulation.
-- On large codebases (100k+ nodes), lazy fields create massive heap waste
-- (each unevaluated thunk = 16-24 bytes overhead + pointer indirection).
-- Bang patterns force immediate evaluation, reducing memory by 3-4×.
--
-- Migration note: Fields nodeSourceLocation, nodeSourceUrl, nodeCapturedAt,
-- nodeAuthor, nodeContributor are LEGACY and will be removed once all
-- referencing modules are updated to use nodeLineStart, nodeCommunityId,
-- nodeDegree, nodeIsBridge, nodeExtra instead.
{-# LANGUAGE StrictData #-}
module Graphos.Domain.Types.Node
  ( -- * Node types
    NodeId
  , Node(..)
  , FileType(..)
  ) where

import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), FromJSON(..), Value, object, (.=), (.:), (.:?), withObject, withText)
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
-- Fields are organized: spec-required first, legacy last.
-- Legacy fields (nodeSourceLocation..nodeContributor) will be removed
-- once all modules are migrated to the new field names.
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
    -- Legacy fields (to be removed after migration)
  , nodeSourceLocation :: !(Maybe Text)    -- ^ LEGACY: use nodeLineStart
  , nodeSourceUrl      :: !(Maybe Text)    -- ^ LEGACY: to be removed
  , nodeCapturedAt     :: !(Maybe Text)    -- ^ LEGACY: use nodeExtra
  , nodeAuthor         :: !(Maybe Text)    -- ^ LEGACY: to be removed
  , nodeContributor    :: !(Maybe Text)    -- ^ LEGACY: to be removed
  } deriving (Eq, Show, Generic)

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
    , "source_location" .= nodeSourceLocation n  -- legacy
    , "source_url"     .= nodeSourceUrl n        -- legacy
    , "captured_at"    .= nodeCapturedAt n       -- legacy
    , "author"         .= nodeAuthor n           -- legacy
    , "contributor"    .= nodeContributor n       -- legacy
    ]

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v -> Node
    <$> v .:  "id"
    <*> v .:  "label"
    <*> v .:  "file_type"
    <*> v .:  "source_file"
    <*> v .:? "line_start"
    <*> v .:? "line_end"
    <*> v .:? "signature"
    <*> v .:? "community_id"
    <*> v .:? "kind"
    <*> v .:? "degree"
    <*> v .:? "is_bridge"
    <*> v .:? "extra"
    <*> v .:? "source_location"   -- legacy
    <*> v .:? "source_url"         -- legacy
    <*> v .:? "captured_at"        -- legacy
    <*> v .:? "author"             -- legacy
    <*> v .:? "contributor"        -- legacy