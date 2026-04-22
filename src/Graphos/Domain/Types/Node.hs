-- | Node types for the knowledge graph.
-- Pure data types with no IO dependencies.
{-# LANGUAGE LambdaCase #-}
module Graphos.Domain.Types.Node
  ( -- * Node types
    NodeId
  , Node(..)
  , FileType(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), (.:?), withObject, withText)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Unique identifier for a node (derived from file + entity name)
type NodeId = Text

-- | File type classification
data FileType
  = CodeFile
  | DocumentFile
  | PaperFile
  | ImageFile
  | VideoFile
  deriving (Eq, Show, Generic)

instance ToJSON FileType where
  toJSON CodeFile     = "code"
  toJSON DocumentFile = "document"
  toJSON PaperFile    = "paper"
  toJSON ImageFile    = "image"
  toJSON VideoFile    = "video"

instance FromJSON FileType where
  parseJSON = withText "FileType" $ \t -> case t of
    "code"     -> pure CodeFile
    "document" -> pure DocumentFile
    "paper"    -> pure PaperFile
    "image"    -> pure ImageFile
    "video"    -> pure VideoFile
    _          -> fail $ "Unknown file type: " ++ T.unpack t

-- | A node in the knowledge graph
data Node = Node
  { nodeId           :: NodeId
  , nodeLabel        :: Text
  , nodeFileType     :: FileType
  , nodeSourceFile   :: Text
  , nodeSourceLocation :: Maybe Text
  , nodeLineEnd      :: Maybe Int        -- ^ End line number (for exact code range)
  , nodeKind         :: Maybe Text       -- ^ Symbol kind: "Function", "Class", "Method", "Interface", etc.
  , nodeSignature    :: Maybe Text       -- ^ Type signature or declaration header
  , nodeSourceUrl    :: Maybe Text
  , nodeCapturedAt   :: Maybe Text
  , nodeAuthor       :: Maybe Text
  , nodeContributor  :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON Node where
  toJSON n = object
    [ "id"              .= nodeId n
    , "label"           .= nodeLabel n
    , "file_type"       .= nodeFileType n
    , "source_file"     .= nodeSourceFile n
    , "source_location" .= nodeSourceLocation n
    , "line_end"        .= nodeLineEnd n
    , "kind"            .= nodeKind n
    , "signature"       .= nodeSignature n
    , "source_url"      .= nodeSourceUrl n
    , "captured_at"     .= nodeCapturedAt n
    , "author"          .= nodeAuthor n
    , "contributor"     .= nodeContributor n
    ]

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v -> Node
    <$> v .: "id"
    <*> v .: "label"
    <*> v .: "file_type"
    <*> v .: "source_file"
    <*> v .:? "source_location"
    <*> v .:? "line_end"
    <*> v .:? "kind"
    <*> v .:? "signature"
    <*> v .:? "source_url"
    <*> v .:? "captured_at"
    <*> v .:? "author"
    <*> v .:? "contributor"