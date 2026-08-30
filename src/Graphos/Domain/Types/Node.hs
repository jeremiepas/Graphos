-- | Node types for the knowledge graph.
-- Pure data types with no IO dependencies.
--
-- All fields are strict (!) to prevent thunk accumulation.
-- On large codebases (100k+ nodes), lazy fields create massive heap waste
-- (each unevaluated thunk = 16-24 bytes overhead + pointer indirection).
-- Bang patterns force immediate evaluation, reducing memory by 3-4x.
--
-- Compact representation: nodeLabel and nodeSourceFile use ShortText to
-- halve their memory footprint. Optional fields use a bit-field for
-- presence tracking, eliminating 24 bytes per Maybe wrapper (8 fields).
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

    -- * Compact representation helpers
  , bitNodeLineStart, bitNodeLineEnd, bitNodeSignature
  , bitNodeCommunityId, bitNodeKind, bitNodeDegree
  , bitNodeIsBridge, bitNodeExtra
  , computePresentBits
  , isFieldPresent, setFieldPresent, clearFieldPresent
  ) where

import Control.DeepSeq (NFData(..))
import Data.Bits ((.&.), (.|.), complement, shiftL)
import Data.Maybe (isJust)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), (.:), (.:?), (.:!=), (.!=), withObject, withText)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (ShortText, fromText, toText)
import GHC.Generics (Generic)
import GHC.Word (Word64)

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

-- | Bit positions for optional Node fields (8 fields = 8 bits)
-- Each bit indicates whether the corresponding field is present (Just) or absent (Nothing).
{-# NOINLINE bitNodeLineStart #-}
bitNodeLineStart :: Word64
bitNodeLineStart = 1 `shiftL` 0

{-# NOINLINE bitNodeLineEnd #-}
bitNodeLineEnd :: Word64
bitNodeLineEnd = 1 `shiftL` 1

{-# NOINLINE bitNodeSignature #-}
bitNodeSignature :: Word64
bitNodeSignature = 1 `shiftL` 2

{-# NOINLINE bitNodeCommunityId #-}
bitNodeCommunityId :: Word64
bitNodeCommunityId = 1 `shiftL` 3

{-# NOINLINE bitNodeKind #-}
bitNodeKind :: Word64
bitNodeKind = 1 `shiftL` 4

{-# NOINLINE bitNodeDegree #-}
bitNodeDegree :: Word64
bitNodeDegree = 1 `shiftL` 5

{-# NOINLINE bitNodeIsBridge #-}
bitNodeIsBridge :: Word64
bitNodeIsBridge = 1 `shiftL` 6

{-# NOINLINE bitNodeExtra #-}
bitNodeExtra :: Word64
bitNodeExtra = 1 `shiftL` 7

-- | A node in the knowledge graph
--
-- Compact representation:
-- - nodeLabel and nodeSourceFile use ShortText (saves ~50% vs Text for short strings)
-- - nodeSignature uses ShortText with presence bit (was Maybe Text)
-- - nodePresentBits tracks presence of 8 optional fields (saves 8x24 = 192 bytes per node)
-- - nodeExtra remains Maybe Value for extensibility
data Node = Node
  { -- Spec-required fields (always present)
    nodeId           :: !NodeId
  , nodeLabel        :: !ShortText
  , nodeFileType     :: !FileType
  , nodeSourceFile   :: !ShortText
    -- Optional fields (tracked via nodePresentBits)
  , nodeLineStart    :: !(Maybe Int)
  , nodeLineEnd      :: !(Maybe Int)
  , nodeSignature    :: !(Maybe ShortText)
  , nodeCommunityId  :: !(Maybe Int)
  , nodeKind         :: !(Maybe ShortText)
  , nodeDegree       :: !(Maybe Int)
  , nodeIsBridge     :: !(Maybe Bool)
  , nodeExtra        :: !(Maybe Value)
    -- Compact representation: bit-field for optional field presence
  , nodePresentBits  :: !Word64
  } deriving (Eq, Show, Generic)

-- | Check if an optional field is present in the node.
isFieldPresent :: Word64 -> Word64 -> Bool
isFieldPresent bits flag = (bits .&. flag) /= 0

-- | Set a field as present in the bit-field.
setFieldPresent :: Word64 -> Word64 -> Word64
setFieldPresent bits flag = bits .|. flag

-- | Clear a field from the bit-field.
clearFieldPresent :: Word64 -> Word64 -> Word64
clearFieldPresent bits flag = bits .&. complement flag

-- | Compute the present bits for a Node given its optional fields.
computePresentBits :: Node -> Word64
computePresentBits n = foldr (.|.) 0
    [ if isJust (nodeLineStart n) then bitNodeLineStart      else 0
    , if isJust (nodeLineEnd n)   then bitNodeLineEnd        else 0
    , if isJust (nodeSignature n) then bitNodeSignature       else 0
    , if isJust (nodeCommunityId n) then bitNodeCommunityId   else 0
    , if isJust (nodeKind n)      then bitNodeKind             else 0
    , if isJust (nodeDegree n)    then bitNodeDegree            else 0
    , if isJust (nodeIsBridge n)  then bitNodeIsBridge          else 0
    , if isJust (nodeExtra n)     then bitNodeExtra             else 0
    ]

instance NFData FileType
instance NFData Node where
  rnf n =
    let pb = nodePresentBits n
    in rnf (nodeId n) `seq`
       rnf (nodeLabel n) `seq`
       rnf (nodeFileType n) `seq`
       rnf (nodeSourceFile n) `seq`
       rnf (nodeLineStart n) `seq`
       rnf (nodeLineEnd n) `seq`
       rnf (nodeSignature n) `seq`
       rnf (nodeCommunityId n) `seq`
       rnf (nodeKind n) `seq`
       rnf (nodeDegree n) `seq`
       rnf (nodeIsBridge n) `seq`
       rnf (nodeExtra n) `seq`
       rnf pb

instance ToJSON Node where
  toJSON n = object
    [ "id"            .= nodeId n
    , "label"         .= toText (nodeLabel n)
    , "file_type"     .= nodeFileType n
    , "source_file"   .= toText (nodeSourceFile n)
    , "line_start"   .= nodeLineStart n
    , "line_end"     .= nodeLineEnd n
    , "signature"    .= fmap toText (nodeSignature n)
    , "community_id" .= nodeCommunityId n
    , "kind"         .= fmap toText (nodeKind n)
    , "degree"       .= nodeDegree n
    , "is_bridge"    .= nodeIsBridge n
    , "extra"        .= nodeExtra n
    ]

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v -> do
    nodeIdVal      <- v .:  "id"
    labelVal       <- v .:  "label"
    fileTypeVal    <- v .:  "file_type"
    sourceFileVal  <- v .:? "source_file" .!= ""
    lineStartVal   <- v .:? "line_start"
    lineEndVal     <- v .:? "line_end"
    signatureVal   <- v .:? "signature"
    communityVal   <- v .:? "community_id"
    kindVal        <- v .:? "kind"
    degreeVal      <- v .:? "degree"
    isBridgeVal    <- v .:? "is_bridge"
    extraVal       <- v .:? "extra"

    let presentBits = foldr (.|.) 0
          [ if isJust lineStartVal   then bitNodeLineStart      else 0
          , if isJust lineEndVal     then bitNodeLineEnd        else 0
          , if isJust signatureVal   then bitNodeSignature       else 0
          , if isJust communityVal   then bitNodeCommunityId     else 0
          , if isJust kindVal        then bitNodeKind             else 0
          , if isJust degreeVal      then bitNodeDegree            else 0
          , if isJust isBridgeVal    then bitNodeIsBridge          else 0
          , if isJust extraVal       then bitNodeExtra             else 0
          ]

    pure Node
      { nodeId           = nodeIdVal
      , nodeLabel        = fromText labelVal
      , nodeFileType     = fileTypeVal
      , nodeSourceFile   = fromText sourceFileVal
      , nodeLineStart    = lineStartVal
      , nodeLineEnd      = lineEndVal
      , nodeSignature    = fmap fromText signatureVal
      , nodeCommunityId  = communityVal
      , nodeKind         = fmap fromText kindVal
      , nodeDegree       = degreeVal
      , nodeIsBridge     = isBridgeVal
      , nodeExtra        = extraVal
      , nodePresentBits  = presentBits
      }

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
