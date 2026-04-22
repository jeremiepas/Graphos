-- | Analysis result types for the knowledge graph.
-- Pure data types with no IO dependencies.
module Graphos.Domain.Types.Analysis
  ( -- * Analysis types
    Analysis(..)
  , GodNode(..)
  , SurprisingConnection(..)
  , SuggestedQuestion(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)

import Graphos.Domain.Types.Edge (Confidence)
import Graphos.Domain.Types.Graph (CommunityMap, CohesionMap)
import Graphos.Domain.Types.Node (NodeId)

-- | Analysis results
data Analysis = Analysis
  { analysisCommunities :: CommunityMap
  , analysisCohesion     :: CohesionMap
  , analysisGodNodes     :: [GodNode]
  , analysisSurprises    :: [SurprisingConnection]
  , analysisQuestions    :: [SuggestedQuestion]
  } deriving (Eq, Show)

-- | A god node (high-degree hub)
data GodNode = GodNode
  { gnId    :: NodeId
  , gnLabel :: Text
  , gnEdges :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON GodNode where
  toJSON g = object
    [ "id"     .= gnId g
    , "label"  .= gnLabel g
    , "edges"  .= gnEdges g
    ]

instance FromJSON GodNode where
  parseJSON = withObject "GodNode" $ \v -> GodNode
    <$> v .: "id"
    <*> v .: "label"
    <*> v .: "edges"

-- | A surprising cross-community connection
data SurprisingConnection = SurprisingConnection
  { scSource      :: Text
  , scTarget      :: Text
  , scSourceFiles :: [Text]
  , scConfidence  :: Confidence
  , scRelation    :: Text
  , scWhy         :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON SurprisingConnection where
  toJSON s = object
    [ "source"        .= scSource s
    , "target"        .= scTarget s
    , "source_files"  .= scSourceFiles s
    , "confidence"    .= scConfidence s
    , "relation"      .= scRelation s
    , "why"           .= scWhy s
    ]

-- | A suggested question from graph analysis
data SuggestedQuestion = SuggestedQuestion
  { sqType     :: Text
  , sqQuestion :: Maybe Text
  , sqWhy      :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON SuggestedQuestion where
  toJSON q = object
    [ "type"     .= sqType q
    , "question" .= sqQuestion q
    , "why"      .= sqWhy q
    ]