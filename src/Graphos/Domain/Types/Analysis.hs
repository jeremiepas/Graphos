-- | Analysis result types for the knowledge graph.
-- Pure data types with no IO dependencies.
module Graphos.Domain.Types.Analysis
  ( -- * Analysis types
    Analysis(..)
  , ClusterQuality(..)
  , ClusterEvalRow(..)
  , ClusterEvalReport(..)
  , BunchSkip(..)
  , BunchRunInfo(..)
  , GodNode(..)
  , SurprisingConnection(..)
  , SuggestedQuestion(..)
    -- * Community aggregate types
  , CommunityAggregate(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)

import Graphos.Domain.Types.Edge (Confidence)
import Graphos.Domain.Types.Graph (CommunityMap, CohesionMap, NullModel(..))
import Graphos.Domain.Types.Node (NodeId)

-- | Analysis results.
--
-- Connectivity artifacts (articulation points, biconnected-component count)
-- are computed once per run from a shared FGL conversion and carried here so
-- report generation and exports consume them instead of recomputing
-- (bounded-report-export).
data Analysis = Analysis
  { analysisCommunities   :: !CommunityMap
  , analysisNullModel     :: !NullModel
  , analysisCohesion      :: !CohesionMap
  , analysisGodNodes      :: ![GodNode]
  , analysisSurprises    :: ![SurprisingConnection]
  , analysisQuestions    :: ![SuggestedQuestion]
  , analysisArticulation :: ![NodeId]  -- ^ Articulation (bridge) nodes of the analyzed graph
  , analysisBccCount     :: !Int       -- ^ Number of biconnected components
  , analysisPartitionQuality :: !(Maybe ClusterQuality)
    -- ^ Quality of 'analysisCommunities' and the algorithm that produced it
    -- (bunch-search-based-modularization); 'Nothing' when not clustered
  , analysisClusterEval :: !(Maybe ClusterEvalReport)
    -- ^ Ranking of the configuration sweep when @--cluster-algorithm auto@
    -- ran (clustering-selection); 'Nothing' for every other algorithm
  } deriving (Eq, Show, Generic)

instance NFData Analysis

-- | Why Bunch did not run: the MDG's file count against the cap.
data BunchSkip = BunchSkip
  { skipFiles :: !Int
  , skipCap   :: !Int
  } deriving (Eq, Show, Generic)

instance NFData BunchSkip

-- | How a Bunch partition was obtained (bunch-real-projects D3): the file
-- count of each search run (one entry when the MDG was not split) and the
-- global refinement that followed.
data BunchRunInfo = BunchRunInfo
  { briRuns   :: ![Int]  -- ^ files per search run, in run order
  , briMoves  :: !Int    -- ^ refinement moves applied
  , briPasses :: !Int    -- ^ refinement passes run
  } deriving (Eq, Show, Generic)

instance NFData BunchRunInfo

-- | Quality of the cluster stage's final partition, algorithm-independent.
data ClusterQuality = ClusterQuality
  { cqAlgorithm     :: !Text             -- ^ @"leiden"@ or @"bunch"@
  , cqMQ            :: !Double           -- ^ modularization quality (unit weights)
  , cqModularity    :: !Double           -- ^ global modularity Q
  , cqIntraFraction :: !Double           -- ^ intra-edge fraction of the partition
  , cqClusters      :: !Int              -- ^ cluster count
  , cqSkip          :: !(Maybe BunchSkip) -- ^ Bunch skipped above the file cap
  , cqBunchRuns     :: !(Maybe BunchRunInfo) -- ^ Bunch runs and refinement; 'Nothing' for Leiden
  , cqScoredRelations :: !(Maybe [Text])    -- ^ relations MQ / modularity / intra fraction were scored over;
                                            --   'Nothing' = every edge of the graph (Leiden)
  } deriving (Eq, Show, Generic)

instance NFData ClusterQuality

-- | One row of the configuration sweep ranking (clustering-selection): the
-- configuration name, its external score and the shape of its partition.
data ClusterEvalRow = ClusterEvalRow
  { cerConfig     :: !Text    -- ^ @leiden@, @bunch@ or @<distance>_<linkage>_<divisor>@
  , cerMojofm     :: !Double  -- ^ MoJoFM against the ground truth
  , cerClusters   :: !Int     -- ^ cluster count
  , cerSingletons :: !Int     -- ^ singleton clusters
  , cerModularity :: !Double  -- ^ modularity of the partition
  } deriving (Eq, Show, Generic)

instance NFData ClusterEvalRow

instance ToJSON ClusterEvalRow where
  toJSON r = object
    [ "config"     .= cerConfig r
    , "mojofm"     .= cerMojofm r
    , "clusters"   .= cerClusters r
    , "singletons" .= cerSingletons r
    , "modularity" .= cerModularity r
    ]

instance FromJSON ClusterEvalRow where
  parseJSON = withObject "ClusterEvalRow" $ \v -> ClusterEvalRow
    <$> v .: "config"
    <*> v .: "mojofm"
    <*> v .: "clusters"
    <*> v .: "singletons"
    <*> v .: "modularity"

-- | The complete sweep result written to @cluster_eval.json@ and rendered as
-- a ranking section in @GRAPH_REPORT.md@.
data ClusterEvalReport = ClusterEvalReport
  { cerTruth        :: !Text           -- ^ ground-truth source (directory depth or file)
  , cerTruthGroups  :: !Int            -- ^ groups in the ground truth
  , cerRows         :: ![ClusterEvalRow]
  , cerSelected     :: !Text           -- ^ config name applied to node communities
  , cerSkipped      :: ![Text]         -- ^ configurations skipped (file cap), with the reason
  } deriving (Eq, Show, Generic)

instance NFData ClusterEvalReport

instance ToJSON ClusterEvalReport where
  toJSON r = object
    [ "truth"        .= cerTruth r
    , "truth_groups" .= cerTruthGroups r
    , "selected"     .= cerSelected r
    , "rows"         .= cerRows r
    , "skipped"      .= cerSkipped r
    ]

instance FromJSON ClusterEvalReport where
  parseJSON = withObject "ClusterEvalReport" $ \v -> ClusterEvalReport
    <$> v .: "truth"
    <*> v .: "truth_groups"
    <*> v .: "rows"
    <*> v .: "selected"
    <*> v .: "skipped"

-- | A god node (high-degree hub)
data GodNode = GodNode
  { gnId    :: NodeId
  , gnLabel :: Text
  , gnEdges :: Int
  } deriving (Eq, Show, Generic)

instance NFData GodNode

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

instance NFData SurprisingConnection

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

instance NFData SuggestedQuestion

instance ToJSON SuggestedQuestion where
  toJSON q = object
    [ "type"     .= sqType q
    , "question" .= sqQuestion q
    , "why"      .= sqWhy q
    ]

-- | A community aggregate computed from Leiden detection
data CommunityAggregate = CommunityAggregate
  { caId                     :: !Text
  , caMemberCount            :: !Int
  , caCohesion               :: !Double
  , caBridgeCount            :: !Int
  , caColor                  :: !Text
  , caLabel                  :: !Text
  , caRepresentativeLabels   :: ![Text]
  , caInterCommunityEdges    :: ![(Int, Int)]
  , caDominantKind           :: !(Maybe Text)
  , caMixedRatio             :: !Double
  , caCodeDocEdges           :: !Int
   } deriving (Eq, Show, Generic)

instance ToJSON CommunityAggregate where
  toJSON ca = object
    [ "id"                      .= caId ca
    , "member_count"            .= caMemberCount ca
    , "cohesion"                .= caCohesion ca
    , "bridge_count"            .= caBridgeCount ca
    , "color"                   .= caColor ca
    , "label"                   .= caLabel ca
    , "representative_labels"   .= caRepresentativeLabels ca
    , "inter_community_edges"   .= caInterCommunityEdges ca
    , "dominant_kind"           .= caDominantKind ca
    , "mixed_ratio"             .= caMixedRatio ca
    , "code_doc_edges"          .= caCodeDocEdges ca
    ]

instance FromJSON CommunityAggregate where
  parseJSON = withObject "CommunityAggregate" $ \v -> CommunityAggregate
    <$> v .: "id"
    <*> v .: "member_count"
    <*> v .: "cohesion"
    <*> v .: "bridge_count"
    <*> v .: "color"
    <*> v .: "label"
    <*> v .: "representative_labels"
    <*> v .: "inter_community_edges"
    <*> v .: "dominant_kind"
    <*> v .: "mixed_ratio"
    <*> v .: "code_doc_edges"