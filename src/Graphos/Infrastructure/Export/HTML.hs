{-# LANGUAGE TemplateHaskell #-}
-- | HTML export - multi-depth LOD (level-of-detail) graph visualization
-- Embeds an interned, style-free JSON payload inline for self-contained HTML.
-- Streams to handle to reduce peak memory (avoids building full HTML Text in memory).
--
-- The viewer application (assets/viewer/viewer.js), its stylesheet
-- (assets/viewer/viewer.css) and the vendored rendering bundle
-- (assets/viewer/vis-network.min.js) are embedded at compile time with
-- 'file-embed' — this module contains only document assembly and payload
-- projection, never viewer JavaScript or CSS as string literals.
--
-- Community aggregates are NOT computed here. They are produced once by
-- 'Graphos.UseCase.Cluster.computeCommunityAggregates' and passed in via the
-- 'VisCommunityAggregate' list. This module only projects them into the HTML
-- payload format and converts the canonical text community id to the numeric id
-- used by the viewer.
module Graphos.Infrastructure.Export.HTML
  ( exportHTML
  , communityAggregatesToJSON
  , computePayload
  , convertAggregate
  , VisCommunityAggregate(..)
  , VisPayload(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=), encode, eitherDecode)
import Data.FileEmbed (embedFile)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (toText)
import System.IO (IOMode(..), hFlush, hClose, openFile, hPutStr)
import qualified Data.Set as Set

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges, articulationPoints, gCompositions)
import Graphos.Domain.Community (cohesionScore, CommunityComposition(..))
import Graphos.UseCase.Cluster (colorForCommunity)

-- | Convert the canonical 'CommunityAggregate' (used in graph.json) into the
-- HTML-specific view record. This keeps the single computation site in
-- 'Graphos.UseCase.Cluster.computeCommunityAggregates' while allowing the HTML
-- payload to use a numeric community id and carry composition fields.
convertAggregate :: CommunityAggregate -> VisCommunityAggregate
convertAggregate ca = VisCommunityAggregate
  { vcaId                     = read (T.unpack (caId ca))
  , vcaMemberCount            = caMemberCount ca
  , vcaCohesion               = caCohesion ca
  , vcaBridgeCount            = caBridgeCount ca
  , vcaColor                  = caColor ca
  , vcaLabel                  = caLabel ca
  , vcaRepresentativeLabels   = caRepresentativeLabels ca
  , vcaInterCommunityEdges    = sum [count | (_, count) <- caInterCommunityEdges ca]
  , vcaDominantKind           = caDominantKind ca
  , vcaMixedRatio             = caMixedRatio ca
  , vcaCodeDocEdges           = caCodeDocEdges ca
  }

-- | Viewer assets, embedded at compile time so the emitted document is
-- self-contained. These are source files, not string literals.
viewerCss :: BSL.ByteString
viewerCss = BSL.fromStrict $(embedFile "assets/viewer/viewer.css")

viewerJs :: BSL.ByteString
viewerJs = BSL.fromStrict $(embedFile "assets/viewer/viewer.js")

visNetworkBundle :: BSL.ByteString
visNetworkBundle = BSL.fromStrict $(embedFile "assets/viewer/vis-network.min.js")

-- | Export graph as interactive HTML with multi-depth LOD viewer
exportHTML :: Graph -> Analysis -> Maybe (Map.Map CommunityId Text) -> [VisCommunityAggregate] -> FilePath -> IO ()
exportHTML g analysis mLabels aggregates htmlPath = do
  let payload = computePayload g analysis mLabels aggregates
  h <- openFile htmlPath WriteMode
  -- Document head + body skeleton (static markup and asset containers)
  hPutStr h $ T.unpack htmlDocStart
  BSL.hPut h visNetworkBundle
  hPutStr h "</script>\n"
  hPutStr h "<style>\n"
  BSL.hPut h viewerCss
  hPutStr h "\n</style>\n"
  hPutStr h $ T.unpack (htmlBodySkeleton g analysis)
  -- Stream payload JSON directly to handle
  BSL.hPut h (encode payload)
  -- Close the payload script, embed the viewer application, close the document
  hPutStr h ";\n</script>\n<script>\n"
  BSL.hPut h viewerJs
  hPutStr h "\n</script>\n</body></html>\n"
  hFlush h
  hClose h

-- | Compute the interned, style-free payload for the viewer.
computePayload :: Graph -> Analysis -> Maybe (Map.Map CommunityId Text) -> [VisCommunityAggregate] -> VisPayload
computePayload g analysis mLabels aggregates =
  let
    commMap = analysisCommunities analysis
    nodeMap = gNodes g
    edgeMap = gEdges g

    -- 1. Collect strings for interning (deterministic order)
    allNodeIds = [nid | nid <- Map.keys nodeMap]
    allSourceFiles = [toText (nodeSourceFile n) | n <- Map.elems nodeMap]
    allKinds = [toText k | n <- Map.elems nodeMap, Just k <- [nodeKind n]]
    allRelations = [relationToText (edgeRelation e) | e <- Map.elems edgeMap]

    uniqueNodeIds = Set.toAscList (Set.fromList allNodeIds)
    uniqueFiles   = Set.toAscList (Set.fromList allSourceFiles)
    uniqueKinds   = Set.toAscList (Set.fromList allKinds)
    uniqueRels    = Set.toAscList (Set.fromList allRelations)

    nodeIdToIdx = Map.fromList (zip uniqueNodeIds [0::Int ..])
    fileToIdx   = Map.fromList (zip uniqueFiles   [0::Int ..])
    kindToIdx   = Map.fromList (zip uniqueKinds   [0::Int ..])
    relToIdx    = Map.fromList (zip uniqueRels    [0::Int ..])

    -- 2. Build Nodes
    nodes = [ VisNode
               { vnLabel      = truncateLabel (sanitize (toText (nodeLabel n)))
               , vnFileIdx    = Map.findWithDefault 0 (toText (nodeSourceFile n)) fileToIdx
               , vnLine       = maybe 0 id (nodeLineStart n)
               , vnCommId     = maybe (-1) id (nodeCommunityId n)
               , vnDegree     = maybe 0 id (nodeDegree n)
               , vnIsBridge   = maybe False id (nodeIsBridge n)
               , vnKindIdx    = maybe 0 (\k -> Map.findWithDefault 0 (toText k) kindToIdx) (nodeKind n)
               , vnFileType   = fileTypeToIdx (nodeFileType n)
              }
            | n <- Map.elems nodeMap
            ]

    -- 3. Build Edges
    edges = [ VisEdge
              { veFromIdx = Map.findWithDefault 0 (edgeSource e) nodeIdToIdx
              , veToIdx   = Map.findWithDefault 0 (edgeTarget e) nodeIdToIdx
              , veRelIdx  = Map.findWithDefault 0 (relationToText (edgeRelation e)) relToIdx
              }
            | e <- Map.elems edgeMap
            ]

    -- 4. Build Aggregates from the pre-computed list
    aggregates' = case aggregates of
      [] -> communityAggregatesToJSON g commMap mLabels
      _  -> aggregates

  in VisPayload
       { vpNodes      = nodes
       , vpEdges      = edges
       , vpStrings    = uniqueNodeIds
       , vpFiles      = uniqueFiles
       , vpKinds      = uniqueKinds
       , vpRelations  = uniqueRels
       , vpAggregates = aggregates'
       }
  where
    sanitize t = T.filter (\c -> c /= '\n' && c /= '\r' && c /= '"' && c /= '\'' && c /= '`') t
    truncateLabel t = if T.length t > 80 then T.take 80 t <> "…" else t
    fileTypeToIdx ft = case ft of
      CodeFile  -> 0
      DocFile   -> 1
      PaperFile -> 2
      ImageFile -> 3
      VideoFile -> 4
      AudioFile -> 5
      OfficeFile -> 6

-- | Document head: everything before the vendored renderer bundle.
htmlDocStart :: Text
htmlDocStart =
  T.unlines
    [ "<!DOCTYPE html>"
    , "<html lang='en'><head>"
    , "<meta charset='utf-8'>"
    , "<meta name='viewport' content='width=device-width, initial-scale=1'>"
    , "<title>Graphos Knowledge Graph</title>"
    , "<meta name='graphos-renderer' content='vis-network 10.1.1'>"
    , "<script>"
    ]

-- | Body skeleton: the stylesheet, static markup and the payload bootstrap.
-- The stylesheet and renderer bundle are streamed as separate handle writes
-- between 'htmlDocStart' and this text (see 'exportHTML').
htmlBodySkeleton :: Graph -> Analysis -> Text
htmlBodySkeleton g analysis =
  T.unlines
    [ "</head><body>"
    , "<header>"
    , "  <div>"
    , "    <h1 id='headerTitle'>Graphos Knowledge Graph</h1>"
    , "    <div class='stats'>" <> statsText <> "</div>"
    , "  </div>"
    , "  <div class='header-controls'>"
    , "    <div class='search-box'>"
    , "      <input type='text' id='searchInput' placeholder='Search notes...' autocomplete='off' />"
    , "      <span class='search-count' id='searchCount'></span>"
    , "      <button class='btn-reset' id='btnReset' style='display:none' title='Clear search and show full graph'>Reset</button>"
    , "    </div>"
    , "    <div class='depth-selector'>"
    , "      <label for='depthSelector' title='View depth'>Depth</label>"
    , "      <select id='depthSelector' title='View depth'>"
    , "        <option value='overview'>Overview</option>"
    , "        <option value='community'>Community</option>"
    , "        <option value='full'>Full</option>"
    , "        <option value='custom'>Custom</option>"
    , "      </select>"
    , "      <input type='number' class='neighborhood-input' id='neighborhoodHops' min='1' max='6' step='1' value='2' title='N-hop radius for Custom depth' />"
    , "    </div>"
    , "  </div>"
    , "</header>"
    , "<div class='layout'>"
    , "  <div id='graph'><div id='loading'>Loading graph...</div></div>"
    , "  <div id='sidebar'>"
    , "    <div class='sidebar-scroll'>"
    , "      <div class='sidebar-section' id='searchResults'>"
    , "        <h3>Search Results</h3>"
    , "        <div id='resultsList'></div>"
    , "      </div>"
    , "      <div class='sidebar-section' id='facetSection'>"
    , "        <h3>Facets</h3>"
    , "        <input type='text' class='facet-text' id='facetText' placeholder='Filter label / source path...' autocomplete='off' />"
    , "        <div class='facet-group' id='facetFileType'></div>"
    , "        <div class='facet-group' id='facetKind'></div>"
    , "        <div class='facet-group' id='facetRelation'></div>"
    , "        <div class='facet-group' id='facetBridge'></div>"
    , "        <div class='facet-group' id='facetCommunity'></div>"
    , "      </div>"
    , "      <div class='sidebar-section' id='nodeDetail'>"
    , "        <div class='selected-info' id='selectedInfo'>"
    , "          <div class='label' id='selectedLabel'></div>"
    , "          <div class='kind' id='selectedKind'></div>"
    , "          <div class='file' id='selectedFile'></div>"
    , "          <div class='community-tag' id='selectedCommunity'></div>"
    , "          <div class='detail-grid' id='selectedDetail'></div>"
    , "          <div id='selectedSignature' style='display:none'></div>"
    , "          <div class='neighbors' id='selectedNeighbors'></div>"
    , "        </div>"
    , "      </div>"
    , "      <div class='sidebar-section' id='communitiesSection'>"
    , "        <h3>Depth</h3>"
    , "        <p id='phaseHint'>Click a community dot to explore.</p>"
    , "      </div>"
    , "      <div class='sidebar-section'>"
    , "        <h3>Legend</h3>"
    , "        <div class='legend' id='legendList'></div>"
    , "      </div>"
    , "    </div>"
    , "  </div>"
    , "</div>"
    , "<script>"
    , "  // Graphos embedded payload (self-contained HTML, no fetch needed)"
    , "  const _payloadData = "
    ]
  where
    statsText = T.pack $ show (Map.size $ gNodes g) ++ " nodes, "
              ++ show (Map.size $ gEdges g) ++ " edges, "
               ++ show (length $ analysisCommunities analysis) ++ " communities"

-- | Node data for JSON export
data VisNode = VisNode
  { vnLabel      :: Text
  , vnFileIdx    :: Int
  , vnLine       :: Int
  , vnCommId     :: Int
  , vnDegree     :: Int
  , vnIsBridge   :: Bool
  , vnKindIdx    :: Int
  , vnFileType   :: Int
  } deriving (Show, Generic)

instance ToJSON VisNode where
  toJSON n = object
    [ "label"        .= vnLabel n
    , "file_idx"     .= vnFileIdx n
    , "line"         .= vnLine n
    , "community_id" .= vnCommId n
    , "degree"       .= vnDegree n
    , "is_bridge"    .= vnIsBridge n
    , "kind_idx"     .= vnKindIdx n
    , "file_type"    .= vnFileType n
    ]

-- | Edge data for JSON export
data VisEdge = VisEdge
  { veFromIdx    :: Int
  , veToIdx      :: Int
  , veRelIdx     :: Int
  } deriving (Show, Generic)

instance ToJSON VisEdge where
  toJSON e = toJSON [veFromIdx e, veToIdx e, veRelIdx e]

-- | Community aggregate data for JSON export
data VisCommunityAggregate = VisCommunityAggregate
  { vcaId                     :: Int
  , vcaMemberCount            :: Int
  , vcaCohesion               :: Double
  , vcaBridgeCount            :: Int
  , vcaColor                  :: Text
  , vcaLabel                  :: Text
  , vcaRepresentativeLabels   :: [Text]
  , vcaInterCommunityEdges    :: Int
  , vcaDominantKind           :: Maybe Text
  , vcaMixedRatio             :: Double
  , vcaCodeDocEdges           :: Int
  } deriving (Show, Generic)

instance ToJSON VisCommunityAggregate where
  toJSON ca = object
    [ "id"                       .= vcaId ca
    , "member_count"             .= vcaMemberCount ca
    , "cohesion"                 .= vcaCohesion ca
    , "bridge_count"             .= vcaBridgeCount ca
    , "color"                    .= vcaColor ca
    , "label"                    .= vcaLabel ca
    , "representative_labels"    .= vcaRepresentativeLabels ca
    , "inter_community_edges"    .= vcaInterCommunityEdges ca
    , "dominant_kind"            .= vcaDominantKind ca
    , "mixed_ratio"              .= vcaMixedRatio ca
    , "code_doc_edges"           .= vcaCodeDocEdges ca
    ]

-- | Full interned payload for the viewer
data VisPayload = VisPayload
  { vpNodes      :: [VisNode]
  , vpEdges      :: [VisEdge]
  , vpStrings    :: [Text]
  , vpFiles      :: [Text]
  , vpKinds      :: [Text]
  , vpRelations  :: [Text]
  , vpAggregates :: [VisCommunityAggregate]
  } deriving (Show, Generic)

instance ToJSON VisPayload where
  toJSON p = object
    [ "nodes"      .= vpNodes p
    , "edges"      .= vpEdges p
    , "strings"    .= vpStrings p
    , "files"      .= vpFiles p
    , "kinds"      .= vpKinds p
    , "relations"  .= vpRelations p
    , "aggregates" .= vpAggregates p
    ]

-- | Convert community aggregates to JSON.
communityAggregatesToJSON :: Graph -> CommunityMap -> Maybe (Map.Map CommunityId Text) -> [VisCommunityAggregate]
communityAggregatesToJSON g commMap mLabels =
  let sanitize t = T.filter (\c -> c /= '\n' && c /= '\r' && c /= '"' && c /= '\'' && c /= '`') t
      truncateLabel t = if T.length t > 80 then T.take 80 t <> "…" else t
      artPoints = articulationPoints g
      artSet = Set.fromList artPoints
      nodeMap = gNodes g
      isBridge m = case Map.lookup m nodeMap of
        Just n -> sanitize (nodeId n) `Set.member` artSet
        Nothing -> False
      compMap = case gCompositions g of
        Just cv -> case eitherDecode (encode cv) of
          Right comps -> Map.fromList [(cid, comp) | (cid, comp) <- Map.toList comps]
          Left _ -> Map.empty
        Nothing -> Map.empty
   in [ VisCommunityAggregate
          { vcaId                     = cid
          , vcaMemberCount            = length members
          , vcaCohesion               = cohesionScore g members
          , vcaBridgeCount            = length [m | m <- members, isBridge m]
          , vcaColor                  = colorForCommunity cid
          , vcaLabel                  = case mLabels of
                                        Just m  -> maybe (T.pack ("Community " ++ show cid)) id (Map.lookup cid m >>= \t -> if T.null t then Nothing else Just t)
                                        Nothing -> T.pack ("Community " ++ show cid)
          , vcaRepresentativeLabels   = take 3 [truncateLabel (sanitize (toText (nodeLabel n))) | nid <- take 10 members, Just n <- [Map.lookup nid nodeMap]]
          , vcaInterCommunityEdges    = 0
          , vcaDominantKind           = compDominantKind comp
          , vcaMixedRatio             = compMixedRatio comp
          , vcaCodeDocEdges           = compCodeDocEdges comp
          }
        | (cid, members) <- Map.toList commMap
        , let comp = Map.findWithDefault emptyComp cid compMap
        ]
  where
    emptyComp :: CommunityComposition
    emptyComp = CommunityComposition
      { ccCodeCount    = 0
      , ccDocCount     = 0
      , ccOtherCount   = 0
      , ccDominantKind = Nothing
      , ccMixedRatio   = 0.0
      , ccCodeDocEdges = 0
      }
    compDominantKind :: CommunityComposition -> Maybe Text
    compDominantKind c = ccDominantKind c
    compMixedRatio :: CommunityComposition -> Double
    compMixedRatio c = ccMixedRatio c
    compCodeDocEdges :: CommunityComposition -> Int
    compCodeDocEdges c = ccCodeDocEdges c

