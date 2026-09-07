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
  , renderResearchHtml
  , VisCommunityAggregate(..)
  , VisPayload(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=), encode, eitherDecode, Value)
import Data.FileEmbed (embedFile)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson.Key as Key
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Short (toText)
import System.IO (hFlush, hPutStr)
import qualified Data.Set as Set

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges, articulationPoints, gCompositions)
import Graphos.Domain.Community (cohesionScore, CommunityComposition(..))
import Graphos.Domain.HexColor (assignTermColors, unHexColor)
import Graphos.Domain.Query.Research (ResearchView(..))
import Graphos.UseCase.Cluster (colorForCommunity)
import Graphos.Infrastructure.FileSystem.AtomicWrite (withAtomicHandle)

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
  -- Stream into a temp file and rename over the target only when complete.
  withAtomicHandle htmlPath $ \h -> do
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

-- | Render a ResearchView as a self-contained interactive HTML document.
--
-- Mirrors the offline-first approach of the existing graph.html, but draws the
-- research subgraph with vis-network loaded from a CDN, embeds the ResearchView
-- JSON in a <script> blob, colors nodes by their first-discovering term, shows a
-- per-term discovery legend, and populates a detail panel on hover/click.
renderResearchHtml :: ResearchView -> Text
renderResearchHtml rv =
  let title = "Research View — " <> T.intercalate ", " (rvTerms rv)
      dataJson = escapeForScript (decodeUtf8 (BSL.toStrict (encode rv)))
      colorsJson = termColorJSON rv
   in T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang='en'><head>"
        , "<meta charset='utf-8'>"
        , "<meta name='viewport' content='width=device-width, initial-scale=1'>"
        , "<title>" <> title <> "</title>"
        , "<meta name='graphos-renderer' content='vis-network 10.1.1'>"
        , "<script>"
        , "document.write('<script src=https://unpkg.com/vis-network@10.1.1/dist/vis-network.min.js><\\/script>');"
        , "</script>"
        , "<style>" <> researchCss <> "</style>"
        , "</head><body>"
        , "<div id='sidebar'>"
        , "  <h1 id='researchTitle'>" <> title <> "</h1>"
        , "  <p class='meta'><span id='stats'></span></p>"
        , "  <section class='legend-section'>"
        , "    <h3>Discovered by</h3>"
        , "    <div id='legend'></div>"
        , "  </section>"
        , "  <section class='detail-section'>"
        , "    <h3>Node details</h3>"
        , "    <div id='research-detail'><p class='hint'>Hover or click a node to inspect.</p></div>"
        , "  </section>"
        , "  <section class='communities-section'>"
        , "    <h3>Communities</h3>"
        , "    <div id='community-summary'></div>"
        , "  </section>"
        , "</div>"
        , "<div id='graph'><div id='loading'>Loading graph...</div></div>"
        , "<script type='application/json' id='research-data'>" <> dataJson <> "</script>"
        , "<script>" <> researchJs colorsJson <> "</script>"
        , "</body></html>"
        ]

termColorJSON :: ResearchView -> Value
termColorJSON rv =
  object [(Key.fromText t, toJSON (T.unpack (unHexColor c))) | (t, c) <- Map.toList (assignTermColors (rvTerms rv))]

escapeForScript :: Text -> Text
escapeForScript = T.replace "<" "\\u003c"

researchCss :: Text
researchCss =
  T.unlines
    [ "* { box-sizing: border-box; }"
    , "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; margin: 0; color: #1a1a1a; display: flex; }"
    , "#sidebar { width: 340px; min-width: 340px; padding: 20px; overflow-y: auto; background: #fafafa; border-right: 1px solid #e5e5e5; }"
    , "#researchTitle { font-size: 18px; margin: 0 0 4px; }"
    , ".meta { color: #666; font-size: 13px; margin: 0 0 20px; }"
    , ".legend-section, .detail-section, .communities-section { margin-bottom: 24px; }"
    , "h3 { font-size: 13px; text-transform: uppercase; letter-spacing: 0.05em; color: #888; margin: 0 0 12px; }"
    , ".legend-item { display: flex; align-items: center; gap: 8px; padding: 6px 8px; margin-bottom: 4px; background: #fff; border: 1px solid #eee; border-radius: 6px; font-size: 13px; cursor: default; }"
    , ".legend-dot { width: 12px; height: 12px; border-radius: 50%; flex: none; }"
    , ".hint { color: #999; font-size: 13px; margin: 0; }"
    , ".detail-name { font-size: 15px; font-weight: 600; margin-bottom: 10px; word-break: break-all; }"
    , ".row { display: flex; justify-content: space-between; gap: 12px; padding: 5px 0; border-bottom: 1px solid #f0f0f0; font-size: 13px; }"
    , ".row .k { color: #888; }"
    , ".row .v { color: #1a1a1a; word-break: break-word; text-align: right; }"
    , ".scores { margin-top: 8px; }"
    , "#graph { flex: 1; position: relative; }"
    , "#loading { position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); color: #999; }"
    , "@media (max-width: 720px) { #sidebar { width: auto; min-width: 0; max-height: 40vh; }"
    ]

researchJs :: Value -> Text
researchJs colorsJson =
  T.unlines
    [ "(function(){"
    , "var rv = JSON.parse(document.getElementById('research-data').textContent);"
    , "var termColors = " <> escapeForScript (decodeUtf8 (BSL.toStrict (encode colorsJson))) <> ";"
    , "document.getElementById('stats').textContent = rv.nodes.length + ' nodes, ' + rv.edges.length + ' edges, ' + (rv.communities ? Object.keys(rv.communities).length : 0) + ' communities';"
    , "var legendEl = document.getElementById('legend');"
    , "rv.terms.forEach(function(t){ var row = document.createElement('div'); row.className='legend-item'; var dot = document.createElement('span'); dot.className='legend-dot'; dot.style.backgroundColor = termColors[t] || '#888888'; var label = document.createElement('span'); label.textContent = t; row.appendChild(dot); row.appendChild(label); legendEl.appendChild(row); });"
    , "var nodeData = new vis.DataSet(rv.nodes.map(function(n){ var c = n.discovered_by && n.discovered_by.length ? (termColors[n.discovered_by[0]] || '#888888') : '#888888'; return { id: n.id, label: n.label, color: { background: c, border: '#333333' }, title: n.id }; }));"
    , "var edgeData = new vis.DataSet(rv.edges.map(function(e){ var w = Math.max(1, Math.min(5, ((e.confidence || 0) * 3))); return { from: e.source, to: e.target, label: e.type || '', width: w, color: { color: '#999999' }, title: (e.type || '') + ' (' + (e.confidence || 0) + ')' }; }));"
    , "var network = new vis.Network(document.getElementById('graph'), { nodes: nodeData, edges: edgeData }, { physics: { forceAtlas2BasedCentroid: true, stabilization: { enabled: true, iterations: 250 } }, interaction: { hover: true, tooltipOnHover: false } });"
    , "function findNode(id){ for (var i=0;i<rv.nodes.length;i++){ if (rv.nodes[i].id === id) return rv.nodes[i]; } return null; }"
    , "function esc(s){ return (s==null?'':String(s)).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;'); }"
    , "function nodeIdsFromEvent(e){ if (!e) return []; if (e.nodes) return e.nodes; if (e.node) return [e.node]; return []; }"
    , "function updateDetail(e){ var ids = nodeIdsFromEvent(e); var detail = document.getElementById('research-detail'); if (!ids.length){ detail.innerHTML = \"<p class='hint'>Hover or click a node to inspect.</p>\"; return; } var n = findNode(ids[0]); if (!n){ detail.innerHTML = \"\"; return; } var html = \"<div class='detail-name'>\" + esc(n.label || ids[0]) + \"</div>\"; html += \"<div class='row'><span class='k'>Source</span><span class='v'>\" + esc(n.source_file || '') + \"</span></div>\"; html += \"<div class='row'><span class='k'>Community</span><span class='v'>\" + (n.community || '\\u2014') + \"</span></div>\"; html += \"<div class='row'><span class='k'>Best score</span><span class='v'>\" + (n.best_score != null ? n.best_score : '\\u2014') + \"</span></div>\"; html += \"<div class='row'><span class='k'>Discovered by</span><span class='v'>\" + (n.discovered_by && n.discovered_by.length ? n.discovered_by.join(', ') : '\\u2014') + \"</span></div>\"; if (n.scores && n.scores.length){ html += \"<div class='scores'>\"; n.scores.forEach(function(s){ html += \"<div class='row'><span class='k'>\" + esc(s.term) + \"</span><span class='v'>\" + s.score + \"</span></div>\"; }); html += \"</div>\"; } detail.innerHTML = html; }"
    , "network.on('select', function(e){ updateDetail(e); });"
    , "network.on('blur', function(){ updateDetail(); });"
    , "network.on('hoverNode', function(e){ updateDetail(e); });"
    , "network.on('blurNode', function(){ updateDetail(); });"
    , "var commEl = document.getElementById('community-summary');"
    , "if (rv.communities){ Object.keys(rv.communities).forEach(function(cid){ var item = document.createElement('div'); item.className='legend-item'; item.style.marginBottom='4px'; item.textContent = (rv.communities[cid].label || ('Community ' + cid)) + ' \\u2014 ' + rv.communities[cid].member_count + ' members'; commEl.appendChild(item); }); }"
    , "window.addEventListener('resize', function(){ network.fit(); });"
    , "updateDetail();"
    , "})();"
    ]

