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
    allSourceFiles = [nodeSourceFile n | n <- Map.elems nodeMap]
    allKinds = [k | n <- Map.elems nodeMap, Just k <- [nodeKind n]]
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
              { vnLabel      = truncateLabel (sanitize (nodeLabel n))
              , vnFileIdx    = Map.findWithDefault 0 (nodeSourceFile n) fileToIdx
              , vnLine       = maybe 0 id (nodeLineStart n)
              , vnCommId     = maybe (-1) id (nodeCommunityId n)
              , vnDegree     = maybe 0 id (nodeDegree n)
              , vnIsBridge   = maybe False id (nodeIsBridge n)
              , vnKindIdx    = maybe 0 (\k -> Map.findWithDefault 0 k kindToIdx) (nodeKind n)
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
    , "      <label for='depthSelect' title='View depth'>Depth</label>"
    , "      <select id='depthSelect' title='View depth'>"
    , "        <option value='overview'>Overview</option>"
    , "        <option value='community'>Community</option>"
    , "        <option value='full'>Full</option>"
    , "        <option value='custom'>Custom</option>"
    , "      </select>"
    , "      <input type='number' class='hops-input' id='hopsInput' min='1' max='6' step='1' value='2' title='N-hop radius for Custom depth' />"
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

-- | HTML body: JavaScript code and closing tags.
htmlBody :: Text
htmlBody =
  T.unlines
    [ "  // Expand interned payload into the legacy shape used below"
    , "  const nodeIdStrings = _payloadData.strings;"
    , "  const fileStrings = _payloadData.files;"
    , "  const kindStrings = _payloadData.kinds;"
    , "  const relationStrings = _payloadData.relations;"
    , "  const rawNodes = _payloadData.nodes;"
    , "  const rawEdges = _payloadData.edges;"
    , "  let allNodes = rawNodes.map(function(n, i) {"
    , "    return {"
    , "      id: nodeIdStrings[i],"
    , "      label: n.label,"
    , "      source_file: fileStrings[n.file_idx],"
    , "      line: n.line,"
    , "      community_id: n.community_id,"
    , "      degree: n.degree,"
    , "      is_bridge: n.is_bridge,"
    , "      kind: kindStrings[n.kind_idx],"
    , "      file_type: n.file_type"
    , "    };"
    , "  });"
    , "  let allEdges = rawEdges.map(function(e) {"
    , "    const rel = relationStrings[e[2]];"
    , "    return {"
    , "      from: nodeIdStrings[e[0]],"
    , "      to: nodeIdStrings[e[1]],"
    , "      relation: rel,"
    , "      title: rel,"
    , "      label: rel,"
    , "      dashes: rel === 'Inferred',"
    , "      width: rel === 'Inferred' ? 1 : 2"
    , "    };"
    , "  });"
    , "  let communityAggregates = _payloadData.aggregates;"
    , "  let overviewNetwork = null;"
    , "  let drilldownNetwork = null;"
    , "  let overviewNodesDataset = null;"
    , "  let overviewEdgesDataset = null;"
    , "  let drilldownNodesDataset = null;"
    , "  let drilldownEdgesDataset = null;"
    , "  let viewerState = {"
    , "    depth: 'overview',"
    , "    selection: null,"
    , "    hops: 2,"
    , "    facets: {},"
    , "    searchResults: []"
    , "  };"
    , ""
    , "  function dispatch(action, payload) {"
    , "    switch (action) {"
    , "      case 'SET_DEPTH':"
    , "        viewerState.depth = payload;"
    , "        break;"
    , "      case 'SET_SELECTION':"
    , "        viewerState.selection = payload;"
    , "        break;"
    , "      case 'SET_HOPS':"
    , "        viewerState.hops = payload;"
    , "        break;"
    , "      case 'SET_FACETS':"
    , "        viewerState.facets = payload;"
    , "        break;"
    , "      case 'SET_SEARCH':"
    , "        viewerState.searchResults = payload;"
    , "        break;"
    , "    }"
    , "    // Side effects triggered by state changes"
    , "    handleStateChange();"
    , "  }"
    , ""
    , "  function handleStateChange() {"
    , "    currentPhase = viewerState.depth;"
    , "    expandedCommunity = viewerState.selection;"
    , "    console.log('State changed:', viewerState);"
    , "  }"
    , ""
    , "  let currentPhase = viewerState.depth;"
    , "  let expandedCommunity = viewerState.selection;"

    , "  let nodeCommMap = {};"
    , "  let apiAvailable = true;"
    , "  let currentHighlightedNodes = null;"
    , ""
    , "  // Build lookup: nodeId -> communityId"
    , "  allNodes.forEach(n => {"
    , "    if (n.community_id !== undefined && n.community_id !== null) {"
    , "      nodeCommMap[n.id] = n.community_id;"
    , "    }"
    , "  });"
    , ""
    , "  // Build community -> nodes map"
    , "  let commToNodes = {};"
    , "  allNodes.forEach(n => {"
    , "    const cid = n.community_id || -1;"
    , "    if (!commToNodes[cid]) commToNodes[cid] = [];"
    , "    commToNodes[cid].push(n);"
    , "  });"
    , ""
    , "  // Build community -> edges map"
    , "  let commToEdges = {};"
    , "  allEdges.forEach(e => {"
    , "    const src = e.from;"
    , "    const tgt = e.to;"
    , "    const srcComm = nodeCommMap[src] || -1;"
    , "    const tgtComm = nodeCommMap[tgt] || -1;"
    , "    if (!commToEdges[srcComm]) commToEdges[srcComm] = [];"
    , "    commToEdges[srcComm].push(e);"
    , "  });"
    , ""
    , "  // Return CSS class based on node id prefix"
    , "  function nodeTypeClass(nodeId) {"
    , "    if (nodeId.includes('_doc_')) return 'docresult';"
    , "    if (nodeId.includes('_h1_')) return 'h1result';"
    , "    if (nodeId.includes('_h2_')) return 'h2result';"
    , "    return 'docresult';"
    , "  }"
    , ""
    , "  // Extract short filename from full path"
    , "  function shortPath(filePath) {"
    , "    if (!filePath) return '';"
    , "    const parts = filePath.split('/');"
    , "    return parts.slice(-2).join('/');"
    , "  }"
    , ""
    , "  // Escape HTML"
    , "  function escHtml(s) {"
    , "    if (!s) return '';"
    , "    return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/\"/g,'&quot;');"
    , "  }"
    , ""
    , "  // Render community list in sidebar"
    , "  function renderCommunityList() {"
    , "    const container = document.getElementById('communityList');"
    , "    const sorted = Array.from(communityAggregates).sort((a, b) => b.member_count - a.member_count).slice(0, 50);"
    , "    container.innerHTML = sorted.map(c =>"
    , "      '<div class=\"comm-item\" data-community=\"' + c.id + '\"'"
    , "      + '<strong style=\"color:' + c.color + '\">'"
    , "      + 'Community ' + c.id + '</strong>'"
    , "      + ' <span class=\"members\">'"
    , "      + ' — ' + c.member_count + ' members, '"
    , "      + c.bridge_count + ' bridges</span>'"
    , "      + '<br><span style=\"color:#4ade80;font-size:10px\">'"
    , "      + 'cohesion: ' + c.cohesion.toFixed(3) + '</span>'"
    , "      + '<br><span style=\"color:#888;font-size:10px\">'"
    , "      + ' ' + escHtml(c.label) + '</span>'"
    , "      + '</div>'"
    , "    ).join('');"
    , ""
    , "    // Bind click to expand community"
    , "    container.querySelectorAll('.comm-item[data-community]').forEach(item => {"
    , "      item.addEventListener('click', function() {"
    , "        const cid = parseInt(this.getAttribute('data-community'), 10);"
    , "        expandCommunity(cid);"
    , "      });"
    , "    });"
    , "  }"
    , ""
    , "  // Build overview community dots"
    , "  function buildOverviewData() {"
    , "    const dots = communityAggregates.map(c => {"
    , "      const size = Math.max(4, Math.min(20, Math.sqrt(c.member_count) * 2));"
    , "      return {"
    , "        id: 'comm_' + c.id,"
    , "        label: '',"
    , "        title: c.label + ' — ' + c.member_count + ' members, cohesion: ' + c.cohesion.toFixed(3),"
    , "        color: { background: c.color, border: '#1a1a2e', highlight: c.color, hover: c.color },"
    , "        size: size,"
    , "        shape: 'dot',"
    , "        font: { size: 0 },"
    , "        borderWidth: 1,"
    , "        group: c.id"
    , "      };"
    , "    });"
    , "    return dots;"
    , "  }"
    , ""
    , "  // Build overview edges (inter-community connections)"
    , "  function buildOverviewEdges() {"
    , "    const edges = [];"
    , "    const seen = new Set();"
    , "    allEdges.forEach(e => {"
    , "      const srcComm = nodeCommMap[e.from];"
    , "      const tgtComm = nodeCommMap[e.to];"
    , "      if (srcComm !== undefined && tgtComm !== undefined && srcComm !== tgtComm) {"
    , "        const key = Math.min(srcComm, tgtComm) + '-' + Math.max(srcComm, tgtComm);"
    , "        if (!seen.has(key)) {"
    , "          seen.add(key);"
    , "          edges.push({"
    , "            from: 'comm_' + Math.min(srcComm, tgtComm),"
    , "            to: 'comm_' + Math.max(srcComm, tgtComm),"
    , "            color: { color: '#3a3a5e', opacity: 0.3 },"
    , "            width: 1,"
    , "            smooth: false,"
    , "            dashes: true"
    , "          });"
    , "        }"
    , "      }"
    , "    });"
    , "    return edges;"
    , "  }"
    , ""
    , "  // Build drill-down data for a community"
    , "  function buildDrilldownData(cid) {"
    , "    const members = commToNodes[cid] || [];"
    , "    const memberIds = new Set(members.map(n => n.id));"
    , "    const nodes = members.map(n => ({ ...n }));"
    , "    const edges = allEdges.filter(e => memberIds.has(e.from) && memberIds.has(e.to));"
    , "    return { nodes, edges };"
    , "  }"
    , ""
    , "  // Expand a community into drill-down view"
    , "  function expandCommunity(cid) {"
    , "    if (viewerState.depth === 'drilldown') return;"
    , "    dispatch('SET_DEPTH', 'drilldown');"
    , "    dispatch('SET_SELECTION', cid);"
    , "  }"


    , ""
    , "  // Back to overview"
    , "  function backToOverview() {"
    , "    if (viewerState.depth === 'overview') return;"
    , "    dispatch('SET_DEPTH', 'overview');"
    , "    dispatch('SET_SELECTION', null);"
    , "  }"

    , ""
    , "  // Focus on a node in the graph"
    , "  function focusNode(nodeId) {"
    , "    if (viewerState.depth === 'overview') return;"
    , "    if (!drilldownNetwork) return;"
    , "    drilldownNetwork.focus(nodeId, { scale: 1.5, animation: false });"
    , "    drilldownNetwork.selectNodes([nodeId]);"
    , "    showNodeDetail(nodeId);"
    , "  }"

    , ""
    , "  // Show node detail in sidebar"
    , "  function showNodeDetail(nodeId) {"
    , "    let node = null;"
    , "    if (currentPhase === 'drilldown' && drilldownNodesDataset) {"
    , "      const all = drilldownNodesDataset.get();"
    , "      node = all.find(n => n.id === nodeId);"
    , "    }"
    , "    if (!node) return;"
    , "    const info = document.getElementById('selectedInfo');"
    , "    info.style.display = 'block';"
    , "    document.getElementById('selectedLabel').textContent = node.label;"
    , "    document.getElementById('selectedFile').textContent = node.source_file || '';"
    , "    const cid = node.community_id || -1;"
    , "    const agg = communityAggregates.find(c => c.id === cid);"
    , "    document.getElementById('selectedCommunity').textContent = (agg ? agg.label : ('Community ' + cid));"
    , "    // Count neighbors"
    , "    const neighborEdges = (drilldownEdgesDataset ? drilldownEdgesDataset.get() : allEdges).filter(e => e.from === nodeId || e.to === nodeId);"
    , "    const neighborIds = new Set();"
    , "    neighborEdges.forEach(e => {"
    , "      if (e.from === nodeId) neighborIds.add(e.to);"
    , "      if (e.to === nodeId) neighborIds.add(e.from);"
    , "    });"
    , "    document.getElementById('selectedNeighbors').innerHTML = '<span>' + neighborIds.size + '</span> connections';"
    , "  }"
    , ""
    , "  // Build search results HTML"
    , "  function showSearchResults(query) {"
    , "    const el = document.getElementById('searchResults');"
    , "    const list = document.getElementById('resultsList');"
    , "    const countEl = document.getElementById('searchCount');"
    , ""
    , "    if (!query || query.length < 2) {"
    , "      el.classList.remove('active');"
    , "      list.innerHTML = '';"
    , "      countEl.textContent = '';"
    , "      resetHighlight();"
    , "      return;"
    , "    }"
    , ""
    , "    // Try API first if available"
    , "    if (apiAvailable) {"
    , "      fetch('/api/query?q=' + encodeURIComponent(query) + '&mode=bfs')"
    , "        .then(r => r.json())"
    , "        .then(data => renderApiResults(data, el, list, countEl))"
    , "        .catch(() => {"
    , "          apiAvailable = false;"
    , "          renderSubstringResults(query, el, list, countEl);"
    , "        });"
    , "    } else {"
    , "      renderSubstringResults(query, el, list, countEl);"
    , "    }"
    , "  }"
    , ""
    , "  // Render API query results"
    , "  function renderApiResults(data, el, list, countEl) {"
    , "    const verdict = data.verdict || '';"
    , "    const hash = data.hash || '';"
    , "    const suggestions = data.suggestions || [];"
    , "    const nodes = (data.nodes || []).sort((a, b) => (b.score || 0) - (a.score || 0)).slice(0, 20);"
    , ""
    , "    countEl.textContent = verdict + (hash ? ' [hash: ' + hash.substring(0, 8) + ']' : ''); "
    , ""
    , "    let html = '';"
    , "    if (verdict) {"
    , "      html += '<div class=\"search-verdict\">' + escHtml(verdict) + '</div>';"
    , "    }"
    , "    if (suggestions.length > 0) {"
    , "      html += '<div class=\"search-suggestions\">Did you mean: ' + suggestions.map(s => '<a href=\"#\" onclick=\"document.getElementById(\\'searchInput\\').value=' + JSON.stringify(s) + '; doSearch(); return false;\">' + escHtml(s) + '</a>').join(', ') + '</div>';"
    , "    }"
    , "    html += '<div class=\"search-results\">';"
    , "    nodes.forEach(n => {"
    , "      html += '<div class=\"result-item scored\" data-nodeid=\"' + n.id + '\" data-score=\"' + (n.score || 0) + '\">'"
    , "        + '<div class=\"rlabel\">' + escHtml(n.label || n.id) + '</div>'"
    , "        + '<div class=\"rfile\">' + escHtml(shortPath(n.source_file || n.sourceFile || '')) + '</div>'"
    , "        + '<div class=\"rcommunity\">Community ' + (n.community_id || '?') + ' — score: ' + (n.score || 0).toFixed(4) + '</div>'"
    , "        + '</div>';"
    , "    });"
    , "    html += '</div>';"
    , ""
    , "    list.innerHTML = html;"
    , "    el.classList.add('active');"
    , ""
    , "    // Bind click to focus node"
    , "    list.querySelectorAll('.result-item[data-nodeid]').forEach(item => {"
    , "      item.addEventListener('click', function() {"
    , "        const nid = this.getAttribute('data-nodeid');"
    , "        focusNode(nid);"
    , "        highlightSubgraph([nid]);"
    , "      });"
    , "    });"
    , "    // Also highlight all result nodes"
    , "    if (nodes.length > 0) {"
    , "      const ids = nodes.map(n => n.id);"
    , "      highlightSubgraph(ids);"
    , "    }"
    , "  }"
    , ""
    , "  // Fallback: client-side substring filter"
    , "  function renderSubstringResults(query, el, list, countEl) {"
    , "    const q = query.toLowerCase();"
    , "    const matches = allNodes.filter(n =>"
    , "      n.label.toLowerCase().includes(q) ||"
    , "      (n.source_file && n.source_file.toLowerCase().includes(q))"
    , "    );"
    , ""
    , "    countEl.textContent = matches.length + ' found';"
    , ""
    , "    if (matches.length === 0) {"
    , "      el.classList.add('active');"
    , "      list.innerHTML = '<div class=\"no-results\">No notes found for \"' + query + '\"</div>';"
    , "      return;"
    , "    }"
    , ""
    , "    // Sort: doc nodes first, then h1, then h2, then others"
    , "    const typeOrder = { docresult: 0, h1result: 1, h2result: 2 };"
    , "    matches.sort((a, b) => {"
    , "      const ta = typeOrder[nodeTypeClass(a.id)] ?? 3;"
    , "      const tb = typeOrder[nodeTypeClass(b.id)] ?? 3;"
    , "      if (ta !== tb) return ta - tb;"
    , "      return a.label.localeCompare(b.label);"
    , "    });"
    , ""
    , "    const shown = matches.slice(0, 50);"
    , "    list.innerHTML = shown.map(n =>"
    , "      '<div class=\"result-item ' + nodeTypeClass(n.id) + '\" data-nodeid=\"' + n.id + '\">'"
    , "      + '<div class=\"rlabel\">' + escHtml(n.label) + '</div>'"
    , "      + '<div class=\"rfile\">' + escHtml(shortPath(n.source_file)) + '</div>'"
    , "      + '<div class=\"rcommunity\">Community ' + (n.community_id || '?') + '</div>'"
    , "      + '</div>'"
    , "    ).join('');"
    , ""
    , "    el.classList.add('active');"
    , ""
    , "    // Bind click to focus node"
    , "    list.querySelectorAll('.result-item').forEach(item => {"
    , "      item.addEventListener('click', function() {"
    , "        const nid = this.getAttribute('data-nodeid');"
    , "        focusNode(nid);"
    , "      });"
    , "    });"
    , "  }"
    , ""
    , "  // Highlight a subgraph: matched nodes bright, others dimmed"
    , "  function highlightSubgraph(nodeIds) {"
    , "    const idSet = new Set(nodeIds);"
    , "    const activeDataset = (currentPhase === 'drilldown' && drilldownNodesDataset) || overviewNodesDataset;"
    , "    if (!activeDataset) return;"
    , "    const nodes = activeDataset.get();"
    , "    const colors = {};"
    , "    nodes.forEach(n => {"
    , "      if (n.color && n.color.background) colors[n.id] = n.color.background;"
    , "    });"
    , "    const highlighted = nodes.map(n => {"
    , "      if (idSet.has(n.id)) {"
    , "        return { ...n, color: { background: '#fbbf24', opacity: 1 }, size: (n.size || 10) * 1.5, borderWidth: 3 };"
    , "      } else {"
    , "        return { ...n, color: { background: n.color && n.color.background ? n.color.background : '#888', opacity: 0.2 }, borderWidth: 1 };"
    , "      }"
    , "    });"
    , "    activeDataset.update(highlighted);"
    , "    currentHighlightedNodes = { dataset: activeDataset, colors: colors, ids: nodeIds };"
    , "  }"
    , ""
    , "  function resetHighlight() {"
    , "    if (!currentHighlightedNodes) return;"
    , "    const { dataset, colors, ids } = currentHighlightedNodes;"
    , "    const nodes = dataset.get();"
    , "    const restored = nodes.map(n => {"
    , "      if (colors[n.id]) {"
    , "        return { ...n, color: { background: colors[n.id], opacity: 1 }, borderWidth: 1 };"
    , "      } else {"
    , "        return { ...n, color: { background: '#888', opacity: 1 }, borderWidth: 1 };"
    , "      }"
    , "    });"
    , "    dataset.update(restored);"
    , "    currentHighlightedNodes = null;"
    , "  }"
    , ""
    , "  // Debounce helper"
    , "  let debounceTimer = null;"
    , "  function debounce(fn, ms) {"
    , "    return function(...args) {"
    , "      clearTimeout(debounceTimer);"
    , "      debounceTimer = setTimeout(() => fn.apply(this, args), ms);"
    , "    };"
    , "  }"
    , ""
    , "  // Initialize overview phase"
    , "  let overviewData = null;"
    , "  let overviewEdgesData = null;"
    , ""
    , "  function initOverview() {"
    , "    if (typeof vis === 'undefined' || window._visLoadFailed) {"
    , "      const loading = document.getElementById('loading');"
    , "      loading.innerHTML = '<div style=\"text-align:center;max-width:400px\">'"
    , "        + '<div style=\"font-size:18px;color:#f87171;margin-bottom:8px\">Could not load vis-network</div>'"
    , "        + '<div style=\"font-size:12px;color:#888\">The graph visualization library failed to load from the CDN.</div>'"
    , "        + '<div style=\"font-size:11px;color:#666;margin-top:8px\">Check your internet connection, or serve via:</div>'"
    , "        + '<div style=\"font-size:11px;color:#7dd3fc;margin-top:4px;font-family:monospace\">graphos serve --dir graphos-out --port 8080</div>'"
    , "        + '</div>';"
    , "      return;"
    , "    }"
    , ""
    , "    overviewData = buildOverviewData();"
    , "    overviewEdgesData = buildOverviewEdges();"
    , ""
    , "    const container = document.getElementById('graph');"
    , "    document.getElementById('loading').style.display = 'none';"
    , ""
    , "    overviewNodesDataset = new vis.DataSet(overviewData);"
    , "    overviewEdgesDataset = new vis.DataSet(overviewEdgesData);"
    , ""
    , "    const options = {"
    , "      nodes: {"
    , "        shape: 'dot',"
    , "        size: 10,"
    , "        font: { size: 0 },"
    , "        borderWidth: 1,"
    , "        shadow: { enabled: false }"
    , "      },"
    , "      edges: {"
    , "        arrows: {},"
    , "        color: { color: '#3a3a5e', opacity: 0.3 },"
    , "        shadow: { enabled: false },"
    , "        smooth: false,"
    , "        dashes: true,"
    , "        width: 1"
    , "      },"
    , "      physics: {"
    , "        enabled: true,"
    , "        stabilization: { enabled: true, iterations: 300, fit: true },"
    , "        forceAtlas2Based: {"
    , "          type: 'ForceAtlas2', "
    , "          gravitationalConstant: -50,"
    , "          centralGravity: 0.01,"
    , "          springLength: 100,"
    , "          springConstant: 0.001,"
    , "          damping: 0.4"
    , "        },"
    , "        maxVelocity: 50,"
    , "        solver: 'forceAtlas2Based'"
    , "      },"
    , "      interaction: {"
    , "        hover: true,"
    , "        tooltipDelay: 200,"
    , "        navigationButtons: false,"
    , "        keyboard: false,"
    , "        zoomView: true,"
    , "        dragView: true,"
    , "        dragCanvas: true,"
    , "        hideEdgesOnDrag: true,"
    , "        hideEdgesOnZoom: true"
    , "      }"
    , "    };"
    , ""
    , "    overviewNetwork = new vis.Network(container, { nodes: overviewNodesDataset, edges: overviewEdgesDataset }, options);"
    , ""
    , "    overviewNetwork.once('stabilizationIterationsDone', function() {"
    , "      overviewNetwork.setOptions({ physics: { enabled: false } });"
    , "    });"
    , ""
    , "    overviewNetwork.on('click', function(params) {"
    , "      if (params.nodes.length > 0) {"
    , "        const nodeId = params.nodes[0];"
    , "        if (nodeId.startsWith('comm_')) {"
    , "          const cid = parseInt(nodeId.substring(5), 10);"
    , "          expandCommunity(cid);"
    , "        }"
    , "      }"
    , "    });"
    , "  }"
    , ""
    , "  // Wire search input"
    , "  document.addEventListener('DOMContentLoaded', function() {"
    , "    renderCommunityList();"
    , "    initOverview();"
    , ""
    , "    const input = document.getElementById('searchInput');"
    , "    const btn = document.getElementById('btnReset');"
    , "    const btnBack = document.getElementById('btnBack');"
    , "    const doSearch = debounce(function() {"
    , "      const q = input.value.trim();"
    , "      showSearchResults(q);"
    , "      btn.style.display = q.length >= 2 ? 'inline-block' : 'none';"
    , "    }, 200);"
    , "    input.addEventListener('input', doSearch);"
    , "    input.addEventListener('keydown', function(e) {"
    , "      if (e.key === 'Escape') { input.value = ''; doSearch(); }"
    , "    });"
    , "    btn.addEventListener('click', function() {"
    , "      input.value = '';"
    , "      showSearchResults('');"
    , "      btn.style.display = 'none';"
    , "      document.getElementById('searchCount').textContent = '';"
    , "      resetHighlight();"
    , "    });"
    , "    btnBack.addEventListener('click', function() {"
    , "      backToOverview();"
    , "    });"
    , "  });"
    , ""
    , "</script>"
    , "</body></html>"
    ]

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
          , vcaRepresentativeLabels   = take 3 [truncateLabel (sanitize (nodeLabel n)) | nid <- take 10 members, Just n <- [Map.lookup nid nodeMap]]
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

