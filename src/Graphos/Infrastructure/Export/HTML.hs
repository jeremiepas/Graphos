-- | HTML export - two-phase LOD (level-of-detail) graph visualization
-- Phase 1: Overview showing one dot per community (forceAtlas2Based physics)
-- Phase 2: Drill-down expanding a single community into member nodes
-- Embeds JSON data inline for self-contained HTML that works from file://
-- Streams to handle to reduce peak memory (avoids building full HTML Text in memory).
module Graphos.Infrastructure.Export.HTML
  ( exportHTML
  ) where

import Data.Aeson (ToJSON(..), object, (.=), encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (IOMode(..), hFlush, hClose, openFile, hPutStr)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges, articulationPoints)
import Graphos.Domain.Community (cohesionScore)
import qualified Data.Set as Set

-- | Community color palette (distinct, accessible colors)
communityColors :: [Text]
communityColors =
  [ "#7dd3fc", "#f472b6", "#34d399", "#fbbf24", "#a78bfa"
  , "#fb923c", "#2dd4bf", "#f87171", "#818cf8", "#4ade80"
  , "#e879f9", "#38bdf8", "#facc15", "#fb7185", "#22d3ee"
  , "#c084fc"
  ]

colorForCommunity :: Int -> Text
colorForCommunity cid = communityColors !! (cid `mod` length communityColors)

-- | Export graph as interactive HTML with two-phase LOD viewer
exportHTML :: Graph -> Analysis -> FilePath -> IO ()
exportHTML g analysis htmlPath = do
  h <- openFile htmlPath WriteMode
  -- Write HTML header + CSS + sidebar (static content)
  hPutStr h $ T.unpack (htmlHeader g analysis)
  -- Stream nodes JSON directly to handle
  hPutStr h "  const _nodesData = "
  BSL.hPut h (encode (nodesToJSON g (analysisCommunities analysis) (articulationPoints g)))
  hPutStr h ";\n"
  -- Stream edges JSON directly to handle
  hPutStr h "  const _edgesData = "
  BSL.hPut h (encode (edgesToJSON g))
  hPutStr h ";\n"
  -- Stream community aggregates JSON
  hPutStr h "  const _communityAggregatesData = "
  BSL.hPut h (encode (communityAggregatesToJSON g (analysisCommunities analysis)))
  hPutStr h ";\n"
  -- Write the rest of the HTML (JS + closing tags)
  hPutStr h $ T.unpack htmlBody
  hFlush h
  hClose h

-- | HTML header: everything before the embedded JSON data.
htmlHeader :: Graph -> Analysis -> Text
htmlHeader g analysis =
  T.unlines
    [ "<!DOCTYPE html>"
    , "<html lang='en'><head>"
    , "<meta charset='utf-8'>"
    , "<meta name='viewport' content='width=device-width, initial-scale=1'>"
    , "<title>Graphos Knowledge Graph</title>"
    , "<script src='https://unpkg.com/vis-network/standalone/umd/vis-network.min.js' onerror=\"window._visLoadFailed=true\"></script>"
    , "<style>"
    , "  * { box-sizing: border-box; margin: 0; padding: 0; }"
    , "  body { background: #0f0f1a; color: #e0e0e0; font-family: 'Inter', -apple-system, sans-serif; height: 100vh; display: flex; flex-direction: column; overflow: hidden; }"
    , "  header { background: #1a1a2e; padding: 10px 20px; border-bottom: 1px solid #2a2a4e; display: flex; justify-content: space-between; align-items: center; flex-shrink: 0; }"
    , "  header h1 { font-size: 18px; color: #7dd3fc; }"
    , "  header .stats { font-size: 11px; color: #888; margin-top: 2px; }"
    , "  .search-box { display: flex; align-items: center; gap: 8px; }"
    , "  .search-box input { background: #252540; border: 1px solid #3a3a5e; border-radius: 6px; padding: 7px 14px; color: #e0e0e0; font-size: 13px; width: 260px; outline: none; }"
    , "  .search-box input:focus { border-color: #7dd3fc; }"
    , "  .search-box input::placeholder { color: #666; }"
    , "  .search-box .search-count { font-size: 11px; color: #888; min-width: 60px; }"
    , "  .layout { display: flex; flex: 1; min-height: 0; }"
    , "  #graph { flex: 1; position: relative; }"
    , "  #graph canvas { outline: none; }"
    , "  #sidebar { width: 320px; background: #1a1a2e; border-left: 1px solid #2a2a4e; padding: 0; overflow-y: auto; flex-shrink: 0; display: flex; flex-direction: column; }"
    , "  .sidebar-section { padding: 14px 16px; }"
    , "  .sidebar-section + .sidebar-section { border-top: 1px solid #2a2a4e; }"
    , "  #sidebar h3 { color: #7dd3fc; font-size: 13px; margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.5px; }"
    , "  #sidebar p { font-size: 12px; color: #888; margin-bottom: 10px; }"
    , "  #searchResults { display: none; }"
    , "  #searchResults.active { display: block; }"
    , "  .result-item { background: #252540; border-radius: 6px; padding: 10px 12px; margin-bottom: 6px; font-size: 12px; cursor: pointer; border-left: 3px solid #7dd3fc; transition: background 0.15s; }"
    , "  .result-item:hover { background: #2a2a50; }"
    , "  .result-item .rlabel { color: #e0e0e0; font-weight: 600; font-size: 13px; margin-bottom: 3px; }"
    , "  .result-item .rfile { color: #7dd3fc; font-size: 11px; word-break: break-all; margin-bottom: 3px; }"
    , "  .result-item .rcommunity { color: #4ade80; font-size: 10px; }"
    , "  .result-item.h1result { border-left-color: #fbbf24; }"
    , "  .result-item.h2result { border-left-color: #a78bfa; }"
    , "  .result-item.docresult { border-left-color: #7dd3fc; }"
    , "  .selected-info { background: #252540; border-radius: 6px; padding: 10px 12px; margin-bottom: 8px; font-size: 12px; display: none; }"
    , "  .selected-info .label { color: #e0e0e0; font-weight: bold; font-size: 14px; margin-bottom: 4px; }"
    , "  .selected-info .file { color: #7dd3fc; font-size: 11px; word-break: break-all; margin-bottom: 4px; }"
    , "  .selected-info .community-tag { display: inline-block; background: #1a1a2e; padding: 2px 8px; border-radius: 4px; font-size: 10px; color: #4ade80; }"
    , "  .selected-info .neighbors { margin-top: 6px; font-size: 11px; color: #888; }"
    , "  .selected-info .neighbors span { color: #fbbf24; }"
    , "  .comm-item { background: #252540; border-radius: 6px; padding: 8px 10px; margin-bottom: 6px; font-size: 12px; cursor: pointer; }"
    , "  .comm-item:hover { background: #2a2a50; }"
    , "  .comm-item strong { color: #e0e0e0; }"
    , "  .comm-item .members { color: #888; }"
    , "  .comm-item .cohesion { color: #4ade80; font-size: 11px; }"
    , "  .legend { margin-top: 0; padding-top: 0; }"
    , "  .legend-item { display: flex; align-items: center; gap: 6px; font-size: 11px; margin-bottom: 4px; }"
    , "  .legend-dot { width: 10px; height: 10px; border-radius: 50%; flex-shrink: 0; }"
    , "  #loading { position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); font-size: 16px; color: #7dd3fc; z-index: 10; }"
    , "  .no-results { color: #888; font-size: 12px; text-align: center; padding: 20px; }"
    , "  .btn-reset { background: #252540; border: 1px solid #3a3a5e; color: #7dd3fc; padding: 4px 10px; border-radius: 4px; cursor: pointer; font-size: 11px; }"
    , "  .btn-reset:hover { background: #2a2a50; }"
    , "  .btn-back { background: #252540; border: 1px solid #3a3a5e; color: #7dd3fc; padding: 4px 10px; border-radius: 4px; cursor: pointer; font-size: 11px; margin-right: 8px; display: none; }"
    , "  .btn-back:hover { background: #2a2a50; }"
    , "  .sidebar-scroll { flex: 1; overflow-y: auto; }"
    , "  .overview-hint { color: #888; font-size: 11px; text-align: center; padding: 10px; }"
    , "</style>"
    , "</head><body>"
    , "<header>"
    , "  <div>"
    , "    <h1>Graphos Knowledge Graph</h1>"
    , "    <div class='stats'>" <> statsText <> "</div>"
    , "  </div>"
    , "  <div class='search-box'>"
    , "    <button class='btn-back' id='btnBack' title='Back to overview'>← Back</button>"
    , "    <input type='text' id='searchInput' placeholder='Search notes...' autocomplete='off' />"
    , "    <span class='search-count' id='searchCount'></span>"
    , "    <button class='btn-reset' id='btnReset' style='display:none' title='Clear search and show full graph'>Reset</button>"
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
    , "      <div class='sidebar-section' id='nodeDetail'>"
    , "        <div class='selected-info' id='selectedInfo'>"
    , "          <div class='label' id='selectedLabel'></div>"
    , "          <div class='file' id='selectedFile'></div>"
    , "          <div class='community-tag' id='selectedCommunity'></div>"
    , "          <div class='neighbors' id='selectedNeighbors'></div>"
    , "        </div>"
    , "      </div>"
    , "      <div class='sidebar-section' id='communitiesSection'>"
    , "        <h3>Communities</h3>"
    , "        <p id='phaseHint'>Click a community dot to explore.</p>"
    , "        <div id='communityList'></div>"
    , "      </div>"
    , "      <div class='sidebar-section'>"
    , "        <div class='legend'>"
    , "          <h3>Legend</h3>"
    , "          <div class='legend-item'><div class='legend-dot' style='background:#7dd3fc'></div> Community member</div>"
    , "          <div class='legend-item'><div class='legend-dot' style='background:#fbbf24'></div> H1 heading (document)</div>"
    , "          <div class='legend-item'><div class='legend-dot' style='background:#a78bfa'></div> H2 heading (section)</div>"
    , "          <div class='legend-item'><div style='width:20px; border-top: 2px dashed #6a6a8a'></div> Bridge edge</div>"
    , "        </div>"
    , "      </div>"
    , "    </div>"
    , "  </div>"
    , "</div>"
    , "<script>"
    , "  // Embedded graph data (self-contained HTML, no fetch needed)"
    ]
  where
    statsText = T.pack $ show (Map.size $ gNodes g) ++ " nodes, "
              ++ show (Map.size $ gEdges g) ++ " edges, "
              ++ show (length $ analysisCommunities analysis) ++ " communities"

-- | HTML body: JavaScript code and closing tags.
htmlBody :: Text
htmlBody =
  T.unlines
    [ ""
    , "  let allNodes = _nodesData;"
    , "  let allEdges = _edgesData;"
    , "  let communityAggregates = _communityAggregatesData;"
    , "  let overviewNetwork = null;"
    , "  let drilldownNetwork = null;"
    , "  let overviewNodesDataset = null;"
    , "  let overviewEdgesDataset = null;"
    , "  let drilldownNodesDataset = null;"
    , "  let drilldownEdgesDataset = null;"
    , "  let currentPhase = 'overview';"
    , "  let expandedCommunity = null;"
    , "  let nodeCommMap = {};"
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
    , "    const items = communityAggregates.sort((a, b) => b.member_count - a.member_count).slice(0, 50);"
    , "    container.innerHTML = items.map(c =>"
    , "      '<div class=\"comm-item\" data-community=\"' + c.id + '\">'"
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
    , "        const cid = this.getAttribute('data-community');"
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
    , "        title: 'Community ' + c.id + ': ' + c.member_count + ' members, cohesion: ' + c.cohesion.toFixed(3),"
    , "        color: { background: c.color, border: '#1a1a2e', highlight: c.color, hover: c.color },"
    , "        size: size,"
    , "        shape: 'dot',"
    , "        font: { size: 0 },"
    , "        borderWidth: 1,"
    , "        group: parseInt(c.id)"
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
    , "    if (currentPhase === 'drilldown') return;"
    , "    expandedCommunity = cid;"
    , "    currentPhase = 'drilldown';"
    , ""
    , "    const { nodes, edges } = buildDrilldownData(cid);"
    , "    const agg = communityAggregates.find(c => c.id === cid);"
    , "    const color = agg ? agg.color : '#7dd3fc';"
    , ""
    , "    // Update nodes with community color"
    , "    nodes.forEach(n => {"
    , "      n.color = { background: color, border: '#1a1a2e', highlight: color, hover: color };"
    , "    });"
    , ""
    , "    // Remove old network if exists"
    , "    if (drilldownNetwork) {"
    , "      const container = document.getElementById('graph');"
    , "      const oldCanvas = container.querySelector('canvas');"
    , "      if (oldCanvas) oldCanvas.remove();"
    , "      drilldownNetwork.destroy();"
    , "      drilldownNetwork = null;"
    , "    }"
    , ""
    , "    const container = document.getElementById('graph');"
    , "    document.getElementById('loading').style.display = 'none';"
    , ""
    , "    drilldownNodesDataset = new vis.DataSet(nodes);"
    , "    drilldownEdgesDataset = new vis.DataSet(edges);"
    , ""
    , "    const options = {"
    , "      nodes: {"
    , "        shape: 'dot',"
    , "        size: 12,"
    , "        font: { color: '#e0e0e0', size: 10, face: 'Inter, -apple-system, sans-serif', strokeWidth: 0 },"
    , "        borderWidth: 2,"
    , "        borderWidthSelected: 4,"
    , "        shadow: { enabled: false }"
    , "      },"
    , "      edges: {"
    , "        arrows: { to: { enabled: true, scaleFactor: 0.5, type: 'arrow' } },"
    , "        color: { color: '#8b8baa', highlight: '#7dd3fc', hover: '#a0a0cc' },"
    , "        shadow: { enabled: false },"
    , "        smooth: false,"
    , "        font: { color: '#777', size: 9, strokeWidth: 0, align: 'middle' }"
    , "      },"
    , "      physics: {"
    , "        enabled: true,"
    , "        stabilization: { enabled: true, iterations: 300, fit: true },"
    , "        barnesHut: { gravitationalConstant: -2000, centralGravity: 0.03, springLength: 80, springConstant: 0.05, damping: 0.4 },"
    , "        maxVelocity: 3,"
    , "        minVelocity: 0.2,"
    , "        solver: 'barnesHut'"
    , "      },"
    , "      interaction: {"
    , "        hover: true,"
    , "        tooltipDelay: 200,"
    , "        navigationButtons: false,"
    , "        keyboard: true,"
    , "        zoomView: true,"
    , "        dragView: true"
    , "      }"
    , "    };"
    , ""
    , "    drilldownNetwork = new vis.Network(container, { nodes: drilldownNodesDataset, edges: drilldownEdgesDataset }, options);"
    , ""
    , "    drilldownNetwork.once('stabilizationIterationsDone', function() {"
    , "      drilldownNetwork.setOptions({ physics: { enabled: false } });"
    , "    });"
    , ""
    , "    // Show back button"
    , "    document.getElementById('btnBack').style.display = 'inline-block';"
    , "    document.getElementById('phaseHint').textContent = 'Exploring Community ' + cid + ' — ' + nodes.length + ' nodes';"
    , ""
    , "    // Click node to show details"
    , "    drilldownNetwork.on('click', function(params) {"
    , "      if (params.nodes.length > 0) {"
    , "        showNodeDetail(params.nodes[0]);"
    , "      } else {"
    , "        document.getElementById('selectedInfo').style.display = 'none';"
    , "      }"
    , "    });"
    , "  }"
    , ""
    , "  // Back to overview"
    , "  function backToOverview() {"
    , "    if (currentPhase === 'overview') return;"
    , "    currentPhase = 'overview';"
    , "    expandedCommunity = null;"
    , ""
    , "    // Remove drilldown network"
    , "    if (drilldownNetwork) {"
    , "      const container = document.getElementById('graph');"
    , "      const oldCanvas = container.querySelector('canvas');"
    , "      if (oldCanvas) oldCanvas.remove();"
    , "      drilldownNetwork.destroy();"
    , "      drilldownNetwork = null;"
    , "      drilldownNodesDataset = null;"
    , "      drilldownEdgesDataset = null;"
    , "    }"
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
    , "        solver: 'forceAtlas2Based',"
    , "        hideEdgesOnDrag: true,"
    , "        hideEdgesOnZoom: true"
    , "      },"
    , "      interaction: {"
    , "        hover: true,"
    , "        tooltipDelay: 200,"
    , "        navigationButtons: false,"
    , "        keyboard: false,"
    , "        zoomView: true,"
    , "        dragView: true,"
    , "        dragCanvas: true"
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
    , "          const cid = nodeId.substring(5);"
    , "          expandCommunity(cid);"
    , "        }"
    , "      }"
    , "    });"
    , ""
    , "    document.getElementById('btnBack').style.display = 'none';"
    , "    document.getElementById('phaseHint').textContent = 'Click a community dot to explore.';"
    , "  }"
    , ""
    , "  // Focus on a node in the graph"
    , "  function focusNode(nodeId) {"
    , "    if (currentPhase === 'overview') return;"
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
    , "    document.getElementById('selectedFile').textContent = node.source_file || node.title || '';"
    , "    const cid = node.community_id || -1;"
    , "    const agg = communityAggregates.find(c => c.id === cid);"
    , "    document.getElementById('selectedCommunity').textContent = 'Community ' + cid + (agg ? ' (' + agg.label + ')' : '');'"
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
    , "      return;"
    , "    }"
    , ""
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
    , "        solver: 'forceAtlas2Based',"
    , "        hideEdgesOnDrag: true,"
    , "        hideEdgesOnZoom: true"
    , "      },"
    , "      interaction: {"
    , "        hover: true,"
    , "        tooltipDelay: 200,"
    , "        navigationButtons: false,"
    , "        keyboard: false,"
    , "        zoomView: true,"
    , "        dragView: true,"
    , "        dragCanvas: true"
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
    , "          const cid = nodeId.substring(5);"
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
    , "    });"
    , "    btnBack.addEventListener('click', function() {"
    , "      backToOverview();"
    , "    });"
    , "  });"
    , ""
    , "</script>"
    , "</body></html>"
    ]

-- ───────────────────────────────────────────────
-- JSON data generation (using Aeson, NOT manual string building)
-- ───────────────────────────────────────────────

-- | Node data for JSON export
data VisNode = VisNode
  { vnId         :: Text
  , vnLabel      :: Text
  , vnTitle      :: Text
  , vnSourceFile :: Text
  , vnBgColor    :: Text
  , vnBorder     :: Text
  , vnBw         :: Int
  , vnGroup      :: Int
  , vnCommId     :: Maybe Int
  } deriving (Show)

instance ToJSON VisNode where
  toJSON n = object
    [ "id"             .= vnId n
    , "label"          .= vnLabel n
    , "title"          .= vnTitle n
    , "source_file"    .= vnSourceFile n
    , "color"          .= object ["background" .= vnBgColor n, "border" .= vnBorder n]
    , "borderWidth"    .= vnBw n
    , "group"          .= vnGroup n
     , "community_id" .= vnCommId n
    ]

-- | Edge data for JSON export
data VisEdge = VisEdge
  { veFrom   :: Text
  , veTo     :: Text
  , veTitle  :: Text
  , veLabel  :: Text
  , veDashes :: Bool
  , veWidth  :: Int
  , veColor  :: Text
  } deriving (Show)

instance ToJSON VisEdge where
  toJSON e = object
    [ "from"   .= veFrom e
    , "to"     .= veTo e
    , "title"  .= veTitle e
    , "label"  .= veLabel e
    , "dashes" .= veDashes e
    , "width"  .= veWidth e
    , "color"  .= object ["color" .= veColor e, "highlight" .= ("#7dd3fc" :: Text), "hover" .= ("#a0a0cc" :: Text)]
    , "arrows" .= object ["to" .= object ["enabled" .= True, "scaleFactor" .= (0.5 :: Double)]]
    ]

-- | Community aggregate data for JSON export
data VisCommunityAggregate = VisCommunityAggregate
  { vcaId                     :: Text
  , vcaMemberCount            :: Int
  , vcaCohesion               :: Double
  , vcaBridgeCount            :: Int
  , vcaColor                  :: Text
  , vcaLabel                  :: Text
  , vcaRepresentativeLabels   :: [Text]
  , vcaInterCommunityEdges    :: Int
  } deriving (Show)

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
    ]

-- | Convert graph nodes to JSON (via Aeson, no manual string building)
nodesToJSON :: Graph -> CommunityMap -> [NodeId] -> [VisNode]
nodesToJSON g commMap artPoints =
  let nodeCommMap = Map.fromList [(nid, cid) | (cid, nids) <- Map.toList commMap, nid <- nids]
      artSet = Set.fromList artPoints
      sanitize t = T.filter (\c -> c /= '\n' && c /= '\r' && c /= '"' && c /= '\'' && c /= '`') t
      truncateLabel t = if T.length t > 80 then T.take 80 t <> "…" else t
   in [ VisNode
         { vnId         = sanitize (nodeId n)
         , vnLabel      = truncateLabel (sanitize (nodeLabel n))
         , vnTitle      = sanitize (nodeSourceFile n) <> " [" <> T.pack (show cid) <> "]"
         , vnSourceFile = nodeSourceFile n
         , vnBgColor    = if nid `Set.member` artSet then "#f87171" else colorForCommunity cid
         , vnBorder     = if nid `Set.member` artSet then "#f87171" else "#333"
         , vnBw         = if nid `Set.member` artSet then 3 else 1
         , vnGroup      = cid
         , vnCommId     = Just cid
         }
       | n <- Map.elems (gNodes g)
       , let nid = sanitize (nodeId n)
       , let cid = Map.findWithDefault (-1) nid nodeCommMap
       ]

-- | Convert graph edges to JSON (via Aeson, no manual string building)
edgesToJSON :: Graph -> [VisEdge]
edgesToJSON g =
  let sanitize t = T.filter (\c -> c /= '\n' && c /= '\r' && c /= '"' && c /= '\'' && c /= '`') t
      nodeSet = Set.fromList [sanitize (nodeId n) | n <- Map.elems (gNodes g)]
  in [ VisEdge
      { veFrom   = sanitize (edgeSource e)
      , veTo     = sanitize (edgeTarget e)
      , veTitle  = relLabel
      , veLabel  = relLabel
      , veDashes = isInferred
      , veWidth  = if isInferred then 1 else 2
      , veColor  = if isInferred then "#6a6a8a" else "#8b8baa"
      }
    | ((_, _), e) <- Map.toList (gEdges g)
     , let src = sanitize (edgeSource e)
           tgt = sanitize (edgeTarget e)
           isInferred = case edgeConfidence e of Confidence c -> c < 1.0
           relLabel = relationToText (edgeRelation e)
    , src `Set.member` nodeSet
    , tgt `Set.member` nodeSet
    ]

-- | Convert community aggregates to JSON
communityAggregatesToJSON :: Graph -> CommunityMap -> [VisCommunityAggregate]
communityAggregatesToJSON g commMap =
  let sanitize t = T.filter (\c -> c /= '\n' && c /= '\r' && c /= '"' && c /= '\'' && c /= '`') t
      truncateLabel t = if T.length t > 80 then T.take 80 t <> "…" else t
      artPoints = articulationPoints g
      artSet = Set.fromList artPoints
      nodeMap = gNodes g
      isBridge m = case Map.lookup m nodeMap of
        Just n -> sanitize (nodeId n) `Set.member` artSet
        Nothing -> False
   in [ VisCommunityAggregate
         { vcaId                     = T.pack (show cid)
         , vcaMemberCount            = length members
         , vcaCohesion               = cohesionScore g members
         , vcaBridgeCount            = length [m | m <- members, isBridge m]
         , vcaColor                  = colorForCommunity cid
         , vcaLabel                  = T.pack ("Community " ++ show cid)
         , vcaRepresentativeLabels   = take 3 [truncateLabel (sanitize (nodeLabel n)) | nid <- take 10 members, Just n <- [Map.lookup nid nodeMap]]
         , vcaInterCommunityEdges    = 0 -- Placeholder: computed inter-community edge count
         }
       | (cid, members) <- Map.toList commMap
       ]
