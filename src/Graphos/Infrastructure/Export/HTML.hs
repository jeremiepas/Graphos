-- | HTML export - interactive graph visualization with community coloring
-- Uses external JSON data files to avoid 1MB+ inline JSON that breaks browsers.
module Graphos.Infrastructure.Export.HTML
  ( exportHTML
  ) where

import Data.Aeson (ToJSON(..), object, (.=), encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

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

-- | Export graph as interactive HTML with vis.js
-- Writes three files: graph.html, graph_nodes.json, graph_edges.json
exportHTML :: Graph -> Analysis -> FilePath -> IO ()
exportHTML g analysis htmlPath = do
  let baseDir = takeDirectory htmlPath
      nodesPath = baseDir ++ "/graph_nodes.json"
      edgesPath = baseDir ++ "/graph_edges.json"
      -- Use relative filenames for fetch() (same directory as HTML)
      nodesFetch = "graph_nodes.json"
      edgesFetch = "graph_edges.json"
  
  -- Write node and edge data as separate JSON files
  BSL.writeFile nodesPath (encode (nodesToJSON g (analysisCommunities analysis) (articulationPoints g)))
  BSL.writeFile edgesPath (encode (edgesToJSON g))
  
  -- Write the HTML that loads them
  let html = buildHTML g analysis nodesFetch edgesFetch
  writeFile htmlPath (T.unpack html)

  where
    takeDirectory path = case reverse (dropWhile (/= '/') (reverse path)) of
      ""  -> "."
      dir -> dir

-- | Build the HTML page - loads data via fetch() instead of inlining
buildHTML :: Graph -> Analysis -> FilePath -> FilePath -> Text
buildHTML g analysis nodesPath edgesPath =
  T.unlines
    [ "<!DOCTYPE html>"
    , "<html lang='en'><head>"
    , "<meta charset='utf-8'>"
    , "<meta name='viewport' content='width=device-width, initial-scale=1'>"
    , "<title>Graphos Knowledge Graph</title>"
    , "<script src='https://unpkg.com/vis-network/standalone/umd/vis-network.min.js'></script>"
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
    , "  .sidebar-scroll { flex: 1; overflow-y: auto; }"
    , "</style>"
    , "</head><body>"
    , "<header>"
    , "  <div>"
    , "    <h1>Graphos Knowledge Graph</h1>"
    , "    <div class='stats'>" <> statsText <> "</div>"
    , "  </div>"
    , "  <div class='search-box'>"
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
    , "        <p>Click a community to focus. Click a node for details.</p>"
    , commInfo
    , "      </div>"
    , "      <div class='sidebar-section'>"
    , "        <div class='legend'>"
    , "          <h3>Legend</h3>"
    , "          <div class='legend-item'><div class='legend-dot' style='background:#7dd3fc'></div> Community member</div>"
    , "          <div class='legend-item'><div class='legend-dot' style='background:#fbbf24'></div> H1 heading (document)</div>"
    , "          <div class='legend-item'><div class='legend-dot' style='background:#a78bfa'></div> H2 heading (section)</div>"
    , "          <div class='legend-item'><div class='legend-dot' style='background:#f87171; border: 2px solid #f87171'></div> Bridge node</div>"
    , "          <div class='legend-item'><div style='width:20px; border-top: 2px dashed #6a6a8a'></div> Inferred edge</div>"
    , "        </div>"
    , "      </div>"
    , "    </div>"
    , "  </div>"
    , "</div>"
    , "<script>"
    , "  let allNodes = [];"
    , "  let allEdges = [];"
    , "  let network = null;"
    , "  let nodesDataset = null;"
    , "  let edgesDataset = null;"
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
    , "      + '<div class=\"rcommunity\">Community ' + n.group + '</div>'"
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
    , "  // Focus on a node in the graph"
    , "  function focusNode(nodeId) {"
    , "    if (!network) return;"
    , "    network.focus(nodeId, { scale: 1.5, animation: false });"
    , "    network.selectNodes([nodeId]);"
    , "    showNodeDetail(nodeId);"
    , "  }"
    , ""
    , "  // Show node detail in sidebar"
    , "  function showNodeDetail(nodeId) {"
    , "    const node = allNodes.find(n => n.id === nodeId);"
    , "    if (!node) return;"
    , "    const info = document.getElementById('selectedInfo');"
    , "    info.style.display = 'block';"
    , "    document.getElementById('selectedLabel').textContent = node.label;"
    , "    document.getElementById('selectedFile').textContent = node.source_file || node.title || '';"
    , "    document.getElementById('selectedCommunity').textContent = 'Community ' + node.group;"
    , "    // Count neighbors"
    , "    const neighborEdges = allEdges.filter(e => e.from === nodeId || e.to === nodeId);"
    , "    const neighborIds = new Set();"
    , "    neighborEdges.forEach(e => {"
    , "      if (e.from === nodeId) neighborIds.add(e.to);"
    , "      if (e.to === nodeId) neighborIds.add(e.from);"
    , "    });"
    , "    document.getElementById('selectedNeighbors').innerHTML = '<span>' + neighborIds.size + '</span> connections';"
    , "  }"
    , ""
    , "  // Escape HTML"
    , "  function escHtml(s) {"
    , "    if (!s) return '';"
    , "    return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/\"/g,'&quot;');"
    , "  }"
    , ""
    , "  // Filter graph to show only matching nodes + their neighbors"
    , "  function filterGraph(query) {"
    , "    if (!nodesDataset || !edgesDataset) return;"
    , ""
    , "    if (!query || query.length < 2) {"
    , "      // Reset: show all"
    , "      nodesDataset.update(allNodes.map(n => ({...n, opacity: 1.0, font: { color: '#e0e0e0', size: 14 }})));"
    , "      edgesDataset.update(allEdges.map(e => ({...e, opacity: 1.0})));"
    , "      return;"
    , "    }"
    , ""
    , "    const q = query.toLowerCase();"
    , "    const matchIds = new Set(allNodes"
    , "      .filter(n => n.label.toLowerCase().includes(q) || (n.source_file && n.source_file.toLowerCase().includes(q)))"
    , "      .map(n => n.id));"
    , ""
    , "    // Also include direct neighbors of matched nodes"
    , "    const neighborIds = new Set();"
    , "    allEdges.forEach(e => {"
    , "      if (matchIds.has(e.from)) neighborIds.add(e.to);"
    , "      if (matchIds.has(e.to)) neighborIds.add(e.from);"
    , "    });"
    , ""
    , "    const visibleIds = new Set([...matchIds, ...neighborIds]);"
    , ""
    , "    nodesDataset.update(allNodes.map(n => {"
    , "      const isMatch = matchIds.has(n.id);"
    , "      const isNeighbor = neighborIds.has(n.id);"
    , "      return {"
    , "        ...n,"
    , "        opacity: isMatch ? 1.0 : (isNeighbor ? 0.5 : 0.08),"
    , "        font: { color: isMatch ? '#ffffff' : (isNeighbor ? '#888' : '#333'), size: isMatch ? 16 : 12 }"
    , "      };"
    , "    }));"
    , ""
    , "    edgesDataset.update(allEdges.map(e => {"
    , "      const bothVisible = visibleIds.has(e.from) && visibleIds.has(e.to);"
    , "      const oneMatch = matchIds.has(e.from) || matchIds.has(e.to);"
    , "      return {"
    , "        ...e,"
    , "        opacity: (bothVisible && oneMatch) ? 0.8 : 0.05"
    , "      };"
    , "    }));"
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
    , "  async function loadGraph() {"
    , "    const container = document.getElementById('graph');"
    , ""
    , "    // Fetch JSON data with proper error handling"
    , "    let nodesResp, edgesResp;"
    , "    try {"
    , "      nodesResp = await fetch('" <> T.pack nodesPath <> "');"
    , "      if (!nodesResp.ok) throw new Error('Cannot load " <> T.pack nodesPath <> " (HTTP ' + nodesResp.status + '). Serve this directory with a web server, not file://.');"
    , "    } catch(e) {"
    , "      throw new Error('Fetch failed for " <> T.pack nodesPath <> ": ' + e.message + '. Use: python3 -m http.server in the output directory.');"
    , "    }"
    , "    try {"
    , "      edgesResp = await fetch('" <> T.pack edgesPath <> "');"
    , "      if (!edgesResp.ok) throw new Error('Cannot load " <> T.pack edgesPath <> " (HTTP ' + edgesResp.status + ').');"
    , "    } catch(e) {"
    , "      throw new Error('Fetch failed for " <> T.pack edgesPath <> ": ' + e.message);"
    , "    }"
    , ""
    , "    allNodes = await nodesResp.json();"
    , "    allEdges = await edgesResp.json();"
    , "    document.getElementById('loading').style.display = 'none';"
    , ""
    , "    // Create vis.js datasets for dynamic updates"
    , "    nodesDataset = new vis.DataSet(allNodes);"
    , "    edgesDataset = new vis.DataSet(allEdges);"
    , ""
    , "    const options = {"
    , "      nodes: {"
    , "        shape: 'dot',"
    , "        size: 22,"
    , "        font: { color: '#e0e0e0', size: 14, face: 'Inter, -apple-system, sans-serif', strokeWidth: 0 },"
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
    , "        barnesHut: { gravitationalConstant: -8000, centralGravity: 0.05, springLength: 150, springConstant: 0.01, damping: 0.5 },"
    , "        maxVelocity: 5,"
    , "        minVelocity: 0.5,"
    , "        solver: 'barnesHut'"
    , "      },"
    , "      interaction: {"
    , "        hover: true,"
    , "        tooltipDelay: 200,"
    , "        navigationButtons: true,"
    , "        keyboard: true,"
    , "        zoomView: true,"
    , "        dragView: true"
    , "      }"
    , "    };"
    , ""
    , "    network = new vis.Network(container, { nodes: nodesDataset, edges: edgesDataset }, options);"
    , ""
    , "    // After stabilization, disable physics so graph is static and readable"
    , "    network.once('stabilizationIterationsDone', function() {"
    , "      network.setOptions({ physics: { enabled: false } });"
    , "    });"
    , ""
    , "    // Click node to show details"
    , "    network.on('click', function(params) {"
    , "      if (params.nodes.length > 0) {"
    , "        showNodeDetail(params.nodes[0]);"
    , "      } else {"
    , "        document.getElementById('selectedInfo').style.display = 'none';"
    , "      }"
    , "    });"
    , "  }"
    , ""
    , "  // Wire search input"
    , "  document.addEventListener('DOMContentLoaded', function() {"
    , "    const input = document.getElementById('searchInput');"
    , "    const btn = document.getElementById('btnReset');"
    , "    const doSearch = debounce(function() {"
    , "      const q = input.value.trim();"
    , "      showSearchResults(q);"
    , "      filterGraph(q);"
    , "      btn.style.display = q.length >= 2 ? 'inline-block' : 'none';"
    , "    }, 200);"
    , "    input.addEventListener('input', doSearch);"
    , "    input.addEventListener('keydown', function(e) {"
    , "      if (e.key === 'Escape') { input.value = ''; doSearch(); }"
    , "    });"
    , "    btn.addEventListener('click', function() {"
    , "      input.value = '';"
    , "      showSearchResults('');"
    , "      filterGraph('');"
    , "      btn.style.display = 'none';"
    , "      document.getElementById('searchCount').textContent = '';"
    , "    });"
    , ""
    , "    // Community items: click to focus on that community in the graph"
    , "    document.querySelectorAll('.comm-item[data-community]').forEach(item => {"
    , "      item.addEventListener('click', function() {"
    , "        const cid = this.getAttribute('data-community');"
    , "        if (!network || !nodesDataset) return;"
    , "        const matchIds = allNodes.filter(n => String(n.group) === cid).map(n => n.id);"
    , "        if (matchIds.length === 0) return;"
    , "        // Highlight this community, dim others"
    , "        nodesDataset.update(allNodes.map(n => {"
    , "          const inComm = String(n.group) === cid;"
    , "          return { ...n, opacity: inComm ? 1.0 : 0.08, font: { color: inComm ? '#ffffff' : '#333', size: inComm ? 16 : 12 } };"
    , "        }));"
    , "        edgesDataset.update(allEdges.map(e => {"
    , "          const bothInComm = matchIds.includes(e.from) && matchIds.includes(e.to);"
    , "          return { ...e, opacity: bothInComm ? 0.8 : 0.05 };"
    , "        }));"
    , "        network.fit({ nodes: matchIds, animation: false });"
    , "        input.value = ''; btn.style.display = 'none';"
    , "        document.getElementById('searchCount').textContent = '';"
    , "      });"
    , "    });"
    , "  });"
    , ""
    , "  loadGraph().catch(err => {"
    , "    const loading = document.getElementById('loading');"
    , "    loading.innerHTML = '<div style=\"text-align:center;max-width:400px\">'"
    , "      + '<div style=\"font-size:18px;color:#f87171;margin-bottom:8px\">Error loading graph</div>'"
    , "      + '<div style=\"font-size:12px;color:#888\">' + escHtml(err.message) + '</div>'"
    , "      + '<div style=\"font-size:11px;color:#666;margin-top:8px\">Serve the output directory with a web server:</div>'"
    , "      + '<div style=\"font-size:11px;color:#7dd3fc;margin-top:4px;font-family:monospace\">cd graphos-out && python3 -m http.server 8080</div>'"
    , "      + '</div>';"
    , "  });"
    , "</script>"
    , "</body></html>"
    ]
  where
    statsText = T.pack $ show (Map.size $ gNodes g) ++ " nodes, "
             ++ show (Map.size $ gEdges g) ++ " edges, "
             ++ show (length $ analysisCommunities analysis) ++ " communities, "
             ++ show (length $ articulationPoints g) ++ " bridge nodes"
    commInfo = communitiesToHTML (analysisCommunities analysis) g

-- | Convert communities to sidebar HTML with cohesion scores
-- Community items are clickable to focus the graph view on that community
communitiesToHTML :: CommunityMap -> Graph -> Text
communitiesToHTML commMap g =
  let items = [T.unlines
        [ "    <div class='comm-item' data-community='" <> T.pack (show cid) <> "'>"
        , "      <strong style='color:" <> colorForCommunity cid <> "'>Community " <> T.pack (show cid) <> "</strong>"
        , "      <span class='members'> — " <> T.pack (show (length members)) <> " members</span>"
        , "      <br><span class='cohesion'>cohesion: " <> T.pack (take 5 (show (cohesionScore g members))) <> "</span>"
        , "      <br><span class='members'>" <> T.intercalate ", " (take 3 [nodeLabel n | nid <- members, Just n <- [Map.lookup nid (gNodes g)]]) <> "</span>"
        , "    </div>"
        ]
        | (cid, members) <- Map.toList commMap]
  in T.concat items

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
  } deriving (Show)

instance ToJSON VisNode where
  toJSON n = object
    [ "id"          .= vnId n
    , "label"       .= vnLabel n
    , "title"       .= vnTitle n
    , "source_file" .= vnSourceFile n
    , "color"       .= object ["background" .= vnBgColor n, "border" .= vnBorder n]
    , "borderWidth" .= vnBw n
    , "group"       .= vnGroup n
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

-- | Convert graph nodes to JSON (via Aeson, no manual string building)
nodesToJSON :: Graph -> CommunityMap -> [NodeId] -> [VisNode]
nodesToJSON g commMap artPoints =
  let nodeCommMap = Map.fromList [(nid, cid) | (cid, nids) <- Map.toList commMap, nid <- nids]
      artSet = Set.fromList artPoints
   in [ VisNode
        { vnId         = nodeId n
        , vnLabel      = nodeLabel n
        , vnTitle      = nodeSourceFile n <> " [" <> T.pack (show cid) <> "]"
        , vnSourceFile = nodeSourceFile n
        , vnBgColor    = if nid `Set.member` artSet then "#f87171" else colorForCommunity cid
        , vnBorder     = if nid `Set.member` artSet then "#f87171" else "#333"
        , vnBw         = if nid `Set.member` artSet then 3 else 1
        , vnGroup      = cid
        }
      | n <- Map.elems (gNodes g)
      , let nid = nodeId n
      , let cid = Map.findWithDefault (-1) nid nodeCommMap
      ]

-- | Convert graph edges to JSON (via Aeson, no manual string building)
edgesToJSON :: Graph -> [VisEdge]
edgesToJSON g =
  [ VisEdge
    { veFrom   = edgeSource e
    , veTo     = edgeTarget e
    , veTitle  = relLabel
    , veLabel  = relLabel
    , veDashes = isInferred
    , veWidth  = if isInferred then 1 else 2
    , veColor  = if isInferred then "#6a6a8a" else "#8b8baa"
    }
  | ((_, _), e) <- Map.toList (gEdges g)
  , let isInferred = edgeConfidence e == Inferred
        relLabel = relationToText (edgeRelation e)
  ]