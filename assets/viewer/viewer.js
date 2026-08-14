/** @typedef {{ depth: 'Overview' | 'Community' | 'Full' | 'Custom', selection: string | null, hops: number, facets: Object, searchResults: Array }} ViewerState */

// Expand interned payload into the legacy shape used below
const nodeIdStrings = _payloadData.strings;
const fileStrings = _payloadData.files;
const kindStrings = _payloadData.kinds;
const relationStrings = _payloadData.relations;
const rawNodes = _payloadData.nodes;
const rawEdges = _payloadData.edges;

let allNodes = rawNodes.map(function(n, i) {
  return {
    id: nodeIdStrings[i],
    label: n.label,
    source_file: fileStrings[n.file_idx],
    line: n.line,
    community_id: n.community_id,
    degree: n.degree,
    is_bridge: n.is_bridge,
    kind: kindStrings[n.kind_idx],
    file_type: n.file_type
  };
});

let allEdges = rawEdges.map(function(e) {
  const rel = relationStrings[e[2]];
  return {
    from: nodeIdStrings[e[0]],
    to: nodeIdStrings[e[1]],
    relation: rel,
    title: rel,
    label: rel,
    dashes: rel === 'Inferred',
    width: rel === 'Inferred' ? 1 : 2
  };
});

let communityAggregates = _payloadData.aggregates;
let overviewNodesDataset = null;
let overviewEdgesDataset = null;
let drilldownNodesDataset = null;
let drilldownEdgesDataset = null;

/** @type {ViewerState} */
let viewerState = null;
let network = null;
let nodeCommMap = {};

// Build lookup: nodeId -> communityId
allNodes.forEach(n => {
  if (n.community_id !== undefined && n.community_id !== null) {
    nodeCommMap[n.id] = n.community_id;
  }
});

// Build community -> nodes map
let commToNodes = {};
allNodes.forEach(n => {
  const cid = n.community_id || -1;
  if (!commToNodes[cid]) commToNodes[cid] = [];
  commToNodes[cid].push(n);
});

// Build community -> edges map
let commToEdges = {};
allEdges.forEach(e => {
  const src = e.from;
  const tgt = e.to;
  const srcComm = nodeCommMap[src] || -1;
  const tgtComm = nodeCommMap[tgt] || -1;
  if (!commToEdges[srcComm]) commToEdges[srcComm] = [];
  commToEdges[srcComm].push(e);
});

function initialState() {
  return {
    depth: 'Overview',
    selection: null,
    hops: 2,
    facets: {},
    searchResults: []
  };
}

function saveState(state) {
  sessionStorage.setItem('graphos_viewer_state', JSON.stringify(state));
}

function loadState() {
  try {
    const saved = sessionStorage.getItem('graphos_viewer_state');
    if (saved) {
      const parsed = JSON.parse(saved);
      if (parsed && typeof parsed.depth === 'string') return parsed;
    }
  } catch (e) {
    console.error('Failed to load state from sessionStorage', e);
  }
  return initialState();
}

viewerState = loadState();

function applyState(newState) {
  const oldDepth = viewerState.depth;
  const newDepth = newState.depth;
  viewerState = newState;
  saveState(viewerState);
  
  if (network && oldDepth !== newDepth) {
    network.destroy();
    network = null;
  }
  
  if (typeof render === 'function') render();
}

function dispatch(action, payload) {
  const newState = { ...viewerState };
  switch (action) {
    case 'SET_DEPTH':
      newState.depth = payload;
      break;
    case 'SET_SELECTION':
      newState.selection = payload;
      break;
    case 'SET_HOPS':
      newState.hops = payload;
      break;
    case 'SET_FACETS':
      newState.facets = payload;
      break;
    case 'SET_SEARCH':
      newState.searchResults = payload;
      break;
  }
  applyState(newState);
}

function render() {
  // Update depth buttons
  const depthButtons = document.querySelectorAll('.depth-btn');
  depthButtons.forEach(btn => {
    if (btn.getAttribute('data-depth') === viewerState.depth) {
      btn.classList.add('active');
    } else {
      btn.classList.remove('active');
    }
  });

  // Update hop selector visibility
  const hopSelector = document.getElementById('hopSelector');
  if (hopSelector) {
    hopSelector.style.display = (viewerState.depth === 'Custom') ? 'inline-block' : 'none';
    hopSelector.value = viewerState.hops;
  }

  renderGraph();
}

function getNetworkOptions(depth) {
  const options = {
    nodes: {
      shape: 'dot',
      size: 10,
      font: { size: 0 },
      borderWidth: 1,
      shadow: { enabled: false }
    },
    edges: {
      arrows: {},
      color: { color: '#3a3a5e', opacity: 0.3 },
      shadow: { enabled: false },
      smooth: false,
      dashes: true,
      width: 1
    },
    physics: {
      enabled: true,
      stabilization: { enabled: true, iterations: 300, fit: true },
      forceAtlas2Based: {
        type: 'ForceAtlas2', 
        gravitationalConstant: -50,
        centralGravity: 0.01,
        springLength: 100,
        springConstant: 0.001,
        damping: 0.4
      },
      maxVelocity: 50,
      solver: 'forceAtlas2Based'
    },
    interaction: {
      hover: true,
      tooltipDelay: 200,
      navigationButtons: false,
      keyboard: false,
      zoomView: true,
      dragView: true,
      dragCanvas: true,
      hideEdgesOnDrag: true,
      hideEdgesOnZoom: true
    }
  };
  
  return options;
}

function renderGraph() {
  const container = document.getElementById('graph');
  if (!container) return;

  let nodes, edges, depth;
  depth = viewerState.depth;

  if (depth === 'Overview') {
    nodes = overviewNodesDataset;
    edges = overviewEdgesDataset;
  } else if (depth === 'Community') {
    const cid = viewerState.selection;
    const data = buildDrilldownData(cid);
    nodes = new vis.DataSet(data.nodes);
    edges = new vis.DataSet(data.edges);
    drilldownNodesDataset = nodes;
    drilldownEdgesDataset = edges;
  } else if (depth === 'Full') {
    nodes = new vis.DataSet(allNodes);
    edges = new vis.DataSet(allEdges);
  } else if (depth === 'Custom') {
    const startNodeId = viewerState.selection;
    if (startNodeId) {
      const data = bfs(startNodeId, viewerState.hops);
      nodes = new vis.DataSet(data.nodes);
      edges = new vis.DataSet(data.edges);
    } else {
      return;
    }
  } else {
    return;
  }

  const options = getNetworkOptions(depth);

  if (!network) {
    network = new vis.Network(container, { nodes, edges }, options);
    
    if (depth === 'Overview') {
      network.once('stabilizationIterationsDone', function() {
        network.setOptions({ physics: { enabled: false } });
      });
    }

    network.on('click', function(params) {
      if (params.nodes.length > 0) {
        const nodeId = params.nodes[0];
        if (nodeId.startsWith('comm_')) {
          const cid = parseInt(nodeId.substring(5), 10);
          expandCommunity(cid);
        } else {
          if (viewerState.depth !== 'Overview') {
            focusNode(nodeId);
          }
        }
      }
    });
  } else {
    network.setData({ nodes, edges });
    network.setOptions(options);
  }
}

function bfs(startNodeId, maxHops) {
  const visited = new Map(); // nodeId -> distance
  const queue = [{ id: startNodeId, dist: 0 }];
  visited.set(startNodeId, 0);

  const adj = {};
  allEdges.forEach(e => {
    if (!adj[e.from]) adj[e.from] = [];
    if (!adj[e.to]) adj[e.to] = [];
    adj[e.from].push(e.to);
    adj[e.to].push(e.from);
  });

  while (queue.length > 0) {
    const { id, dist } = queue.shift();
    if (dist >= maxHops) continue;

    const neighbors = adj[id] || [];
    for (const neighbor of neighbors) {
      if (!visited.has(neighbor)) {
        visited.set(neighbor, dist + 1);
        queue.push({ id: neighbor, dist: dist + 1 });
      }
    }
  }

  const nodeIds = Array.from(visited.keys());
  const nodeIdsSet = new Set(nodeIds);
  const nodes = allNodes.filter(n => nodeIdsSet.has(n.id)).map(n => ({ ...n }));
  const edges = allEdges.filter(e => nodeIdsSet.has(e.from) && nodeIdsSet.has(e.to));

  return { nodes, edges };
}

function buildDrilldownData(cid) {
  const members = commToNodes[cid] || [];
  const memberIds = new Set(members.map(n => n.id));
  const nodes = members.map(n => ({ ...n }));
  const edges = allEdges.filter(e => memberIds.has(e.from) && memberIds.has(e.to));
  return { nodes, edges };
}

function expandCommunity(cid) {
  if (viewerState.depth === 'Community') return;
  dispatch('SET_DEPTH', 'Community');
  dispatch('SET_SELECTION', cid);
}

function focusNode(nodeId) {
  if (viewerState.depth === 'Overview') return;
  if (!network) return;
  network.focus(nodeId, { scale: 1.5, animation: false });
  network.selectNodes([nodeId]);
  showNodeDetail(nodeId);
}

function showNodeDetail(nodeId) {
  let node = null;
  if (viewerState.depth === 'Community' && drilldownNodesDataset) {
    const all = drilldownNodesDataset.get();
    node = all.find(n => n.id === nodeId);
  } else {
    // Fallback to allNodes for other depths
    node = allNodes.find(n => n.id === nodeId);
  }

  if (!node) return;
  const info = document.getElementById('selectedInfo');
  info.style.display = 'block';
  document.getElementById('selectedLabel').textContent = node.label;
  document.getElementById('selectedFile').textContent = node.source_file || '';
  const cid = node.community_id || -1;
  const agg = communityAggregates.find(c => c.id === cid);
  document.getElementById('selectedCommunity').textContent = (agg ? agg.label : ('Community ' + cid));
  
  // Count neighbors
  const currentEdges = (viewerState.depth === 'Community' && drilldownEdgesDataset) ? drilldownEdgesDataset.get() : allEdges;
  const neighborEdges = currentEdges.filter(e => e.from === nodeId || e.to === nodeId);
  const neighborIds = new Set();
  neighborEdges.forEach(e => {
    if (e.from === nodeId) neighborIds.add(e.to);
    if (e.to === nodeId) neighborIds.add(e.from);
  });
  document.getElementById('selectedNeighbors').innerHTML = '<span>' + neighborIds.size + '</span> connections';
}

function renderCommunityList() {
  const container = document.getElementById('communityList');
  if (!container) return;
  const sorted = Array.from(communityAggregates).sort((a, b) => b.member_count - a.member_count).slice(0, 50);
  container.innerHTML = sorted.map(c =>
    '<div class=\"comm-item\" data-community=\"' + c.id + '\"'
    + '<strong style=\"color:' + c.color + '\">'
    + 'Community ' + c.id + '</strong>'
    + ' <span class=\"members\">'
    + ' — ' + c.member_count + ' members, '
    + c.bridge_count + ' bridges</span>'
    + '<br><span style=\"color:#4ade80;font-size:10px\">'
    + 'cohesion: ' + c.cohesion.toFixed(3) + '</span>'
    + '<br><span style=\"color:#888;font-size:10px\">'
    + ' ' + escHtml(c.label) + '</span>'
    + '</div>'
  ).join('');

  container.querySelectorAll('.comm-item[data-community]').forEach(item => {
    item.addEventListener('click', function() {
      const cid = parseInt(this.getAttribute('data-community'), 10);
      expandCommunity(cid);
    });
  });
}

function buildOverviewData() {
  return communityAggregates.map(c => {
    const size = Math.max(4, Math.min(20, Math.sqrt(c.member_count) * 2));
    return {
      id: 'comm_' + c.id,
      label: '',
      title: c.label + ' — ' + c.member_count + ' members, cohesion: ' + c.cohesion.toFixed(3),
      color: { background: c.color, border: '#1a1a2e', highlight: c.color, hover: c.color },
      size: size,
      shape: 'dot',
      font: { size: 0 },
      borderWidth: 1,
      group: c.id
    };
  });
}

function buildOverviewEdges() {
  const edges = [];
  const seen = new Set();
  allEdges.forEach(e => {
    const srcComm = nodeCommMap[e.from];
    const tgtComm = nodeCommMap[e.to];
    if (srcComm !== undefined && tgtComm !== undefined && srcComm !== tgtComm) {
      const key = Math.min(srcComm, tgtComm) + '-' + Math.max(srcComm, tgtComm);
      if (!seen.has(key)) {
        seen.add(key);
        edges.push({
          from: 'comm_' + Math.min(srcComm, tgtComm),
          to: 'comm_' + Math.max(srcComm, tgtComm),
          color: { color: '#3a3a5e', opacity: 0.3 },
          width: 1,
          smooth: false,
          dashes: true
        });
      }
    }
  });
  return edges;
}

function escHtml(s) {
  if (!s) return '';
  return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/\"/g,'&quot;');
}

function nodeTypeClass(nodeId) {
  if (nodeId.includes('_doc_')) return 'docresult';
  if (nodeId.includes('_h1_')) return 'h1result';
  if (nodeId.includes('_h2_')) return 'h2result';
  return 'docresult';
}

function shortPath(filePath) {
  if (!filePath) return '';
  const parts = filePath.split('/');
  return parts.slice(-2).join('/');
}

function showSearchResults(query) {
  const el = document.getElementById('searchResults');
  const list = document.getElementById('resultsList');
  const countEl = document.getElementById('searchCount');

  if (!query || query.length < 2) {
    el.classList.remove('active');
    list.innerHTML = '';
    countEl.textContent = '';
    resetHighlight();
    return;
  }

  // For simplicity in this refactor, we use the client-side fallback
  renderSubstringResults(query, el, list, countEl);
}

function renderSubstringResults(query, el, list, countEl) {
  const q = query.toLowerCase();
  const matches = allNodes.filter(n =>
    n.label.toLowerCase().includes(q) ||
    (n.source_file && n.source_file.toLowerCase().includes(q))
  );

  countEl.textContent = matches.length + ' found';

  if (matches.length === 0) {
    el.classList.add('active');
    list.innerHTML = '<div class=\"no-results\">No notes found for \"' + query + '\"</div>';
    return;
  }

  const typeOrder = { docresult: 0, h1result: 1, h2result: 2 };
  matches.sort((a, b) => {
    const ta = typeOrder[nodeTypeClass(a.id)] ?? 3;
    const tb = typeOrder[nodeTypeClass(b.id)] ?? 3;
    if (ta !== tb) return ta - tb;
    return a.label.localeCompare(b.label);
  });

  const shown = matches.slice(0, 50);
  list.innerHTML = shown.map(n =>
    '<div class=\"result-item ' + nodeTypeClass(n.id) + '\" data-nodeid=\"' + n.id + '\">'
    + '<div class=\"rlabel\">' + escHtml(n.label) + '</div>'
    + '<div class=\"rfile\">' + escHtml(shortPath(n.source_file)) + '</div>'
    + '<div class=\"rcommunity\">Community ' + (n.community_id || '?') + '</div>'
    + '</div>'
  ).join('');

  el.classList.add('active');

  list.querySelectorAll('.result-item').forEach(item => {
    item.addEventListener('click', function() {
      const nid = this.getAttribute('data-nodeid');
      focusNode(nid);
    });
  });
}

function highlightSubgraph(nodeIds) {
  const idSet = new Set(nodeIds);
  const activeDataset = (viewerState.depth === 'Community' && drilldownNodesDataset) || overviewNodesDataset;
  if (!activeDataset) return;
  const nodes = activeDataset.get();
  const colors = {};
  nodes.forEach(n => {
    if (n.color && n.color.background) colors[n.id] = n.color.background;
  });
  const highlighted = nodes.map(n => {
    if (idSet.has(n.id)) {
      return { ...n, color: { background: '#fbbf24', opacity: 1 }, size: (n.size || 10) * 1.5, borderWidth: 3 };
    } else {
      return { ...n, color: { background: n.color && n.color.background ? n.color.background : '#888', opacity: 0.2 }, borderWidth: 1 };
    }
  });
  activeDataset.update(highlighted);
  currentHighlightedNodes = { dataset: activeDataset, colors: colors, ids: nodeIds };
}

function resetHighlight() {
  if (!currentHighlightedNodes) return;
  const { dataset, colors, ids } = currentHighlightedNodes;
  const nodes = dataset.get();
  const restored = nodes.map(n => {
    if (colors[n.id]) {
      return { ...n, color: { background: colors[n.id], opacity: 1 }, borderWidth: 1 };
    } else {
      return { ...n, color: { background: '#888', opacity: 1 }, borderWidth: 1 };
    }
  });
  dataset.update(restored);
  currentHighlightedNodes = null;
}

let debounceTimer = null;
function debounce(fn, ms) {
  return function(...args) {
    clearTimeout(debounceTimer);
    debounceTimer = setTimeout(() => fn.apply(this, args), ms);
  };
}

let currentHighlightedNodes = null;

// Initialize
document.addEventListener('DOMContentLoaded', function() {
  // Pre-compute overview data
  overviewNodesDataset = new vis.DataSet(buildOverviewData());
  overviewEdgesDataset = new vis.DataSet(buildOverviewEdges());

  renderCommunityList();
  
  // Initial render
  render();

  // Depth selector
  const depthButtons = document.querySelectorAll('.depth-btn');
  depthButtons.forEach(btn => {
    btn.addEventListener('click', function() {
      const depth = this.getAttribute('data-depth');
      dispatch('SET_DEPTH', depth);
    });
  });

  // Hop selector
  const hopSelector = document.getElementById('hopSelector');
  if (hopSelector) {
    hopSelector.addEventListener('change', function() {
      dispatch('SET_HOPS', parseInt(this.value, 10));
    });
  }

  // Search input
  const input = document.getElementById('searchInput');
  const btn = document.getElementById('btnReset');
  const doSearch = debounce(function() {
    const q = input.value.trim();
    showSearchResults(q);
    btn.style.display = q.length >= 2 ? 'inline-block' : 'none';
  }, 200);
  input.addEventListener('input', doSearch);
  input.addEventListener('keydown', function(e) {
    if (e.key === 'Escape') { input.value = ''; doSearch(); }
  });
  btn.addEventListener('click', function() {
    input.value = '';
    showSearchResults('');
    btn.style.display = 'none';
    document.getElementById('searchCount').textContent = '';
    resetHighlight();
  });
});
