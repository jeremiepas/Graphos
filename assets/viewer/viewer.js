/* Graphos HTML viewer. Self-contained; all data inline via _payloadData.
   Depth: Overview | Community | Full | Custom (N-hop BFS).
   Facets: file_type, kind, community_id, is_bridge, edge relation, free-text.
   State persisted in sessionStorage under 4096 bytes; stale refs fall back to Overview.
   No back button — use the depth selector.
*/
'use strict';

/** @typedef {{ depth: 'Overview'|'Community'|'Full'|'Custom', selection: string|null, hops: number, facets: object, searchResults: Array }} ViewerState */

// ---------------------------------------------------------------------------
// Expand interned payload
// ---------------------------------------------------------------------------
const nodeIdStrings = _payloadData.strings;
const fileStrings   = _payloadData.files;
const kindStrings   = _payloadData.kinds;
const relationStrings = _payloadData.relations;
const rawNodes      = _payloadData.nodes;
const rawEdges      = _payloadData.edges;

/** All nodes expanded from the interned payload. */
var allNodes = rawNodes.map(function(n, i) {
  return {
    id:           nodeIdStrings[i],
    label:        n.label,
    source_file:  fileStrings[n.file_idx],
    line:         n.line,
    community_id: n.community_id,
    degree:       n.degree,
    is_bridge:    n.is_bridge,
    kind:         kindStrings[n.kind_idx],
    file_type:    n.file_type
  };
});

/** All edges expanded from the interned payload. */
var allEdges = rawEdges.map(function(e) {
  var rel = relationStrings[e[2]];
  return {
    from:     nodeIdStrings[e[0]],
    to:       nodeIdStrings[e[1]],
    relation: rel
  };
});

/** Community aggregates (immutable source; never sort in place). */
var communityAggregates = _payloadData.aggregates;

// ---------------------------------------------------------------------------
// Indexes built once at load
// ---------------------------------------------------------------------------
/** nodeId -> communityId */
var nodeCommMap = {};
allNodes.forEach(function(n) {
  if (n.community_id !== undefined && n.community_id !== null) {
    nodeCommMap[n.id] = n.community_id;
  }
});

/** communityId -> [node, ...] */
var commToNodes = {};
allNodes.forEach(function(n) {
  var cid = (n.community_id !== undefined && n.community_id !== null) ? n.community_id : -1;
  if (!commToNodes[cid]) commToNodes[cid] = [];
  commToNodes[cid].push(n);
});

/** nodeId -> Set<nodeId> (adjacency for BFS) */
var adjMap = {};
allEdges.forEach(function(e) {
  if (!adjMap[e.from]) adjMap[e.from] = [];
  if (!adjMap[e.to])   adjMap[e.to]   = [];
  adjMap[e.from].push(e.to);
  adjMap[e.to].push(e.from);
});

/** communityId -> label (from aggregates) */
var commLabel = {};
communityAggregates.forEach(function(c) { commLabel[c.id] = c.label; });

/** communityId -> color */
var commColor = {};
communityAggregates.forEach(function(c) { commColor[c.id] = c.color; });

// ---------------------------------------------------------------------------
// Relation-keyed edge styling (defined once, applied per renderer call)
// ---------------------------------------------------------------------------
var RELATION_STYLES = {
  'Contains':   { color: { color: '#4a4a6a', opacity: 0.4 }, dashes: false, width: 1 },
  'contains':   { color: { color: '#4a4a6a', opacity: 0.4 }, dashes: false, width: 1 },
  'Imports':    { color: { color: '#7dd3fc', opacity: 0.6 }, dashes: false, width: 1.5 },
  'imports':    { color: { color: '#7dd3fc', opacity: 0.6 }, dashes: false, width: 1.5 },
  'DependsOn':  { color: { color: '#a78bfa', opacity: 0.7 }, dashes: false, width: 2 },
  'depends_on': { color: { color: '#a78bfa', opacity: 0.7 }, dashes: false, width: 2 },
  'Calls':      { color: { color: '#4ade80', opacity: 0.6 }, dashes: false, width: 1.5 },
  'calls':      { color: { color: '#4ade80', opacity: 0.6 }, dashes: false, width: 1.5 },
  'Inferred':   { color: { color: '#888888', opacity: 0.3 }, dashes: true,  width: 1 },
  'inferred':   { color: { color: '#888888', opacity: 0.3 }, dashes: true,  width: 1 }
};
var DEFAULT_EDGE_STYLE = { color: { color: '#3a3a5e', opacity: 0.3 }, dashes: false, width: 1 };

function edgeStyle(relation) {
  return RELATION_STYLES[relation] || DEFAULT_EDGE_STYLE;
}

// ---------------------------------------------------------------------------
// View state + sessionStorage persistence
// ---------------------------------------------------------------------------

/** @type {ViewerState} */
var viewerState = null;
var network = null;
var overviewNodesDataset = null;
var overviewEdgesDataset = null;
var drilldownNodesDataset = null;
var drilldownEdgesDataset = null;
var currentHighlightedNodes = null;

function initialState() {
  return { depth: 'Overview', selection: null, hops: 2, facets: {}, searchResults: [] };
}

function saveState(state) {
  try {
    var s = JSON.stringify(state);
    if (s.length < 4096) {
      sessionStorage.setItem('graphos_viewer_state', s);
    }
  } catch (e) { /* quota or private mode */ }
}

function loadState() {
  try {
    var saved = sessionStorage.getItem('graphos_viewer_state');
    if (saved) {
      var parsed = JSON.parse(saved);
      if (parsed && typeof parsed.depth === 'string') {
        // Stale-reference check: if selection references unknown node/community, fall back
        if (parsed.selection !== null) {
          var commExists = commToNodes[parsed.selection] !== undefined;
          var nodeExists = allNodes.some(function(n) { return n.id === parsed.selection; });
          if (!commExists && !nodeExists) {
            parsed.selection = null;
            parsed.depth = 'Overview';
          }
        }
        return parsed;
      }
    }
  } catch (e) { /* ignore */ }
  return initialState();
}

viewerState = loadState();

// ---------------------------------------------------------------------------
// One-dispatcher pattern
// ---------------------------------------------------------------------------

function applyState(newState) {
  var oldDepth = viewerState.depth;
  var newDepth = newState.depth;
  viewerState = newState;
  saveState(viewerState);

  if (network && oldDepth !== newDepth) {
    network.destroy();
    network = null;
  }

  render();
}

function dispatch(action, payload) {
  var newState = Object.assign({}, viewerState);
  switch (action) {
    case 'SET_DEPTH':     newState.depth = payload; break;
    case 'SET_SELECTION': newState.selection = payload; break;
    case 'SET_HOPS':      newState.hops = Math.max(1, Math.min(6, payload)); break;
    case 'SET_FACETS':    newState.facets = payload; break;
    case 'SET_SEARCH':    newState.searchResults = payload; break;
    case 'TOGGLE_FACET': {
      var f = Object.assign({}, viewerState.facets);
      if (f[payload]) { delete f[payload]; } else { f[payload] = true; }
      newState.facets = f;
      break;
    }
  }
  applyState(newState);
}

// ---------------------------------------------------------------------------
// Facet filtering
// ---------------------------------------------------------------------------

/**
 * Build a list of file-type display names (0=Code,1=Doc,2=Paper,3=Image,4=Video,5=Audio,6=Office)
 */
var FILE_TYPE_NAMES = ['code', 'doc', 'paper', 'image', 'video', 'audio', 'office'];

function fileTypeName(ft) {
  return FILE_TYPE_NAMES[ft] || ('type' + ft);
}

/**
 * Return filtered nodes and edges given current facets and free-text filter.
 * Returns { nodes, edges } (new arrays, allNodes/allEdges not mutated).
 */
function applyFacets(nodes, edges) {
  var facets = viewerState.facets;
  var textFilter = '';
  var textEl = document.getElementById('facetText');
  if (textEl) textFilter = textEl.value.trim().toLowerCase();

  // Active filters
  var activeFileTypes = [];
  var activeKinds = [];
  var activeCommunities = [];
  var activeBridge = null;
  var activeRelations = [];

  Object.keys(facets).forEach(function(key) {
    if (!facets[key]) return;
    if (key.startsWith('ft:'))   activeFileTypes.push(parseInt(key.slice(3), 10));
    if (key.startsWith('kind:')) activeKinds.push(key.slice(5));
    if (key.startsWith('comm:')) activeCommunities.push(parseInt(key.slice(5), 10));
    if (key === 'bridge:yes')    activeBridge = true;
    if (key === 'bridge:no')     activeBridge = false;
    if (key.startsWith('rel:'))  activeRelations.push(key.slice(4));
  });

  var filteredNodes = nodes.filter(function(n) {
    if (activeFileTypes.length > 0 && activeFileTypes.indexOf(n.file_type) === -1) return false;
    if (activeKinds.length > 0 && activeKinds.indexOf(n.kind) === -1) return false;
    if (activeCommunities.length > 0 && activeCommunities.indexOf(n.community_id) === -1) return false;
    if (activeBridge !== null && n.is_bridge !== activeBridge) return false;
    if (textFilter) {
      var lbl = (n.label || '').toLowerCase();
      var sf = (n.source_file || '').toLowerCase();
      if (lbl.indexOf(textFilter) === -1 && sf.indexOf(textFilter) === -1) return false;
    }
    return true;
  });

  var nodeIdSet = {};
  filteredNodes.forEach(function(n) { nodeIdSet[n.id] = true; });

  var filteredEdges = edges.filter(function(e) {
    if (activeRelations.length > 0 && activeRelations.indexOf(e.relation) === -1) return false;
    // keep edge only if both endpoints are visible
    return nodeIdSet[e.from] && nodeIdSet[e.to];
  });

  return { nodes: filteredNodes, edges: filteredEdges };
}

/**
 * Compute per-facet match counts over the current node/edge set.
 * Returns { fileTypes: {ft: count}, kinds: {kind: count}, communities: {cid: count},
 *           bridgeYes: n, bridgeNo: n, relations: {rel: count} }
 */
function facetCounts(nodes, edges) {
  var fileTypes = {}, kinds = {}, communities = {}, relations = {};
  var bridgeYes = 0, bridgeNo = 0;
  nodes.forEach(function(n) {
    var ft = fileTypeName(n.file_type);
    fileTypes[ft] = (fileTypes[ft] || 0) + 1;
    if (n.kind) kinds[n.kind] = (kinds[n.kind] || 0) + 1;
    if (n.community_id !== undefined && n.community_id !== null) {
      communities[n.community_id] = (communities[n.community_id] || 0) + 1;
    }
    if (n.is_bridge) bridgeYes++; else bridgeNo++;
  });
  edges.forEach(function(e) {
    if (e.relation) relations[e.relation] = (relations[e.relation] || 0) + 1;
  });
  return { fileTypes: fileTypes, kinds: kinds, communities: communities,
           bridgeYes: bridgeYes, bridgeNo: bridgeNo, relations: relations };
}

function renderFacets(nodes, edges) {
  var counts = facetCounts(nodes, edges);
  var facets = viewerState.facets;

  function makeSection(containerId, title, items) {
    var el = document.getElementById(containerId);
    if (!el) return;
    var html = '<div class="facet-title">' + escHtml(title) + '</div>';
    items.forEach(function(item) {
      var checked = facets[item.key] ? 'checked' : '';
      html += '<label class="facet-option">'
        + '<input type="checkbox" class="facet-check" data-facet="' + escHtml(item.key) + '" ' + checked + '>'
        + '<span class="facet-label">' + escHtml(item.label) + '</span>'
        + '<span class="facet-count">' + item.count + '</span>'
        + '</label>';
    });
    el.innerHTML = html;
    el.querySelectorAll('.facet-check').forEach(function(cb) {
      cb.addEventListener('change', function() {
        dispatch('TOGGLE_FACET', this.getAttribute('data-facet'));
      });
    });
  }

  // File type facet
  var ftItems = Object.keys(counts.fileTypes).sort().map(function(ft) {
    return { key: 'ft:' + FILE_TYPE_NAMES.indexOf(ft), label: ft, count: counts.fileTypes[ft] };
  }).filter(function(x) { return x.key !== 'ft:-1'; });
  makeSection('facetFileType', 'File Type', ftItems);

  // Kind facet
  var kindItems = Object.keys(counts.kinds).sort().map(function(k) {
    return { key: 'kind:' + k, label: k, count: counts.kinds[k] };
  });
  makeSection('facetKind', 'Node Kind', kindItems);

  // Relation facet
  var relItems = Object.keys(counts.relations).sort().map(function(r) {
    return { key: 'rel:' + r, label: r, count: counts.relations[r] };
  });
  makeSection('facetRelation', 'Edge Relation', relItems);

  // Bridge facet
  var bridgeEl = document.getElementById('facetBridge');
  if (bridgeEl) {
    var bHtml = '<div class="facet-title">Bridge</div>';
    bHtml += '<label class="facet-option"><input type="checkbox" class="facet-check" data-facet="bridge:yes" '
      + (facets['bridge:yes'] ? 'checked' : '') + '>'
      + '<span class="facet-label">Is bridge</span><span class="facet-count">' + counts.bridgeYes + '</span></label>';
    bridgeEl.innerHTML = bHtml;
    bridgeEl.querySelectorAll('.facet-check').forEach(function(cb) {
      cb.addEventListener('change', function() {
        dispatch('TOGGLE_FACET', this.getAttribute('data-facet'));
      });
    });
  }

  // Community facet (top 10 by count)
  var commItems = Object.keys(counts.communities)
    .map(function(cid) {
      return { key: 'comm:' + cid, label: commLabel[cid] || ('Community ' + cid), count: counts.communities[cid] };
    })
    .sort(function(a, b) { return b.count - a.count; })
    .slice(0, 10);
  makeSection('facetCommunity', 'Community', commItems);
}

// ---------------------------------------------------------------------------
// Renderer options — BASE_OPTIONS (defined once, cloned per render call)
// ---------------------------------------------------------------------------

var BASE_OPTIONS = {
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
    dashes: false,
    width: 1
  },
  physics: {
    enabled: true,
    stabilization: { enabled: true, iterations: 300, fit: true },
    forceAtlas2Based: {
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
      hover: true, tooltipDelay: 200, navigationButtons: false, keyboard: true,
      zoomView: true, dragView: true,
      hideEdgesOnDrag: true, hideEdgesOnZoom: true
    }
};

function getNetworkOptions() {
  return JSON.parse(JSON.stringify(BASE_OPTIONS));
}

// ---------------------------------------------------------------------------
// BFS neighbourhood
// ---------------------------------------------------------------------------

/**
 * Compute N-hop neighbourhood around startId.
 * @param {string} startId
 * @param {number} hops
 * @returns {{ nodes: Array, edges: Array }}
 */
function neighborhoodNodeIds(startId, hops) {
  var visited = {};
  visited[startId] = 0;
  var queue = [{ id: startId, dist: 0 }];

  while (queue.length > 0) {
    var item = queue.shift();
    var id = item.id;
    var dist = item.dist;
    if (dist >= hops) continue;
    var neighbors = adjMap[id] || [];
    for (var i = 0; i < neighbors.length; i++) {
      var nb = neighbors[i];
      if (!(nb in visited)) {
        visited[nb] = dist + 1;
        queue.push({ id: nb, dist: dist + 1 });
      }
    }
  }

  var nodeIdsSet = visited;
  var nodes = allNodes.filter(function(n) { return n.id in nodeIdsSet; });
  var edges = allEdges.filter(function(e) { return (e.from in nodeIdsSet) && (e.to in nodeIdsSet); });
  return { nodes: nodes, edges: edges };
}

// ---------------------------------------------------------------------------
// Graph render dispatcher
// ---------------------------------------------------------------------------

/**
 * Apply vis-network edge styling based on relation.
 * @param {Array} edges
 * @returns {Array}
 */
function styledEdges(edges) {
  return edges.map(function(e) {
    var st = edgeStyle(e.relation);
    return Object.assign({}, e, {
      title: e.relation,
      color: st.color,
      dashes: st.dashes,
      width: st.width
    });
  });
}

/**
 * Apply vis-network node styling (color from community palette).
 * @param {Array} nodes
 * @returns {Array}
 */
function styledNodes(nodes) {
  return nodes.map(function(n) {
    var bg = commColor[n.community_id] || '#888';
    return Object.assign({}, n, {
      color: { background: bg, border: '#1a1a2e', highlight: bg, hover: bg },
      title: (n.source_file || '') + (n.line ? ':' + n.line : '') + ' — ' + (commLabel[n.community_id] || ('Community ' + n.community_id))
    });
  });
}

/**
 * Create or update the single vis.Network instance.
 * All depth branches funnel through here, ensuring exactly one instantiation call exists.
 * @param {vis.DataSet} nodesDataset
 * @param {vis.DataSet} edgesDataset
 * @param {object} opts
 * @param {boolean} [listenStabilize]
 */
function mountNetwork(nodesDataset, edgesDataset, opts, listenStabilize) {
  var container = document.getElementById('graph');
  if (!container) return;
  if (!network) {
    network = new vis.Network(container, { nodes: nodesDataset, edges: edgesDataset }, opts);
    if (listenStabilize) {
      network.once('stabilizationIterationsDone', function() {
        network.setOptions({ physics: { enabled: false } });
      });
    }
    network.on('click', onNetworkClick);
  } else {
    network.setData({ nodes: nodesDataset, edges: edgesDataset });
    network.setOptions(opts);
  }
}

function renderGraph() {
  var depth = viewerState.depth;

  if (depth === 'Overview') {
    var oData = buildOverviewData();
    var oNodes = new vis.DataSet(oData.nodes);
    var oEdges = new vis.DataSet(oData.edges);
    overviewNodesDataset = oNodes;
    overviewEdgesDataset = oEdges;
    var opts = getNetworkOptions();
    opts.physics.stabilization.iterations = 200;
    mountNetwork(oNodes, oEdges, opts, true);
    updatePhaseHint('Click a community dot to explore.');
    return;
  }

  if (depth === 'Community') {
    var cid = viewerState.selection;
    if (cid === null || cid === undefined) {
      updatePhaseHint('Select a community from the legend.');
      return;
    }
    var dData = buildDrilldownData(cid);
    var filtered = applyFacets(dData.nodes, dData.edges);
    drilldownNodesDataset = new vis.DataSet(styledNodes(filtered.nodes));
    drilldownEdgesDataset = new vis.DataSet(styledEdges(filtered.edges));
    mountNetwork(drilldownNodesDataset, drilldownEdgesDataset, getNetworkOptions(), true);
    var lbl = commLabel[cid] || ('Community ' + cid);
    updatePhaseHint('Exploring ' + lbl + ' — ' + dData.nodes.length + ' nodes');
    return;
  }

  if (depth === 'Full') {
    if (allNodes.length > 2000) {
      if (!confirm('Rendering all ' + allNodes.length + ' nodes may be slow. Continue?')) return;
    }
    var fullFiltered = applyFacets(allNodes, allEdges);
    var fNodes = new vis.DataSet(styledNodes(fullFiltered.nodes));
    var fEdges = new vis.DataSet(styledEdges(fullFiltered.edges));
    var opts3 = getNetworkOptions();
    opts3.physics.stabilization.iterations = 100;
    mountNetwork(fNodes, fEdges, opts3, false);
    updatePhaseHint('Showing all nodes.');
    return;
  }

  if (depth === 'Custom') {
    var startNodeId = viewerState.selection;
    if (!startNodeId || startNodeId.startsWith('comm_')) {
      updatePhaseHint('Select a node first, then choose Custom depth.');
      return;
    }
    var hops = viewerState.hops;
    var nbData = neighborhoodNodeIds(startNodeId, hops);
    if (nbData.nodes.length > 2000) {
      if (!confirm('This expansion covers ' + nbData.nodes.length + ' nodes. Continue?')) return;
    }
    var nbFiltered = applyFacets(nbData.nodes, nbData.edges);
    var nbNodes = new vis.DataSet(styledNodes(nbFiltered.nodes));
    var nbEdges = new vis.DataSet(styledEdges(nbFiltered.edges));
    mountNetwork(nbNodes, nbEdges, getNetworkOptions(), false);
    var startNode = allNodes.find(function(n) { return n.id === startNodeId; });
    var startLabel = startNode ? startNode.label : startNodeId;
    updatePhaseHint(hops + '-hop neighbourhood of “' + startLabel + '” — ' + nbData.nodes.length + ' nodes');
    return;
  }
}

function updatePhaseHint(text) {
  var el = document.getElementById('phaseHint');
  if (el) el.textContent = text;
}

function onNetworkClick(params) {
  if (params.nodes.length > 0) {
    var nodeId = params.nodes[0];
    if (nodeId.startsWith('comm_')) {
      var cid = parseInt(nodeId.slice(5), 10);
      // Switch to Community depth and select it
      var ns = Object.assign({}, viewerState, { depth: 'Community', selection: cid });
      applyState(ns);
    } else {
      dispatch('SET_SELECTION', nodeId);
      showNodeDetail(nodeId);
    }
  }
}

// ---------------------------------------------------------------------------
// Main render dispatcher
// ---------------------------------------------------------------------------

function render() {
  // Sync depth selector UI
  var depthSelect = document.getElementById('depthSelect');
  if (depthSelect) depthSelect.value = viewerState.depth.toLowerCase();

  // Sync hops input visibility
  var hopsInput = document.getElementById('hopsInput');
  if (hopsInput) {
    hopsInput.style.display = viewerState.depth === 'Custom' ? 'inline-block' : 'none';
    hopsInput.value = viewerState.hops;
  }

  // Render facets based on current base data
  var baseNodes = viewerState.depth === 'Community'
    ? (commToNodes[viewerState.selection] || [])
    : allNodes;
  var baseEdges = viewerState.depth === 'Community'
    ? allEdges.filter(function(e) {
        var s = viewerState.selection;
        return nodeCommMap[e.from] === s && nodeCommMap[e.to] === s;
      })
    : allEdges;
  renderFacets(baseNodes, baseEdges);

  // Render graph
  renderGraph();

  // Render legend
  renderLegend();
}

// ---------------------------------------------------------------------------
// Overview data builders
// ---------------------------------------------------------------------------

function buildOverviewData() {
  var nodes = communityAggregates.map(function(c) {
    var size = Math.max(4, Math.min(20, Math.sqrt(c.member_count) * 2));
    return {
      id:          'comm_' + c.id,
      label:       '',
      title:       c.label + ' — ' + c.member_count + ' members, cohesion: ' + c.cohesion.toFixed(3),
      color:       { background: c.color, border: '#1a1a2e', highlight: c.color, hover: c.color },
      size:        size,
      shape:       'dot',
      font:        { size: 0 },
      borderWidth: 1,
      group:       c.id
    };
  });

  var edges = [];
  var seen = {};
  allEdges.forEach(function(e) {
    var sc = nodeCommMap[e.from];
    var tc = nodeCommMap[e.to];
    if (sc !== undefined && tc !== undefined && sc !== tc) {
      var key = Math.min(sc, tc) + '-' + Math.max(sc, tc);
      if (!seen[key]) {
        seen[key] = true;
        edges.push({
          from:   'comm_' + Math.min(sc, tc),
          to:     'comm_' + Math.max(sc, tc),
          color:  { color: '#3a3a5e', opacity: 0.3 },
          width:  1,
          smooth: false,
          dashes: true
        });
      }
    }
  });

  return { nodes: nodes, edges: edges };
}

function buildDrilldownData(cid) {
  var members = commToNodes[cid] || [];
  var memberIds = {};
  members.forEach(function(n) { memberIds[n.id] = true; });
  var edges = allEdges.filter(function(e) { return memberIds[e.from] && memberIds[e.to]; });
  return { nodes: members.slice(), edges: edges };
}

// ---------------------------------------------------------------------------
// Legend (non-mutating)
// ---------------------------------------------------------------------------

function renderLegend() {
  var container = document.getElementById('legendList');
  if (!container) return;

  // Sort a copy; do NOT mutate the source array
  var sorted = communityAggregates.slice()
    .sort(function(a, b) { return b.member_count - a.member_count; })
    .slice(0, 20);

  var html = '<div class="legend-divider">Communities</div>';
  sorted.forEach(function(c) {
    var active = (viewerState.facets['comm:' + c.id]) ? ' active' : '';
    html += '<div class="legend-item' + active + '" data-comm="' + c.id + '">'
      + '<div class="legend-dot" style="background:' + c.color + '"></div>'
      + '<span class="legend-label">' + escHtml(c.label) + '</span>'
      + '<span class="legend-count">' + c.member_count + '</span>'
      + '</div>';
  });

  // Relation key
  html += '<div class="legend-divider">Edge Relations</div>';
  var shownRels = {};
  allEdges.forEach(function(e) { if (e.relation) shownRels[e.relation] = true; });
  Object.keys(shownRels).sort().forEach(function(rel) {
    var st = edgeStyle(rel);
    var lineColor = st.color.color || '#888';
    var dash = st.dashes ? 'stroke-dasharray="4 2"' : '';
    html += '<div class="legend-item">'
      + '<svg class="legend-line" height="10" viewBox="0 0 20 10"><line x1="0" y1="5" x2="20" y2="5" stroke="' + lineColor + '" stroke-width="2" ' + dash + '/></svg>'
      + '<span class="legend-label">' + escHtml(rel) + '</span>'
      + '</div>';
  });

  container.innerHTML = html;
  container.querySelectorAll('.legend-item[data-comm]').forEach(function(item) {
    item.addEventListener('click', function() {
      dispatch('TOGGLE_FACET', 'comm:' + this.getAttribute('data-comm'));
    });
  });
}

// ---------------------------------------------------------------------------
// Node detail panel
// ---------------------------------------------------------------------------

function showNodeDetail(nodeId) {
  var node = allNodes.find(function(n) { return n.id === nodeId; });
  if (!node) return;

  var info = document.getElementById('selectedInfo');
  if (!info) return;
  info.style.display = 'block';

  document.getElementById('selectedLabel').textContent = node.label;

  var kindEl = document.getElementById('selectedKind');
  if (kindEl) kindEl.textContent = node.kind || '';

  var fileEl = document.getElementById('selectedFile');
  if (fileEl) fileEl.textContent = (node.source_file || '') + (node.line ? ':' + node.line : '');

  var commEl = document.getElementById('selectedCommunity');
  if (commEl) {
    var lbl = commLabel[node.community_id] || ('Community ' + node.community_id);
    commEl.textContent = lbl;
  }

  var detailEl = document.getElementById('selectedDetail');
  if (detailEl) {
    detailEl.innerHTML =
      '<div class="detail-row"><span class="detail-key">Degree</span><span class="detail-value">' + (node.degree || 0) + '</span></div>'
      + '<div class="detail-row"><span class="detail-key">Bridge</span><span class="detail-value">' + (node.is_bridge ? 'yes' : 'no') + '</span></div>'
      + '<div class="detail-row"><span class="detail-key">Kind</span><span class="detail-value">' + escHtml(node.kind || '') + '</span></div>'
      + '<div class="detail-row"><span class="detail-key">Type</span><span class="detail-value">' + fileTypeName(node.file_type) + '</span></div>';
  }

  // Neighbours grouped by relation
  renderNeighbours(nodeId);

  // Signature: fetch from /api/explain if served, omit on file://
  fetchSignature(nodeId);
}

var MAX_NEIGHBOURS_PER_GROUP = 8;

function renderNeighbours(nodeId) {
  var el = document.getElementById('selectedNeighbors');
  if (!el) return;

  var inByRel = {}, outByRel = {};
  allEdges.forEach(function(e) {
    if (e.to === nodeId) {
      if (!inByRel[e.relation]) inByRel[e.relation] = [];
      inByRel[e.relation].push(e.from);
    }
    if (e.from === nodeId) {
      if (!outByRel[e.relation]) outByRel[e.relation] = [];
      outByRel[e.relation].push(e.to);
    }
  });

  function chipHtml(nid, dir) {
    var n = allNodes.find(function(x) { return x.id === nid; });
    var lbl = n ? n.label : nid;
    return '<span class="neighbor-chip" data-nodeid="' + escAttr(nid) + '">'
      + '<span class="dir">' + dir + '</span> '
      + escHtml(lbl)
      + '</span>';
  }

  function groupHtml(byRel, arrow) {
    var html = '';
    Object.keys(byRel).sort().forEach(function(rel) {
      var ids = byRel[rel];
      html += '<div class="neighbor-group">'
        + '<div class="neighbor-group-title">' + arrow + ' ' + escHtml(rel) + '</div>';
      var shown = ids.slice(0, MAX_NEIGHBOURS_PER_GROUP);
      shown.forEach(function(nid) { html += chipHtml(nid, arrow); });
      var more = ids.length - shown.length;
      if (more > 0) html += '<div class="neighbor-more">and ' + more + ' more</div>';
      html += '</div>';
    });
    return html;
  }

  el.innerHTML = groupHtml(inByRel, '←') + groupHtml(outByRel, '→');

  el.querySelectorAll('.neighbor-chip[data-nodeid]').forEach(function(chip) {
    chip.addEventListener('click', function() {
      var nid = this.getAttribute('data-nodeid');
      dispatch('SET_SELECTION', nid);
      showNodeDetail(nid);
      if (network) {
        network.focus(nid, { scale: 1.5, animation: false });
        network.selectNodes([nid]);
      }
    });
  });
}

function fetchSignature(nodeId) {
  var sigEl = document.getElementById('selectedSignature');
  if (!sigEl) return;
  sigEl.style.display = 'none';
  sigEl.innerHTML = '';

  // Detect if served (not file://)
  if (typeof window !== 'undefined' && window.location && window.location.protocol === 'file:') {
    return; // Degrade silently on file://
  }

  // Try /api/explain
  try {
    var url = '/api/explain?id=' + encodeURIComponent(nodeId);
    fetch(url).then(function(r) {
      if (!r.ok) return null;
      return r.json();
    }).then(function(data) {
      if (!data || !data.signature) return;
      sigEl.style.display = 'block';
      sigEl.innerHTML = '<div class="signature-label">Signature</div>'
        + '<div class="signature">' + escHtml(data.signature) + '</div>';
    }).catch(function() { /* ignore */ });
  } catch (e) { /* ignore */ }
}

// ---------------------------------------------------------------------------
// Search (debounced /api/query with client-side fallback)
// ---------------------------------------------------------------------------

function debounce(fn, ms) {
  var timer = null;
  return function() {
    var args = arguments;
    var ctx = this;
    clearTimeout(timer);
    timer = setTimeout(function() { fn.apply(ctx, args); }, ms);
  };
}

function showSearchResults(query) {
  var el = document.getElementById('searchResults');
  var list = document.getElementById('resultsList');
  var countEl = document.getElementById('searchCount');

  if (!query || query.length < 2) {
    el.classList.remove('active');
    list.innerHTML = '';
    if (countEl) countEl.textContent = '';
    resetHighlight();
    return;
  }

  // Try API if served, fall back to substring
  var isFileUrl = typeof window !== 'undefined' && window.location && window.location.protocol === 'file:';
  if (!isFileUrl) {
    tryApiSearch(query, el, list, countEl);
  } else {
    renderSubstringResults(query, el, list, countEl);
  }
}

function tryApiSearch(query, el, list, countEl) {
  var url = '/api/query?q=' + encodeURIComponent(query);
  fetch(url).then(function(r) {
    if (!r.ok) throw new Error('API error');
    return r.json();
  }).then(function(data) {
    renderApiResults(data, el, list, countEl);
  }).catch(function() {
    renderSubstringResults(query, el, list, countEl);
  });
}

function renderApiResults(data, el, list, countEl) {
  if (!data) return;
  el.classList.add('active');

  var html = '';
  if (data.verdict) {
    html += '<div class="search-verdict">' + escHtml(data.verdict)
      + (data.best_score !== undefined ? ' (score: ' + data.best_score.toFixed(2) + ')' : '')
      + '</div>';
  }
  if (data.suggestions && data.suggestions.length > 0) {
    html += '<div class="search-suggestions">Suggestions: '
      + data.suggestions.map(function(s) { return '<a>' + escHtml(s) + '</a>'; }).join(' ')
      + '</div>';
  }
  list.innerHTML = html;

  var results = data.results || data.nodes || [];
  if (countEl) countEl.textContent = results.length + ' found';

  results.forEach(function(r) {
    var nid = r.id || r.hash || '';
    var isFiltered = !allNodes.some(function(n) { return n.id === nid; });
    var div = document.createElement('div');
    div.className = 'result-item scored' + (isFiltered ? ' filtered' : '');
    div.setAttribute('data-nodeid', nid);
    div.innerHTML = '<div class="rlabel">' + escHtml(r.label || r.name || nid) + '</div>'
      + (r.source_file ? '<div class="rfile">' + escHtml(shortPath(r.source_file)) + '</div>' : '')
      + (isFiltered ? '<div class="filtered-note">filtered out</div>' : '');
    div.addEventListener('click', function() { focusNode(nid); });
    list.appendChild(div);
  });
}

function renderSubstringResults(query, el, list, countEl) {
  var q = query.toLowerCase();
  var facets = viewerState.facets;
  var hasFacets = Object.keys(facets).length > 0;

  var matches = allNodes.filter(function(n) {
    return (n.label || '').toLowerCase().indexOf(q) !== -1
      || ((n.source_file || '').toLowerCase().indexOf(q) !== -1);
  });

  if (countEl) countEl.textContent = matches.length + ' found';

  if (matches.length === 0) {
    el.classList.add('active');
    list.innerHTML = '<div class="no-results">No nodes found for "' + escHtml(query) + '"</div>';
    return;
  }

  matches.sort(function(a, b) { return a.label.localeCompare(b.label); });
  var shown = matches.slice(0, 50);

  // Check which are filtered by facets
  var visibleIds = {};
  if (hasFacets) {
    var activeSet = applyFacets(allNodes, allEdges);
    activeSet.nodes.forEach(function(n) { visibleIds[n.id] = true; });
  }

  list.innerHTML = '';
  shown.forEach(function(n) {
    var isFiltered = hasFacets && !visibleIds[n.id];
    var div = document.createElement('div');
    div.className = 'result-item' + (isFiltered ? ' filtered' : '');
    div.setAttribute('data-nodeid', n.id);
    div.innerHTML = '<div class="rlabel">' + escHtml(n.label) + '</div>'
      + '<div class="rfile">' + escHtml(shortPath(n.source_file)) + '</div>'
      + '<div class="rcommunity">' + escHtml(commLabel[n.community_id] || ('Community ' + n.community_id)) + '</div>'
      + (isFiltered ? '<div class="filtered-note">filtered out</div>' : '');
    div.addEventListener('click', function() { focusNode(n.id); });
    list.appendChild(div);
  });

  el.classList.add('active');
  highlightSubgraph(shown.map(function(n) { return n.id; }));
}

// ---------------------------------------------------------------------------
// Highlight / reset
// ---------------------------------------------------------------------------

function highlightSubgraph(nodeIds) {
  var idSet = {};
  nodeIds.forEach(function(id) { idSet[id] = true; });
  var activeDataset = (viewerState.depth === 'Community' && drilldownNodesDataset) || overviewNodesDataset;
  if (!activeDataset) return;
  var nodes = activeDataset.get();
  var savedColors = {};
  nodes.forEach(function(n) { if (n.color && n.color.background) savedColors[n.id] = n.color.background; });
  var updated = nodes.map(function(n) {
    if (idSet[n.id]) {
      return Object.assign({}, n, { color: { background: '#fbbf24', opacity: 1 }, size: (n.size || 10) * 1.5, borderWidth: 3 });
    } else {
      var bg = savedColors[n.id] || '#888';
      return Object.assign({}, n, { color: { background: bg, opacity: 0.2 }, borderWidth: 1 });
    }
  });
  activeDataset.update(updated);
  currentHighlightedNodes = { dataset: activeDataset, colors: savedColors, ids: nodeIds };
}

function resetHighlight() {
  if (!currentHighlightedNodes) return;
  var dataset = currentHighlightedNodes.dataset;
  var colors = currentHighlightedNodes.colors;
  var nodes = dataset.get();
  var restored = nodes.map(function(n) {
    return Object.assign({}, n, { color: { background: colors[n.id] || '#888', opacity: 1 }, borderWidth: 1 });
  });
  dataset.update(restored);
  currentHighlightedNodes = null;
}

function focusNode(nodeId) {
  dispatch('SET_SELECTION', nodeId);
  showNodeDetail(nodeId);
  if (network) {
    network.focus(nodeId, { scale: 1.5, animation: false });
    network.selectNodes([nodeId]);
  }
}

// ---------------------------------------------------------------------------
// Utility helpers
// ---------------------------------------------------------------------------

function escHtml(s) {
  if (!s && s !== 0) return '';
  return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

function escAttr(s) {
  return escHtml(s);
}

function shortPath(filePath) {
  if (!filePath) return '';
  var parts = filePath.split('/');
  return parts.slice(-2).join('/');
}

// ---------------------------------------------------------------------------
// Bootstrap
// ---------------------------------------------------------------------------

document.addEventListener('DOMContentLoaded', function() {
  // Depth selector (select element)
  var depthSelect = document.getElementById('depthSelect');
  if (depthSelect) {
    depthSelect.addEventListener('change', function() {
      var depth = this.value.charAt(0).toUpperCase() + this.value.slice(1);
      dispatch('SET_DEPTH', depth);
    });
  }

  // Hops input
  var hopsInput = document.getElementById('hopsInput');
  if (hopsInput) {
    hopsInput.addEventListener('change', function() {
      dispatch('SET_HOPS', parseInt(this.value, 10));
    });
  }

  // Facet free-text filter
  var facetText = document.getElementById('facetText');
  if (facetText) {
    facetText.addEventListener('input', debounce(function() { render(); }, 200));
  }

  // Search input
  var input = document.getElementById('searchInput');
  var btnReset = document.getElementById('btnReset');
  if (input) {
    var doSearch = debounce(function() {
      var q = input.value.trim();
      showSearchResults(q);
      if (btnReset) btnReset.style.display = q.length >= 2 ? 'inline-block' : 'none';
    }, 200);
    input.addEventListener('input', doSearch);
    input.addEventListener('keydown', function(e) {
      if (e.key === 'Escape') { input.value = ''; doSearch(); }
    });
  }
  if (btnReset) {
    btnReset.addEventListener('click', function() {
      if (input) input.value = '';
      showSearchResults('');
      btnReset.style.display = 'none';
      var countEl = document.getElementById('searchCount');
      if (countEl) countEl.textContent = '';
      resetHighlight();
    });
  }

  // Initial render
  render();
});
