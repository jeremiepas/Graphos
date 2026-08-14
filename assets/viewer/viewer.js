/* Graphos HTML viewer.
 *
 * Self-contained viewer operating on the embedded `_payloadData`:
 *   nodes:  [{label, file_idx, line, community_id, degree, is_bridge, kind_idx, file_type}]
 *   edges:  [[srcIdx, tgtIdx, relIdx]]
 *   strings/files/kinds/relations: interned string tables
 *   aggregates: [{id, member_count, cohesion, bridge_count, color, label, ...}]
 *
 * Depth levels: overview | community | full | custom.
 * Renderer configuration is defined once in BASE_OPTIONS, with per-depth
 * overrides in DEPTH_OPTIONS. Interaction keys live in the interaction section.
 */

(function () {
  'use strict';

  var DEPTHS = ['overview', 'community', 'full', 'custom'];
  var STORAGE_KEY = 'graphosViewerStateV2';
  var MAX_SAVED_BYTES = 4096;
  var MAX_HOPS = 6;
  var MIN_HOPS = 1;
  var WARN_NODE_COUNT = 2000;
  var FILE_TYPES = ['Code', 'Doc', 'Paper', 'Image', 'Video', 'Audio', 'Office'];

  var SERVED = (typeof location !== 'undefined') &&
    location.protocol !== 'file:' && location.protocol !== 'about:';

  /* ── Payload expansion ─────────────────────────────────────────── */
  var strings = _payloadData.strings || [];
  var files = _payloadData.files || [];
  var kinds = _payloadData.kinds || [];
  var relations = _payloadData.relations || [];
  var rawNodes = _payloadData.nodes || [];
  var rawEdges = _payloadData.edges || [];
  var aggregates = (_payloadData.aggregates || []).slice();

  var allNodes = rawNodes.map(function (n, i) {
    return {
      id: strings[i],
      label: n.label,
      source_file: files[n.file_idx] || '',
      line: n.line || 0,
      community_id: n.community_id,
      degree: n.degree || 0,
      is_bridge: !!n.is_bridge,
      kind: kinds[n.kind_idx] || '',
      file_type: n.file_type
    };
  });

  var allEdges = rawEdges.map(function (e, i) {
    return {
      id: 'e' + i,
      from: strings[e[0]],
      to: strings[e[1]],
      relation: relations[e[2]] || ''
    };
  });

  var nodeById = new Map();
  allNodes.forEach(function (n) { nodeById.set(n.id, n); });

  var adjacency = new Map();
  allEdges.forEach(function (e) {
    if (!adjacency.has(e.from)) adjacency.set(e.from, []);
    if (!adjacency.has(e.to)) adjacency.set(e.to, []);
    adjacency.get(e.from).push(e.to);
    adjacency.get(e.to).push(e.from);
  });

  var commToNodes = {};
  allNodes.forEach(function (n) {
    var cid = n.community_id == null ? -1 : n.community_id;
    if (!commToNodes[cid]) commToNodes[cid] = [];
    commToNodes[cid].push(n);
  });

  var commIds = new Set(aggregates.map(function (c) { return c.id; }));

  var nodeCommMap = {};
  allNodes.forEach(function (n) {
    nodeCommMap[n.id] = n.community_id == null ? -1 : n.community_id;
  });

  /* ── Renderer configuration: one base definition + named overrides ── */
  var BASE_OPTIONS = {
    nodes: {
      shape: 'dot',
      size: 12,
      font: { color: '#e0e0e0', size: 10, face: 'Inter, -apple-system, sans-serif', strokeWidth: 0 },
      borderWidth: 2,
      borderWidthSelected: 4,
      shadow: { enabled: false }
    },
    edges: {
      arrows: { to: { enabled: true, scaleFactor: 0.5, type: 'arrow' } },
      shadow: { enabled: false },
      smooth: false,
      font: { color: '#777', size: 9, strokeWidth: 0, align: 'middle' }
    },
    physics: {
      enabled: true,
      stabilization: { enabled: true, iterations: 300, fit: true },
      barnesHut: {
        gravitationalConstant: -2000, centralGravity: 0.03,
        springLength: 80, springConstant: 0.05, damping: 0.4
      },
      maxVelocity: 3, minVelocity: 0.2, solver: 'barnesHut'
    },
    interaction: {
      hover: true, tooltipDelay: 200, navigationButtons: false, keyboard: true,
      zoomView: true, dragView: true,
      hideEdgesOnDrag: true, hideEdgesOnZoom: true
    }
  };

  var DEPTH_OPTIONS = {
    overview: {
      nodes: { size: 10, borderWidth: 1 },
      edges: {
        arrows: {},
        color: { color: '#3a3a5e', highlight: '#7dd3fc', hover: '#3a3a5e' },
        dashes: true, width: 1
      },
      physics: {
        stabilization: { enabled: true, iterations: 300, fit: true },
        forceAtlas2Based: {
          gravitationalConstant: -50, centralGravity: 0.01,
          springLength: 100, springConstant: 0.001, damping: 0.4
        },
        maxVelocity: 50, solver: 'forceAtlas2Based'
      },
      interaction: { keyboard: false }
    },
    community: {},
    full: {},
    custom: {}
  };

  function deepMerge(base, override) {
    var out = {};
    Object.keys(base).forEach(function (k) { out[k] = base[k]; });
    Object.keys(override).forEach(function (k) {
      var bv = base[k], ov = override[k];
      if (ov && typeof ov === 'object' && !Array.isArray(ov) &&
          bv && typeof bv === 'object' && !Array.isArray(bv)) {
        out[k] = deepMerge(bv, ov);
      } else {
        out[k] = ov;
      }
    });
    return out;
  }

  function optionsFor(depth) {
    return deepMerge(BASE_OPTIONS, DEPTH_OPTIONS[depth] || {});
  }

  /* ── Relation-keyed edge styles (defined once, shown in the legend) ── */
  var RELATION_STYLES = {
    contains: {
      color: { color: '#7dd3fc', highlight: '#7dd3fc', hover: '#7dd3fc' },
      width: 1, dashes: false, arrows: { to: { enabled: false } }
    },
    imports: {
      color: { color: '#fbbf24', highlight: '#fbbf24', hover: '#fbbf24' },
      width: 1.5, dashes: false
    },
    depends_on: {
      color: { color: '#a78bfa', highlight: '#a78bfa', hover: '#a78bfa' },
      width: 1.5, dashes: true
    },
    calls: {
      color: { color: '#f472b6', highlight: '#f472b6', hover: '#f472b6' },
      width: 1.5, dashes: false
    },
    requires: {
      color: { color: '#34d399', highlight: '#34d399', hover: '#34d399' },
      width: 1.25, dashes: false
    },
    Inferred: {
      color: { color: '#6a6a8a', highlight: '#6a6a8a', hover: '#6a6a8a' },
      width: 1, dashes: true
    }
  };
  var DEFAULT_EDGE_STYLE = {
    color: { color: '#8b8baa', highlight: '#8b8baa', hover: '#8b8baa' },
    width: 1, dashes: true
  };

  function edgeStyle(rel) {
    return RELATION_STYLES[rel] || DEFAULT_EDGE_STYLE;
  }

  function relationLine(rel) {
    var s = edgeStyle(rel);
    return '2px ' + (s.dashes ? 'dashed' : 'solid') + ' ' + (s.color && s.color.color || '#8b8baa');
  }

  /* ── View state + dispatcher ────────────────────────────────────── */
  function defaultFacets() {
    return { fileType: [], kind: [], community: [], relation: [], bridge: [], text: '' };
  }

  var viewerState = loadState() || {
    depth: 'overview', selection: null, communitySel: null, hops: 2, facets: defaultFacets()
  };

  function loadState() {
    try {
      var raw = sessionStorage.getItem(STORAGE_KEY);
      if (!raw) return null;
      if (raw.length >= MAX_SAVED_BYTES) return null;
      var s = JSON.parse(raw);
      if (!s || DEPTHS.indexOf(s.depth) < 0) return null;
      var hops = parseInt(s.hops, 10);
      s.hops = (isNaN(hops)) ? 2 : clamp(hops, MIN_HOPS, MAX_HOPS);
      s.facets = (s.facets && typeof s.facets === 'object') ? s.facets : defaultFacets();
      if (s.communitySel != null && !commIds.has(s.communitySel)) s.communitySel = null;
      if (s.selection != null && !nodeById.has(s.selection)) s.selection = null;
      if (s.depth === 'community' && s.communitySel == null) s.depth = 'overview';
      if (s.depth === 'custom' && s.selection == null) s.depth = 'overview';
      return s;
    } catch (e) {
      return null;
    }
  }

  function saveState() {
    try {
      var s = JSON.stringify(viewerState);
      if (s.length < MAX_SAVED_BYTES) sessionStorage.setItem(STORAGE_KEY, s);
    } catch (e) { /* storage unavailable: degrade silently */ }
  }

  function clamp(v, lo, hi) {
    v = parseInt(v, 10);
    if (isNaN(v)) return lo;
    return Math.min(hi, Math.max(lo, v));
  }

  function firstCommunityId() {
    return aggregates.length > 0 ? aggregates[0].id : null;
  }

  function dispatch(action, payload) {
    var rerender = false;
    switch (action) {
      case 'SET_DEPTH':
        viewerState.depth = payload;
        if (payload === 'community' && viewerState.communitySel == null) {
          viewerState.communitySel = firstCommunityId();
        }
        rerender = true;
        break;
      case 'SET_COMMUNITY':
        viewerState.communitySel = payload;
        if (viewerState.depth !== 'community') viewerState.depth = 'community';
        rerender = true;
        break;
      case 'SET_HOPS':
        viewerState.hops = clamp(payload, MIN_HOPS, MAX_HOPS);
        rerender = true;
        break;
      case 'SET_FACETS':
        viewerState.facets = payload || defaultFacets();
        rerender = true;
        break;
      case 'SET_SELECTION':
        viewerState.selection = payload;
        if (viewerState.depth === 'custom') rerender = true;
        break;
      case 'OPEN_COMMUNITY':
        viewerState.communitySel = payload;
        viewerState.depth = 'community';
        rerender = true;
        break;
      case 'OPEN_NODE':
        viewerState.selection = payload;
        if (viewerState.depth === 'overview') {
          var n = nodeById.get(payload);
          viewerState.depth = 'community';
          viewerState.communitySel = n ? n.community_id : null;
          if (viewerState.communitySel == null) viewerState.communitySel = firstCommunityId();
          rerender = true;
        } else if (viewerState.depth === 'community') {
          var nn = nodeById.get(payload);
          if (nn && nn.community_id !== viewerState.communitySel) {
            viewerState.communitySel = nn.community_id;
            rerender = true;
          }
        } else if (viewerState.depth === 'custom') {
          rerender = true;
        }
        break;
    }
    saveState();
    syncControls();
    renderFacets();
    renderLegend();
    if (rerender) renderDepth(viewerState.depth);
    if (viewerState.selection != null) showNodeDetail(viewerState.selection);
  }

  /* ── Filtering (conjunctive across facets + text) ───────────────── */
  function passesFacetDims(n, skip) {
    var f = viewerState.facets;
    if (skip !== 'fileType' && f.fileType.length && f.fileType.indexOf(n.file_type) < 0) return false;
    if (skip !== 'kind' && f.kind.length && f.kind.indexOf(n.kind) < 0) return false;
    if (skip !== 'community' && f.community.length && f.community.indexOf(n.community_id) < 0) return false;
    if (skip !== 'bridge' && f.bridge.length && f.bridge.indexOf(!!n.is_bridge) < 0) return false;
    return true;
  }

  function matchesText(n) {
    var t = (viewerState.facets.text || '').toLowerCase();
    if (!t) return true;
    return n.label.toLowerCase().indexOf(t) >= 0 ||
      (n.source_file || '').toLowerCase().indexOf(t) >= 0;
  }

  function nodePasses(n) {
    return passesFacetDims(n, null) && matchesText(n);
  }

  function edgePassesRelation(e) {
    var rel = viewerState.facets.relation;
    return rel.length === 0 || rel.indexOf(e.relation) >= 0;
  }

  function edgePasses(e, visibleIdSet) {
    if (!edgePassesRelation(e)) return false;
    return visibleIdSet.has(e.from) && visibleIdSet.has(e.to);
  }

  function filteredNodes() {
    return allNodes.filter(nodePasses);
  }

  /* ── Facet controls with match counts ───────────────────────────── */
  function facetCounts() {
    var f = viewerState.facets;
    var counts = { fileType: {}, kind: {}, community: {}, bridge: { true: 0, false: 0 }, relation: {} };

    allNodes.forEach(function (n) {
      if (passesFacetDims(n, 'fileType') && matchesText(n)) {
        counts.fileType[n.file_type] = (counts.fileType[n.file_type] || 0) + 1;
      }
      if (passesFacetDims(n, 'kind') && matchesText(n)) {
        counts.kind[n.kind] = (counts.kind[n.kind] || 0) + 1;
      }
      if (passesFacetDims(n, 'community') && matchesText(n)) {
        if (n.community_id != null && commIds.has(n.community_id)) {
          counts.community[n.community_id] = (counts.community[n.community_id] || 0) + 1;
        }
      }
      if (passesFacetDims(n, 'bridge') && matchesText(n)) {
        counts.bridge[!!n.is_bridge] += 1;
      }
    });

    var visible = filteredNodes();
    var visibleIds = new Set(visible.map(function (n) { return n.id; }));
    allEdges.forEach(function (e) {
      if (visibleIds.has(e.from) && visibleIds.has(e.to)) {
        counts.relation[e.relation] = (counts.relation[e.relation] || 0) + 1;
      }
    });

    return counts;
  }

  function renderFacets() {
    var counts = facetCounts();
    var f = viewerState.facets;

    renderFacetGroup('facetFileType', FILE_TYPES.map(function (name, idx) {
      return { value: idx, label: name, count: counts.fileType[idx] || 0, dim: 'fileType' };
    }), f.fileType);

    var kindOpts = Object.keys(counts.kind)
      .map(function (k) { return { value: k, label: k, count: counts.kind[k], dim: 'kind' }; })
      .sort(function (a, b) { return b.count - a.count; })
      .slice(0, 20);
    renderFacetGroup('facetKind', kindOpts, f.kind);

    var commOpts = aggregates.slice()
      .sort(function (a, b) { return b.member_count - a.member_count; })
      .slice(0, 30)
      .map(function (c) {
        return { value: c.id, label: c.label, count: counts.community[c.id] || 0, dim: 'community' };
      });
    renderFacetGroup('facetCommunity', commOpts, f.community);

    var relOpts = Object.keys(counts.relation)
      .map(function (r) { return { value: r, label: r, count: counts.relation[r], dim: 'relation' }; })
      .sort(function (a, b) { return b.count - a.count; });
    renderFacetGroup('facetRelation', relOpts, f.relation);

    renderFacetGroup('facetBridge', [
      { value: true, label: 'Bridge', count: counts.bridge.true, dim: 'bridge' },
      { value: false, label: 'Non-bridge', count: counts.bridge.false, dim: 'bridge' }
    ], f.bridge);
  }

  function renderFacetGroup(elId, options, active) {
    var el = document.getElementById(elId);
    if (!el) return;
    el.innerHTML = options.map(function (o) {
      var checked = active.indexOf(o.value) >= 0 ? ' checked' : '';
      var label = escHtml(String(o.label));
      return '<label class="facet-option">'
        + '<input type="checkbox" class="facet-check" data-dim="' + o.dim + '"'
        + ' data-value="' + encodeURIComponent(String(o.value)) + '"' + checked + '>'
        + '<span class="facet-label" title="' + label + '">' + label + '</span>'
        + '<span class="facet-count">' + o.count + '</span>'
        + '</label>';
    }).join('');
    el.querySelectorAll('.facet-check').forEach(function (cb) {
      cb.addEventListener('change', function () {
        toggleFacet(this.getAttribute('data-dim'), decodeURIComponent(this.getAttribute('data-value')));
      });
    });
  }

  function toggleFacet(dim, value) {
    if (dim === 'bridge') value = value === 'true';
    var arr = viewerState.facets[dim] || [];
    var i = arr.indexOf(value);
    if (i >= 0) arr.splice(i, 1); else arr.push(value);
    viewerState.facets[dim] = arr;
    dispatch('SET_FACETS', viewerState.facets);
  }

  /* ── Legend (non-mutating; click-to-filter) ─────────────────────── */
  function renderLegend() {
    var el = document.getElementById('legendList');
    if (!el) return;
    var activeComm = viewerState.facets.community;

    var sorted = aggregates.slice().sort(function (a, b) { return b.member_count - a.member_count; });
    var html = '<div class="legend-divider">Communities</div>';
    html += sorted.slice(0, 30).map(function (c) {
      var active = activeComm.indexOf(c.id) >= 0 ? ' active' : '';
      return '<div class="legend-item' + active + '" data-community="' + c.id + '">'
        + '<span class="legend-dot" style="background:' + escAttr(c.color) + '"></span>'
        + '<span class="legend-label" title="' + escHtml(c.label) + '">' + escHtml(c.label) + '</span>'
        + '<span class="legend-count">' + c.member_count + '</span>'
        + '</div>';
    }).join('');

    html += '<div class="legend-divider">Edges by relation</div>';
    var knownRels = relations.filter(function (r) { return edgeStyle(r); });
    knownRels.forEach(function (rel) {
      html += '<div class="legend-item">'
        + '<span class="legend-line" style="border-top:' + relationLine(rel) + '"></span>'
        + '<span class="legend-label">' + escHtml(rel) + '</span>'
        + '</div>';
    });

    el.innerHTML = html;
    el.querySelectorAll('.legend-item[data-community]').forEach(function (item) {
      item.addEventListener('click', function () {
        var cid = parseInt(this.getAttribute('data-community'), 10);
        toggleFacet('community', cid);
      });
    });
  }

  /* ── Rendering per depth ────────────────────────────────────────── */
  var network = null;
  var currentHighlightedNodes = null;

  function destroyNetwork() {
    if (network) { network.destroy(); network = null; }
    var container = document.getElementById('graph');
    container.querySelectorAll('canvas').forEach(function (c) { c.remove(); });
    currentHighlightedNodes = null;
  }

  function showLoading() {
    document.getElementById('loading').style.display = 'block';
  }

  function hideLoading() {
    document.getElementById('loading').style.display = 'none';
  }

  function setHint(text) {
    var el = document.getElementById('phaseHint');
    if (el) el.textContent = text;
  }

  function communityLabel(cid) {
    var agg = aggregates.find(function (a) { return a.id === cid; });
    if (agg) return agg.label;
    return cid == null || cid < 0 ? 'No community' : 'Community ' + cid;
  }

  function renderDepth(depth) {
    destroyNetwork();
    showLoading();
    syncControls();

    var data = null;
    var hint = '';
    switch (depth) {
      case 'overview': data = buildOverview(); hint = 'Click a community dot to explore.'; break;
      case 'community': data = buildCommunity(); break;
      case 'full': data = buildFull(); break;
      case 'custom': data = buildCustom(); break;
    }

    if (!data) {
      hideLoading();
      if (hint) setHint(hint);
      return;
    }

    if (data.nodes.length > WARN_NODE_COUNT &&
        !window.confirm('This expansion renders ' + data.nodes.length +
          ' nodes. Continue? (pan/zoom stays responsive; consider a narrower depth or facets)')) {
      hideLoading();
      return;
    }

    var container = document.getElementById('graph');
    hideLoading();

    var nodesDS = new vis.DataSet(data.nodes);
    var edgesDS = new vis.DataSet(data.edges);
    network = new vis.Network(container, { nodes: nodesDS, edges: edgesDS }, optionsFor(depth));

    network.once('stabilizationIterationsDone', function () {
      network.setOptions({ physics: { enabled: false } });
    });

    network.on('click', function (params) {
      if (params.nodes.length > 0) {
        handleNodeClick(params.nodes[0]);
      }
    });

    if (data.hint) setHint(data.hint);
    if (viewerState.selection != null && data.nodeIdSet && data.nodeIdSet.has(viewerState.selection)) {
      network.selectNodes([viewerState.selection]);
    }
  }

  function handleNodeClick(nodeId) {
    if (typeof nodeId === 'string' && nodeId.indexOf('comm_') === 0) {
      var cid = parseInt(nodeId.substring(5), 10);
      dispatch('OPEN_COMMUNITY', cid);
      return;
    }
    dispatch('OPEN_NODE', nodeId);
  }

  function nodeShape(n, color, size) {
    return {
      id: n.id,
      label: n.label,
      title: (n.source_file || '') + (n.line ? ':' + n.line : ''),
      color: {
        background: color, border: '#1a1a2e',
        highlight: { background: color, border: '#fbbf24' },
        hover: { background: color, border: color }
      },
      size: size,
      borderWidth: 1
    };
  }

  function visEdge(e) {
    var style = edgeStyle(e.relation);
    return Object.assign({ id: e.id, from: e.from, to: e.to, title: e.relation, label: e.relation }, style);
  }

  function buildOverview() {
    var nodes = [];
    var edges = [];
    var visible = filteredNodes();
    var perComm = {};
    visible.forEach(function (n) {
      var cid = n.community_id == null ? -1 : n.community_id;
      if (!commIds.has(cid)) cid = -1;
      if (!perComm[cid]) perComm[cid] = [];
      perComm[cid].push(n.id);
    });

    Object.keys(perComm).forEach(function (k) {
      var cid = parseInt(k, 10);
      var agg = aggregates.find(function (a) { return a.id === cid; });
      var label = agg ? agg.label : 'No community';
      var color = agg ? agg.color : '#7dd3fc';
      var count = perComm[cid].length;
      nodes.push({
        id: 'comm_' + cid,
        label: '',
        title: label + ' — ' + count + (count === 1 ? ' member' : ' members'),
        color: { background: color, border: '#1a1a2e', highlight: { background: color, border: '#1a1a2e' }, hover: { background: color, border: '#1a1a2e' } },
        size: Math.max(4, Math.min(20, Math.sqrt(count) * 2)),
        shape: 'dot',
        font: { size: 0 },
        borderWidth: 1
      });
    });

    var seen = new Set();
    var cidOf = function (id) {
      var cid = nodeCommMap[id];
      return (commIds.has(cid)) ? cid : -1;
    };
    visible.forEach(function (n) {
      adjacency.get(n.id).forEach(function (nb) {
        if (!visibleSet(n)) return;
        var sc = cidOf(n.id);
        var tc = cidOf(nb);
        if (sc !== tc) {
          var key = Math.min(sc, tc) + '-' + Math.max(sc, tc);
          if (!seen.has(key)) {
            seen.add(key);
            edges.push({
              id: 'ov_' + key,
              from: 'comm_' + sc,
              to: 'comm_' + tc,
              color: { color: '#3a3a5e', opacity: 0.3 },
              width: 1,
              smooth: false,
              dashes: true
            });
          }
        }
      });
    });

    return { nodes: nodes, edges: edges, hint: 'Click a community dot to explore.', nodeIdSet: null };
  }

  function visibleSet(n) {
    return nodePasses(n);
  }

  function buildCommunity() {
    var cid = viewerState.communitySel;
    if (cid == null || !commToNodes[cid]) {
      cid = firstCommunityId();
    }
    var members = (commToNodes[cid] || []).filter(nodePasses);
    if (members.length === 0) {
      setHint('No nodes match the active filters in ' + communityLabel(cid) + '.');
      return null;
    }
    var agg = aggregates.find(function (a) { return a.id === cid; });
    var color = agg ? agg.color : '#7dd3fc';

    var memberIds = new Set(members.map(function (n) { return n.id; }));
    var nodes = members.map(function (n) { return nodeShape(n, color, 12); });

    // Collapsed dots for other communities receiving bridge edges.
    var dotCids = new Set();
    var internalEdges = [];
    var bridgeEdges = [];
    allEdges.forEach(function (e) {
      var inFrom = memberIds.has(e.from);
      var inTo = memberIds.has(e.to);
      if (inFrom && inTo) {
        if (edgePassesRelation(e)) internalEdges.push(e);
      } else if (inFrom || inTo) {
        var insideId = inFrom ? e.from : e.to;
        var outsideId = inFrom ? e.to : e.from;
        var outNode = nodeById.get(outsideId);
        if (outNode && nodePasses(outNode) && edgePassesRelation(e)) {
          var ocid = outNode.community_id == null ? -1 : outNode.community_id;
          if (ocid !== cid) {
            dotCids.add(ocid);
            bridgeEdges.push({
              id: e.id,
              from: inFrom ? e.from : 'comm_' + ocid,
              to: inFrom ? 'comm_' + ocid : e.to,
              relation: e.relation
            });
          }
        }
      }
    });

    dotCids.forEach(function (ocid) {
      var oagg = aggregates.find(function (a) { return a.id === ocid; });
      var count = (commToNodes[ocid] || []).length;
      nodes.push({
        id: 'comm_' + ocid,
        label: '',
        title: (oagg ? oagg.label : 'No community') + ' — ' + count + (count === 1 ? ' member' : ' members'),
        color: {
          background: oagg ? oagg.color : '#7dd3fc',
          border: '#1a1a2e',
          highlight: { background: oagg ? oagg.color : '#7dd3fc', border: '#1a1a2e' }
        },
        size: Math.max(4, Math.min(14, Math.sqrt(count) * 2)),
        shape: 'dot',
        font: { size: 0 },
        borderWidth: 1
      });
    });

    var hint = 'Exploring ' + communityLabel(cid) + ' — ' + members.length +
      (members.length === 1 ? ' node' : ' nodes');
    var edges = internalEdges.map(visEdge).concat(bridgeEdges.map(visEdge));
    var allIds = new Set(nodes.map(function (n) { return n.id; }));
    return { nodes: nodes, edges: edges, hint: hint, nodeIdSet: allIds };
  }

  function buildFull() {
    var nodes = filteredNodes();
    if (nodes.length === 0) {
      setHint('No nodes match the active filters.');
      return null;
    }
    var ids = new Set(nodes.map(function (n) { return n.id; }));
    var edges = allEdges
      .filter(function (e) { return edgePasses(e, ids); })
      .map(visEdge);
    var hint = 'Showing all ' + nodes.length + (nodes.length === 1 ? ' node' : ' nodes');
    return {
      nodes: nodes.map(function (n) {
        var cid = n.community_id;
        var agg = aggregates.find(function (a) { return a.id === cid; });
        return nodeShape(n, agg ? agg.color : '#7dd3fc', 12);
      }),
      edges: edges,
      hint: hint,
      nodeIdSet: ids
    };
  }

  function neighborhoodNodeIds(startId, hops) {
    var seen = new Set([startId]);
    var frontier = [startId];
    for (var h = 0; h < hops; h++) {
      var next = [];
      frontier.forEach(function (id) {
        (adjacency.get(id) || []).forEach(function (nb) {
          if (!seen.has(nb)) { seen.add(nb); next.push(nb); }
        });
      });
      frontier = next;
    }
    return seen;
  }

  function buildCustom() {
    if (viewerState.selection == null) {
      setHint('Select a node first, then choose Custom depth to expand its N-hop neighbourhood.');
      return null;
    }
    var start = viewerState.selection;
    var ids = neighborhoodNodeIds(start, viewerState.hops);
    var nodes = allNodes.filter(function (n) { return ids.has(n.id) && nodePasses(n); });
    if (nodes.length === 0) {
      setHint('No nodes match the active filters in this neighbourhood.');
      return null;
    }
    var renderIds = new Set(nodes.map(function (n) { return n.id; }));
    var edges = allEdges
      .filter(function (e) { return edgePasses(e, renderIds); })
      .map(visEdge);
    var hint = 'N=' + viewerState.hops + ' neighbourhood of ' + communityLabel(nodeById.get(start).community_id) +
      ' — ' + start + ' — ' + nodes.length + (nodes.length === 1 ? ' node' : ' nodes');
    return {
      nodes: nodes.map(function (n) {
        var agg = aggregates.find(function (a) { return a.id === n.community_id; });
        return nodeShape(n, agg ? agg.color : '#7dd3fc', 12);
      }),
      edges: edges,
      hint: hint,
      nodeIdSet: renderIds
    };
  }

  /* ── Detail panel ───────────────────────────────────────────────── */
  function showNodeDetail(nodeId) {
    var n = nodeById.get(nodeId);
    if (!n) return;
    var info = document.getElementById('selectedInfo');
    document.getElementById('selectedLabel').textContent = n.label;
    document.getElementById('selectedKind').textContent = n.kind || '';
    document.getElementById('selectedFile').textContent =
      n.source_file + (n.line ? ':' + n.line : '');
    var agg = aggregates.find(function (a) { return a.id === n.community_id; });
    var tag = document.getElementById('selectedCommunity');
    tag.textContent = communityLabel(n.community_id);
    tag.style.background = agg ? agg.color : '#1a1a2e';

    var grid = document.getElementById('selectedDetail');
    grid.innerHTML =
      '<div class="detail-row"><span class="detail-key">Degree</span><span class="detail-value">' + n.degree + '</span></div>'
      + '<div class="detail-row"><span class="detail-key">Bridge</span><span class="detail-value">' + (n.is_bridge ? 'yes' : 'no') + '</span></div>'
      + '<div class="detail-row"><span class="detail-key">Community</span><span class="detail-value">' + escHtml(communityLabel(n.community_id)) + '</span></div>'
      + '<div class="detail-row"><span class="detail-key">Members</span><span class="detail-value">' + ((commToNodes[n.community_id == null ? -1 : n.community_id] || []).length) + '</span></div>';

    renderNeighbors(n);
    fetchSignature(nodeId);
    info.style.display = 'block';
  }

  function neighborGroups(nodeId) {
    var groups = {};
    allEdges.forEach(function (e) {
      var other = null;
      var dir = '';
      if (e.from === nodeId) { other = e.to; dir = '\u2192'; }
      else if (e.to === nodeId) { other = e.from; dir = '\u2190'; }
      else return;
      if (!groups[e.relation]) groups[e.relation] = [];
      groups[e.relation].push({ id: other, dir: dir });
    });
    Object.keys(groups).forEach(function (rel) {
      groups[rel].sort(function (a, b) {
        var da = nodeById.get(a.id).degree || 0;
        var db = nodeById.get(b.id).degree || 0;
        return db - da;
      });
    });
    return groups;
  }

  function renderNeighbors(n) {
    var el = document.getElementById('selectedNeighbors');
    var groups = neighborGroups(n.id);
    var total = 0;
    Object.keys(groups).forEach(function (k) { total += groups[k].length; });

    var html = '<span>' + total + '</span> connections';
    var groupKeys = Object.keys(groups).sort(function (a, b) {
      return groups[b].length - groups[a].length;
    }).slice(0, 8);

    groupKeys.forEach(function (rel) {
      var list = groups[rel];
      html += '<div class="neighbor-group"><div class="neighbor-group-title">' + escHtml(rel) + ' (' + list.length + ')</div>';
      var shown = list.slice(0, 5);
      shown.forEach(function (g) {
        var nb = nodeById.get(g.id);
        html += '<span class="neighbor-chip" data-nodeid="' + escAttr(g.id) + '" title="' + escAttr(nb.source_file) + '">'
          + '<span class="dir">' + g.dir + '</span> ' + escHtml(nb.label)
          + '</span>';
      });
      if (list.length > 5) {
        html += '<div class="neighbor-more">and ' + (list.length - 5) + ' more in this group</div>';
      }
      html += '</div>';
    });
    if (groupKeys.length === 8 && Object.keys(groups).length > 8) {
      html += '<div class="neighbor-more">and ' +
        (Object.keys(groups).length - 8) + ' more relation groups</div>';
    }

    el.innerHTML = html;
    el.querySelectorAll('.neighbor-chip[data-nodeid]').forEach(function (chip) {
      chip.addEventListener('click', function () {
        dispatch('OPEN_NODE', this.getAttribute('data-nodeid'));
      });
    });
  }

  var signatureCache = {};
  function fetchSignature(nodeId) {
    var box = document.getElementById('selectedSignature');
    if (!SERVED) {
      box.style.display = 'none';
      return;
    }
    if (signatureCache[nodeId] !== undefined) {
      renderSignature(nodeId, signatureCache[nodeId]);
      return;
    }
    box.style.display = 'none';
    fetch('/api/explain?node=' + encodeURIComponent(nodeId))
      .then(function (r) { return r.json(); })
      .then(function (d) {
        signatureCache[nodeId] = d;
        renderSignature(nodeId, d);
      })
      .catch(function () { box.style.display = 'none'; });
  }

  function renderSignature(nodeId, data) {
    var box = document.getElementById('selectedSignature');
    if (!data || !data.id) { box.style.display = 'none'; return; }
    var lines = ['id: ' + data.id, 'label: ' + (data.label || '')];
    if (data.source_file) lines.push('source_file: ' + data.source_file);
    if (data.community != null) lines.push('community: ' + data.community);
    box.innerHTML = '<div class="signature-label">Signature</div><div class="signature">' + escHtml(lines.join('\n')) + '</div>';
    box.style.display = 'block';
  }

  /* ── Search (navigator-query-view surface) ──────────────────────── */
  var apiAvailable = true;

  function doSearch(query) {
    var el = document.getElementById('searchResults');
    var list = document.getElementById('resultsList');
    var countEl = document.getElementById('searchCount');
    var btn = document.getElementById('btnReset');

    if (!query || query.length < 2) {
      el.classList.remove('active');
      list.innerHTML = '';
      countEl.textContent = '';
      btn.style.display = 'none';
      resetHighlight();
      return;
    }
    btn.style.display = 'inline-block';
    if (apiAvailable) {
      fetch('/api/query?q=' + encodeURIComponent(query) + '&mode=bfs')
        .then(function (r) { return r.json(); })
        .then(function (data) { renderApiResults(data, el, list, countEl); })
        .catch(function () {
          apiAvailable = false;
          renderSubstringResults(query, el, list, countEl);
        });
    } else {
      renderSubstringResults(query, el, list, countEl);
    }
  }

  function facetExcluded(n) {
    return !passesFacetDims(n, null);
  }

  function renderApiResults(data, el, list, countEl) {
    var verdict = data.verdict || '';
    var hash = data.hash || '';
    var suggestions = data.suggestions || [];
    var nodes = (data.nodes || [])
      .sort(function (a, b) { return (b.score || 0) - (a.score || 0); })
      .slice(0, 20);

    countEl.textContent = verdict + (hash ? ' [hash: ' + hash.substring(0, 8) + ']' : '');

    var html = '';
    if (verdict) html += '<div class="search-verdict">' + escHtml(verdict) + '</div>';
    if (suggestions.length > 0) {
      html += '<div class="search-suggestions">Did you mean: ' + suggestions.map(function (s) {
        return '<a data-suggestion="' + escAttr(s) + '">' + escHtml(s) + '</a>';
      }).join(', ') + '</div>';
    }
    nodes.forEach(function (n) {
      var excluded = n.id != null && nodeById.get(n.id) ? facetExcluded(nodeById.get(n.id)) : false;
      html += '<div class="result-item scored' + (excluded ? ' filtered' : '') + '" data-nodeid="' + escAttr(n.id) + '">'
        + '<div class="rlabel">' + escHtml(n.label || n.id) + '</div>'
        + '<div class="rfile">' + escHtml(shortPath(n.source_file || n.sourceFile || '')) + '</div>'
        + '<div class="rcommunity">' + escHtml(communityLabel(n.community_id)) + ' — score: ' + (n.score || 0).toFixed(4) + '</div>'
        + (excluded ? '<div class="filtered-note">excluded by active facets</div>' : '')
        + '</div>';
    });

    list.innerHTML = html;
    el.classList.add('active');

    list.querySelectorAll('.result-item[data-nodeid]').forEach(function (item) {
      item.addEventListener('click', function () {
        focusResult(this.getAttribute('data-nodeid'));
      });
    });
    list.querySelectorAll('.search-suggestions a[data-suggestion]').forEach(function (a) {
      a.addEventListener('click', function () {
        document.getElementById('searchInput').value = this.getAttribute('data-suggestion');
        var doSearchBound = debouncedSearch();
        doSearchBound();
      });
    });

    var ids = nodes.map(function (n) { return n.id; }).filter(function (i) { return i != null; });
    if (ids.length > 0) highlightSubgraph(ids);
  }

  function renderSubstringResults(query, el, list, countEl) {
    var q = query.toLowerCase();
    var matches = allNodes.filter(function (n) {
      return n.label.toLowerCase().indexOf(q) >= 0 ||
        (n.source_file || '').toLowerCase().indexOf(q) >= 0;
    });

    countEl.textContent = matches.length + ' found';

    if (matches.length === 0) {
      el.classList.add('active');
      list.innerHTML = '<div class="no-results">No notes found for "' + escHtml(query) + '"</div>';
      return;
    }

    var typeOrder = { docresult: 0, h1result: 1, h2result: 2 };
    matches.sort(function (a, b) {
      var ta = typeOrder[nodeTypeClass(a.id)] != null ? typeOrder[nodeTypeClass(a.id)] : 3;
      var tb = typeOrder[nodeTypeClass(b.id)] != null ? typeOrder[nodeTypeClass(b.id)] : 3;
      if (ta !== tb) return ta - tb;
      return a.label.localeCompare(b.label);
    });

    var shown = matches.slice(0, 50);
    list.innerHTML = shown.map(function (n) {
      var excluded = facetExcluded(n);
      return '<div class="result-item ' + nodeTypeClass(n.id) + (excluded ? ' filtered' : '') + '" data-nodeid="' + escAttr(n.id) + '">'
        + '<div class="rlabel">' + escHtml(n.label) + '</div>'
        + '<div class="rfile">' + escHtml(shortPath(n.source_file)) + '</div>'
        + '<div class="rcommunity">' + escHtml(communityLabel(n.community_id)) + '</div>'
        + (excluded ? '<div class="filtered-note">excluded by active facets</div>' : '')
        + '</div>';
    }).join('');

    el.classList.add('active');
    list.querySelectorAll('.result-item').forEach(function (item) {
      item.addEventListener('click', function () {
        focusResult(this.getAttribute('data-nodeid'));
      });
    });

    var ids = shown.map(function (n) { return n.id; });
    highlightSubgraph(ids);
  }

  function focusResult(nodeId) {
    dispatch('OPEN_NODE', nodeId);
    if (network) {
      network.focus(nodeId, { scale: 1.5, animation: false });
      network.selectNodes([nodeId]);
    }
    showNodeDetail(nodeId);
  }

  function highlightSubgraph(nodeIds) {
    if (!network) return;
    var idSet = new Set(nodeIds);
    var nodesDS = network.body.data.nodes;
    if (!nodesDS) return;
    var current = nodesDS.get();
    var colors = {};
    current.forEach(function (n) { if (n.color && n.color.background) colors[n.id] = n.color.background; });
    var updated = current.map(function (n) {
      if (idSet.has(n.id)) {
        return Object.assign({}, n, {
          color: { background: '#fbbf24', opacity: 1 },
          size: (n.size || 10) * 1.5,
          borderWidth: 3
        });
      }
      return Object.assign({}, n, {
        color: { background: (colors[n.id] || '#888'), opacity: 0.2 },
        borderWidth: 1
      });
    });
    nodesDS.update(updated);
    currentHighlightedNodes = { dataset: nodesDS, colors: colors };
  }

  function resetHighlight() {
    if (!currentHighlightedNodes || !network) return;
    var ds = currentHighlightedNodes.dataset;
    var colors = currentHighlightedNodes.colors;
    var current = ds.get();
    var restored = current.map(function (n) {
      return Object.assign({}, n, {
        color: { background: colors[n.id] || '#888', opacity: 1 },
        borderWidth: 1,
        size: 12
      });
    });
    ds.update(restored);
    currentHighlightedNodes = null;
  }

  /* ── Small helpers ──────────────────────────────────────────────── */
  function nodeTypeClass(nodeId) {
    if (nodeId.indexOf('_doc_') >= 0) return 'docresult';
    if (nodeId.indexOf('_h1_') >= 0) return 'h1result';
    if (nodeId.indexOf('_h2_') >= 0) return 'h2result';
    return 'docresult';
  }

  function shortPath(filePath) {
    if (!filePath) return '';
    var parts = filePath.split('/');
    return parts.slice(-2).join('/');
  }

  function escHtml(s) {
    if (s == null) return '';
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;')
      .replace(/>/g, '&gt;').replace(/"/g, '&quot;');
  }

  function escAttr(s) {
    return escHtml(s).replace(/'/g, '&#39;');
  }

   function updateHopsVisibility(depth) {
     var hops = document.getElementById('neighborhoodHops');
     if (hops) {
       if (depth === 'custom') hops.classList.add('active');
       else hops.classList.remove('active');
     }
   }

   function syncControls() {
     var sel = document.getElementById('depthSelector');
     if (sel && sel.value !== viewerState.depth) sel.value = viewerState.depth;
     var hops = document.getElementById('neighborhoodHops');
     if (hops && String(hops.value) !== String(viewerState.hops)) hops.value = viewerState.hops;
     updateHopsVisibility(viewerState.depth);
     var text = document.getElementById('facetText');
     if (text && text.value !== (viewerState.facets.text || '')) text.value = viewerState.facets.text || '';
   }

  /* ── Init ───────────────────────────────────────────────────────── */
  var debounceTimer = null;
  function debouncedSearch() {
    return function () {
      clearTimeout(debounceTimer);
      debounceTimer = setTimeout(function () {
        var input = document.getElementById('searchInput');
        doSearch(input.value.trim());
      }, 200);
    };
  }

  document.addEventListener('DOMContentLoaded', function () {
    if (typeof vis === 'undefined') {
      var loading = document.getElementById('loading');
      loading.innerHTML = '<div style="text-align:center;max-width:400px">'
        + '<div style="font-size:18px;color:#f87171;margin-bottom:8px">vis-network failed to load</div>'
        + '<div style="font-size:12px;color:#888">The vendored renderer bundle is missing from this document.</div>'
        + '</div>';
      return;
    }

    renderFacets();
    renderLegend();
    renderDepth(viewerState.depth);
    if (viewerState.selection != null) showNodeDetail(viewerState.selection);

    var input = document.getElementById('searchInput');
    var btn = document.getElementById('btnReset');
    var search = debouncedSearch();
    input.addEventListener('input', search);
    input.addEventListener('keydown', function (e) {
      if (e.key === 'Escape') { input.value = ''; search(); }
    });
    btn.addEventListener('click', function () {
      input.value = '';
      doSearch('');
    });

     var depthSel = document.getElementById('depthSelector');
     depthSel.addEventListener('change', function () {
       dispatch('SET_DEPTH', this.value);
       updateHopsVisibility(this.value);
     });
     var hops = document.getElementById('neighborhoodHops');
     hops.addEventListener('change', function () {
       dispatch('SET_HOPS', this.value);
     });

    var facetText = document.getElementById('facetText');
    var facetTimer = null;
    facetText.addEventListener('input', function () {
      clearTimeout(facetTimer);
      facetTimer = setTimeout(function () {
        viewerState.facets.text = facetText.value.trim().toLowerCase();
        dispatch('SET_FACETS', viewerState.facets);
      }, 250);
    });

    window.addEventListener('resize', function () {
      if (network) network.redraw();
    });
  });
})();
