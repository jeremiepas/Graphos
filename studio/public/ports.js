/* Graphos Studio port glue (studio-app-shell).
 *
 * All application state lives in the Elm Model; this file holds ONLY the
 * vis-network handle and its datasets (renderer state, not app state), plus
 * the localStorage bridge. Rendering commands arrive on `toRenderer`;
 * interaction events go back on `fromRenderer`.
 */
(function () {
  "use strict";

  var STORAGE_PREFIX = "graphos-studio:";

  // ── Flags: localStorage snapshot + OS theme preference ──────────────────
  var stored = {};
  try {
    for (var i = 0; i < localStorage.length; i++) {
      var key = localStorage.key(i);
      if (key && key.indexOf(STORAGE_PREFIX) === 0) {
        try {
          stored[key] = JSON.parse(localStorage.getItem(key));
        } catch (_e) {
          /* unparseable entry: skip */
        }
      }
    }
  } catch (_e) {
    /* storage unavailable (private mode): boot with empty snapshot */
  }

  var app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: {
      stored: stored,
      osDark:
        window.matchMedia &&
        window.matchMedia("(prefers-color-scheme: dark)").matches,
    },
  });

  // ── Persistence port ─────────────────────────────────────────────────────
  app.ports.persist.subscribe(function (msg) {
    try {
      localStorage.setItem(msg.key, JSON.stringify(msg.value));
    } catch (_e) {
      /* quota/private mode: persistence is best-effort */
    }
  });

  // ── Renderer port ────────────────────────────────────────────────────────
  var network = null; // the ONLY state here: the renderer handle
  var nodesDataset = null;
  var edgesDataset = null;

  function ensureNetwork(canvasTokens) {
    var container = document.getElementById("graph-canvas");
    if (!container) {
      return null;
    }
    if (network) {
      return network;
    }
    nodesDataset = new vis.DataSet([]);
    edgesDataset = new vis.DataSet([]);
    network = new vis.Network(
      container,
      { nodes: nodesDataset, edges: edgesDataset },
      {
        physics: {
          solver: "forceAtlas2Based",
          stabilization: { iterations: 150 },
        },
        interaction: {
          hover: true,
          tooltipDelay: 200,
          hideEdgesOnDrag: true,
        },
        nodes: {
          scaling: { min: 8, max: 42 },
          font: { color: canvasTokens.label, size: 12 },
        },
        edges: {
          color: { color: canvasTokens.edge },
          smooth: false,
          arrows: {},
        },
      }
    );
    network.on("click", function (params) {
      if (params.nodes.length > 0) {
        app.ports.fromRenderer.send({ tag: "click", id: String(params.nodes[0]) });
      } else {
        app.ports.fromRenderer.send({ tag: "clickBackground" });
      }
    });
    network.once("stabilizationIterationsDone", function () {
      network.setOptions({ physics: { enabled: false } });
    });
    return network;
  }

  app.ports.toRenderer.subscribe(function (msg) {
    // The canvas div is rendered by Elm; wait one frame so it exists.
    requestAnimationFrame(function () {
      if (msg.tag !== "render") {
        return;
      }
      var net = ensureNetwork(msg.canvas);
      if (!net) {
        return;
      }
      var container = document.getElementById("graph-canvas");
      container.style.background = msg.canvas.bg;
      net.setOptions({
        physics: { enabled: true, stabilization: { iterations: 150 } },
        nodes: { font: { color: msg.canvas.label } },
        edges: { color: { color: msg.canvas.edge } },
      });
      nodesDataset.clear();
      nodesDataset.add(msg.nodes);
      edgesDataset.clear();
      edgesDataset.add(msg.edges);
      if (msg.selected) {
        try {
          net.selectNodes([msg.selected]);
        } catch (_e) {
          /* selected node filtered out: ignore */
        }
      }
      net.once("stabilizationIterationsDone", function () {
        net.setOptions({ physics: { enabled: false } });
        if (msg.fit) {
          net.fit({ animation: false });
        }
      });
    });
  });

  // ── Focus-trap port ────────────────────────────────────────────────────
  // JS owns native-Tab prevention and focus movement; Elm owns the stop index.
  // openDialog captures the active element (restored on close), moveFocus moves
  // to a CSS selector computed by Studio.DialogFocus, restoreFocus hands focus
  // back. The Elm side only ever *dispatches* the stop; this file performs it.
  var dialogTrap = null;
  var capturedElement = null;

  function dialogRoot() {
    return document.getElementById("studio-dialog");
  }

  function withinDialog(el) {
    return el && el.closest && el.closest("#studio-dialog");
  }

  function detachTabTrap() {
    var root = dialogRoot();
    if (root && dialogTrap) {
      root.removeEventListener("keydown", dialogTrap);
      dialogTrap = null;
    }
  }

  function handleKeydown(e) {
    // Trap Tab / Shift+Tab: never let the browser move focus outside the modal.
    if (e.key === "Tab") {
      e.preventDefault();
      e.stopPropagation();
    }
  }

  function focusBySelector(selector) {
    try {
      var el = document.querySelector(selector);
      if (el && typeof el.focus === "function") {
        el.focus();
      }
    } catch (_e) {
      /* selector matched nothing: leave focus untouched */
    }
  }

  app.ports.openDialog.subscribe(function (msg) {
    // Remember what had focus so restoreFocus can return to it on close.
    capturedElement = document.activeElement;
    detachTabTrap();
    requestAnimationFrame(function () {
      var root = dialogRoot();
      if (!root) {
        return;
      }
      dialogTrap = handleKeydown;
      root.addEventListener("keydown", dialogTrap);
      var sel = msg.stops && msg.stops[msg.index] ? msg.stops[msg.index] : "";
      focusBySelector(sel);
    });
  });

  app.ports.moveFocus.subscribe(function (selector) {
    focusBySelector(selector);
  });

  app.ports.restoreFocus.subscribe(function () {
    detachTabTrap();
    try {
      if (capturedElement && typeof capturedElement.focus === "function") {
        capturedElement.focus();
      }
    } catch (_e) {
      /* restore is best-effort */
    }
    capturedElement = null;
  });
})();
