## Context

The `graph.html` viewer (PRD §12, `refactor-html-large-graph-lod`) is a two-phase level-of-detail explorer: an overview phase renders one dot per community, and a drill-down phase expands a single community into its member nodes. The JS state machine has exactly two states — `currentPhase: 'overview' | 'drilldown'` — and the only user-facing navigation is a `btnBack` "← Back" button and clicking community dots. This works for the 158K-node LOD target but leaves the common < 10K-node case underserved: there is no way to see the full graph at the node level, no way to keep a community expanded while exploring another, and no way to navigate an N-hop neighborhood around a focal node without dropping to the CLI (`graphos neighbors <id> --depth N`). The viewer is generated entirely by `src/Graphos/Infrastructure/Export/HTML.hs` (Infrastructure layer, PRD §4.1) as embedded HTML/CSS/JS in Haskell string templates; no Domain or UseCase code is involved in the viewer itself.

This change adds a **profondeur (depth) view selector** to the viewer header so users can switch abstraction levels explicitly. It is a viewer-only change confined to `Infrastructure/Export/HTML.hs`. The 158K-node LOD path stays the default; the selector adds ergonomics for the small-to-mid case and parity with the CLI `neighbors` command.

## Goals / Non-Goals

**Goals:**
- Let users pick an abstraction level in `graph.html`: Overview, Community, Full, or Custom (N-hop neighborhood).
- Make the LOD default safe for large graphs (no auto-load into Full on 158K nodes).
- Match the CLI `graphos neighbors <id> --depth N` neighborhood semantics in the viewer, client-side, with no new backend data.
- Persist the selected depth across reloads via `sessionStorage`.
- Keep the change in the Infrastructure layer only (PRD §4.1) — no Domain/UseCase edits.

**Non-Goals:**
- Replacing vis-network with sigma.js / WebGL (deferred to the `refactor-html-large-graph-lod` design's long-term path; this change keeps vis-network).
- Adding new fields to `graph.json` (the neighborhood BFS runs client-side over already-shipped `allEdges`).
- Touching the Domain or UseCase layers.
- Unit-testing canvas-rendered behavior (consistent with the existing `refactor-html-large-graph-lod` decision: the JSON the viewer consumes is tested, the render is not).
- Making Full depth fast on 158K nodes (out of scope; the selector defaults to Overview and Full is opt-in).

## Decisions

### Decision 1: Multi-depth state replaces two-phase state machine

Replace `currentPhase: 'overview' | 'drilldown'` with `currentDepth: 'overview' | 'community' | 'full' | 'custom'`. A single `switchDepth(level)` dispatcher routes to per-depth render functions. The existing `expandCommunity` and `backToOverview` functions become internal helpers called by the dispatcher.

- Alternatives considered:
  - **A) Keep two-phase, add Full/Custom as overlays** — rejected: overlaying two renderers doubles canvas management and the back-button affordance conflicts with a selector.
  - **B) N-level stack with push/pop** — rejected: users want to jump between non-adjacent levels (Overview → Custom), not a linear stack; a selector with discrete levels is simpler.
- **Selected**: a flat `currentDepth` enum + dispatcher, because it matches the selector UI 1:1 and avoids state-stack bugs.

### Decision 2: Neighborhood BFS runs client-side over `allEdges`

`buildNeighborhoodData(nodeId, hops)` performs BFS over the embedded `allEdges` array, building an adjacency map once on load (`nodeAdj`), then expanding `hops` levels. Returns the induced subgraph (nodes within N hops + edges among them). No `graph.json` change, no backend round-trip.

- Alternatives considered:
  - **A) Add a `neighborhood` field to `graph.json` precomputed per node** — rejected: O(N×E) size blowup; the 158K-node graph is already 157 MB.
  - **B) Query the MCP server from the browser** — rejected: `graph.html` is self-contained and works from `file://`; a fetch dependency breaks the no-server contract (PRD §12, `html-lod-viewer` "Self-contained HTML" requirement).
  - **C) Lazy-compute adjacency on first Custom use** — rejected: building `nodeAdj` up front is O(E) and cheap (~10 ms for 10K edges); lazy adds complexity for no measurable gain.
- **Selected**: build `nodeAdj` once on load, BFS on demand. Matches the `graphos neighbors` CLI semantics (same edges, same hop count) for free.

### Decision 3: `sessionStorage` over `localStorage`

Persist `graphos_depth`, `graphos_neighborhood_node`, `graphos_neighborhood_hops` in `sessionStorage` (per-tab, cleared on close) rather than `localStorage` (persistent across sessions).

- Alternatives considered:
  - **A) `localStorage`** — rejected: a user who selects Full once on a 158K graph would get a frozen tab on every future visit until they clear storage; `sessionStorage` scopes the preference to the browsing session and avoids polluting the LOD default.
  - **B) URL query string** — rejected: `graphos serve` is the delivery path and query strings are awkward to share; `sessionStorage` is invisible and survives reloads.
- **Selected**: `sessionStorage` for safety (large-graph users never inherit a Full-depth preference) and reload persistence.

### Decision 4: Remove `btnBack`, route everything through the selector

The "← Back" button is removed from the header markup. The back-to-overview affordance becomes selecting `Overview` in the depth selector. No dead UI element (a button with no handler) remains in the DOM.

- Alternatives considered:
  - **A) Keep `btnBack` as a shortcut for Overview** — rejected: two competing navigation affordances (button + selector) confuse the mental model; the selector is the single source of truth for depth.
  - **B) Repurpose `btnBack` as a "previous depth" history button** — rejected: no user request for undo-style navigation; adds state-stack complexity for marginal value.
- **Selected**: remove `btnBack`; the selector is the only depth control. The `backToOverview` JS function survives as an internal helper.

### Decision 5: Full depth is opt-in with a soft size warning

Full depth renders all individual nodes with vis-network. It is safe up to ~5K nodes in practice; above that the user may see slowdowns. The selector does not disable Full, but the legend tooltip carries a "recommended for < 5K nodes" hint. The default on load is always Overview regardless of graph size, so large graphs never auto-load into Full.

- Alternatives considered:
  - **A) Hard-disable Full above 5K nodes** — rejected: power users with strong machines may want it; a soft hint respects user agency.
  - **B) Auto-paginate Full into community chunks** — rejected: that is what Community depth already is; Full means "all at once."
- **Selected**: opt-in Full with a tooltip warning. The default-Overview invariant is the real safety mechanism.

## Risks / Trade-offs

- [Full depth freezes on large graphs] → Mitigation: default is always Overview; Full requires explicit selection; legend tooltip warns at > 5K nodes. No auto-load path exists.
- [Client-side BFS is slow above N=4 on dense graphs] → Mitigation: cap the neighborhood input at `max=6` (matching the CLI's effective range); add a tooltip warning if the induced subgraph exceeds 2K nodes (computed before render).
- [`sessionStorage` quota exceeded] → Mitigation: keys store short strings (`'overview'`, a node id, an int); total < 1 KB; quota is ~5 MB. Wrap writes in `try/catch` to avoid throwing on private-mode browsers.
- [Removing `btnBack` breaks muscle memory for existing users] → Mitigation: the selector is in the same header location; selecting Overview is one click, same as the back button was.
- [Two `vis.Network` instances coexist during a depth switch] → Mitigation: `switchDepth` destroys the previous network and removes its canvas before creating the new one (spec requirement: no overlapping canvases).

## Verification Strategy (Check)

- **Build gate**: `cabal build` completes with zero warnings (`-Wall -Wcompat -Werror`). The change is in `Infrastructure/Export/HTML.hs` only; no new exports, no new types.
- **Test gate**: `cabal test` passes (no new Hspec tests — viewer behavior is canvas-rendered; consistent with the `refactor-html-large-graph-lod` decision).
- **Manual acceptance** (the primary Check for this viewer-only change):
  1. `cabal run graphos -- example/` produces `graphos-out/graph.html`.
  2. `graphos serve --dir graphos-out --port 8080` serves it; open `http://localhost:8080/graph.html`.
  3. Selector is present in the header, defaults to Overview.
  4. Switch to Full on the example graph (< 1K nodes): all nodes render within 1s, no freeze.
  5. Switch to Custom: neighborhood input appears (default 2); click a node; rendered set matches `graphos neighbors <id> --depth 2` from the CLI (cross-check node count).
  6. Change neighborhood input to 3: subgraph re-renders with the N=3 set.
  7. Reload the page: selected depth (and Custom parameters) are restored from `sessionStorage`.
  8. No `btnBack` element exists in the DOM (verify via devtools).
  9. The 158K-node path (if available) still defaults to Overview and does not regress.
- **Regression check**: the existing `refactor-html-large-graph-lod` requirements (overview renders community dots, drill-down expands a community, self-contained HTML, served via `graphos serve`) still hold — the delta spec preserves them as MODIFIED with the depth-selector framing.

## Iteration & Rollback (Act)

- **If Check fails** (e.g., Full depth freezes below 5K nodes, or `sessionStorage` restore is buggy):
  - Roll back `Infrastructure/Export/HTML.hs` to the pre-change state (single-file revert; no Domain/UseCase touched, no `graph.json` change to undo).
  - File a follow-up PDCA cycle targeting the specific failure (e.g., "cap Full at 3K", "fix sessionStorage restore on Safari private mode").
- **If Full depth proves usable above 5K** in practice, raise the legend tooltip threshold and document the measured ceiling in the spec.
- **If the neighborhood BFS is slow** above N=4 on real graphs, cap the input at 4 in the next cycle and add a "large neighborhood" warning.
- **If the depth-selector pattern proves ergonomic**, standardize it for future viewer work (Obsidian vault explorer, SVG export level-of-detail) in a follow-up spec.

## Migration Plan

- **Deploy**: rebuild `graph.html` via `cabal run graphos -- <path>`. Existing `graphos-out/graph.html` files are regenerated on the next run; no in-place patch needed.
- **Rollback**: revert `Infrastructure/Export/HTML.hs` and re-run the export. No data migration, no `graph.json` schema change, no consumer impact.
- **User-facing change**: the "← Back" button is gone; users navigate depth via the selector. Document in the next CHANGELOG entry.

## Open Questions

- Should the neighborhood input also accept a node-id text entry (for users who know the id but do not want to click), or is click-only sufficient for v1? (Leaning click-only for v1; revisit if requested.)
- Should the legend tooltip threshold for Full (5K nodes) be derived from a measured benchmark on the target browser, or kept as a conservative static hint? (Static hint for v1; measure in a follow-up cycle.)