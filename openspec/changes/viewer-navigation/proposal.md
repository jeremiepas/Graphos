# viewer-navigation

## Why

The graph view served by `graphos serve` (default `http://localhost:8080/`) renders and filters
well, but it does not *navigate*. The viewer keeps its whole position — depth phase, selected
community/node, hop radius, facets — in a single state object persisted to sessionStorage only.
Concretely, four things a user reaches for are missing:

1. **No way back.** Every click through overview → community → node → neighbour overwrites the
   state. Browser Back leaves the page entirely; there is no in-viewer history. Exploring a
   91 MB graph means one wrong drill-down discards your context.
2. **No shareable position.** The URL never changes. You cannot send a teammate (or paste into an
   LLM conversation) a link to "community 483, node `mod_Auth`, doc nodes only" — the address bar
   always says `graph.html`.
3. **No orientation.** In drill-down there is only a phase hint; nothing shows *where* you are
   (Overview ▸ community ▸ node) or lets you jump one level up in a click.
4. **Mouse-only.** The only keyboard affordance is Escape clearing the search input. No shortcut
   focuses search, closes the panel, steps back, or fits the graph.

Additionally, the CLI already answers "how do A and B connect" (`graphos path`, spec `05-path`),
but the viewer — where the question actually arises, with two nodes on screen — has no path
affordance at all.

This capability is additive to `html-viewer-interaction` (facets, detail panel) and
`navigator-query-view` (search), which remain unchanged; it makes the positions those features
produce addressable, traversable and reachable from the keyboard.

## What Changes

- The viewer SHALL encode its navigation state (depth, selection, hops, facets) in the URL hash
  fragment and restore it on load — deep links that survive reload and work on both
  `graphos serve` and `file://`.
- Navigation actions (depth/selection changes) SHALL integrate with browser history so Back and
  Forward traverse viewer positions instead of leaving the page; on-screen back/forward controls
  mirror this.
- A clickable breadcrumb trail (Overview ▸ community label ▸ node label) SHALL show the current
  position and allow jumping to any ancestor level.
- A small keyboard map SHALL exist: focus search, escape (close panel → up one level), fit view,
  and a help overlay listing the shortcuts. Shortcuts SHALL be inert while typing in inputs.
- The node detail panel SHALL gain "path from here / path to here" endpoints; the viewer SHALL
  compute the shortest path over the loaded graph client-side and highlight it using the existing
  subgraph-highlight mechanism, mirroring the CLI `graphos path` semantics.
- **BREAKING**: none. sessionStorage persistence remains as fallback when the URL carries no
  state; all changes are client-side within `assets/viewer/viewer.js` + `viewer.css`.

## Capabilities

### New Capabilities

- `viewer-navigation`: URL-addressable viewer state (hash deep links), browser-history-integrated
  back/forward, breadcrumb trail, keyboard navigation map, and in-viewer shortest-path
  highlighting between two selected nodes.

### Modified Capabilities

- none — `html-viewer-interaction`, `navigator-query-view`, `html-lod-viewer` and
  `query-http-port` are untouched; this capability only consumes state they already produce.

## Impact

- **Code**: `assets/viewer/viewer.js` (state ↔ URL codec, history integration, breadcrumb +
  keyboard handlers, client-side BFS path), `assets/viewer/viewer.css` (breadcrumb, help overlay,
  path-endpoint affordances). No Haskell module changes expected; `Graphos.Infrastructure.Export.HTML`
  only if the viewer shell needs new mount points for breadcrumb/help containers.
- **APIs**: none. Path computation is client-side over the embedded payload; no new endpoints.
- **Dependencies**: none new — vis-network and the existing single-dispatcher state model suffice.
