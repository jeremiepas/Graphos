# elm-graph-studio

## Why

Everything Graphos ships for humans today is an **export artifact**: `graph.html` is emitted by
the pipeline, read-only by design, and its interaction surface — however good the LOD viewer,
facets, navigation and (planned) groups become — ends at *looking* at the graph. There is no
workbench: no way to edit a node you know is mislabeled, no way to carve out a subgraph and
keep it, no persistent workspace that survives the next pipeline run, no product-grade design
language. The pieces exist on the server — `/api/query` (scored search), `/api/cypher/mutate`
(a specced openCypher write subset), the views catalog (`json-graph-web-view`), and the slice +
group-evaluation endpoints specced by `progressive-graph-interface` — but nothing consumes them
as an *application*.

Graphos Studio is that application: a standalone Elm SPA living in `studio/`, usable two ways —
**load a `graph.json` directly** (drag-and-drop, offline, small graphs) or **connect to
`graphos serve`** (full API surface, large graphs via slices, live editing). Elm is the
deliberate stack choice: pure, typed, ML-family syntax matching the Haskell codebase; The Elm
Architecture is the same single-dispatcher state model the viewer already converged on — and
whose navigation semantics this repo has already formally verified (the `viewer-navigation`
Lean model is, structurally, a TEA `update` function). The studio inherits those semantics
natively instead of re-inventing them.

## What Changes

- A new **`studio/` application** (Elm SPA) SHALL be added to the repo, with its own build
  (`elm make`), tests (`elm-test`), and devenv toolchain — no Haskell changes required.
- **Dual data sources**: open a local `graph.json` (File API, size-guarded) or connect to a
  `graphos serve` origin — using slice/group endpoints when present, falling back to
  fetching `graph.json` for small graphs on older servers.
- A **studio design system**: tokens (color, type, spacing), a component library, light/dark
  themes, and community-color integration — every screen built from it.
- **Graph preview + navigation** with the `viewer-navigation` semantics (position model,
  history push/replace, breadcrumb, Escape hierarchy, URL deep links) rendered through the
  vendored vis-network renderer via Elm ports.
- **Graph editing**: create/update/delete nodes and edges with undo/redo — applied through
  `/api/cypher/mutate` when connected (per the `cypher-mutation` spec), or kept as a local
  edit log with export when file-loaded.
- **Subgraph extraction**: select a scope (community, group, neighborhood, manual selection)
  and export it as a normalized view JSON in the `json-graph-web-view` canonical shape —
  downloadable, and droppable into `graphos-out/views/` for the existing catalog to serve.
- **Groups**: Obsidian-style query+color groups reusing `query-groups` semantics —
  server-evaluated when connected, locally evaluated (label/path/kind matching) in file mode.

## Capabilities

### New Capabilities

- `studio-app-shell`: the Elm application — TEA structure, routing/deep links, navigation
  (history, breadcrumb, keyboard), build and repo integration.
- `studio-design-system`: tokens, components, theming; the visual foundation every studio
  surface uses.
- `studio-data-sources`: local-file and connected modes, capability detection, graph-hash
  tracking, size guards.
- `studio-graph-editing`: preview-adjacent editing with undo/redo, mutation via the specced
  cypher write subset when connected, local edit log + export otherwise.
- `studio-subgraph-extract`: scope selection and export to the canonical view shape.
- `studio-groups`: query+color groups in the studio, dual-mode evaluation.

### Modified Capabilities

- none — the studio consumes existing and already-proposed server capabilities
  (`query-http-port`, `cypher-mutation`, `json-graph-web-view`'s catalog,
  `progressive-graph-interface`'s slice/group endpoints) without changing them. Server-side
  persistence of studio artifacts (e.g. POST of extracted views) is explicitly deferred.

## Impact

- **Code**: new `studio/` directory (Elm sources, `elm.json`, port glue, vendored
  vis-network shared with `assets/viewer/`); `devenv.nix` gains the Elm toolchain
  (elm, elm-test, elm-review); a CI job for `elm make --optimize` + `elm-test`.
- **APIs**: none added or changed; the studio is a pure consumer.
- **Dependencies**: Elm 0.19 toolchain (dev-time only — the built app is static JS/HTML
  servable by `graphos serve` or any static host).
- **Out of scope**: mobile/desktop targets, collaborative editing, server-side storage of
  studio state, and any change to the exported `graph.html` viewer.
