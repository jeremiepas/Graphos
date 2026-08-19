## Why

Graphos can already render the pipeline-produced `graph.json` in `graph.html`, but there is no way to visualize arbitrary JSON a user or LLM produces on the fly — e.g. an LLM tool call that returns a hypothesis graph, or a query JSON dump from another source. Users must run the full detect→extract→build→cluster pipeline just to look at a handful of nodes/edges they already have. The community has produced no IETF RFC for graph-description JSON; the closest widely-deployed shape is vis-network's `{nodes, edges}` (which `graph.html` already consumes internally) and Cytoscape.js `{elements}`. Accepting those shapes directly lets any JSON-producing client (LLM tool, ad-hoc script, exported query) drop a file into `graphos-out/views/` and have `graphos serve` render it on the existing single `graph.html` — no pipeline, no per-view HTML, no duplication of the viewer.

## What Changes

- `graphos serve` SHALL discover JSON graph files under `graphos-out/views/*.json` and expose them to `graph.html` as a navigable catalog.
- `graph.html` SHALL gain a view selector that lists discovered view files (by filename or declared title) and, on selection, fetches the chosen JSON and renders it on the existing vis-network canvas — reusing the same LOD renderer, no per-view HTML generated.
- A new `/api/views` endpoint SHALL enumerate available view files (id, title, node/edge counts, source path) so the viewer does not need to scrape the static directory listing.
- A new `/api/view?id=<name>` endpoint SHALL return the raw view JSON (with normalized nodes/edges) for the viewer to render.
- An accepted input shape SHALL be defined: vis-network `{nodes:[{id,label,...}], edges:[{from,to,...}]}` as the canonical shape, with `id`/`from`/`to` being strings (names allowed). A Cytoscape.js `{elements:[...]}` shape SHALL be accepted and normalized to the canonical shape server-side.
- A minimal validation/normalization step SHALL reject malformed files with a clear error rather than rendering a broken canvas, and SHALL coerce missing fields to defaults (`label` ← `id`, `relation` ← `"related"`).
- **BREAKING**: none. The default `graphos-out/graph.json` view remains the landing page; the views catalog only appears when `graphos-out/views/` exists and contains `.json` files.

## Capabilities

### New Capabilities
- `json-view-ingest`: Accept arbitrary graph-description JSON files (vis-network or Cytoscape.js shape, string ids/names), validate, normalize to the canonical nodes/edges shape, and persist under `graphos-out/views/<id>.json`.
- `view-catalog-api`: HTTP endpoints (`/api/views`, `/api/view?id=<name>`) on `graphos serve` that enumerate and serve normalized view JSON from `graphos-out/views/`.
- `html-view-selector`: In-`graph.html` view selector that calls `/api/views`, lists discovered views, and on selection loads `/api/view?id=<name>` into the existing vis-network canvas, with fallback to the embedded `graph.json` when no views directory or API is present (offline/file:// stays usable).

### Modified Capabilities
- `query-http-port`: The `graphos serve` server SHALL additionally expose `/api/views` and `/api/view?id=<name>` on the same port, sharing the static-file + `/api/*` routing model and the same CORS/405 behavior. Default landing (`graph.html`) unchanged.
- `html-lod-viewer`: The viewer SHALL be able to render any normalized view JSON (not only the embedded pipeline `graph.json` + `community_aggregates`), degrading gracefully when `community_aggregates` are absent (no overview phase, straight to flat node render).

## Impact

- **Code**: `Graphos.Infrastructure.Server.Static` (route `/api/views`, `/api/view`), new `Graphos.Infrastructure.Server.ViewAPI` module, `Graphos.Infrastructure.Export.HTML` (view selector + fetch hook in `graph.html`), new `Graphos.UseCase.IngestView` for validation/normalization, new `Graphos.Domain.View` types (`ViewId`, `ViewGraph`, normalization).
- **APIs**: Two new HTTP endpoints; no change to existing `/api/query` family.
- **Dependencies**: None new — Aeson + Warp already in use.
- **Storage**: New `graphos-out/views/` directory; `.json` files only, no per-view HTML.
- **Out of scope** (deferred to a separate change): LLM-driven hypothesis flow (tool-call → auto-persist → auto-open), rendering non-vis-network shapes natively, view mutation/merge.