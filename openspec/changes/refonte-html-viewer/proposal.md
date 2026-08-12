## Why

`graph.html` for a 104,101-node / 122,347-edge graph weighs **101.2 MB**. Measured breakdown of
that file:

| Section | Bytes | Per item |
|---|---|---|
| `_nodesData` | 42,604,385 | **409 B/node** |
| `_edgesData` | 53,902,316 | **441 B/edge** |
| `_communityAggregatesData` | 4,545,244 | — |
| Everything else (HTML + CSS + JS) | 175,855 | — |

**99.8% of the artifact is payload, and the payload is mostly repetition.** Every edge
serializes `color{color,highlight,hover}`, `arrows{to{enabled,scaleFactor}}`, `dashes`, `width`,
plus `title` and `label` holding the identical string (`HTML.hs:846–856`, `:919–920`), and
references its endpoints by full node id — ids that are themselves long text
(`33781_mcp-backend_export function createMcpBackend(...)`). Every node serializes a `color`
object that is a pure function of its community (`HTML.hs:900`), a `group` that duplicates
`community_id` (`HTML.hs:903–904`), and a `title` that concatenates `source_file` and
`community_id` (`HTML.hs:898`) — both already present as their own keys. None of this varies
per item; all of it is styling that belongs in a stylesheet or a group definition.

The consequences are known and recorded in this repo: the archived
`refactor-html-large-graph-lod/design.md:7` measured *"the browser parses 157 MB and holds
~1–1.5 GB heap"* and concluded the wall is **parse + heap, not render**. That archived design
proposed abandoning inline JSON and vis-network entirely; it was never implemented, never
promoted into a spec, and its benchmark evidence file
(`archive/2026-08-11-refactor-html-large-graph-lod/tasks/07-78k-node-benchmark/check.md`) still
has every Evidence and Verdict field empty. Meanwhile the active spec
(`html-lod-viewer/spec.md:72–74`) mandates the opposite — inline, `file://`-capable, self-contained
HTML. **The repo currently ships a viewer whose own architecture decision is unresolved.**

This change resolves it the cheap way first: keep inline data and vis-network, and delete the
repetition. That alone is a ~4× reduction with no renderer risk and no loss of `file://`
capability, which is what the active spec requires.

Two further problems make the viewer hard to change at all:

1. **The viewer is 675 lines of CSS and JavaScript written as Haskell string literals**
   (`HTML.hs:60–173` header/CSS, `:176–804` body/JS) inside a 983-line module. There is no
   linting, no formatting, no type checking and **zero tests over the generated HTML**
   (`tests/…/HTMLSpec.hs` only asserts label fallback on an *empty* graph). Both prior HTML
   changes explicitly waived viewer tests. The cost shows: three near-identical vis-network
   `options` blocks (`HTML.hs:352–384`, `:429–470`, `:713–754`); `hideEdgesOnDrag`/
   `hideEdgesOnZoom` placed inside `physics` instead of `interaction` (`HTML.hs:458–459`,
   `:742–743`), so the documented anti-freeze mitigation is inert; CSS classes emitted by
   `renderApiResults` (`.search-verdict`, `.search-suggestions`, `.result-item.scored`,
   `HTML.hs:565–572`) that have no rules in the stylesheet; `communityAggregates.sort()`
   mutating the source array before `.slice(0,50)` (`HTML.hs:245`).
2. **The payload is computed twice, divergently.** `HTML.hs:935–983` recomputes community
   aggregates instead of using `UseCase/Cluster.hs:96–160`, recomputes `articulationPoints`
   a second time (`HTML.hs:44` then `:939`), recomputes `cohesionScore` per community
   (`HTML.hs:953`) although `analysisCohesion` is already available, and round-trips
   `gCompositions` through `encode`/`eitherDecode` (`HTML.hs:945–949`). The duplicate emits
   `inter_community_edges = 0` unconditionally (`HTML.hs:960`), violating
   `html-lod-viewer/spec.md:27`. It also types `community_id` as a **string** in aggregates
   (`HTML.hs:951`) while nodes carry it as a **number** (`HTML.hs:832`), so the node-detail
   panel's `find(c => c.id === cid)` (`HTML.hs:513–515`) never matches and the panel always shows
   a bare `Community <n>` — one of the two residual scenarios of `fix-community-labels-in-html`
   (the other, `phaseHint` using the label, is `HTML.hs:394`).

Finally, an external reference implementation now exists. Building a configuration-system
subgraph viewer produced a 617-node HTML whose interaction model — faceted
filters over node/edge metadata, a details panel with `file:line` and neighbour lists, a legend
driven by community aggregates, and edge styling keyed by relation — is what the Graphos viewer
lacks. That file is the design reference for the interaction requirements here. It is *not* a
size reference: at 1,022 B/node it is larger per node than Graphos', because it embeds
signatures. The lesson taken from it is the **shape of the view model and the facet/panel
interaction**, not its payload economics.

## What Changes

- **Payload contract (`html-view-model`)**: a single projected view model replaces the current
  ad-hoc `VisNode`/`VisEdge` records.
  - Node ids, `source_file`, `kind` and `relation` values are **interned into string tables**;
    nodes and edges reference them by integer index.
  - Edges become positional triples `[sourceIdx, targetIdx, relationIdx]`; no per-edge `color`,
    `arrows`, `dashes`, `width`, `title` or `label`.
  - Nodes carry only varying data: label, file index, line, community, degree, bridge flag,
    kind index, file type. No `color`, no `group`, no `title`.
  - Styling moves to vis-network `groups` plus CSS, derived once from the community palette.
  - Signatures are **not** embedded; the detail panel fetches them from `/api/explain` when a
    server is present and degrades gracefully on `file://` (same pattern as
    `navigator-query-view/spec.md:18–20`).
  - Aggregates come from `UseCase/Cluster.hs` — computed once, in one place, with real
    `inter_community_edges`.
- **Size and latency budget (`html-lod-viewer`)**: first enforceable budget — ≤ 200 B/node,
  ≤ 24 B/edge, and ≤ 30 MB total for the 104K-node reference corpus (from 101.2 MB), with the
  existing latency targets retained and actually measured.
- **Viewer assets (`html-viewer-assets`)**: the CSS and JS move out of Haskell string literals
  into real `.css`/`.js` files embedded at compile time (`file-embed`, already precedented in
  this repo). vis-network is **vendored and embedded instead of loaded from
  `unpkg.com`** (`HTML.hs:68`), making the file genuinely self-contained rather than
  "self-contained for data, network-dependent for the renderer". One shared `options` object
  replaces the three duplicates.
- **Faceted interaction (`html-viewer-interaction`)**: filter chips/toggles over file type,
  node kind, community, edge relation and bridge status; a text filter; a details panel showing
  kind, `source_file:line`, community label, degree and in/out neighbours; a legend generated
  from community aggregates with colors and member counts; relation-keyed edge styling. All
  client-side, all preserving the existing `/api/query` search surface.
- **Depth selector (`html-depth-selector`)**: `Overview | Community | Full | Custom (N-hop)`
  with `sessionStorage` persistence — **absorbed from `add-profondeur-view-selector`**, which
  this change supersedes (see below).
- **Golden-file tests**: the first tests over generated HTML — payload shape, interning
  correctness, budget assertions, and a JS syntax check of the emitted document.
- **Absorbed defect fixes**: `community_id` type unification, `inter_community_edges`,
  `hideEdgesOnDrag` placement, missing CSS rules, `sort()` mutation, duplicated
  `articulationPoints`/`cohesionScore` computation, and the two residual
  `fix-community-labels-in-html` scenarios (overview dot title and `phaseHint` use the label).
- **BREAKING**: the inline data shape changes (`_nodesData`/`_edgesData` become interned tables).
  Nothing outside `HTML.hs` and the viewer JS reads them — `graph.json` is unaffected, the
  `graphos serve` contract is unaffected, `/api/*` is unaffected. `btnBack` is removed, replaced
  by the depth selector (inherited from the superseded change).

### Supersession

This change **supersedes `add-profondeur-view-selector`** (0/24 tasks, entirely unimplemented,
confined to the same 983-line module). Its `html-depth-selector` requirements are carried here in
condensed form and its `html-lod-viewer` modifications are folded into this change's delta.
Shipping both independently would mean two rewrites of the same file with conflicting
assumptions about how the viewer is assembled. Task 9 archives it as superseded.

## Capabilities

### New Capabilities
- `html-view-model`: the interned, style-free payload contract embedded in `graph.html`, and the
  single source of aggregate computation.
- `html-viewer-assets`: viewer CSS/JS as compile-time-embedded assets, with a vendored renderer
  and no network dependency.
- `html-viewer-interaction`: facet filtering, details panel, legend and relation-keyed edge
  styling.
- `html-depth-selector`: absorbed from the superseded `add-profondeur-view-selector`.

### Modified Capabilities
- `html-lod-viewer`: gains a payload size budget, has its self-containment requirement
  strengthened to include the renderer, and its two-phase state machine generalized to the four
  depth levels.

## Impact

- **Code**:
  - `src/Graphos/Infrastructure/Export/HTML.hs` (983 lines) — reduced to payload projection,
    interning and asset assembly; `htmlHeader` (`:60–173`) and `htmlBody` (`:176–804`) removed in
    favour of embedded assets; `VisNode`/`VisEdge`/`VisCommunityAggregate` (`:806–983`) replaced
    by the view-model records; aggregate recomputation deleted in favour of
    `UseCase/Cluster.hs:96–160`.
  - New `assets/viewer/viewer.css`, `assets/viewer/viewer.js`, `assets/viewer/vis-network.min.js`
    (vendored, version-pinned, license recorded).
  - `graphos.cabal` — `file-embed` dependency and `data-files`/`extra-source-files` entries.
  - `src/Graphos/UseCase/Export.hs:33–37` and `Port/ExportPort.hs:31` — unchanged signature; the
    exporter now also receives the precomputed aggregates rather than recomputing them.
  - `tests/Graphos/Infrastructure/Export/HTMLSpec.hs` — extended from 37 lines to real coverage.
- **APIs**: no change to `graph.json`, `graphos serve`, or `/api/*`. The inline payload shape is
  internal to the viewer.
- **Dependencies**: `file-embed` (Haskell). A pinned vis-network bundle is vendored into the
  repository (~600 KB, one-time, MIT/Apache-2.0 — license file recorded alongside).
- **Specs**: supersedes `add-profondeur-view-selector`; unblocks — but does not implement —
  `cluster-composition`'s composition badge (the view model carries the composition fields) and
  `research-view`'s HTML (`app/Main.hs:328` still prints "HTML export not yet implemented"; the
  refonted assets are reusable by it).
- **Docs**: PRD §12 (`PRD.md:680–693`) is a 10-row format table with no viewer section and no size
  budget; the `html-lod-viewer` latency requirement cites a PRD §16.1 row that does not exist
  (`PRD.md:797–805`). Both are corrected.
- **Deliberately out of scope**: renderer replacement (sigma.js/WebGL) and sidecar data stores —
  the archived design's own conclusion is that parse+heap dominates, and this change removes
  ~75% of the bytes being parsed without that risk; if the budget still fails at 158K nodes, the
  archived architecture becomes the justified follow-up. Also out of scope: the dead `--svg`
  flag (`Parser.hs:73`, `cfgSVG` never read, `exportSVG` never called) and `graphos serve`
  gzip/ETag/streaming (`Static.hs:60–65` buffers the whole file per GET).

## PDCA Cycle

- **Plan**: Make `graph.html` an artifact whose size is proportional to its information content,
  and whose viewer is editable, testable and offline. Success is measured on the 104,101-node /
  122,347-edge reference corpus: total size ≤ 30 MB (from 101.2 MB), ≤ 200 B/node (from 409),
  ≤ 24 B/edge (from 441), zero network requests when opened from `file://` (today: one CDN
  script), overview load < 3 s and drill-down < 500 ms per the retained latency targets, and a
  non-empty test suite over the generated document (today: zero).
- **Do**: Land the payload contract and interning first (it is the whole size win and is
  independent of the viewer rewrite), then extract the assets, then the facets/panel/legend, then
  the depth selector, then the absorbed defect fixes.
- **Check**: Golden-file and property tests on the emitted payload (interning round-trips, no
  per-item styling keys, budget assertions computed from the emitted bytes), a JS syntax check of
  the generated document, and a recorded manual browser pass on the reference corpus for the
  latency and interaction criteria — the evidence the archived change left empty.
- **Act**: If the budget is met and the browser pass is green, close the spec/design contradiction
  by recording in `html-lod-viewer` that inline data remains the architecture at this scale, and
  open the sidecar/WebGL follow-up scoped to the measured residual. If the browser still stalls at
  158K nodes after a 4× payload reduction, that measurement is the trigger to revive the archived
  architecture — with evidence this time.
