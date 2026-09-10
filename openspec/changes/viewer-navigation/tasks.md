# Tasks — Viewer Navigation

## 1. Hash codec + restore precedence (foundation)

### 1.1 State ↔ hash codec
- [ ] Define the encode/decode pair in `assets/viewer/viewer.js` for `{depth, selection, hops, facets}` (exclude `searchResults`)
- [ ] Check criteria first: decode(encode(s)) round-trips for Overview, community-only, community+node, with and without facets; malformed hash decodes to `null`, never throws
- [ ] Guard hash length the same way sessionStorage guards 4 KB; on overflow keep depth+selection, drop facets from the URL

### 1.2 Restore precedence on load
- [ ] Load order: hash → sessionStorage → `initialState()`, reusing the existing stale-reference check (unknown node/community → Overview) for both sources
- [ ] Check: opening a deep link in a fresh tab restores position on `graphos serve` AND on `file://` with zero network requests (spec scenarios "Deep link restores position", "Works on file://", "Stale deep link degrades to Overview")

### 1.3 Write hash on dispatch
- [ ] Update the hash from `applyState` so every mutation path is covered once
- [ ] Check: after drill-down + node select, copying the URL into a new tab reproduces the view (scenario "URL reflects navigation")

## 2. History integration (depends on 1)

### 2.1 Push/replace split
- [ ] Route `SET_DEPTH`/`SET_SELECTION` through history-pushing navigation; `SET_FACETS`/`TOGGLE_FACET`/`SET_HOPS` replace the current entry
- [ ] Handle hashchange/popstate by feeding the decoded state through the same restore path as load — one code path for external positions
- [ ] Check: Overview → community → node, then Back twice lands on community then Overview without page unload; Forward re-selects the node (scenarios "Back returns…", "Forward replays…"); four facet toggles then Back skips all of them (scenario "Facet toggles do not pollute history")

### 2.2 On-screen back/forward controls
- [ ] Add toolbar back/forward buttons mirroring browser history, disabled at the ends
- [ ] Add the buttons' classes to `assets/viewer/viewer.css`
- [ ] Check: back control disabled on fresh load, enabled after first drill-down (scenario "On-screen controls mirror history")

## 3. Breadcrumb (depends on 1; parallel-safe with 2)

### 3.1 Trail derivation + render
- [ ] Derive up to three segments (Overview ▸ community label ▸ node label) from state + community aggregates; label falls back to community id; render into a dedicated container; last segment not a link; ellipsis + title on overflow
- [ ] Ancestor clicks dispatch normal navigation (inherits history/URL behavior from 2)
- [ ] Mount point added in `Graphos.Infrastructure.Export.HTML` if the shell lacks a container
- [ ] Check: trail matches spec scenario "Trail reflects drill-down"; clicking the community segment clears selection and is Back-able; trail correct immediately after deep-link restore

## 4. Keyboard map (parallel-safe with 2–3)

### 4.1 Declarative shortcut table + handler
- [ ] Single table: key, inert-while-typing guard, action, description; one document-level keydown handler; leave vis-network's own keyboard pan/zoom untouched; migrate the existing Escape-in-search binding into the table
- [ ] Shortcuts: focus-search, Escape hierarchy (panel → up level → clear search highlight), fit view, help overlay
- [ ] Check: search shortcut focuses input without typing the key; Escape twice from open panel = panel closed then Overview; bound keys typed in inputs insert normally (spec scenarios 1–3 of the keyboard requirement)

### 4.2 Help overlay
- [ ] Overlay rendered from the shortcut table; closes on Escape or outside click; CSS classes covered in viewer.css
- [ ] Check: overlay lists every table entry; Escape closes only the overlay (scenario "Help overlay lists bindings")

## 5. Shortest path (depends on 1; parallel-safe with 2–4)

### 5.1 Adjacency index + BFS
- [ ] Build the undirected adjacency index once at load; BFS shortest path between two node ids; check criteria first: known 3-hop fixture path found exactly, disconnected pair returns no-path, endpoint==endpoint returns trivial path
- [ ] If load-time indexing is too costly on the reference corpus, build lazily on first path request and record the latency in the change notes

### 5.2 Panel affordances + path view
- [ ] "Path from here"/"path to here" actions on the node detail panel writing endpoints into viewer state; on both set, run BFS and render via `highlightSubgraph` with path members exempt from facet filtering; summary shows endpoints + hop count; single clear action restores prior depth/selection/facets via `resetHighlight`
- [ ] No-path case: explicit message, current highlighting untouched
- [ ] Cross-community case: path nodes render even when outside the current drill-down scope
- [ ] Check: the four spec scenarios of the path requirement pass on the reference corpus

## 6. Structural tests + validation (last; depends on all)

### 6.1 HTMLSpec extensions
- [ ] Extend `tests/Graphos/Infrastructure/Export/HTMLSpec.hs`: breadcrumb + overlay mount points present in the emitted document; new viewer.js classes covered by viewer.css (existing cross-check picks these up); exactly one shortcut-table definition; no external references introduced
- [ ] `cabal build` green (assets embed, `-Werror` dev flag), `cabal test` green

### 6.2 Spec conformance pass
- [ ] Walk every scenario of `viewer-navigation/spec.md` on `graphos serve` and one `file://` open; tick each scenario in the PR description
- [ ] `openspec change validate viewer-navigation` passes

### 6.3 Formal model stays green
- [ ] If implementation deviates from the modeled semantics (action classification, escape order, restore precedence, path checking), update `lean/` to match and keep `lake build` green — the model is the reference for the dispatcher's navigation semantics
- [ ] `cd lean && lake build` (or `nix shell nixpkgs#lean4 -c lake build`) passes with no `sorry`
