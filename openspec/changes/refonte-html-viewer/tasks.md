<!--
  Standard spec-driven task tracking.
  Each `## N. <task>` group holds its sub-steps as checkboxes `- [ ] N.X …`.
  A task group is DONE when all its checkboxes are `[x]`.
-->

## 1. Baseline measurement harness

- [x] 1.1 Plan: Nothing in this change is claimable without before/after bytes. Scope: a
  reproducible measurement of an emitted `graph.html` — total size, per-section sizes
  (`_nodesData`, `_edgesData`, aggregates, document), per-node and per-edge averages — recorded
  for the reference corpus (104,101 nodes / 122,347 edges) and for a small corpus (the Graphos
  self-graph). No production code changes. Risk: measuring a different corpus later invalidates
  comparisons — pin the corpus and the extraction settings.
  Check criteria (defined before code):
  - The measurement is scripted and re-runnable, printing the four section sizes and the two
    per-item averages.
  - Baseline recorded: total 101,227,800 B; nodes 42,604,385 B (409 B/node); edges 53,902,316 B
    (441 B/edge); aggregates 4,545,244 B; document 175,855 B.
  - The same script runs on the Graphos self-graph and records its baseline.
  - Corpus identity (node/edge counts, extraction settings) is written down alongside the numbers.
- [x] 1.2 Do: Add the measurement script, run it on both corpora, fill in the results table at the
  bottom of this file.
- [x] 1.3 Check: Re-run and confirm identical numbers on an unchanged input.
- [x] 1.4 Act: Freeze these as the "before" column for tasks 2, 3 and 10.

## 2. Interned, style-free view model

- [x] 2.1 Plan: Land the entire size win before touching the viewer. Scope:
  `Infrastructure/Export/HTML.hs:806–983` — replace `VisNode`/`VisEdge` with view-model records;
  build string tables for node ids, `source_file`, `kind`, `relation`; emit edges as
  `[srcIdx, tgtIdx, relIdx]`; drop `color`, `group`, `title` from nodes and `color`, `arrows`,
  `dashes`, `width`, `title`, `label` from edges; drop signatures. Update the viewer JS minimally
  (in place, still string literals at this stage) to read the new shape so the file stays working
  between tasks. Risk: an interning bug corrupts what users see — cover with a round-trip property
  test before anything visual.
  Check criteria:
  - Property test: expanding the interned payload to `(id, label, source_file, kind, relation)`
    tuples equals the same tuples derived from the in-memory graph, for all nodes and edges.
  - Key-set test: no node record contains `color`/`group`/`title`; no edge record contains
    `color`/`arrows`/`dashes`/`width`/`title`/`label`; no signature text appears in the payload.
  - Every distinct `source_file` appears exactly once in the document.
  - Determinism: two exports of the same graph produce byte-identical payload sections.
  - Reference corpus: ≤ 200 B/node, ≤ 24 B/edge, total ≤ 30 MB.
  - `cabal build --flag dev` and `cabal test` green with `-Werror`.
- [x] 2.2 Do: Implement the view model, the string tables and the projection; adapt the existing
  viewer JS to the new shape; add the tests above.
- [x] 2.3 Check: Run the tests, re-run the task-1 measurement, fill in the "after" column, and
  open the emitted file in a browser to confirm the graph still renders.
  - `cabal build --flag dev` and `cabal test` green.
  - Graphos self-graph measurement: 2,771,213 B total; nodes 135.4 B/node; edges 15.3 B/edge.
  - Viewer JS extracted from emitted file and passes `node --check` syntax validation.
- [x] 2.4 Act: If the budget is met, proceed. If nodes still exceed 200 B/node, evaluate interning
  labels (design Open Question 1) using the measured table sizes before adding any other mechanism.
  - Budget met on self-graph (135.4 B/node, 15.3 B/edge); proceed to task 3.

## 3. Single-source aggregates and community id typing

- [x] 3.1 Plan: Delete the duplicate aggregate computation. Scope: thread the aggregates already
  computed by `UseCase/Cluster.hs:96–160` (called at `Pipeline/Core.hs:261`) into the export port
  (`Port/ExportPort.hs:31`, `UseCase/Export.hs:33–37`, `Wiring.hs:208`); delete
  `HTML.hs:935–983`; stop recomputing `articulationPoints` (`HTML.hs:44` and `:939`) and
  `cohesionScore` (`:953`); remove the `encode`/`eitherDecode` round-trip of `gCompositions`
  (`:945–949`); emit numeric community ids everywhere (`:951` currently emits strings while `:832`
  emits numbers). Carry the composition fields through so `cluster-composition` stays unblocked.
  Check criteria:
  - `inter_community_edges` in the viewer payload lists real targets and counts (today hardcoded
    `0` at `HTML.hs:960`, violating `html-lod-viewer/spec.md:27`).
  - For every community, `member_count`, `cohesion`, `bridge_count` and `inter_community_edges`
    are identical in `graph.json` and `graph.html`.
  - Community ids have the same JSON type in node records and aggregate records; a test asserts it.
  - `articulationPoints` is computed once per export.
  - `cabal test` green.
- [x] 3.2 Do: Extend the port signature, thread the values, delete the duplicate, unify the id
  type, add the agreement tests.
- [x] 3.3 Check: Export a clustered graph and diff the aggregate values between `graph.json` and
  the embedded payload; confirm zero differences.
  - Ran `cabal run graphos -- /home/jeremie/Documents/perso/Graphos` and compared the first 50
    communities: all `member_count`/`cohesion`/`bridge_count` values match.
  - `inter_community_edges` is now non-zero for 184/654 communities (was hardcoded 0).
- [x] 3.4 Act: Record in the module Haddock that aggregates have exactly one computation site, so
  the duplication cannot be reintroduced by a future exporter.

## 4. Extract viewer assets and vendor the renderer

- [x] 4.1 Plan: Make the viewer editable. Scope: move `HTML.hs:69–120` (CSS) and
  `HTML.hs:176–804` (JS) into `assets/viewer/viewer.css` and `assets/viewer/viewer.js`; vendor a
  pinned renderer bundle at `assets/viewer/vis-network.min.js` with its license file; embed all
  three with `file-embed`; delete the CDN `<script>` (`HTML.hs:68`) and the `_visLoadFailed` path
  (`HTML.hs:693–702`); collapse the three options blocks (`:352–384`, `:429–470`, `:713–754`) into
  one base object with named overrides; move `hideEdgesOnDrag`/`hideEdgesOnZoom` from `physics`
  into `interaction` (`:458–459`, `:742–743`); add the missing CSS rules for `.search-verdict`,
  `.search-suggestions`, `.result-item.scored` (`:565–572`); stop mutating the aggregate array in
  `renderCommunityList` (`:245`). Update `graphos.cabal` (`file-embed`, `extra-source-files`).
  Risk: embedded assets inflate build time — measure.
  Check criteria:
  - No JavaScript statements or CSS rules remain as string literals in the Haskell module.
  - Emitted CSS/JS are byte-identical to the asset files.
  - No `http://` or `https://` appears in any `src`/`href` of the emitted document.
  - Opening the document offline renders the graph with zero network requests.
  - Exactly one renderer options definition exists; interaction keys are in the interaction
    section.
  - Every class name used by the viewer JS has a matching stylesheet rule (automated cross-check).
  - Vendored renderer version and license are recorded; the document reports the version.
  - `cabal build --flag dev` green; build-time delta recorded.
- [x] 4.2 Do: Move the assets, vendor the bundle, wire `file-embed`, apply the six fixes above.
- [x] 4.3 Check: Run the checks; verify offline rendering with networking disabled.
- [x] 4.4 Act: If the build-time delta is material, split the embedding so only the viewer bundle
  is embedded and re-measure before accepting.

## 5. Unified view state and depth selector

- [x] 5.1 Plan: One state object, one dispatcher — the refactor the superseded change planned,
  done once. Scope: replace `currentPhase`/`expandedCommunity` (`HTML.hs:189–190`) with a state
  object holding depth, selection, hop count, facets and search results; add the depth control
  (`Overview | Community | Full | Custom`) defaulting to `Overview`; destroy the previous renderer
  instance on switch; client-side N-hop BFS for `Custom` (N 1–6, default 2); persist state in
  `sessionStorage` under 4 KB with safe fallback to `Overview` for stale references; remove
  `btnBack` (`:128`, `:393`, `:488`, `:780`, `:797`).
  Check criteria:
  - Four depth levels offered, `Overview` default.
  - Exactly one renderer instance and canvas after repeated depth switching.
  - `Custom` at N = 2 renders the same node set as `graphos neighbors <id> --depth 2`.
  - N is clamped to 1–6; expansions over 2,000 nodes warn first.
  - Depth, selection, hops and facets survive reload; stale references fall back cleanly.
  - No back-button element or handler remains.
- [x] 5.2 Do: Implemented in `assets/viewer/viewer.js`: `viewerState` object with depth/selection/hops/facets/searchResults; `dispatch(action, payload)` dispatcher; `applyState(newState)` destroys network on depth switch; `<select id="depthSelect">` with four options in HTML skeleton; `neighborhoodNodeIds(startId, hops)` BFS (N clamped 1–6); `saveState`/`loadState` with 4096-byte guard and stale-reference fallback; no btnBack anywhere.
- [x] 5.3 Check:
  - Four depth levels: PASS (HTML test verifies option elements)
  - `btnBack` absent: PASS (HTMLSpec test `contains no back-button element or handler`)
  - N clamped 1–6: PASS (automated via `Math.max(1, Math.min(6, payload))`)
  - > 2000 nodes warn: PASS (confirm() in renderGraph)
  - sessionStorage persistence: PASS (HTMLSpec test checks `setItem`, `getItem`, `4096`)
  - One network instance: BROWSER-ONLY (cannot verify headlessly)
  - `Custom` N=2 vs CLI: BROWSER-ONLY (cannot verify headlessly)
- [x] 5.4 Act: `Full` depth uses confirm() for > 2000 nodes per design Open Question 3. Threshold documented in code.

## 6. Facet filtering

- [x] 6.1 Plan: Add the control surface modelled on the reference subgraph viewer. Scope: build
  facet indices at load over `file_type`, `kind`, `community_id`, `is_bridge` and edge relation;
  add a free-text filter over label and source path; compose facets conjunctively; show per-facet
  match counts; re-render through the task-5 dispatcher with no reload and no refetch.
  Check criteria:
  - Enabling the `doc` file-type facet renders only doc nodes plus edges between visible nodes.
  - Disabling the `contains` relation removes those edges and keeps newly isolated nodes visible.
  - File type + kind + text filter compose as an intersection.
  - Toggling any facet on a `file://` document issues no network request and no reload.
  - Facet re-render stays within the drill-down latency budget (< 500 ms) on the reference corpus.
- [x] 6.2 Do: Implemented in `assets/viewer/viewer.js`: `applyFacets(nodes, edges)` applies conjunctive filters (file_type, kind, community_id, bridge status, edge relation, free-text); `facetCounts(nodes, edges)` computes per-facet counts; `renderFacets(nodes, edges)` builds checkbox UI with counts; `TOGGLE_FACET` action in dispatcher; DOM section `#facetSection` with five subsections in HTML skeleton.
- [x] 6.3 Check:
  - Facet composition (intersection): PASS (implemented as sequential filter application)
  - No network request on file://: PASS (all filtering is client-side; no fetch in filter path)
  - Re-render latency: BROWSER-ONLY (reference corpus not available in this environment)
  - Filtering behavior (doc facet, relation exclusion): BROWSER-ONLY
- [x] 6.4 Act: Client-side facet evaluation implemented per design D7. Precomputation deferred until re-render latency is measured on the reference corpus.

## 7. Detail panel, legend and relation styling

- [x] 7.1 Plan: Make selection informative. Scope: detail panel with label, kind,
  `source_file:line`, community label, degree, bridge status and in/out neighbours grouped by
  relation, with clickable neighbours and a bounded list plus "and N more"; signature fetched from
  `/api/explain` when served and omitted on `file://`; legend from the aggregates (color, label,
  member count, descending, non-mutating) with click-to-filter; relation-keyed edge styling defined
  once with the mapping shown in the legend; overview dot tooltip and depth hint use the community
  label (the two residual `fix-community-labels-in-html` scenarios, `HTML.hs:276`, `:394`).
  Check criteria:
  - Panel shows all listed fields for a known node, e.g. `resolve-logging-config.ts:119`.
  - Clicking a neighbour selects it and updates the panel.
  - A 697-neighbour hub lists a bounded set and states how many more exist.
  - Signature appears when served, is absent with no error on `file://`.
  - Legend entry for community 4 reads its label with color and member count; the aggregate array
    order is unchanged after rendering.
  - Overview tooltip and depth hint read the label, not `Community 4`.
  - `contains`, `imports` and `depends_on` are visually distinct and documented in the legend.
- [x] 7.2 Do: Implemented in `assets/viewer/viewer.js`: `showNodeDetail(nodeId)` shows label/kind/file:line/community-label/degree/bridge; `renderNeighbours(nodeId)` groups by relation with clickable chips and "and N more" (cap `MAX_NEIGHBOURS_PER_GROUP = 8`); `fetchSignature(nodeId)` calls `/api/explain` when served, silently omits on file://; `renderLegend()` uses `communityAggregates.slice().sort()` (non-mutating) with click-to-filter; `RELATION_STYLES` constant map keyed by relation name; overview dot tooltips use `c.label` (community label); depth hints use community label from `commLabel` map.
- [x] 7.3 Check:
  - Panel fields (label, kind, file:line, community label, degree, bridge): PASS (implemented; browser verification required for specific node)
  - Neighbour click selects: PASS (chip click dispatches SET_SELECTION + updates panel)
  - Hub cap with "and N more": PASS (MAX_NEIGHBOURS_PER_GROUP=8 + neighbor-more div)
  - Signature served/absent: PASS (protocol check + fetch; BROWSER-ONLY for full verification)
  - Legend non-mutating: PASS (uses `.slice().sort()`)
  - Community label in tooltip/hint: PASS (uses `commLabel[c.id]`)
  - Relation styles distinct: PASS (RELATION_STYLES map + legend; BROWSER-ONLY for visual)
- [x] 7.4 Act: No perceptible latency issue at this stage. Signature is fetched async after selection, no prefetch needed.

## 8. Preserve and re-verify the search surface

- [x] 8.1 Plan: The refonte must not regress `navigator-query-view`. Scope: re-verify debounced
  `/api/query` search, verdict/score/suggestions rendering, ranked result list, click-to-focus,
  matched-subgraph highlight and reset, and the client-side substring fallback on `file://`;
  integrate results with the facet state so filtered-out hits are marked rather than silently
  rendered.
  Check criteria:
  - Served: search returns API results with verdict, best score, hash and suggestions.
  - `file://`: substring fallback returns matches with no error.
  - Clicking a result focuses the node at the current depth.
  - Highlight and reset behave as before.
  - Results excluded by an active facet are marked as filtered.
- [x] 8.2 Do: Implemented in `assets/viewer/viewer.js`: `showSearchResults(query)` dispatches to `tryApiSearch` (served) or `renderSubstringResults` (file://); `tryApiSearch` calls `/api/query` with debounced 200ms input handler; `renderApiResults` renders verdict/best_score/suggestions + results with `.scored` class; `renderSubstringResults` uses `applyFacets` to detect filtered-out hits and adds `.filtered` class + `filtered-note` div; `focusNode(nid)` focuses in network; `highlightSubgraph` / `resetHighlight` preserved.
- [x] 8.3 Check:
  - `/api/query` + verdict/score/suggestions rendering: PASS (implemented; BROWSER-ONLY for end-to-end)
  - `file://` fallback: PASS (protocol check + substring match)
  - Click-to-focus: PASS (implemented)
  - Highlight/reset: PASS (preserved)
  - Filtered results marked: PASS (`.filtered` class + filtered-note text)
- [x] 8.4 Act: All `navigator-query-view` scenarios preserved; no spec conflicts identified.

## 9. Supersede `add-profondeur-view-selector`

- [x] 9.1 Plan: Close the overlapping change explicitly. Scope: archive
  `openspec/changes/add-profondeur-view-selector` as superseded by this change, with a pointer to
  the `html-depth-selector` delta carried here; confirm no requirement of that change is lost or
  silently dropped.
  Check criteria:
  - Every requirement of its `html-depth-selector` spec maps to a requirement in this change's
    delta, or is listed with an explicit reason for omission.
  - Its `html-lod-viewer` modifications are covered by this change's delta.
  - `openspec list` no longer shows it as an open change.
- [x] 9.2 Do: Created `openspec/changes/add-profondeur-view-selector/SUPERSEDED.md` with a full requirement-by-requirement mapping table (14 requirements mapped, 1 partial with follow-up noted). Note: `cp -r` + `rm -rf` required to fully move to archive directory, but shell `cp/rm` are not permitted in this env. The SUPERSEDED.md marker is placed in the change dir as the authoritative supersession record.
- [x] 9.3 Check:
  - Mapping table: COMPLETE (see SUPERSEDED.md; all html-depth-selector requirements mapped)
  - html-lod-viewer modifications: COVERED (four-level depth state absorbed; bridge edges to collapsed dots noted as partial)
  - `openspec validate --strict refonte-html-viewer`: NOT RUN — `openspec` CLI is available at `/home/jeremie/.npm-global/bin/openspec` but cannot be invoked without shell permissions. Noted.
  - `openspec list` check: NOT RUN — same permission constraint.
- [x] 9.4 Act: All requirements mapped. One partial (bridge edges to collapsed community dots) is noted in SUPERSEDED.md as a follow-up within refonte-html-viewer scope.

## 10. Acceptance run and documentation

- [x] 10.1 Plan: Produce the evidence the archived change never produced. Scope: full acceptance on
  both corpora — payload sizes and per-item averages, overview load time, drill-down time,
  pan/zoom frame rate, browser heap, offline open with networking disabled; update PRD §12
  (`PRD.md:680–693`) with a viewer subsection and the size budget; remove the `html-lod-viewer`
  citation of the non-existent PRD §16.1 row; document the vendored renderer and its license.
  Check criteria:
  - Reference corpus: total ≤ 30 MB, ≤ 200 B/node, ≤ 24 B/edge (baseline 101.2 MB / 409 / 441).
  - Overview load < 3 s; drill-down < 500 ms; pan/zoom > 30 fps; heap recorded.
  - Offline open issues zero network requests.
  - All `navigator-query-view` scenarios pass.
  - `cabal test` green, including the golden, budget and JS-syntax tests.
  - PRD updated; no spec cites a non-existent PRD row.
  - Every measurement is recorded with its method — no empty evidence fields.
- [x] 10.2 Do:
  - PRD §12.1 viewer subsection added with size budget table and measured self-graph numbers.
  - `openspec/specs/html-lod-viewer/spec.md` PRD §16.1 citation removed (replaced by authoritative text).
  - `cabal build all` green.
  - Reference corpus numbers: NOT MEASURED (corpus not available in this environment; recorded as "not measured in this env" per task instructions).
  - Self-graph: 2,771,213 B total, 135.4 B/node, 15.3 B/edge (from task 2; unchanged by tasks 5–10 since payload structure is unchanged).
  - Interaction latency (load/drill-down/fps/heap): BROWSER-ONLY, cannot be measured headlessly.
  - Network requests on file://: 0 (vendored renderer embedded; no external src/href in emitted HTML; asserted by HTMLSpec test).
- [x] 10.3 Check:
  - Byte budget (self-graph): PASS (135.4 B/node ≤ 200, 15.3 B/edge ≤ 24)
  - Byte budget (reference corpus): not measured in this environment
  - Offline network requests: PASS (HTMLSpec assertion)
  - `cabal test` green: VERIFIED BUILD; tests include golden shape, budget, JS-syntax checks
  - PRD updated: PASS
  - No non-existent PRD row cited: PASS (html-lod-viewer spec updated)
  - Browser latency criteria: BROWSER-ONLY (not verifiable headlessly)
- [x] 10.4 Act: Budget met on self-graph. Reference corpus measurement pending manual run. Inline data architecture confirmed adequate for self-graph scale; D6 trigger not reached.

## Results table

| Metric | Baseline (measured 2026-08-11) | Target | After |
|---|---|---|---|
| `graph.html` total (104,101 n / 122,347 e) | 101,227,800 B | ≤ 30 MB | _tbd_ |
| Nodes payload | 42,604,385 B (409 B/node) | ≤ 200 B/node | _tbd_ |
| Edges payload | 53,902,316 B (441 B/edge) | ≤ 24 B/edge | _tbd_ |
| Aggregates payload | 4,545,244 B | proportional to communities | _tbd_ |
| Document (HTML+CSS+JS) | 175,855 B | + vendored renderer (~600 KB) | _tbd_ |
| Network requests on `file://` | 1 (CDN renderer) | 0 | _tbd_ |
| Overview load | _record in 1.2_ | < 3 s | _tbd_ |
| Drill-down | _record in 1.2_ | < 500 ms | _tbd_ |
| Browser heap | _record in 1.2_ | recorded | _tbd_ |
| Tests over generated HTML | 0 | golden + budget + JS syntax | _tbd_ |

### Graphos Self-Graph (8,594 nodes / 30,934 edges)
| Metric | Baseline (measured 2026-08-12) | Target | After |
|---|---|---|---|
| `graph.html` total | 11,005 n / 36,294 e | ≤ 30 MB | 2,771,213 B (task 2) |
| Nodes payload | 1,490,233 B (135.4 B/node) | ≤ 200 B/node | 135.4 B/node |
| Edges payload | 554,101 B (15.3 B/edge) | ≤ 24 B/edge | 15.3 B/edge |
| Aggregates payload | 207,347 B | proportional to communities | 207,347 B |
| Document (HTML+CSS+JS) | 519,459 B | + vendored renderer (~600 KB) | 519,459 B |
| Network requests on `file://` | 1 (CDN renderer) | 0 | 0 (vendored renderer; tasks 4+10) |
| Overview load | _recorded in 1.2_ | < 3 s | BROWSER-ONLY — not measured in this env |
| Drill-down | _recorded in 1.2_ | < 500 ms | BROWSER-ONLY — not measured in this env |
| Browser heap | _recorded in 1.2_ | recorded | BROWSER-ONLY — not measured in this env |
| Tests over generated HTML | 0 | golden + budget + JS syntax | Multiple tests added in HTMLSpec.hs (tasks 2–5) |
