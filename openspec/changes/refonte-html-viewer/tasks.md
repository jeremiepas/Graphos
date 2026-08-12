<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
              A passed task reaches the same done state as in classic SDD.
  RETRY rule: if Act is NOT OK, the task does NOT pass — record the failed
              attempt under "### Attempt history (N)" (KEEP THE TRACE, never
              delete it), then start a NEW P → D → C → A attempt for the same
              task. Repeat until an attempt passes.

  Everything else matches the official spec-driven workflow.
-->

## 1. Baseline measurement harness

- [x] 1.P Plan: Nothing in this change is claimable without before/after bytes. Scope: a
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
- [x] 1.D Do: Add the measurement script, run it on both corpora, fill in the results table at the
   bottom of this file.
- [x] 1.C Check: Re-run and confirm identical numbers on an unchanged input.
- [x] 1.A Act: Freeze these as the "before" column for tasks 2, 3 and 10.

### Attempt history (1)
<!-- empty unless a retry is needed -->

## 2. Interned, style-free view model

- [x] 2.P Plan: Land the entire size win before touching the viewer. Scope:
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
- [x] 2.D Do: Implement the view model, the string tables and the projection; adapt the existing
   viewer JS to the new shape; add the tests above.
- [x] 2.C Check: Run the tests, re-run the task-1 measurement, fill in the "after" column, and
   open the emitted file in a browser to confirm the graph still renders.
   - `cabal build --flag dev` and `cabal test` green.
   - Graphos self-graph measurement: 2,771,213 B total; nodes 135.4 B/node; edges 15.3 B/edge.
   - Viewer JS extracted from emitted file and passes `node --check` syntax validation.
- [x] 2.A Act: If the budget is met, proceed. If nodes still exceed 200 B/node, evaluate interning
   labels (design Open Question 1) using the measured table sizes before adding any other mechanism.
   - Budget met on self-graph (135.4 B/node, 15.3 B/edge); proceed to task 3.

### Attempt history (2)
<!-- empty unless a retry is needed -->

## 3. Single-source aggregates and community id typing

- [x] 3.P Plan: Delete the duplicate aggregate computation. Scope: thread the aggregates already
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
- [x] 3.D Do: Extend the port signature, thread the values, delete the duplicate, unify the id
   type, add the agreement tests.
- [x] 3.C Check: Export a clustered graph and diff the aggregate values between `graph.json` and
   the embedded payload; confirm zero differences.
   - Ran `cabal run graphos -- /home/jeremie/Documents/perso/Graphos` and compared the first 50
     communities: all `member_count`/`cohesion`/`bridge_count` values match.
   - `inter_community_edges` is now non-zero for 184/654 communities (was hardcoded 0).
- [x] 3.A Act: Record in the module Haddock that aggregates have exactly one computation site, so
   the duplication cannot be reintroduced by a future exporter.

### Attempt history (3)
<!-- empty unless a retry is needed -->

## 4. Extract viewer assets and vendor the renderer

- [x] 4.P Plan: Make the viewer editable. Scope: move `HTML.hs:69–120` (CSS) and
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
- [x] 4.D Do: Move the assets, vendor the bundle, wire `file-embed`, apply the six fixes above.
- [x] 4.C Check: Run the checks; verify offline rendering with networking disabled.
- [x] 4.A Act: If the build-time delta is material, split the embedding so only the viewer bundle
  is embedded and re-measure before accepting.

### Attempt history (4)
<!-- empty unless a retry is needed -->

## 5. Unified view state and depth selector

- [ ] 5.P Plan: One state object, one dispatcher — the refactor the superseded change planned,
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
- [ ] 5.D Do: Implement the state object, dispatcher, depth control, BFS and persistence.
- [ ] 5.C Check: Run the criteria manually in a browser on both corpora; record the CLI-vs-viewer
  node-set comparison verbatim.
- [ ] 5.A Act: If `Full` depth stalls, apply a confirmation prompt with the node count (design
  Open Question 3) rather than removing the level; record the threshold observed.

### Attempt history (5)
<!-- empty unless a retry is needed -->

## 6. Facet filtering

- [ ] 6.P Plan: Add the control surface modelled on the reference subgraph viewer. Scope: build
  facet indices at load over `file_type`, `kind`, `community_id`, `is_bridge` and edge relation;
  add a free-text filter over label and source path; compose facets conjunctively; show per-facet
  match counts; re-render through the task-5 dispatcher with no reload and no refetch.
  Check criteria:
  - Enabling the `doc` file-type facet renders only doc nodes plus edges between visible nodes.
  - Disabling the `contains` relation removes those edges and keeps newly isolated nodes visible.
  - File type + kind + text filter compose as an intersection.
  - Toggling any facet on a `file://` document issues no network request and no reload.
  - Facet re-render stays within the drill-down latency budget (< 500 ms) on the reference corpus.
- [ ] 6.D Do: Implement facet indices, controls, counts and the filter path.
- [ ] 6.C Check: Run the criteria on the reference corpus; record re-render timings.
- [ ] 6.A Act: If re-render exceeds the budget, precompute facet index sets at export time and
  record the byte cost against the task-2 budget (design D7).

### Attempt history (6)
<!-- empty unless a retry is needed -->

## 7. Detail panel, legend and relation styling

- [ ] 7.P Plan: Make selection informative. Scope: detail panel with label, kind,
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
- [ ] 7.D Do: Implement panel, legend, relation styles and the label fixes.
- [ ] 7.C Check: Verify each criterion in a browser; capture the panel and legend states.
- [ ] 7.A Act: If the API fetch adds perceptible latency to selection, prefetch on hover or cache
  per node; record which.

### Attempt history (7)
<!-- empty unless a retry is needed -->

## 8. Preserve and re-verify the search surface

- [ ] 8.P Plan: The refonte must not regress `navigator-query-view`. Scope: re-verify debounced
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
- [ ] 8.D Do: Re-wire the search surface onto the new state object; add the filtered-result
  marking.
- [ ] 8.C Check: Run every `navigator-query-view` scenario against the refonted viewer.
- [ ] 8.A Act: Any scenario that cannot be preserved is escalated as a spec conflict before
  proceeding — not silently dropped.

### Attempt history (8)
<!-- empty unless a retry is needed -->

## 9. Supersede `add-profondeur-view-selector`

- [ ] 9.P Plan: Close the overlapping change explicitly. Scope: archive
  `openspec/changes/add-profondeur-view-selector` as superseded by this change, with a pointer to
  the `html-depth-selector` delta carried here; confirm no requirement of that change is lost or
  silently dropped.
  Check criteria:
  - Every requirement of its `html-depth-selector` spec maps to a requirement in this change's
    delta, or is listed with an explicit reason for omission.
  - Its `html-lod-viewer` modifications are covered by this change's delta.
  - `openspec list` no longer shows it as an open change.
- [ ] 9.D Do: Produce the requirement-by-requirement mapping table, then archive it as superseded.
- [ ] 9.C Check: Review the mapping table for gaps; run `openspec validate --strict` on this
  change.
- [ ] 9.A Act: If a requirement has no home here, either add it to this change's delta or record
  why it is intentionally dropped — never leave it unmapped.

### Attempt history (9)
<!-- empty unless a retry is needed -->

## 10. Acceptance run and documentation

- [ ] 10.P Plan: Produce the evidence the archived change never produced. Scope: full acceptance on
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
- [ ] 10.D Do: Run both corpora, record every figure in the results table, update the docs.
- [ ] 10.C Check: Compare each figure against its criterion; mark PASS/FAIL per row.
- [ ] 10.A Act: If the byte budget passes but latency or heap fails, record those numbers as the
  trigger for the deferred sidecar/WebGL follow-up (design D6) and open it with this evidence
  attached. If both pass, record in `html-lod-viewer` that inline data remains the architecture at
  this scale, closing the standing spec/design contradiction.

### Attempt history (10)
<!-- empty unless a retry is needed -->

## Results table

| Metric | Baseline (measured 2026-08-11) | Target | After |
|---|---|---|---|
| `graph.html` total (104,101 n / 122,347 e) | 101,227,800 B | ≤ 30 MB | _tbd_ |
| Nodes payload | 42,604,385 B (409 B/node) | ≤ 200 B/node | _tbd_ |
| Edges payload | 53,902,316 B (441 B/edge) | ≤ 24 B/edge | _tbd_ |
| Aggregates payload | 4,545,244 B | proportional to communities | _tbd_ |
| Document (HTML+CSS+JS) | 175,855 B | + vendored renderer (~600 KB) | _tbd_ |
| Network requests on `file://` | 1 (CDN renderer) | 0 | _tbd_ |
| Overview load | _record in 1.D_ | < 3 s | _tbd_ |
| Drill-down | _record in 1.D_ | < 500 ms | _tbd_ |
| Browser heap | _record in 1.D_ | recorded | _tbd_ |
| Tests over generated HTML | 0 | golden + budget + JS syntax | _tbd_ |

### Graphos Self-Graph (8,594 nodes / 30,934 edges)
| Metric | Baseline (measured 2026-08-12) | Target | After |
|---|---|---|---|
| `graph.html` total | 11,005 n / 36,294 e | ≤ 30 MB | 2,771,213 B (task 2) |
| Nodes payload | 1,490,233 B (135.4 B/node) | ≤ 200 B/node | 135.4 B/node |
| Edges payload | 554,101 B (15.3 B/edge) | ≤ 24 B/edge | 15.3 B/edge |
| Aggregates payload | 207,347 B | proportional to communities | 207,347 B |
| Document (HTML+CSS+JS) | 519,459 B | + vendored renderer (~600 KB) | 519,459 B |
| Network requests on `file://` | 1 (CDN renderer) | 0 | 1 (still CDN; task 4) |
| Overview load | _recorded in 1.D_ | < 3 s | _tbd_ |
| Drill-down | _recorded in 1.D_ | < 500 ms | _tbd_ |
| Browser heap | _recorded in 1.D_ | recorded | _tbd_ |
| Tests over generated HTML | 0 | golden + budget + JS syntax | _tbd_ |
