# Tasks — Elm Graph Studio

> **v1 implementation status (2026-09-11)** — `studio/` implemented and green:
> `elm make --optimize` + 55 elm-tests passing, devenv toolchain, Studio CI
> workflow with token-lint gate. Covered: TEA shell, navigation with the
> Lean-model invariants tested, tokens+components+themes, file mode (picker,
> drag-drop, size guard, aggregate synthesis), connected mode (probe, legacy
> full-fetch, optimistic mutate + revert), editing (intents/validation/
> inverses/log/undo-redo/export), groups (local eval, first-match-wins,
> isolate/hide, persistence, import/export, server-assisted counts), scope
> algebra + dim preview + canonical export with color baking, component gallery
> (both themes) and dialog keyboard completeness (focus trap, Enter, Escape,
> focus return).
> **Not yet done**: elm-review config (CI lints via grep instead), slice-backed
> data layer beyond probe (server endpoints not shipped), views-catalog
> round-trip walk, task 7.1 cabal-regenerated fixtures, live-server scenario
> pass (6.2/7.2).
> Data sources 2.1–2.3 are complete: a measured 100 MB size guard (recorded in
> `studio/README.md`), a three-mode data-source interface (file / slices /
> legacy) wired through `Main.elm`, slice-mode overview boot, labeled legacy
> unavailability, mid-session Retry that preserves local work, and identity-keyed
> per-graph persistence with corpus A/B isolation — all covered by elm-test.

## 1. Scaffold (foundation)

### 1.1 Toolchain + skeleton
- [x] Add elm, elm-test, elm-review to `devenv.nix`; create `studio/` with `elm.json`, `src/Main.elm` (TEA skeleton), `ports.js`, vendored vis-network shared with `assets/viewer/`
- [x] Check criteria first: `elm make --optimize` and `elm-test` run green in the devenv shell on a fresh clone
- [x] CI job: studio build + test + review beside the Haskell jobs; a broken Elm type fails CI

### 1.2 Design tokens + component base
- [x] `Studio.DesignSystem.Tokens` (color roles, type/spacing scales, radii, elevation; light/dark bindings; community-color harmonization) and `Studio.DesignSystem.Components` (button, input, select, slider, panel, dialog, toast, badge, empty/loading states)
- [x] Lint gate: elm-review rule (or CI grep) — no literal colors/px outside Tokens
- [x] Check: component gallery page renders in both themes; dialog keyboard scenarios (trap, Enter, Escape, focus return)

## 2. Data sources (depends on 1.1)

### 2.1 Contract decoder + file mode
- [x] One decoder for the `graph-json-contract` shape (+ `community_aggregates`; synthesize aggregates when absent); file picker + drag-drop via port; size guard with the documented threshold (measure parse ceiling on reference hardware, record in studio/README.md)
- [x] elm-test: decoder round-trip on the checked-in fixture; guard refusal path
- [x] Check: 40 MB file renders offline (measured ~43 MB decodes in ~206 ms / ~482 MB peak RSS); oversized file refused with responsive tab

### 2.2 Connected mode + capability probe
- [x] Origin input + probe (slice endpoints, group evaluation, write surface); three data-source implementations behind one interface (file / slices / legacy fetch); connection state in the shell (origin, capabilities, hash)
- [x] Check: slice server boots from aggregates with no full-graph request; legacy server falls back with labeled unavailability; unreachable origin shows retry state preserving local work

### 2.3 Graph identity + per-graph persistence
- [x] Identity (server hash / file fingerprint) threaded through persistence keys (groups, tuning, edit log); on identity change drop resident data, never mix
- [x] elm-test: persistence keying; Check: corpus A/B group isolation scenario (logic-level verified)

## 3. Canvas + navigation (depends on 2.1; parallel-safe with 2.2–2.3)

### 3.1 Render port protocol
- [ ] Declarative port commands (setData, setVisibility, setColors, focus, fit, applyThemeTokens) and event subscriptions (click, multiSelect, hover); JS glue holds only the network handle
- [ ] Check: state-lives-in-Model scenario — no state in port glue beyond handles

### 3.2 Navigation with verified semantics
- [ ] Position/tuning types + URL codec + history push/replace + breadcrumb + Escape, implementing the `viewer-navigation` model (Lean model as reference for update clauses)
- [ ] elm-test: codec round-trip; escape-descends; non-position actions preserve back target; breadcrumb(escape p) = dropLast(breadcrumb p)
- [ ] Check: deep-link restore, history scenario (group toggles create no entries), Escape hierarchy

## 4. Editing (depends on 2 + 3)

### 4.1 Edit intents + validation + forms
- [ ] Edit-intent type (relabel, retype, extra-edit, node create/delete, edge create/delete) with client-side validation against the 12-field node schema; detail-panel forms from components; blast-radius confirmation on deletion
- [ ] elm-test: validation table; intent → inverse construction

### 4.2 Connected mutation path
- [ ] Intent → cypher statement translation (`cypher-mutation` subset); optimistic apply + server reconciliation; revert + error surface on rejection; affordances disabled with reason when read-only
- [ ] Check: rename round-trips across reload; rejection reverts scenario

### 4.3 File-mode edit log + export + undo/redo
- [ ] Identity-keyed ordered log, persisted; replay offered on matching load; export edited `graph.json` (contract shape) + changelog JSON; undo/redo zipper over inverses, keyboard-bound, both modes
- [ ] elm-test: log fold determinism; undo∘apply = identity on the model
- [ ] Check: offline-edits-survive-reload, export-carries-edits, hub-deletion undo restores edges

## 5. Groups (depends on 2; parallel-safe with 4)

### 5.1 Panel + semantics + persistence
- [ ] Ordered groups panel (query, color, hide/isolate) with first-match-wins coloring via the visibility/color port; persistence by identity; import/export interchangeable with the viewer's group-set JSON
- [ ] Check: viewer-exported set loads with same order/colors; precedence + isolate scenario

### 5.2 Dual-mode evaluation
- [ ] Server evaluation via the group endpoint when probed; local evaluation (label/path/kind) otherwise; evaluation-mode badge; local counts labeled loaded-graph-only when partial; re-evaluate after edits
- [ ] Check: connected 3,000/200-resident scenario; file-mode local labeling; edit re-evaluation

## 6. Subgraph extraction (depends on 3 + 5)

### 6.1 Scope algebra + preview
- [ ] Scope as set algebra (community ∪ group ∪ neighborhood(depth) ∪ selection) with live counts and boundary-edges toggle; dim + isolated preview via the visibility port; dismiss restores prior position
- [ ] elm-test: algebra (union, boundary edge rule: both-endpoints-in-scope + optional one-hop)
- [ ] Check: composed-scope counts, boundary toggle, preview-isolates scenario

### 6.2 Canonical export
- [ ] Scope → `json-graph-web-view` canonical `{nodes, edges}` encoder (string ids, labels, community metadata, relation; optional group-color baking); title prompt; download port
- [ ] elm-test: encoder output validates against the canonical-shape fixture
- [ ] Check: round-trip — exported file dropped in `graphos-out/views/` lists and renders in the catalog; baked-colors scenario

## 7. Conformance (last)

### 7.1 Contract fixtures stay honest
- [ ] `studio/tests/fixtures/` (graph.json sample, mutate request/response) regenerated by a cabal test so drift from `graph-json-contract`/`cypher-mutation` fails the Haskell suite
- [ ] `cabal build`, `cabal test`, studio CI job all green

### 7.2 Spec conformance pass
- [ ] Walk every scenario across the six studio specs in both modes (file, connected); tick in the PR description
- [ ] `openspec validate elm-graph-studio` passes
