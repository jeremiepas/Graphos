# Design — Elm Graph Studio

## Context

The studio is a standalone frontend under `studio/` consuming server capabilities that exist
(`query-http-port`, `cypher-mutation`, the views catalog) or are proposed
(`progressive-graph-interface`'s slice and group endpoints). It changes no Haskell code: in
clean-architecture terms it sits entirely outside the Domain/UseCase/Infrastructure boundary
as an API client, the same way the MCP clients do. Its internal architecture mirrors the
repo's discipline anyway: pure typed core (Elm Model/update), effects at the edges (commands
and ports).

## Decision 1 — Elm over Flutter Web

### Choice

Elm 0.19 SPA. The user's ask named Flutter with "elm syntax"; the resolution (confirmed) is
Elm itself.

### Alternatives considered

| Alternative | Rejected because |
|---|---|
| Flutter Web (Dart) | Canvas-rendered DOM: heavy bundles, weak text selection/find-in-page — wrong trade for a text-dense workbench; Dart is a second ecosystem in an ML-family repo |
| Flutter + TEA package | Flutter runtime costs remain; TEA-in-Dart is a pattern imitation without the compiler guarantees that make TEA valuable |
| React/TypeScript | Viable, but loses the no-runtime-exceptions guarantee and the 1:1 mapping to the already-verified navigation semantics; the team's languages are Haskell-shaped |

Elm's guarantees fit the artifact: the update function is total, the Model is the only state,
and the `viewer-navigation` Lean model — position, history zipper, escape, breadcrumb — *is*
a TEA update specification; the studio implements it clause for clause.

## Decision 2 — Rendering: vendored vis-network behind Elm ports

### Choice

The canvas is the same vendored vis-network used by `graph.html` (`html-viewer-assets`
vendoring rule), driven through a narrow port protocol: Elm sends declarative render
commands (set data, set visibility/colors, focus, fit, theme tokens); JS sends back
interaction events (click, multi-select, hover). All state stays in the Model; the port glue
holds only the network handle.

### Alternatives considered

| Alternative | Rejected because |
|---|---|
| elm-visualization (pure Elm, SVG) | SVG ceiling far below the drill-down sizes the LOD model produces; would fork rendering behavior from graph.html |
| Custom WebGL renderer | A project of its own; nothing in this change's requirements needs it |
| Reuse viewer.js wholesale via ports | viewer.js owns state (its dispatcher) — two state owners across a port is the split-brain the architecture forbids; only the renderer is shared |

One renderer across viewer and studio also means rendering fixes land in both.

## Decision 3 — One data-source interface, capability-probed

All features consume a single Elm interface (get aggregates / community page / neighborhood /
evaluate group / mutate), with three implementations resolved at connect time: local-file,
connected-with-slices, connected-legacy (full `graph.json` fetch + local evaluation, size-
guarded). Probing at connect (not per-call) keeps failure modes visible in the shell's
connection state.

Alternative considered: building only against the slice API and requiring
`progressive-graph-interface` to land first. Rejected — the studio is useful today against
existing servers and files; capability detection decouples the two changes' schedules.

## Decision 4 — Editing through the specced write subset only

Connected edits translate to `/api/cypher/mutate` statements (the `cypher-mutation` clause
subset); the server's read-only gating and reconciliation rules are inherited, not
reimplemented. File-mode edits are an ordered, identity-keyed local log with export — replay
is offered, never automatic, because a log applied to a different graph version is silent
corruption. Undo carries per-edit inverses recorded at accept time, so one history mechanism
serves both modes.

Alternative considered: a bespoke JSON-patch write endpoint for the studio. Rejected — a
second write path with its own validation would drift from the specced one; if the cypher
subset cannot express a needed edit, `cypher-mutation` gets extended by its own change.

## Decision 5 — Extraction targets the existing canonical view shape

Subgraph extraction encodes to `json-graph-web-view`'s canonical `{nodes, edges}` shape and
downloads. Rationale: every existing consumer works day one (drop into `graphos-out/views/`
and the catalog serves it), and the studio needs no server write surface. Server-side saving
is deferred; it belongs to the views capability if demanded.

## Decision 6 — Design system as a token module with a lint gate

Tokens and components live in dedicated modules; an elm-review rule (or grep-based CI check,
same pattern as HTMLSpec's structural checks) forbids literal colors/sizes outside the token
module. This is the same "generated from one table" discipline the keyboard overlay and
shortcut bindings use — drift is made unrepresentable, not reviewed away.

## Verification strategy

- **Studio**: `elm make --optimize` (total, no runtime exceptions by construction),
  `elm-test` for the pure core — navigation update clauses (asserting the invariants the
  `viewer-navigation` Lean model proves: escape descends, non-position actions preserve the
  back target, codec round-trip), scope algebra, edit-log fold/undo inverses, canonical-shape
  encoder round-trip — and `elm-review` with the token lint. All wired into CI beside the
  Haskell jobs.
- **Server contract**: unchanged server behavior is already covered by `cabal build` and
  `cabal test`; the studio adds no Haskell surface. Contract fixtures (a `graph.json` sample,
  a mutate request/response pair) are checked into `studio/tests/fixtures/` and kept in sync
  with `graph-json-contract` by a cabal test that regenerates them.
- **Scenarios**: walk each spec scenario against a reference corpus twice — file mode
  (offline) and connected mode (`graphos serve`) — plus the views-catalog round-trip for
  extraction.
