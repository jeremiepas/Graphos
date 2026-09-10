# Design — Viewer Navigation

## Context

The viewer (`assets/viewer/viewer.js`, embedded at compile time per `html-viewer-assets`)
already funnels every state mutation through one dispatcher over a single state object
`{depth, selection, hops, facets, searchResults}`, persisted to sessionStorage. This change
gives that state an address (URL hash), a timeline (browser history), a display (breadcrumb),
a keyboard surface, and one new derived view (shortest path). Everything lives in the
Infrastructure layer (viewer assets + at most mount-point additions in
`Graphos.Infrastructure.Export.HTML`); Domain and UseCase are untouched — no Haskell type or
pipeline changes.

## Decision 1 — URL state in the hash fragment, not routes or query params

### Choice

Serialize the navigable subset of viewer state (depth, selection, hops, facets — not
`searchResults`) into the URL hash. On load: hash wins, sessionStorage is the fallback,
initial state the default.

### Alternatives considered

| Alternative | Rejected because |
|---|---|
| History API routes (`/c/483/n/mod_Auth`) | Requires server rewrite rules on `graphos serve` and breaks entirely on `file://`, which `html-viewer-interaction` requires to keep working |
| Query parameters (`?c=483&n=…`) | Changing them without the History API reloads the page — a 91 MB document; on `file://` semantics vary by browser |
| sessionStorage only (status quo) | Not shareable, not addressable; the core gap this change closes |

`searchResults` stays out of the URL: it is derived, potentially large, and
`navigator-query-view` can recompute it from the query text.

## Decision 2 — Browser history as the timeline, push/replace split by action class

### Choice

Position-changing actions (depth, selection) push a history entry; view-tuning actions
(facets, hops) replace the current entry. On-screen back/forward buttons call the browser
history rather than maintaining a parallel stack.

### Alternatives considered

| Alternative | Rejected because |
|---|---|
| In-viewer stack, browser history untouched | Two back buttons with different behaviors; browser Back still exits the page, which is the observed failure |
| Push every action including facet toggles | Back becomes a facet-undo, burying position history under filter tweaks; the spec explicitly forbids this |
| Snapshot full state in each history entry (state object in `history.state`) | Duplicates the hash codec; the hash alone must suffice anyway for deep links, so it is the single source of truth |

Hashchange (user editing the URL, or history traversal) feeds back into the same restore path
as page load, keeping one code path for "apply an external position".

## Decision 3 — Breadcrumb as pure derivation, no own state

The trail is computed from `depth` + `selection` + the already-loaded community aggregates
(labels). It stores nothing. Clicking a segment dispatches the same actions a user would
otherwise reach by other means, so history and URL behavior are inherited, not re-implemented.

Alternative considered: maintaining a visited-trail (actual path the user took). Rejected —
that is what history (Decision 2) is for; a breadcrumb showing *hierarchy* plus history
showing *chronology* is the conventional and less surprising split.

## Decision 4 — Declarative shortcut table

One table maps key → guard (inert-while-typing or not) → action → description. The document
keydown handler and the help overlay both render from it. This satisfies the spec requirement
that bindings and their documentation cannot drift, and makes rebinding a one-line change if a
key collides with a browser or vis-network default (vis pan/zoom keyboard handling stays
enabled and untouched — the table only claims keys vis does not use).

Alternative considered: scattering per-element key handlers (as the existing Escape-in-search
does). Rejected — no single source for the overlay, guards duplicated per handler.

## Decision 5 — Client-side BFS for path, no HTTP

The served document embeds the full node/edge payload, so shortest path is a breadth-first
search over an adjacency index built once at load. Undirected traversal, mirroring CLI
`graphos path` semantics (spec `05-path`) so viewer and CLI never disagree on reachability.
Path rendering reuses `highlightSubgraph`/`resetHighlight` from `navigator-query-view`'s
surface, with path members exempt from facet filtering (a path is an answer, not a filtered
view).

### Alternatives considered

| Alternative | Rejected because |
|---|---|
| New `/api/path` endpoint | Dead on `file://`; adds server surface (`query-http-port` change) for a computation the client can do over data it already holds |
| Dijkstra over confidence weights | CLI path is hop-based; diverging metrics between CLI and viewer would make the two disagree about "the" path |
| Restrict path to current drill-down scope | Cross-community connection is the most valuable case (bridges); spec requires cross-community paths to render |

## Verification strategy

- **Formal model** (`lean/`) — the navigation state machine is modeled in Lean 4 and every
  requirement proved as a theorem (codec round-trip, restore precedence and stale-link
  degradation, push/replace history semantics, breadcrumb/Escape agreement, shortcut-table
  completeness, path soundness and overlay restoration); all 19 spec scenarios are checked as
  executable examples. `lake build` in `lean/` is the check — see `lean/README.md` for the
  requirement → theorem map and what the model abstracts.
- `cabal build` — the viewer assets are embedded at compile time (`html-viewer-assets`), so a
  build proves the assets still embed; dev flag warnings remain errors.
- `cabal test` — extend `tests/Graphos/Infrastructure/Export/HTMLSpec.hs` structural checks:
  breadcrumb and overlay mount points present in the emitted document, every new CSS class
  used by viewer.js covered by viewer.css (existing cross-check), exactly one shortcut-table
  definition, no external references introduced.
- Scenario verification on the reference corpus via `graphos serve` and a `file://` open:
  deep-link restore, Back/Forward traversal, facet-replace behavior, Escape hierarchy,
  path-found / no-path / cross-community cases from the spec.
