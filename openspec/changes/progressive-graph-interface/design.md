# Design — Progressive Graph Interface

## Context

Three constraints shape everything: (1) target graphs reach 4.4 GB of `graph.json`, so
neither the wire nor the browser may ever see the whole payload; (2) `graphos serve` already
holds one shared in-memory graph behind an `IORef` with a `GraphIndex` built for 500k+ nodes
(inverted label index, community reverse index, precomputed adjacency); (3) the viewer
already has a two-phase LOD model whose overview renders purely from `community_aggregates`.
The change is Infrastructure-only: new server routes (Infrastructure.Server), export mode
selection (Infrastructure.Export.HTML), and viewer asset work — group evaluation reuses the
existing UseCase.Query scoring; Domain is untouched.

## Decision 1 — Slice the in-memory graph; keep server storage out of scope

### Choice

The slice API reads from the existing shared load (`SharedLoadRef` + `GraphIndex`). Bounding
happens at the response layer: hard caps, cursors, totals, explicit truncation. Server RAM
stays the current model.

### Alternatives considered

| Alternative | Rejected because |
|---|---|
| On-disk indexed store (SQLite/offset index) read per request | Solves a different problem (server RAM) at much higher cost; `GraphIndex` already answers slice queries in O(k); becomes the natural *follow-up* change if deployments hit RAM limits — the API contract designed here would not change |
| Streaming the full `graph.json` to the client progressively | The browser still ends up holding the graph; 4.4 GB of vis-network DataSet is unrunnable regardless of arrival order |
| Pre-sharding graph.json into per-community files at export | Duplicates data on disk, breaks single-source-of-truth `graphos-out/` contract, and cannot serve neighborhood or group queries that cross shards |

## Decision 2 — Dual-mode viewer selected at export by payload threshold

### Choice

One `graph.html`, two data layers behind one interface: embedded (today's interned payload)
below the threshold, slice-backed above it. Mode is baked at export (flag-overridable); the
viewer code is shared.

### Alternatives considered

| Alternative | Rejected because |
|---|---|
| Always remote | Kills the `file://` self-contained document that `html-lod-viewer` guarantees and small-graph users rely on |
| A separate large-graph page | Two viewers to keep in sync; `html-viewer-assets` exists precisely because viewer duplication rotted before |
| Runtime auto-detection (try embed, fall back) | An above-threshold document with an embedded payload is exactly the failure we are removing; detection must happen before emission |

The data-layer interface (get aggregates / get community page / get neighborhood) is the seam:
facets, detail panel, navigation and groups consume it identically in both modes.

## Decision 3 — Slice granularity: community pages + neighborhoods, hubs first

Communities are the natural loading unit — the LOD model already collapses to community dots,
so "not loaded" has an honest visual (the dot with aggregate data). Degree-descending paging
makes the first page the most informative view of a large community. Neighborhood slices
cover cross-community needs (selection detail, path rendering, expansion) without pulling
whole foreign communities.

Alternative considered: viewport/spatial tiles (load what's on screen). Rejected — requires a
precomputed global layout for 1M+ nodes (its own project), and fights the LOD model instead
of extending it.

## Decision 4 — Groups evaluate server-side on the query engine, Obsidian precedence

Group queries run through `queryGraphWithIndexScored` (same language as the search box — one
language to learn, any expressible search is a group). Server evaluation is the only correct
option in remote mode: client-side evaluation over resident slices would silently miss every
unloaded match and report wrong counts. First-match-wins panel order replicates Obsidian's
semantics; per-community match counts let the *overview* show where a group lives before
anything is loaded — that is the "find node groups" experience at 4.4 GB.

Alternative considered: Cypher for group definitions. Rejected — heavier surface for users,
and the scored query engine already handles the fuzzy "find this cohort" intent groups exist
for.

## Decision 5 — Tuning applies through the live options path; persistence per graph hash

Tuning writes to one state object applied via the running network's options/dataset updates —
never a canvas re-mount (the `html-lod-viewer` latency targets stay authoritative). Defaults
equal today's compiled constants, making the whole capability additive for users who never
open the panel. Persistence for both tuning and groups is localStorage keyed by graph hash:
per-corpus, survives reload, and deliberately not in the `viewer-navigation` URL hash (group
sets and tuning are too large; deep links should address *positions*, not preferences —
import/export covers sharing).

Alternative considered: server-side persistence under `graphos-out/`. Deferred — adds write
endpoints to a read-only server; revisit if teams need shared group sets.

## Verification strategy

- `cabal build` — asset embedding (`html-viewer-assets`) and `-Werror` hold; export mode
  selection compiles into the HTML module.
- `cabal test` — Hspec on the slice endpoints (caps enforced, cursors complete, totals
  present, truncation flags, unknown-node error, hash tracking), group evaluation (counts,
  paging, per-community rollup), and export threshold (below → payload embedded, above → no
  payload, both → no external origins) extending
  `tests/Graphos/Infrastructure/Export/HTMLSpec.hs` structural checks.
- Scenario verification on the reference corpus via `graphos serve`, plus one synthetic
  large corpus (≥ the response caps) to exercise paging, eviction, group counts over
  unloaded regions, and the remote-mode boot path end to end.
