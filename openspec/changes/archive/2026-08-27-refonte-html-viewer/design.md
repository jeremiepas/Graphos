# Design — refonte-html-viewer

## Context

Measured on `graph.html` for a 104,101-node / 122,347-edge graph (tree-sitter
extraction, 2026-08-11):

| Section | Bytes | Share | Per item |
|---|---:|---:|---:|
| `_nodesData` | 42,604,385 | 42.1% | 409 B/node |
| `_edgesData` | 53,902,316 | 53.3% | 441 B/edge |
| `_communityAggregatesData` | 4,545,244 | 4.5% | — |
| HTML + CSS + JS | 175,855 | 0.2% | — |
| **Total** | **101,227,800** | 100% | — |

Where the bytes go, per the code:

- **Edges (`HTML.hs:846–856`, `:919–920`)**: `from`/`to` are full node id strings, and Graphos
  node ids are long — `makeNodeId` produces `<dirhash>_<stem>_<truncated declaration text>`
  (`Extract/TreeSitter/Convert.hs:286–293`), so an id routinely runs 80–200 characters and is
  written **twice per edge**. On top of that every edge carries `title` and `label` (identical
  strings), `dashes`, `width`, a three-key `color` object and a nested `arrows` object — all
  constant per relation.
- **Nodes (`HTML.hs:824–833`)**: the id again, plus `title` (= `source_file` + `community_id`,
  both already separate keys), `group` (= `community_id`), and a two-key `color` object that is
  `colorForCommunity community_id`.

So the dominant cost is **repeated identity strings**, and the second cost is **per-item
constants**. Both are removable without touching the renderer, the LOD strategy, or the
`file://` contract that `html-lod-viewer/spec.md:72–74` requires.

The repo's own archived analysis (`archive/2026-08-11-refactor-html-large-graph-lod/design.md:7`)
found the browser wall is *"parse + heap, not render"* at 157 MB / ~1–1.5 GB heap. That change
proposed replacing inline JSON with a WASM-SQLite sidecar and vis-network with sigma.js; it was
never implemented and its benchmark evidence file was never filled in. Its conclusion still
supports this change's direction: if parse and heap dominate, deleting 75% of the bytes is the
highest-leverage move available, and it is available now.

## Goals / Non-Goals

**Goals**

- Payload proportional to information content: ≤ 200 B/node, ≤ 24 B/edge, ≤ 30 MB for the
  reference corpus.
- A viewer that can be linted, formatted, diffed and tested — i.e. not Haskell string literals.
- Genuine offline self-containment, including the renderer.
- Facet filtering, a useful detail panel, a labelled legend, relation-keyed edge styling.
- One depth model (`Overview | Community | Full | Custom`), absorbed from the superseded change.
- The first automated tests over generated HTML.

**Non-Goals**

- Renderer replacement (sigma.js/WebGL) — deferred, with a measured trigger (see D6).
- Sidecar/streamed data stores, OPFS, COOP/COEP — same deferral.
- `research-view`'s HTML output (`app/Main.hs:328` still prints "HTML export not yet
  implemented") — unblocked by the reusable assets, not implemented here.
- `cluster-composition`'s composition badge — the view model carries the fields; the badge task
  stays with that change.
- `graphos serve` gzip/ETag/streaming (`Static.hs:60–65` buffers the whole file per GET) and the
  dead `--svg` flag (`Parser.hs:73`) — separate follow-ups.

## Decisions

### D1 — Intern identities and constants into string tables

*Alternatives*: (a) shorten node ids at extraction time; (b) gzip the payload and inflate in the
browser; (c) intern into tables and reference by index.

*Choice*: (c).

*Rationale*: (a) changes `graph.json` and every consumer, and node ids are a public contract of
the query family. (b) needs a decompressor in the document and still materializes the same heap
after inflation — it fixes disk size, not the parse+heap wall the archived analysis identified.
(c) shrinks disk *and* heap, is invisible outside the viewer, and is reversible.

Emitted shape (illustrative):

```
strings : ["…node ids…"]        // one entry per node, referenced by index
files   : ["./src/a.ts", …]     // ~3.9K entries for 104K nodes
kinds   : ["Function", …]
relations: ["contains","imports",…]
nodes   : [[labelIdx?, fileIdx, line, commId, degree, bridge, kindIdx, fileType], …]
edges   : [[srcIdx, tgtIdx, relIdx], …]
```

Expected effect on the reference corpus: edges 53.9 MB → ~2.5 MB (three small integers per edge),
nodes 42.6 MB → ~20 MB (ids kept once, constants and duplicates dropped), aggregates trimmed.
The 30 MB budget is set with headroom above that estimate deliberately — it is a ceiling to
enforce, not a prediction to celebrate.

### D2 — Labels stay verbatim, signatures leave the payload

Labels are the primary information carried per node and stay as-is (already truncated at 80 chars,
`HTML.hs:894`). Signatures are the largest optional text and are dropped from the payload: the
detail panel fetches them from `/api/explain` when served, and omits the section on `file://`.
This mirrors the existing offline-degradation pattern that `navigator-query-view/spec.md:18–20`
already establishes for search, so the viewer has exactly one story for "richer when served".

The reference subgraph viewer built for `typescipt-repository` embedded signatures and paid 10% of its
payload for them at 617 nodes; at 104K nodes that trade is not available.

### D3 — Styling by group and CSS, never per item

vis-network supports `groups` styling; relation styling belongs in CSS/edge-group definitions.
Once colors come from group definitions, `color`, `group`, `title`, `dashes`, `width` and `arrows`
disappear from every record. This is the change that makes the per-edge budget of 24 bytes
achievable at all.

### D4 — Assets as files, embedded at compile time

*Alternatives*: (a) keep string literals; (b) `data-files` read at runtime; (c) `file-embed` at
compile time.

*Choice*: (c).

*Rationale*: (b) makes the binary non-relocatable and breaks `graphos` run from an arbitrary
directory. (c) keeps a single self-contained binary while letting the viewer live in real `.js`
and `.css` files that tooling can process; `file-embed` is already a precedent in this repository
(agent-scaffolding templates). The vendored renderer is embedded the same way, which is what
finally makes the "self-contained" claim true — today the document declares itself self-contained
while `<script src='https://unpkg.com/vis-network/…'>` (`HTML.hs:68`) says otherwise, unpinned.

*Consequence*: the repository gains a ~600 KB vendored bundle and its license file; every emitted
document grows by that amount. For a 30 MB artifact this is 2%; for a tiny graph it is the floor.
If that floor matters, an opt-in `--external-renderer` flag is the escape hatch — not a default
CDN.

### D5 — One view state, one dispatcher

Depth, facets, selection and search results become fields of a single state object with one render
dispatcher. The current viewer instead has `currentPhase`/`expandedCommunity` globals
(`HTML.hs:189–190`) and three duplicated options blocks (`:352–384`, `:429–470`, `:713–754`), which
is why `hideEdgesOnDrag` ended up in the wrong section in two of them (`:458–459`, `:742–743`) and
why the depth-selector change had to plan a `currentPhase → currentDepth` refactor of its own.
Absorbing that change means doing the refactor once.

### D6 — Keep vis-network and inline data, with a measured trigger to revisit

The archived design's reversal (sidecar + WebGL, dropping `file://`) is not adopted here, for
three reasons: the active spec mandates the opposite and was never amended; that design was never
benchmarked (its `check.md` evidence fields are empty); and it addresses render and storage while
the same document names parse+heap as the wall — which a 4× byte reduction attacks directly.

The trigger is explicit: **if, after this change, the reference corpus meets the byte budget and
the browser still misses the latency targets, the sidecar/WebGL architecture is justified and this
change's measurements become its evidence.** That is a better position than either shipping a
large rewrite on stale numbers or leaving the contradiction unresolved.

### D7 — Facets are computed client-side from the payload

Facet indices are built at load from fields already present (`file_type`, `kind`, `community_id`,
`is_bridge`, relation). Precomputing them at export time would add bytes to fix a cost that has
not been measured yet. If facet evaluation exceeds the drill-down budget on the reference corpus,
D7 is revisited with numbers.

## Risks / Trade-offs

| Risk | Mitigation |
|---|---|
| Interning bug silently corrupts the graph shown | Round-trip property test: expanding the interned payload must equal the tuples derived from the in-memory graph |
| Signature removal is felt as a regression on `file://` | Panel states explicitly that signatures require a served document; `graphos serve` is already the documented primary delivery path (`html-lod-viewer/spec.md:86`) |
| Vendored renderer adds ~600 KB floor and a license obligation | License recorded alongside the bundle; opt-in `--external-renderer` if the floor matters |
| Rewriting the viewer regresses the search surface | `navigator-query-view` requirements restated as preserved; its scenarios are part of the acceptance run |
| Superseding an open change loses intent | Its `html-depth-selector` requirements are carried here in condensed form; task 9 archives it as superseded with a pointer |
| Budget met but browser still stalls | That is the explicit trigger for the deferred architecture (D6); the measurement is the deliverable either way |

## Migration

- `graph.json` is unchanged. `graphos serve`, `/api/*` and the CLI surface are unchanged.
- The inline payload shape changes; nothing outside `HTML.hs` and the viewer reads it.
- `btnBack` disappears, replaced by the depth control (inherited from the superseded change).
- Users who relied on the CDN renderer being cached across many `graph.html` files now carry a
  vendored copy per document — the trade for offline correctness.

## Open Questions

1. Should node labels also be interned? Labels repeat far less than files, but a corpus with many
   identical short labels (`index`, `main`) might benefit — decide from the measured table sizes in
   task 2 rather than up front.
2. Should the export emit an uncompressed payload plus an optional `.json.gz` sidecar for
   `graphos serve` to send with `Content-Encoding: gzip`? Cheap, but it splits the artifact —
   defer to the serve-path follow-up.
3. Does `Full` depth need a hard node cap, or is a confirmation prompt enough? The superseded
   change assumed "safe up to ~5K nodes"; measure at task 8 before choosing.
