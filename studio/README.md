# Graphos Studio

Standalone Elm workbench for Graphos graphs — spec:
`openspec/changes/elm-graph-studio/`. Load a `graph.json` (drag-and-drop,
offline) or connect to `graphos serve`; navigate (LOD overview → community →
node, deep links, browser Back/Forward, breadcrumb, Escape), edit nodes and
edges with undo/redo, define Obsidian-style query+color groups, and extract
subgraphs to the `json-graph-web-view` canonical shape (drop the download into
`graphos-out/views/` and the views catalog serves it).

## Build & run

```bash
# inside the repo devenv shell (elm/elm-test provided):
cd studio
elm make src/Main.elm --optimize --output=public/studio.js
elm-test
# then serve the static bundle any way you like:
python3 -m http.server -d public 8090     # → http://localhost:8090
```

## Layout

| Path | What |
|---|---|
| `src/Main.elm` | TEA shell: Model/Msg/update/view, ports wiring |
| `src/Studio/Navigation.elm` | viewer-navigation semantics (invariants mirror the Lean model in `openspec/changes/viewer-navigation/lean/`) |
| `src/Studio/Data/Graph.elm` | graph-json-contract types/codecs, aggregate synthesis, BFS, identity fingerprint |
| `src/Studio/Edit.elm` | edit intents, validation, inverses, cypher translation, log |
| `src/Studio/Groups.elm` | query+color groups, first-match-wins, local evaluation |
| `src/Studio/Scope.elm` | extraction scope algebra + canonical view encoder |
| `src/Studio/Api.elm` | graphos serve client: probe, fetch, `/api/cypher/mutate`, group counts |
| `src/Studio/DesignSystem/` | Tokens (single styling source — CI lints hex literals elsewhere) + Components |
| `public/` | `index.html`, `ports.js` (renderer handle + localStorage only), vendored vis-network |
| `tests/` | 55 elm-tests: navigation invariants, codecs, editing, groups, scope |

## Data sources (2.1–2.3)

The studio resolves one of three implementations at connect time
(`Studio.Data.Source.resolveKind`): `FileSource` (offline), `SlicesMode`
(server ships the slice API — boots from `/api/overview`, never
`graph.json`), or `LegacyMode` (server has no slice API — full fetch, slice
affordances labeled with their unavailability reason). Connection state
(origin + capability badges + graph hash) is shown in the shell; a mid-session
connection loss transitions to `Disconnected` and Retry preserves local work
(groups, undo stack, edit log, scope) rather than wiping it.

### File-mode size guard (`sizeThresholdBytes`)

Refuse files above the ceiling *before* reading, so an oversized file keeps the
tab responsive and recommends connected mode. Pure and shared by the file picker
and drag-drop (`Source.exceedsSizeLimit`).

Measured on the reference machine (Node harness, `JSON.parse` as a proxy for the
Elm `Json.Decode` run atop the same bytes):

| Size        | Nodes  | Parse (best) | Parse (avg) | Peak RSS |
|-------------|--------|--------------|-------------|----------|
| 43.4 MB     | 402,764 | 206 ms      | 216 ms      | ~482 MB  |

A ~40 MB offline file decodes well under a sub-second responsiveness bar, so the
100 MB ceiling is retained: files up to 100 MB are accepted offline and anything
larger is refused before any read. Cabal-driven fixture regeneration lives in W7
(AVI-752); this ceiling is set from the data above, not arbitrarily.

## Editing semantics

- **Connected**: optimistic apply → `/api/cypher/mutate` (`cypher-mutation`
  subset, `persist: true`); server rejection reverts via the inverse intent.
- **File mode**: identity-keyed edit log (localStorage), replay offered on
  reload, export as contract `graph.json` + changelog.
