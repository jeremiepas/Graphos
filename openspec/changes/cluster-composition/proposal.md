## Why

After Leiden runs, `Node.fileType` (`CodeFile` | `DocFile` | `PaperFile` | ...) and
`nodeKind` (`"function"` | `"section"` | `"module"` | ...) are first-class fields on every
node — but nothing records **what the community is made of**. A user (or agent) looking at
community 483 sees its label and member count, but not that it's "12 code + 4 docs with 3
code↔doc cross-links." That composition is the single most useful explorer signal: it tells
you "this is a code-dominant cluster I can drill into" vs "this is a mixed cluster where docs
describe the code" vs "this is a docs-only cluster I should read."

The signal is computable in O(N) post-Leiden by counting members by `FileType` and
intersecting the community's edges with `References` edges crossing `CodeFile`↔`DocFile`.
No new extraction, no new inference — just a post-clustering aggregation that's currently
missing.

This change also makes the LLM labeling prompt **composition-aware**. Today `labelPrompt`
passes a flat list of top node labels; the LLM names the cluster after the most frequent
token, which on a mixed cluster is often a code identifier rather than the shared concept.
Tagging top nodes with `(code)`/`(doc)` and including a composition summary lets the LLM
name the unifying concept ("Authentication") rather than the dominant-vocabulary word.

This change is independent of `semantic-edge-inference` but **more valuable when that change
has run** — semantic edges make clusters actually mixed, and composition surfaces that
mixedness. On today's literal-name-only graphs, composition still works (it'll show mostly
pure clusters, which is honest).

## What Changes

- **New `CommunityComposition` record** in `Domain/Community`:
  `ccCodeCount`, `ccDocCount`, `ccOtherCount`, `ccDominantKind`, `ccMixedRatio`,
  `ccCodeDocEdges`. `ccMixedRatio = min(code,doc) / max(code,doc)` (0 = pure, 1 = balanced).
- **`computeCompositions :: Graph -> CommunityMap -> Map CommunityId CommunityComposition`**
  — counts members by `FileType`, derives `ccDominantKind` from the most frequent `nodeKind`,
  counts `References` edges inside the community crossing `CodeFile`↔`DocFile`. O(N + E).
- **Persist to `graph.json`** under a `compositions` key (additive; legacy graphs load as
  empty). Small payload — one record per community, not per node.
- **HTML viewer badge**: render `🔧 N / 📄 M / 🌉 K` (code count / doc count / cross-type
  edges) on community dots (overview) and drill-down headers. Composes with the in-flight
  `add-profondeur-view-selector` depth control — badge shows at every depth.
- **Composition-aware labeling prompt**: `labelPrompt` in `Domain/Labeling.hs` tags each top
  node with `(code)`/`(doc)` based on `nodeFileType`, splits into "Top code nodes:" / "Top
  doc nodes:" lines, includes a composition summary line, and updates the preamble to frame
  the task as mixed code-and-knowledge analysis instructing the LLM to name the unifying
  concept.

## Capabilities

### New Capabilities
- `cluster-composition`: Post-clustering computation of per-community `CommunityComposition`
  (code/doc/other counts, dominant kind, mixed ratio, cross-type edge count) persisted to
  `graph.json` and surfaced in the HTML viewer as a cluster badge.

### Modified Capabilities
- `llm-labeling`: `labelPrompt` now tags top nodes with `(code)`/`(doc)`, splits by corpus,
  includes the composition summary, and instructs the LLM to name the unifying concept of
  mixed clusters rather than the most frequent word.

## Impact

- **Code**:
  - `src/Graphos/Domain/Community.hs` (or new `src/Graphos/Domain/Community/Composition.hs`) —
    `CommunityComposition` record + `computeCompositions`
  - `src/Graphos/Domain/Graph/Core.hs` — add `gCompositions :: Maybe (Map CommunityId
    CommunityComposition)` to `Graph` (additive; `Nothing` default)
  - `src/Graphos/Domain/Graph/Core.hs` — `ToJSON`/`FromJSON` for `compositions` field
  - `src/Graphos/UseCase/Pipeline/Core.hs` — call `computeCompositions` post-Leiden, attach
    to graph, persist under `compositions` key
  - `src/Graphos/UseCase/Load.hs` — parse `compositions` (empty/`Nothing` on legacy)
  - `src/Graphos/Domain/Labeling.hs` — `labelPrompt` tags nodes, splits lines, includes
    composition, updates preamble
  - `src/Graphos/Infrastructure/Export/HTML.hs` — render composition badge on community dots
    + drill-down headers
- **APIs**: `graph.json` gains optional `compositions` field (additive; legacy loads
  unchanged). No CLI surface change in this change — `explorer-queries` adds the
  `--mixed-only` filter that consumes `ccMixedRatio`.
- **Dependencies**: No new libraries. Reuses `aeson`, `containers`.
- **Tests**: Hspec for `CommunityComposition` record (pure-code → ratio 0, balanced → 1,
  cross-type edge count), `computeCompositions` correctness, `labelPrompt` output (mixed
  → both "Top code nodes:" and "Top doc nodes:"; pure-code → only "Top code nodes:"; preamble
  contains "concept" or "unifies"), legacy graph load (no `compositions` → empty), HTML
  badge rendering. `-Wall -Werror` clean.
- **Build**: New module/record + extended prompt; no new dependency.

## PDCA Cycle

- **Plan**: Hypothesis — surfacing per-community composition makes mixed clusters
  navigable (HTML badge tells humans "this is mixed") and improves LLM labels (the prompt
  names the unifying concept instead of the most frequent word). Success measured by: (a)
  `computeCompositions` produces correct counts on a synthetic mixed graph; (b) HTML
  viewer shows the badge on every community at every depth; (c) on a mixed corpus, LLM
  labels name concepts ("Authentication", "Export Pipeline") rather than code identifiers
  ("verifyToken", "renderHTML"); (d) legacy graphs load without `compositions` and all
  existing commands work; (e) pure-code communities show `ccMixedRatio = 0` honestly.
- **Do**: Add `CommunityComposition` + `computeCompositions`; persist to `graph.json`;
  render HTML badge; update `labelPrompt` with composition tags + split lines + new
  preamble.
- **Check**: `cabal test` green with new Hspec cases; build a mixed corpus and confirm
  badges render + LLM labels improve; `--no-embed` graph (no semantic edges) still gets
  composition (honest pure clusters); legacy graph loads; `-Wall -Werror` clean.
- **Act**: If LLM labels don't improve with the new prompt, iterate the preamble wording
  (the signal is there; the prompt may need to explicitly say "name the concept that the
  code implements and the docs describe"). If the badge is too noisy on large graphs, make
  it tooltip-only above 1K communities. If `ccMixedRatio` is 0 on real mixed corpora, the
  literal-name `inferCodeDocEdges` isn't producing cross-type edges — feed into
  `semantic-edge-inference` as the real fix.

## Relationship to other changes

- **`semantic-edge-inference`** (planned): independent. That change makes clusters mixed;
  this change surfaces the mixedness. Either can ship alone; this is more valuable after
  that one lands. No file overlap except `Domain/Graph/Core.hs` (additive field).
- **`explorer-queries`** (planned): that change's `--mixed-only` filter consumes
  `ccMixedRatio` from this change. Merge order: this first, then `explorer-queries` can
  use `ccMixedRatio`. But `explorer-queries` can ship with `--mixed-only` as a no-op if
  compositions are absent (graceful degradation).
- **`add-profondeur-view-selector`** (in progress): the depth selector and the composition
  badge compose — badge renders at every depth level. No conflict; badge is an overlay.