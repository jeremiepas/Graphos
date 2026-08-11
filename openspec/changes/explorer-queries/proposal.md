## Why

The query family (`query`, `path`, `explain`, `symbols`, `neighbors`) is one-shot-search-
shaped: ask, get a result set, stop. An agent (or human) exploring a mixed code+docs corpus
has two problems this can't solve:

1. **"What do I need to understand N?"** — today an agent stitches five commands
   (`explain N` + `symbols N` + `neighbors N -d 1` + `getCommunity N` + `bridgeNodes`) to
   assemble the context around a node. Each is a round-trip; each returns a separate JSON
   doc the agent must reconcile. There's no single "show me N's world" primitive.
2. **"Show me only the docs in this cluster"** — the query family filters by node ID and
   label, not by `FileType` or `nodeKind` or cluster mixedness. `Node.fileType` and
   `nodeKind` are first-class fields, but the explorer can't say `--filetype doc` or
   `--mixed-only`. On a mixed corpus, this means an agent gets code+docs interleaved with no
   way to scope to one side.

This change adds two new subcommands (`around`, `cluster`) and a filter flag family
(`--filetype`, `--kind`, `--mixed-only`, `--code-only`, `--doc-only`) to the query family.
The filter flags are **post-filters** — they narrow results without changing the query
algorithm, keeping `queryGraphWithIndexScored` unchanged. The new subcommands are
**orchestration** — they compose existing primitives (`neighbors`, `communityOfNode`,
`articulationPoints`, `explainNode`) into one JSON document.

`--mixed-only` consumes `ccMixedRatio` from `cluster-composition` (gracefully degrades to a
no-op when compositions are absent). `around` and `cluster` work without compositions (they
just omit the composition field). So this change ships independently but is more useful when
`cluster-composition` has run.

## What Changes

- **New `graphos around <node>` subcommand**: returns `AroundResponse` JSON — the node, its
  in-edges (dependencies) with neighbor nodes, out-edges (dependents) with neighbor nodes,
  its community (id, label, composition if available), and articulation points (bridges) in
  its community. One orchestration call, one JSON. Honors `--json`, `--budget`,
  `--label-width`, `--edges`, and the new filter flags.
- **New `graphos cluster <id>` subcommand**: returns `ClusterResponse` JSON — the community
  id, label, composition (if available), members grouped by `nodeKind`, bridges, and
  `References` edges crossing `CodeFile`↔doc-like inside the community.
- **New filter flags on the query family** (`query`, `symbols`, `neighbors`, `around`,
  `cluster`): `--filetype <code|doc|paper|image|video|audio|office>`, `--kind <text>`,
  `--mixed-only`, `--code-only` (shorthand for `--filetype code`), `--doc-only` (shorthand
  for `--filetype doc`). Post-filters applied in `refineResponse` — no algorithm change.
- **`--mixed-only`** drops nodes whose community has `ccMixedRatio == 0`. When compositions
  are absent (legacy graph), `--mixed-only` is a no-op (returns all nodes) with a warning.
- **HTTP port endpoints** (deferred until `add-query-api-port-and-view` lands): `GET
  /api/around?node=<id>&depth=<n>&filetype=...&kind=...` and `GET /api/cluster?id=<n>` returning
  the same JSON as CLI `--json`.
- **Uniform acceptance**: the new filter flags are accepted by every query-family subcommand
  (`query`, `path`, `explain`, `symbols`, `neighbors`, `around`, `cluster`) without "invalid
  option" errors. Where a flag is semantically inapplicable (e.g. `--mixed-only` on `path`),
  it's accepted as a no-op.

## Capabilities

### New Capabilities
- `explorer-queries`: New `around` and `cluster` subcommands + `--filetype` / `--kind` /
  `--mixed-only` / `--code-only` / `--doc-only` filter flags on the query family, returning
  the existing `QueryResponse` (for filtered queries) and new `AroundResponse` /
  `ClusterResponse` JSON types for the new subcommands.

### Modified Capabilities
- `query-cli-contract`: The uniform flag surface gains `--filetype`, `--kind`, `--mixed-only`,
  `--code-only`, `--doc-only` across the query family; new `around` and `cluster` subcommands
  honor `--json`, `--budget`, `--label-width`, `--edges` and emit a single JSON document with
  no interleaved logs.

## Impact

- **Code**:
  - `src/Graphos/UseCase/Query/Refine.hs` — new `ExplorerFilter` record +
    `applyExplorerFilter :: ExplorerFilter -> GraphIndex -> Map CommunityId CommunityComposition
    -> QueryResponse -> QueryResponse`
  - `src/Graphos/UseCase/Query.hs` — new `aroundNode` and `clusterDetail` orchestration
    functions; new `AroundResponse` / `ClusterResponse` types with `ToJSON`
  - `src/Graphos/CLI/Parser.hs` — new `around`/`cluster` subcommands; `--filetype`/`--kind`/
    `--mixed-only`/`--code-only`/`--doc-only` flags on query family
  - `app/Main.hs` — dispatch new subcommands + flags; route to JSON or text rendering
  - `src/Graphos/Infrastructure/Server/QueryAPI.hs` (deferred until
    `add-query-api-port-and-view` lands) — `GET /api/around`, `GET /api/cluster`
- **APIs**: New CLI subcommands + flags (additive, no breaking change). New HTTP endpoints
  (deferred). No `graph.json` schema change — consumes `gCompositions` from
  `cluster-composition` if present, degrades gracefully if absent.
- **Dependencies**: No new libraries. Reuses existing `UseCase.Query` functions.
- **Tests**: Hspec for `applyExplorerFilter` (filetype/kind/mixed-only narrow; no filter =
  identity; missing compositions → `--mixed-only` no-op), `aroundNode` (returns expected
  fields; unknown node → error; label resolution; filter narrows edges), `clusterDetail`
  (composition + members grouped + cross-type edges; unknown community → error),
  `AroundResponse`/`ClusterResponse` JSON shape, parser accepts all flags on all subcommands,
  `--json` single document, legacy graph graceful degradation. `-Wall -Werror` clean.
- **Build**: New functions + parser extensions; no new dependency.

## PDCA Cycle

- **Plan**: Hypothesis — `around` + `cluster` + filter flags make the query family usable as
  a mixed-corpus explorer by humans (HTML, via the in-flight HTTP port) and agents (CLI/MCP).
  Success measured by: (a) `graphos around <node> --json` returns in < 500ms on a 10K-node
  graph (PRD §16.1); (b) `--filetype doc` narrows query results to `DocFile` nodes only; (c)
  `--mixed-only` drops nodes in pure communities when compositions available, no-ops + warns
  when absent; (d) `around` and `cluster` emit a single JSON document with no interleaved
  logs; (e) all filter flags accepted by all query-family subcommands without "invalid
  option" errors; (f) legacy graph (no compositions) works with all new commands.
- **Do**: Add `ExplorerFilter` + `applyExplorerFilter`; add `aroundNode` / `clusterDetail`
  orchestration; add `AroundResponse` / `ClusterResponse` + `ToJSON`; add CLI subcommands +
  flags; dispatch in `Main.hs`; defer HTTP endpoints to after `add-query-api-port-and-view`.
- **Check**: `cabal test` green with new Hspec cases; time `around` on a 10K-node graph;
  verify `--filetype`/`--kind`/`--mixed-only` narrow correctly; verify legacy graph
  degradation; `-Wall -Werror` clean.
- **Act**: If `around` is widely used by agents, standardize it as the primary entry point
  of the `graphos-query` skill (replacing `query` as the recommended first call). If
  `--mixed-only` no-op on legacy graphs confuses users, document the requirement explicitly
  in the skill. If `cluster <id>` is more useful than `around`, re-prioritize the skill docs.

## Relationship to other changes

- **`cluster-composition`** (planned): `--mixed-only` consumes `ccMixedRatio` from that
  change. This change ships independently: `--mixed-only` is a no-op when compositions
  absent. `around` and `cluster` omit the composition field when absent. Merge order:
  `cluster-composition` first makes `--mixed-only` actually useful, but this change
  doesn't block on it.
- **`add-query-api-port-and-view`** (in progress): the HTTP `/api/*` port family gains
  `/api/around` and `/api/cluster` as new endpoints. No conflict — this change adds endpoints
  to the same Warp app once that change lands. Task 3.7 in this change is explicitly
  deferred until that change merges.
- **`fix-query-cli-ergonomics`** (in progress): that change adds `--json` to `query`/`path`/
  `explain` and display-name resolution to `neighbors`. This change's `around` reuses the
  same `resolveNodeArg` helper. No conflict — additive.
- **`semantic-edge-inference`** (planned): independent. That change makes clusters mixed;
  this change's `--mixed-only` is more useful when clusters are actually mixed. No code
  dependency.