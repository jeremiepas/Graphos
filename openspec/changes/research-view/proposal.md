## Why

Today, exploring a codebase with `graphos query` is one-shot and text/JSON-only: an agent
or human runs `graphos query "phase" --json`, reads a ranked node list, and then must
mentally stitch together the relationships across **multiple query results** to understand
how the pieces fit. Our own chat session demonstrated this gap perfectly — to explain the
Solario Core architecture by phase, we ran eight separate `graphos query` calls ("phase",
"work", "block spec", "governance conformance", "design build implement", "lifecycle state",
"tasks render", "context window max tokens") and then manually assembled the union of ~120
nodes and ~100 edges into an ad-hoc `subgraph-view.html` D3 visualization. That manual
assembly is exactly the work Graphos should do: the graph, the index, the communities, and
the HTML renderer already exist — only the **multi-query → induced-subgraph → HTML** pipeline
is missing.

PRD §7.2/§7.3 promise compact, navigable context, and PRD §13.1 already lists the query
family and HTML export. The gap is the composition: "run N queries, take the union of their
result nodes, induce the subgraph, render it as a self-contained interactive HTML view with
phase/cluster coloring and hover-to-inspect." Agents and humans exploring an unfamiliar
codebase need this "research view" — a single artifact that shows the union of what several
queries found, with edges between the found nodes, so the mental model is visual rather than
textual.

## What Changes

- **New `graphos research <term>... [--subgraph <term>...]` subcommand**: runs the scored
  query path for each supplied term, takes the **union of matched `QueryResponse.nodes[*].id`**,
  induces the subgraph (all edges where both endpoints are in the union), and renders the
  result as a self-contained interactive HTML file (D3 force-directed graph) **and/or** a
  JSON document (`ResearchView` type). One command, one artifact, no manual stitching.
- **New `ResearchView` JSON type**: `{ terms: [...], nodes: [...], edges: [...],
  communities: { id → {label, composition} }, metadata: { generated_at, graph_hash,
  node_count, edge_count } }` — the machine-readable twin of the HTML, so MCP clients and
  agents can consume the same union-of-queries result without parsing HTML.
- **HTML rendering reuses the existing `Infrastructure.Export.HTML` pipeline**: same
  vis-network canvas, same community-based coloring, with two additions — (a) a legend
  listing which query term discovered which nodes (color-coded by "discovered by term N"),
  and (b) a side panel showing node detail (file, community, score per term, degree) on
  hover/click. The HTML is self-contained (offline-usable via `file://`), matching the
  `query-http-port` offline-fallback contract.
- **New `--terms-file <path>` flag**: reads newline-delimited query terms from a file (for
  reproducible research bundles) and a `--label <text>` flag to title the output. Output
  defaults to `graphos-out/research-<label-or-timestamp>.html` and `...json`.
- **HTTP endpoint `GET /api/research?terms=a,b,c`** (deferred until `query-http-port` lands):
  returns the same `ResearchView` JSON as the CLI `--json` path, so the HTML navigator and
  external clients can build research views on the fly.

## Capabilities

### New Capabilities
- `research-view`: Multi-query induced-subgraph research view — runs N scored queries, takes
  the union of matched nodes, induces the subgraph, and renders a self-contained interactive
  HTML (D3 force-directed) + JSON (`ResearchView`) artifact with per-term discovery legend,
  community coloring, and hover-to-inspect node detail.

### Modified Capabilities
- `query-cli-contract`: The uniform flag surface gains the `research` subcommand and its
  flags (`--subgraph`, `--terms-file`, `--label`, `--html`, `--json`); `research` honors
  `--graph`, `--budget`, `--label-width`, `--edges` and the single-JSON-document/no-interleaved-
  logs rule. The existing query-family commands are unchanged.

## Impact

- **Code**:
  - `src/Graphos/UseCase/Query/Research.hs` (new) — `buildResearchView :: Graph ->
    GraphIndex -> [Text] -> Maybe RefineConfig -> ResearchView` (multi-query union + induce)
  - `src/Graphos/Domain/Query/Research.hs` (new) — `ResearchView`, `ResearchNode`,
    `ResearchEdge`, `ResearchCommunity` records + `ToJSON`
  - `src/Graphos/Infrastructure/Export/HTML.hs` (extended) — new `renderResearchHtml`
    function reusing the existing vis-network scaffolding with the discovery-legend and
    detail-panel additions
  - `src/Graphos/CLI/Parser.hs` — new `research` subcommand + flags
  - `app/Main.hs` — dispatch `research` to `buildResearchView` + `renderResearchHtml` / JSON
  - `src/Graphos/Infrastructure/Server/QueryAPI.hs` (deferred) — `GET /api/research`
- **APIs**: New CLI subcommand (additive, no breaking change). New `ResearchView` JSON type.
  New HTTP endpoint (deferred). No `graph.json` schema change — consumes existing graph +
  index + compositions.
- **Dependencies**: No new libraries. Reuses existing `UseCase.Query.queryGraphWithIndexScored`,
    `Infrastructure.Export.HTML`, and `Domain.Graph` induce operations.
- **Tests**: Hspec for `buildResearchView` (union correctness, induced edges, per-term
  discovery attribution, empty-results handling, single-term equivalence to `query --json`
  node set), `ResearchView` JSON shape, `renderResearchHtml` self-contained + legend +
  detail panel, parser acceptance, `-Wall -Werror` clean.
- **Build**: New modules + parser extension; no new dependency.

## Relationship to other changes

- **`explorer-queries`** (planned): `research` is complementary — `around`/`cluster` are
  single-node orchestrations, while `research` is multi-query orchestration. `research` can
  use `around` results as additional seed nodes if `--subgraph` is supplied. No code
  dependency; merge in either order.
- **`query-http-port`** (in progress): `research`'s HTTP endpoint (`GET /api/research`) is
  deferred until that change lands, same as `explorer-queries`'s `/api/around`. The CLI
  path ships independently.
- **`reduce-query-context-noise`** (in progress): `research` benefits from that change's
  budget enforcement — each per-term query inside `research` inherits the capped, ranked
  output, so the union is signal-dense rather than hub-spammy. No code dependency; the
  `RefineConfig` threaded by `research` picks up the noise-control improvements
  automatically.
- **`navigator-query-view`** (in progress): that change makes the HTML navigator call
  `/api/query` for single queries. `research` is a separate artifact (a generated HTML
  file, not an in-navigator feature). A future follow-up could add a "Research" tab to the
  navigator that calls `/api/research`, but that's out of scope here.