## Why

Agents calling `select_context` and `query_graph` today receive far more noise than signal. A 3000-token budget request against this very repo returns 9213 tokens (3× over budget): the 246-degree `Main` hub node is force-included in every response via `take 5 (analysisGodNodes analysis)`, the expansion-hints section lists a 2563-node mega-community (community 7463) alongside 97-node ones, and `query_graph` emits unranked `AMBIGUOUS` edges without verdict or budget truncation (a recent call returned 175 KB and was truncated). PRD §7.2/§7.3 promises compact, budget-respecting context and PRD §16.1 targets < 500 ms query latency — both are violated when the response alone is 9 KB of mostly irrelevant hub spam. Agents cannot use `graphos query` to reduce context exploration if the tool itself emits more context than it saves.

## What Changes

- **Token budget becomes a hard output cap, not a hint.** `select_context` and `query_graph` MUST truncate the rendered payload so `token_estimate ≤ requested budget`, dropping the lowest-ranked nodes/edges first and emitting a trailing `omitted` footer (extends `query-legibility`'s tail-truncation requirement to the MCP path).
- **God/hub nodes stop being unconditionally force-included.** `scGodNodes` is no longer seeded by `take 5 (analysisGodNodes analysis)` in `selectCommunityAware`/`selectRelevanceWeighted`/`selectPathBased`; hub nodes appear only when they are themselves query-relevant or are reached by BFS within budget. The `### Hub Nodes` formatter section is dropped unless a hub is in the selected subgraph.
- **Expansion hints are bounded and relevance-filtered.** `formatExpansionHints` MUST cap at the top N communities by relevance to the query (not list every community in `scCommunityLabels`), MUST hide communities larger than a configurable `--max-hint-community-size` (default 50) since "include community X (2563 nodes)" is never actionable, and MUST omit the chat community.
- **Edges are relevance-ranked and confidence-filtered in MCP output.** `formatKeyEdges` sorts edges by query-relevance (not map order), and the `select_context` MCP tool gains the same `--edges semantic|all` knob already specified in `query-noise-control` for the CLI, defaulting to `semantic` (drops `AMBIGUOUS`-confidence and trivia-target `contains` edges).
- **`query_graph` MCP output gains a JSON shape with verdict, scored nodes, and a result-set hash** (parity with `query-legibility`'s CLI contract), replacing the current unranked `{nodes, edges, traverse}` payload. The handler also stops calling `queryGraph` three times per request (MCP.hs:130-132).
- **`token_estimate` uses the existing `countContextTokens` heuristic instead of raw character length** so the cap is measured in the same units as the budget.

## Capabilities

### New Capabilities
- `context-budget-enforcement`: Hard token-budget capping for `select_context` and `query_graph` MCP output — tail-truncation by relevance rank, `omitted` footer, and a measured (not character-count) token estimate. Bridges the gap between the CLI `query-legibility` tail-truncation contract and the MCP path that currently ignores budget.

### Modified Capabilities
- `query-noise-control`: Extends the existing semantic-edge-filtering, self-edge-collapse, deduplication, and label-elision requirements (currently scoped to the CLI `query` family) to the `select_context` MCP tool and the `query_graph` MCP tool's ranked JSON output. Also constrains `formatExpansionHints` to bound and relevance-filter community suggestions.

## Impact

- **Code**: `src/Graphos/UseCase/SelectContext.hs` (remove god-node force-inclusion in 3 strategies), `src/Graphos/UseCase/FormatContext.hs` (budget-aware truncation, ranked edges, bounded hints, drop unconditional Hub section), `src/Graphos/UseCase/Query.hs` (ranked/budgeted JSON shape for MCP), `src/Graphos/Infrastructure/Server/MCP.hs` (new args, single call to scored query path, `token_estimate` via `countContextTokens`).
- **APIs**: `select_context` and `query_graph` MCP tool schemas gain `--edges` and `--max-hint-community-size`; `query_graph` response shape changes (**BREAKING** for MCP clients expecting `{nodes, edges, traverse}` — now returns `{verdict, best_score, hash, nodes, edges, omitted}`).
- **Dependencies**: None new; reuses existing `Graphos.Domain.Graph.Score` and `FormatContext.countContextTokens`.
- **Systems**: MCP clients (this opencode instance, any other agent) see smaller, ranked payloads — directly enables "small context size for agent" per the user's request.

## PDCA Cycle

- **Plan**: Hypothesis — capping `select_context` output to the requested budget and removing unconditional god-node inclusion will cut average MCP response size by ≥ 60 % (target: 3000-token budget → ≤ 3000 tokens emitted, down from ~9200) while preserving the top-ranked query-relevant node in 100 % of cases. Success measured against PRD §16.1 (< 500 ms) and §7.2 budget table.
- **Do**: Implement `context-budget-enforcement` and extend `query-noise-control` per the specs/design/tasks below.
- **Check**: Hspec scenarios assert (a) `token_estimate ≤ budget` for Focused/Module/CrossModule/Exploratory fixtures, (b) top-ranked node always present after truncation, (c) no community larger than `--max-hint-community-size` appears in expansion hints, (d) `query_graph` returns a `verdict` field. A regression fixture using this repo's own `graph.json` verifies the live 9213→≤3000 reduction.
- **Act**: If the truncation rule keeps the top node but loses the second-ranked node too often, tune the node-vs-edge budget split (design.md §Budget allocation) in the next cycle. Standardize the `omitted` footer format so MCP clients can fall back to `get_neighbors` for expansion.