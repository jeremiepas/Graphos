# Design: reduce-query-context-noise

## Context

The Graphos MCP server exposes `select_context` and `query_graph` to give agents a small,
ranked subgraph so they can avoid re-reading the whole codebase (PRD §7, §8, §13.1). In
practice today those tools emit more context than they save. A live `select_context` call
against this repo with `budget=3000` returned `token_estimate = 9213` — 3× over budget —
dominated by three failure modes observed in `src/Graphos/UseCase/SelectContext.hs` and
`src/Graphos/UseCase/FormatContext.hs`:

1. **Unconditional god-node inclusion.** All three strategies seed `scGodNodes` with
   `take 5 (analysisGodNodes analysis)` and `formatContextForLLM` always renders a
   `### Hub Nodes` section, so the 246-degree `Main` node is force-injected into every
   response regardless of query relevance.
2. **Unbounded expansion hints.** `formatExpansionHints` iterates over every entry in
   `scCommunityLabels`, which for an Exploratory query includes the 2563-node mega-
   community 7463 — a hint no agent can act on.
3. **Unranked, unfiltered edges and no budget truncation.** `formatKeyEdges` takes
   `take 50` edges in `Map.toList` order (no relevance rank), `query_graph` emits raw
   `{nodes, edges, traverse}` with no verdict/hash/budget, and neither tool truncates to
   the requested budget. `query_graph` even calls `queryGraph` three times per request
   (`MCP.hs:130-132`).

Constraints: Domain stays pure and IO-free; UseCase stays pure; all side effects live in
Infrastructure (`MCP.hs`). Haskell conventions: explicit exports, type signatures on all
top-level definitions, no partial functions, strict data.

## Goals / Non-Goals

**Goals**
- `select_context` and `query_graph` MCP output never exceeds the requested budget
  (measured in `countContextTokens` units).
- Highest-ranked query-relevant node always survives truncation.
- God/hub nodes appear only on merit, not by default.
- Expansion hints are bounded, relevance-ranked, and exclude mega-/chat communities.
- `query_graph` MCP gains parity with the CLI `query-legibility` / `query-noise-control`
  contract (verdict, hash, ranked nodes, single invocation).
- Reuses existing `Graphos.Domain.Graph.Score` and `FormatContext.countContextTokens` —
  no new dependencies.

**Non-Goals**
- Changing the CLI `graphos query` renderer (already governed by `query-legibility` /
  `query-noise-control`).
- Replacing the five complexity-classification strategies or the Leiden clustering.
- Adding new MCP tools or changing the JSON-RPC transport.
- Introducing a real subword tokenizer — the `countContextTokens` heuristic stays.

## Decisions

### Decision 1: Truncate in `FormatContext`, driven by a budget-aware formatter

Truncation lives in `FormatContext.formatContextForLLM` (UseCase layer), not in the
strategies. The formatter receives the already-selected `SelectedContext` plus the
budget, renders greedily in relevance rank order, and stops when the next item would
exceed the budget — emitting a footer `- _omitted: N nodes, M edges_`.

| Aspect | Choice | Alternatives |
|--------|--------|--------------|
| Layer | UseCase (`FormatContext`) | (a) Domain — rejected: budget rendering is a presentation concern; (b) Infrastructure (`MCP.hs`) — rejected: would duplicate logic for the CLI path |
| Ranking source | Reuse `scMatchScore` / node relevance order already in `SelectedContext` | Compute a new edge-relevance score — rejected: extra complexity, endpoints' rank is a good proxy |
| Truncation granularity | Per-item (node then edge), greedy | (a) Knapsack by token cost — rejected: overkill for a soft heuristic; (b) Drop a fixed % — rejected: ignores budget semantics |

### Decision 2: Remove god-node force-inclusion in the strategies

`selectCommunityAware`, `selectRelevanceWeighted`, and `selectPathBased` stop unioning
`Set.fromList (map gnId (take 5 (analysisGodNodes analysis)))` into `allCandidateNodes`
and stop setting `scGodNodes` to a forced `take 5`. `scGodNodes` becomes the subset of
god nodes that are *already* in `nodesInBudget` (computed after selection, not before).
`selectArchitectural` is unchanged — it is the one strategy whose purpose is hub/bridge
overview.

| Aspect | Choice | Alternatives |
|--------|--------|--------------|
| Keep `scGodNodes` field | Yes, but populated from the selected set | Remove the field — rejected: `### Hub Nodes` is still useful when a hub is genuinely in scope |
| Architectural strategy | Unchanged (still uses god nodes by design) | Cap its god-node count — deferred to a future cycle |

### Decision 3: Bounded, relevance-filtered expansion hints

`formatExpansionHints` takes the budget-aware formatter's context plus two knobs:
`maxHints` (default 8) and `maxHintCommunitySize` (default 50). It ranks candidate
communities by the aggregate relevance of their members that are in `scNodes`, drops the
chat community (`chatCommunityId`), drops any community larger than the cap, and omits
the section entirely when none survive.

| Aspect | Choice | Alternatives |
|--------|--------|--------------|
| Knob location | MCP tool args `--max-hints`, `--max-hint-community-size` with formatter defaults | Hard-code — rejected: large graphs need tunability |
| Chat filter | Reuse `chatCommunityId` from `Domain.Context` | Re-derive — rejected: already exists |

### Decision 4: `query_graph` MCP adopts the scored JSON shape

Replace `handleQueryGraph`'s body (`MCP.hs:122-133`) with a single call to
`queryGraphWithIndexScored` and emit `{verdict, best_score, hash, nodes, edges, omitted}`
(toJSON of `QueryResponse` plus an `omitted` field). The existing `QueryResponse` type in
`Graphos.Domain.Graph.Score` already carries verdict/bestScore/hash/nodes/edges; we add
an `omitted` record field (or a wrapper) and a `ToJSON` instance if one is missing.

| Aspect | Choice | Alternatives |
|--------|--------|--------------|
| Reuse `QueryResponse` | Yes | New MCP-specific type — rejected: duplicates the scored-query contract |
| Edge semantic filter | Same `--edges semantic|all` knob as `select_context` | Separate filter — rejected: inconsistent |
| **BREAKING** response shape | `{nodes, edges, traverse}` → `{verdict, best_score, hash, nodes, edges, omitted}` | Keep old shape alongside — rejected: two shapes doubles the noise we are trying to cut |

### Decision 5: Token estimate via `countContextTokens`

`handleSelectContext` computes `token_estimate` with `countContextTokens formatted`
instead of `T.length formatted` (`MCP.hs:241`). The budget cap is enforced by the
formatter (Decision 1), and the reported `token_estimate` is the measured post-truncation
cost so callers can verify `token_estimate ≤ budget`.

## Risks / Trade-offs

- **[Risk] Truncation drops a needed second-ranked node** → Mitigation: greedy
  rank-order preservation guarantees the top node; a follow-up cycle can tune the
  node-vs-edge budget split (see Open Questions).
- **[Risk] Breaking `query_graph` clients** → Mitigation: the only known client is this
  opencode instance's `graphos_query_graph` tool, which tolerates a shape change;
  announce in the change log. No external users.
- **[Risk] `countContextTokens` heuristic underestimates real tokens** → Mitigation: it
  is a conservative ceiling on words; we truncate at the *heuristic* budget, so real
  tokens are ≤ heuristic ≤ requested budget with high probability.
- **[Risk] `selectArchitectural` still force-includes hubs** → Accepted: that strategy's
  *purpose* is architectural overview; users who want no hubs should pick a different
  complexity (the classifier already routes overview keywords to Architectural).

## Verification Strategy (Check)

1. `cabal build` clean (no warnings under `-Werror`).
2. `cabal test` — new Hspec modules:
   - `tests/Graphos/UseCase/FormatContextBudgetSpec.hs` — asserts `token_estimate ≤
     budget` for Focused/Module/CrossModule/Exploratory fixtures and that the top-ranked
     node is present after truncation.
   - `tests/Graphos/UseCase/SelectContextNoiseSpec.hs` — asserts `Main` (god node) is
     absent from a Focused Parser query and present only when query-relevant.
   - `tests/Graphos/Infrastructure/Server/MCPQuerySpec.hs` — asserts `query_graph`
     returns `verdict`, `hash`, and a single-invocation path (via a counting wrapper or
     by asserting on the response shape).
   - `tests/Graphos/UseCase/FormatContextHintsSpec.hs` — asserts mega-community and chat
     community are omitted from expansion hints.
3. Regression fixture: load `graphos-out/graph.json`, run the same 3000-token
   `select_context` query that today returns 9213 tokens, assert `token_estimate ≤ 3000`.
4. PRD §16.1 < 500 ms latency target re-measured on the repo's `graph.json` after the
   change (no new traversal introduced, so this is a guard against regressions).

## Iteration & Rollback (Act)

- If Check shows the top node is preserved but the second node is lost in > 20 % of
  fixtures: bump the node-vs-edge split from 60/40 to 70/30 in the next PDCA cycle.
- If the breaking `query_graph` shape breaks an unknown client: the rollback is the
  single edit to `handleQueryGraph` restoring the legacy `{nodes, edges, traverse}` shape
  (kept in git history). Re-introduce it behind a `--legacy-shape` flag rather than
  reverting the whole change.
- Standardize the `omitted` footer text across MCP and CLI so agents can parse it once.
- Feed the measured token-reduction ratio (target ≥ 60 %) back into PRD §7.2's budget
  table as empirical validation of the Focused/Module allocations.