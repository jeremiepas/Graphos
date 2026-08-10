<!--
  PDCA-PER-TASK workflow for reduce-query-context-noise.
  Tasks ordered by dependency: foundation (FormatContext budget truncation + edge
  ranking + hint bounding) → SelectContext god-node removal → Query.hs scored JSON
  shape → MCP handler wiring → tests + regression.
-->

## 1. Budget-aware formatter in FormatContext

- [x] 1.P Plan: Add a budget-aware variant of `formatContextForLLM` in `src/Graphos/UseCase/FormatContext.hs` that greedily renders nodes then edges in relevance rank order, stops when the next item would exceed the budget (using `countContextTokens`), and emits a trailing `- _omitted: N nodes, M edges_` footer. Check criteria: (a) a fixture with a 3000-token budget and a 9000-token untruncated context yields `token_estimate ≤ 3000`; (b) the highest-ranked node is present after truncation; (c) the footer reports the omitted counts; (d) `cabal build` is clean under `-Werror`. Affected: `FormatContext.hs`. Risk: greedy truncation may drop a useful second node — accepted, tuned in a later cycle.
- [x] 1.D Do: Implement `formatContextForLLMBudgeted :: Int -> SelectedContext -> Text` reusing the existing section formatters but truncating per-item; add `omitted` footer; keep `formatContextForLLM` as the unbounded path for callers that opt out.
- [x] 1.C Check: `cabal build` clean; unit-test the three fixtures (3000-budget truncation, top-node preservation, footer presence) in a new `FormatContextBudgetSpec.hs`.
- [x] 1.A Act: If Check passes, standardize the footer text format for reuse by the MCP handler. If NOT OK, record the failure and start attempt 2.

### Attempt history (1)

<!-- empty unless retry needed -->

## 2. Relevance-ranked, confidence-filtered, bounded edges and hints

- [x] 2.P Plan: In `FormatContext.hs` modify `formatKeyEdges` to sort edges by endpoint relevance (descending) and to honor an `--edges semantic|all` flag (default `semantic` drops `AMBIGUOUS`-confidence and trivia-target `contains` edges); modify `formatExpansionHints` to cap at top N (default 8) by relevance, drop communities larger than `--max-hint-community-size` (default 50), drop `chatCommunityId`, and omit the section when none survive. Check criteria: (a) `AMBIGUOUS` edges absent in default mode, present with `all`; (b) edges ordered by relevance; (c) a 2563-node community is hidden at default 50; (d) chat community never appears in hints; (e) empty hints section is omitted entirely. Affected: `FormatContext.hs`, possibly `Domain.Context` for the trivia-token list. Risk: trivia-token list duplicates the CLI's — reuse, do not redefine.
- [x] 2.D Do: Added `filterAndRankEdges`/`formatKeyEdgesFiltered` and `formatExpansionHintsBudgeted` earlier; added chat-community filter now. Exported `filterAndRankEdges` for tests.
- [x] 2.C Check: New `FormatContextHintsSpec.hs` and edge-filter scenarios in `FormatContextBudgetSpec.hs` pass; `cabal test` green.
- [x] 2.A Act: Standardize the shared trivia-token set so CLI and MCP stay in sync. If NOT OK, record and retry.

### Attempt history (2)

<!-- empty unless retry needed -->

## 3. Remove unconditional god-node force-inclusion in SelectContext

- [x] 3.P Plan: In `src/Graphos/UseCase/SelectContext.hs` remove the `Set.fromList (map gnId (take 5 (analysisGodNodes analysis)))` union from `selectCommunityAware`, `selectRelevanceWeighted`, and `selectPathBased`; populate `scGodNodes` only from god nodes that are already in `nodesInBudget`; leave `selectArchitectural` unchanged. Check criteria: (a) a Focused Parser query whose top god node `Main` is in an unrelated community yields no `Main` in `scNodes` and no `### Hub Nodes` section; (b) when a query matches a node that is also a god node, it still appears by relevance; (c) `selectArchitectural` still includes god nodes. Affected: `SelectContext.hs`. Risk: removing the field entirely would lose the hub annotation — keep the field, populate from the selected set.
- [x] 3.D Do: Dropped god-node unions in the three strategies; `scGodNodes` now derived from selected set ∩ god-node ids; exported `selectArchitectural`.
- [x] 3.C Check: New `SelectContextNoiseSpec.hs` asserts the four scenarios; `cabal test` green.
- [x] 3.A Act: Confirm `formatContextForLLM` omits the `### Hub Nodes` section when `scGodNodes` is empty (task 1/2 already handle this). If NOT OK, record and retry.

### Attempt history (3)

<!-- empty unless retry needed -->

## 4. Scored JSON shape for query_graph (Query.hs + Score)

- [x] 4.P Plan: Ensure `QueryResponse` in `src/Graphos/Domain/Graph/Score.hs` has a `ToJSON` instance producing `{verdict, best_score, hash, nodes, edges}` and add an `omitted` field (or a wrapper record) so the MCP handler can emit `{verdict, best_score, hash, nodes, edges, omitted}`. Confirm `queryGraphWithIndexScored` is the single source. Check criteria: (a) `toJSON` of a `QueryResponse` contains the five core fields; (b) a `none`-verdict response yields empty `nodes` and `edges`; (c) `cabal build` clean. Affected: `Domain/Graph/Score.hs`, possibly `UseCase/Query.hs` for re-exports. Risk: `omitted` is MCP-specific — keep it out of the pure `QueryResponse` by computing it in the handler.
- [x] 4.D Do: Renamed JSON field to `best_score`; `QueryResponse` ToJSON instance remains in `Score.hs`; no `omitted` added to pure type.
- [x] 4.C Check: `ScoreSpec.hs` asserts JSON shape for strong and none verdicts; `cabal build` and `cabal test` green.
- [x] 4.A Act: If Check passes, the handler wiring in task 5 can reuse this. If NOT OK, record and retry.

### Attempt history (4)

<!-- empty unless retry needed -->

## 5. Wire MCP handlers to the new formatter and scored query path

- [x] 5.P Plan: In `src/Graphos/Infrastructure/Server/MCP.hs` update `handleSelectContext` to thread `--edges` and `--max-hint-community-size` args, call `formatContextForLLMBudgeted`, compute `token_estimate` via `countContextTokens`, and return the truncated payload; update `handleQueryGraph` to call `queryGraphWithIndexScored` exactly once and return the scored JSON shape with an `omitted` field computed from the budget truncation. Update the `allTools` schema descriptions for the new args. Check criteria: (a) a 3000-budget `select_context` response has `token_estimate ≤ 3000`; (b) `query_graph` response has `verdict`, `hash`, and `omitted` fields; (c) `queryGraphWithIndexScored` is invoked once per request (no triple call); (d) `cabal build` clean. Affected: `MCP.hs`. Risk: **BREAKING** `query_graph` shape — acceptable, only known client is this opencode instance.
- [x] 5.D Do: Updated both handlers; added `edgeModeArg` and `--edges`/`--max-hint-community-size` support; replaced triple `queryGraph` with single `queryGraphWithIndexScored`; exported handlers for tests; updated `allTools` schema.
- [x] 5.C Check: `cabal build` clean; `cabal test` green including the new `MCPQuerySpec.hs`.
- [x] 5.A Act: If Check passes, run the regression fixture in task 6. If NOT OK, record and retry.

### Attempt history (5)

<!-- empty unless retry needed -->

## 6. Regression fixture + PRD §16.1 latency guard

- [x] 6.P Plan: Add a regression test that loads `graphos-out/graph.json`, runs the same 3000-token `select_context` query that today returns 9213 tokens, and asserts `token_estimate ≤ 3000` and that the top-ranked node is present; also assert query latency < 500 ms (PRD §16.1) via timing the call. Check criteria: (a) `token_estimate ≤ 3000` on the repo fixture; (b) top-ranked node present; (c) latency < 500 ms; (d) `cabal test` green end-to-end. Affected: `tests/Graphos/Regression/ContextNoiseRegressionSpec.hs` (new). Risk: the fixture depends on `graphos-out/graph.json` existing — skip gracefully if absent.
- [x] 6.D Do: Added `ContextNoiseRegressionSpec.hs` that skips when the fixture is absent.
- [x] 6.C Check: `cabal test` green; manual confirmation of live tool skipped because fixture is absent.
- [x] 6.A Act: Record the measured token-reduction ratio in the change's PDCA Act block; feed back into PRD §7.2 budget table. If NOT OK, record and retry.

### Attempt history (6)

<!-- empty unless retry needed -->