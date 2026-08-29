## Why

`graphos query` on a large graph (observed 4.4 GB `graph.json`, ~35k nodes) returns
unusable results: every node in the expanded neighborhood scores a flat `0.1` and the
top results are irrelevant fixtures/mocks. The relevance scoring is broken by a
self-comparison bug.

In `Graphos.UseCase.Query`, the full-label boost is computed as
`fullLabelBoost (firstToken (nodeLabel n)) (nodeLabel n)` — i.e. the node's **own**
first token is compared to the node's **own** full label. The query terms are never
compared to the label. Consequences:

- The intended "a query term exactly matches the node's full label → +0.1" boost never
  fires, because the query term is never passed in.
- Every single-word-labeled node in the BFS neighborhood is bumped to exactly `0.1`
  regardless of the query (multi-word labels get `0.0`). This floods the result set with
  `0.1`-scored, query-irrelevant nodes.
- Results are a **stable** sort on score (`sortOn (negate . snScore)`). All the `0.1`
  nodes tie, so their relative order is the arbitrary hash order of the neighborhood
  set — hence "top results were irrelevant fixtures/mocks".

This is the "broken query" reported in [AVI-110](/AVI/issues/AVI-110). The fix makes the
boost a query→label comparison so matched nodes rank first and the neighborhood is
context (score `0.0`), not noise.

## What Changes

- Fix the relevance scoring in `Graphos.UseCase.Query` (`scoredPairs` and `scoredNodes`)
  so the full-label boost compares the **query terms** to the node's full label, not the
  node's own first token to itself.
- Introduce `fullLabelBoostForTerms :: [Text] -> Text -> Double` in
  `Graphos.Domain.Graph.Score`: `+0.1` when **any** query term exactly equals the
  lowercased full label, else `0`.
- Non-matching neighborhood nodes now score `0.0` (not a flat `0.1`), so matched nodes
  rank above context neighbors and the stable-sort tie no longer surfaces arbitrary
  nodes at the top.
- Add regression tests covering: full-label boost fires for a query term equal to a
  node's full label; a non-matching single-word neighbor scores `0.0`; matched nodes
  rank above `0.0`-scored neighbors.

## Capabilities

### New Capabilities
- `query-relevance-scoring`: specifies the relevance scoring formula — normalized
  matched-term score, the query-term full-label boost, `0.0` for non-matching
  neighborhood nodes, and score-descending ranking with matched nodes above context
  neighbors. Refines the query workflow described in `openspec/specs/04-query`.

## Impact

- **Domain/Graph/Score**: new `fullLabelBoostForTerms` helper; `fullLabelBoost`
  (single-term) retained for compatibility.
- **UseCase/Query**: `scoredPairs` and `scoredNodes` use the query-term boost; the
  `firstToken` self-comparison is removed from the scoring path.
- **Behavior**: query result ordering and per-node scores change for affected graphs
  (the `resultHash` over the ordered result ids will differ). This is a correctness fix,
  not a contract change: the JSON/CLI field surface is unchanged.
- **Out of scope (tracked separately):** the >300 s load-time timeout for
  `symbols`/`explain`/`neighbors` on multi-GB graphs (a load-path performance issue —
  full `graph.json` parse + index + FGL build per invocation) and the JSON parse error
  on a partially-written `graph.json` (a write-atomicity/robustness issue). Both are
  real but distinct root causes from the scoring bug and need their own design.
