## Context

Relevance scoring lives in the UseCase layer (`Graphos.UseCase.Query`) and delegates
score primitives to the Domain layer (`Graphos.Domain.Graph.Score`). The scored path is
`queryGraphWithIndexScoredCached`, which:

1. Normalizes the query into terms (lowercase, split, drop tokens ≤ 2 chars).
2. Matches terms via the inverted index → `matched :: [(NodeId, Int)]` (node, raw
   matched-term count).
3. Builds `scoredPairs` (the matched set, for the verdict/best-score) and
   `scoredNodes` (the expanded neighborhood, for the ranked result list).
4. Ranks `scoredNodes` with a stable `sortOn (negate . snScore)`.

The per-node score is `normalizeScore rawScore (length terms) + fullLabelBoost …`. The
bug is in the `fullLabelBoost` argument: it is fed the node's own first token
(`firstToken (nodeLabel n)`) instead of the query terms, so the boost is a
self-comparison that ignores the query.

## Goals / Non-Goals

**Goals:**
- Make the full-label boost a query→label comparison (the intended semantics).
- Ensure non-matching neighborhood nodes score `0.0`, so matched nodes rank first.
- Keep the change minimal and confined to the scoring path (no traversal, index, or
  serialization changes).

**Non-Goals:**
- Changing traversal (BFS/DFS), depth, or the top-5 seed selection.
- Changing the load path, index construction, or FGL caching (the >300 s timeout).
- Changing the JSON/CLI response contract or field names.
- Fixing the partially-written `graph.json` parse error (separate robustness change).

## Decisions

- **Boost over query terms, not the node's own token.** Add
  `fullLabelBoostForTerms :: [Text] -> Text -> Double` in Domain/Graph/Score returning
  `0.1` when any query term equals the lowercased full label, else `0`. Use it in both
  `scoredPairs` and `scoredNodes`.
  - *Alternative considered:* keep `fullLabelBoost` and call it once per query term,
    summing — rejected, a multi-term exact full-label match is not expected and summing
    would over-boost; `max`/any semantics matches the documented "+0.1" intent.
  - *Alternative considered:* drop the boost entirely — rejected, it is a deliberate
    ranking signal for exact-name queries and is cheap.
- **Confine the fix to the UseCase scoring path + one Domain helper.** The Domain
  helper is pure and IO-free (Domain layer rule); the UseCase wires it in (UseCase
  layer rule). No Infrastructure change.
  - *Alternative considered:* fix inline in UseCase only, no Domain helper — rejected,
    the helper keeps the "any query term == full label" rule in one tested place and
    mirrors the existing `fullLabelBoost`/`normalizeScore` Domain primitives.
- **Retain `fullLabelBoost` (single-term) for compatibility.** Existing callers/tests
  may reference it; the new helper is additive.
  - *Alternative considered:* delete `fullLabelBoost` — rejected, unnecessary churn and
    it remains a valid single-term primitive.
- **Keep the stable score-descending sort.** With correct scores, matched nodes
  (`> 0`) sort above context neighbors (`0.0`); the tie among `0.0` context nodes is
  harmless (they are context, not answers).
  - *Alternative considered:* add a secondary sort key (e.g. distance, then id) for
    deterministic context ordering — deferred, not required for correctness and would
    change the `resultHash` beyond the scoring fix.

## Risks / Trade-offs

- [Result ordering + `resultHash` change] → expected for a correctness fix; the hash is
  a "no new information" signal per query+graph, and a fixed query yields a stable (new)
  hash. Callers comparing hashes across the fix should re-baseline.
- [Boost may now fire more often] → only when a query term exactly equals a full label,
  which is the intended exact-name signal; bounded to `+0.1`, below the `0.5` Strong
  threshold, so it cannot flip a `Weak`/`NoMatch` verdict on its own.
- [Behavior diff on large graphs] → verified by regression tests on a small synthetic
  graph; the large-graph timeout is out of scope and tracked separately.

## Verification Strategy

- `cabal build --flag dev` (compiles with `-Wall -Werror`).
- `cabal test` — new Hspec/QuickCheck cases in the Query/Score suites:
  - full-label boost fires when a query term equals a node's full label;
  - a non-matching single-word neighbor scores `0.0` (not `0.1`);
  - matched nodes rank above `0.0`-scored neighbors in `qrespNodes`.
- Manual: `cabal run graphos -- query "<term>" --json` on a small graph; confirm the
  top result is the matched node and context neighbors are `0.0`.
