## 1. Domain scoring helper

- [x] 1.1 Add `fullLabelBoostForTerms :: [Text] -> Text -> Double` to
  `Graphos.Domain.Graph.Score` (export it): `0.1` when any query term equals the
  lowercased full label, else `0`. Check: pure, IO-free, compiles under `-Wall -Werror`.
- [x] 1.2 Unit tests for `fullLabelBoostForTerms`: exact match (case-insensitive) →
  `0.1`; no term equals the label → `0`; multi-term where only one equals → `0.1`.

## 2. UseCase scoring fix

- [x] 2.1 In `Graphos.UseCase.Query`, replace the `fullLabelBoost (firstToken (nodeLabel n)) (nodeLabel n)` self-comparison in `scoredPairs` with `fullLabelBoostForTerms terms (nodeLabel n)`.
- [x] 2.2 Replace the same self-comparison in `scoredNodes` with `fullLabelBoostForTerms terms (nodeLabel n)`.
- [x] 2.3 Remove the now-unused `let term = firstToken (nodeLabel n)` binding in
  `scoredPairs`; keep `firstToken` only if still referenced elsewhere (else drop the
  local). Check: no `-Wunused`/`-Wincomplete-uni-patterns` warnings; builds clean.

## 3. Regression tests (Query)

- [x] 3.1 Test: a node whose full label equals a query term receives the `+0.1` boost on
  top of its normalized score.
- [x] 3.2 Test: a non-matching single-word-labeled neighbor in the expanded set scores
  `0.0` (regression for the flat `0.1` bug).
- [x] 3.3 Test: in `qrespNodes`, matched nodes (score `> 0`) are ordered before
  `0.0`-scored context neighbors.

## 4. Verification

- [x] 4.1 `cabal build --flag dev` green (no warnings, `-Werror`).
- [x] 4.2 `cabal test` green (Score + Query suites).
- [x] 4.3 Manual: `cabal run graphos -- query "<term>" --json` on a small graph; top
  result is the matched node, context neighbors are `0.0`.