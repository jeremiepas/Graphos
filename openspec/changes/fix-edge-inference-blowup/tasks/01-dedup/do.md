# Do
- `dedupOn` in Domain.Analysis (exported, Set-based, BangPatterns).
- Replaced nubBy: Analysis.hs crossCommunitySurprises; Infer.hs inferTransitiveDeps + inferCodeDocEdges.
- InferSpec with QuickCheck property `dedupOn fst xs == nubBy ((==) `on` fst) xs`.
