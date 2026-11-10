# Verification notes — multi-source-graphs

- Build/test: `cabal build` + `cabal test` green (1444 examples, 0 failures, 3 pending environment-gated).
- Manual E2E (generated temp corpus): two source roots with an identically-named file yielded distinct NodeIds with non-null distinct `source` metadata (`alpha`, `beta`).
- Update removal check: removing a source root from `sources:` and re-running `graphos --update` dropped its nodes; surviving NodeId unchanged.
- Remaining: 7.2 full label-cache E2E with a fake LLM port is covered at the unit level by `tests/Graphos/UseCase/LabelCacheSpec.hs`.
