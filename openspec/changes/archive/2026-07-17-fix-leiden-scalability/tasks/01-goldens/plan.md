# Plan: Golden result-equivalence specs

## Goal
Lock the current clustering outputs before any refactor so result equivalence is provable.

## Approach
Add exact membership-set assertions to `tests/Graphos/Domain/CommunitySpec.hs` for deterministic reference graphs: two 4-cliques + bridge, path of 6, triangle + chained pairs.

## Check Criteria
- New specs pass against the CURRENT implementation.
- `cabal build -Werror` clean; existing suite green.
