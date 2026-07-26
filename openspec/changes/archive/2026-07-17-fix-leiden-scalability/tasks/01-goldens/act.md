# Act: Golden result-equivalence specs

- Goldens locked.
- **Bug discovered during capture**: `mergeSmallCommunities` silently LOSES NODES — on triangle{a,b,c}+pair{d,e} (raw communities {b},{c,a},{e,d}), node `b` vanishes because `mergeOne` inserts a stale member snapshot when a previously-merged-into community is itself merged. Node-preservation spec deferred to task 3 alongside the fix (would fail on the current implementation).
