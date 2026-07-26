# Do: Golden result-equivalence specs

- Added `cliqueEdges` and `communityMembershipSets` helpers and three golden cases to `tests/Graphos/Domain/CommunitySpec.hs`:
  - two 4-cliques + bridge → the two cliques
  - path of 6 → two halves
  - triangle + three chained pairs → {a,b,c}, {x1,x2,y1}, {y2,z1,z2}
- Captured outputs via a GHCi harness against the pre-change implementation.
