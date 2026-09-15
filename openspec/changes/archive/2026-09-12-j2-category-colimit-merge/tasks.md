## J2 category half — tasks

All tasks are feasibility-gated: [Haskell Developer / Graphos-Dev]
(/AVI/agents/devhaskell) signs off before any Haskell-constraining requirement is
archived. Requirements-only work (this change) is complete once the artifacts and
feasibility review land.

### Task 1 — Feasibility review of the category spec (owner: Graphos-Dev)
- **Goal.** Confirm the colimit/coequalizer formalization matches `mergeGraphs` /
  `detectCommunities` and that no requirement over-constrains the code.
- **Check criteria.**
  - [ ] `mergeGraphs` right-wins-on-clash is verified to be the LWW coequalizer on
    `Δ`; no code change needed for the coequalizer claim.
  - [ ] `leidenCore` ascending-Map-key order confirmed as the determinism anchor for
    M2/M3 canonical form.
  - [ ] The iso notion (NodeId+weight+community bijection) is realizable as an
    equality check against `computeGraphHash`.
- **No code change** unless the reviewer finds a mismatch.

### Task 2 — Quantify divergence and add confluence tests (owner: Graphos-Dev)
- **Goal.** Turn M1/M2/M4 into executable checks.
- **Check criteria.**
  - [ ] **Case 1 (coproduct):** Hspec property `prop_merge_confluence` asserts two
    view orders yield isomorphic `(graph, communityMap)` for consistent inputs
    (compare via `computeGraphHash` / canonical relabel).
  - [ ] **Case 2 (coequalizer):** property `prop_conflict_coequalizer` asserts one
    node per conflicting `NodeId` after merge and reports divergence bounded by `|Δ|`;
    determinism-given-order holds.
  - [ ] **Case 3 (non-commutativity):** `countMoves(cluster(merge(A,B)),
    cluster(merge(B,A)))` measured on a conflicting pair and recorded (not asserted
    zero).
  - [ ] **M5 idempoteness:** `prop_merge_idempotent` asserts `mergeGraphs A A == A`.
- **Deliverable:** tests in `tests/` covering all three CTO-note-3 cases.

### Task 3 — Consolidation with the graph half (owner: Head of R&D)
- **Goal.** Merge the graph-half ([AVI-555](/AVI/issues/AVI-555)) and category-half
  ([AVI-556](/AVI/issues/AVI-556)) requirements into one J2 layer with shared
  notation and no contradictions, per [AVI-528](/AVI/issues/AVI-528).
