# Tasks for devhaskell-g5-gap-1 Implementation

## Test Implementation Tasks

1. **J1 Property Tests**
   - Implement tests for UQI-1…UQI-8 acceptances
     - ✅ projection disjoint-field independence (UQI-1) — UQIProjectionSpec in main suite
     - ✅ support-graph grounding (UQI-3) — UQISupportSpec in main suite
     - ✅ fresh re-detection (UQI-4) — UQIFreshRedetectionSpec.hs integrated
     - ✅ monotonicity boundary (UQI-5) — UQIMonotonicitySpec.hs integrated into Spec.hs
     - ✅ cohesion cross-reading (UQI-6) — UQICohesionSpec.hs integrated into Spec.hs
     - ⬜ cost model (UQI-7) — not yet implemented
     - ✅ order-independence/old-wins AC-2 extension (UQI-8) — UQIOrderSpec.hs

2. **C3 Optional Test**
   - Implement fiber-product/pullback-on-labels property test
   - ✅ Already present in UQIOrderSpec.hs (LG-C3 fiber-product identity tests)

3. **Integration**
   - Add tests to appropriate test suites
     - ✅ UQICohesionSpec.hs and UQIMonotonicitySpec.hs integrated into Spec.hs
     - ✅ Test suite builds with 1548 test examples

## Acceptance Criteria
- All specified property tests must be implemented (including cost model UQI-7).
- Tests must integrate with existing test infrastructure.
- No changes to specification content beyond test implementation.

## Test status
- `cabal build --enable-tests graphos-test`: green (compiles + links on GHC 9.10.3).
- Full suite: **1380 examples, 0 failures, 3 pending**.
- G5 surface (`-m "SCC"`): 35 examples, 0 failures — `cond`, `isAcyclicDirected`, `isolatedNodes`, `connectedComponents`, `weaklyConnectedComponents`, and INV-C6 coarsening all pass.

## Commit / PR
- Committed to `AVI-512-add-math-in-graphos-project` (per AVI-1348 heartbeat: G5 lives under the math-adoption branch, not `feature/AVI-1381-release-hygiene`).
- PR deferred: no managed GitHub identity available this run (commit-only). Push + open PR on next run with identity.
