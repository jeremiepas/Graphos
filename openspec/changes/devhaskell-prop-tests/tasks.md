# Tasks for devhaskell-prop-tests Implementation

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
   - ✅ Test suite builds successfully with 1548 test examples

## Acceptance Criteria
- All specified property tests must be implemented
- Tests must integrate with existing test infrastructure
- No changes to specification content beyond test implementation