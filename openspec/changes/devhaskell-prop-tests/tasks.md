# Tasks for devhaskell-prop-tests Implementation

## Test Implementation Tasks
1. **J1 Property Tests**
   - Implement tests for UQI-1…UQI-8 acceptances
   - Tests for: projection disjoint-field independence, support-graph grounding, fresh re-detection, monotonicity boundary, cohesion cross-reading, cost model, order-independence/old-wins AC-2 extension

2. **C3 Optional Test**
   - Implement fiber-product/pullback-on-labels property test
   - For LG-C3 merge-confluence specification

3. **Integration**
   - Add tests to appropriate test suites
   - Ensure proper test runner configuration
   - Follow existing test patterns

## Acceptance Criteria
- All specified property tests must be implemented
- Tests must integrate with existing test infrastructure
- No changes to specification content beyond test implementation