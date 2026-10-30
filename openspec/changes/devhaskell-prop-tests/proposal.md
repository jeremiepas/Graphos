# devhaskell-prop-tests: Implement Property Tests for Adoption Specs

## Summary
Implement property tests for the three adoption routes as specified in AVI-1291 feasibility verdict.

## Routes Covered
1. **J1**: `openspec/specs/colimit-modularity-consistency/spec.md` - UQI-1…UQI-8 acceptances
2. **C3 (optional)**: `openspec/specs/merge-confluence/spec.md` - fiber-product/pullback-on-labels test
3. **General**: Support for order-independence/old-wins property testing

## Test Requirements
- Implement UQI-1…UQI-8 property acceptances:
  - Projection disjoint-field independence
  - Support-graph grounding 
  - Fresh re-detection
  - Monotonicity boundary
  - Cohesion cross-reading
  - Cost model
  - Order-independence/old-wins AC-2 extension
- Optional: fiber-product/pullback-on-labels test for LG-C3
- Follow existing test patterns in the codebase

## Acceptance Criteria
- All tests must pass successfully
- Must cover all specified acceptance criteria
- Tests should be integrated into existing test suite structure