# Tasks for AVI-961-intent-projection Implementation

## Test Implementation Tasks

1. **J1 Property Tests**
   - Implement tests for intent projection task acceptance (3.1, 3.2 complete).
     - ✅ Classifier and projection (NodeId identity preserved, well-formedness checks) implemented on main.
     - ✅ cabal build green.

2. **C3 Optional Test**
   - Implement fiber-product/pullback-on-labels property test — not yet present; add to `IntentSpec.hs` if needed.

3. **Integration**
   - Add tests to appropriate test suites:
     - ✅ Classifier and projection specs integrated into Spec.hs.
     - ✅ Test suite builds with cabal.

## Acceptance Criteria
- Implement intent code-graph projection (classifier + well-formedness) for tasks 3.1, 3.2.
- Provide property tests for INV-7 uniqueness / condensation-is-DAG.
- Golden fixture tests following AVI-523 pattern covering cond, isAcyclicDirected, isolatedNodes, connectedComponents, weaklyConnectedComponents, and INV-C6 coarsening.
- Build on existing `stronglyConnectedComponents`; no new Haskell dependencies.

## Commit / PR
- Branch: `feature/AVI-961-intent-projection` (PR #204).
- No managed GitHub identity available this run → commit-only work. Push + open PR on next run with identity.
