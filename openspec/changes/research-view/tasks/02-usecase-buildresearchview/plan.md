# Task 2 — UseCase: buildResearchView (multi-query union + induce) — PLAN

**Task slug**: `02-usecase-buildresearchview`
**Attempt**: 1
**Status**: pending

## Summary

Implement `buildResearchView` in `src/Graphos/UseCase/Query/Research.hs` — the pure orchestration that runs N scored queries, takes the union of matched node IDs, accumulates per-term discovery attribution, computes the induced subgraph, collects community info, and builds the final `ResearchView`. Also implement `expandWithSeeds` for `--subgraph` seed expansion.

## Detail

### Scope

- **New module**: `src/Graphos/UseCase/Query/Research.hs`
- **Core function**: `buildResearchView :: Graph -> GraphIndex -> Map CommunityId CommunityComposition -> [Text] -> Maybe RefineConfig -> ResearchView`
  - Runs `queryGraphWithIndexScored` per term with budget 2000
  - Threads `RefineConfig` for `--edges` + noise control via `refineResponse`
  - Folds results into `Map NodeId ResearchNode` — accumulates `rnDiscoveredBy` (ordered by input term order), `rnScores` (per-term), keeps max as `rnBestScore`
  - Computes induced edges: `filter (\e -> edgeSource e ∈ unionIds && edgeTarget e ∈ unionIds) (gEdges g)`
  - Collects communities: for each union node, look up `CommunityId` via `communityOfNode`; group; attach label + composition (or `Nothing`)
  - Builds `ResearchMetadata` with counts and graph hash (UTCTime is obtained via IO wrapper outside this function)
- **Seed expansion**: `expandWithSeeds :: Graph -> GraphIndex -> Set NodeId -> [Text] -> Set NodeId`
  - Runs queries for seed terms, adds matched nodes to union
  - Performs 1-hop BFS expansion using `neighbors`
  - Returns expanded set (never removes original union nodes)
- **Hspec module**: `test/Graphos/UseCase/Query/ResearchSpec.hs`

### Check Criteria

**Tests to run**:
- `cabal test` — all Hspec cases in `test/Graphos/UseCase/Query/ResearchSpec.hs`
- `cabal build --flag dev` with `-Wall -Werror` — clean, no warnings

**Spec scenarios satisfied**:
- `Scenario: research returns union of multiple queries` (spec § "research returns union of multiple queries")
- `Scenario: single-term equivalence with query` (spec § "single-term equivalence with query")
- `Scenario: research on terms with no matches` (spec § "research on terms with no matches")
- `Scenario: node discovered by multiple terms` (spec § "node discovered by multiple terms")
- `Scenario: node discovered by one term` (spec § "node discovered by one term")
- `Scenario: induced edges respect union` (spec § "induced edges respect union")
- `Scenario: semantic edge filtering applies to induced edges` (spec § "semantic edge filtering applies to induced edges")
- `Scenario: all-edges mode preserves trivia edges` (spec § "all-edges mode preserves trivia edges")
- `Scenario: subgraph expands union by one hop` (spec § "subgraph expands union by one hop")
- `Scenario: subgraph never removes original matches` (spec § "subgraph never removes original matches")

**PASS conditions**:
- `buildResearchView` is a pure function (no IO)
- Union equals set-union of per-term `qrNodes` ids (tested by constructing mock `QueryResponse` values)
- Single-term equivalence: when `terms = [t]`, the `rvNodes` NodeId set equals the node set from a single `queryGraphWithIndexScored` call
- Induced edges have both endpoints in the union
- `discovered_by` attribution is correct: a node matched by two terms has both terms in the list, ordered by input
- Empty results (all terms return no matches) → empty `rvNodes`, empty `rvEdges`, non-empty `rvMetadata` with `node_count: 0, edge_count: 0`
- `--edges semantic` drops trivia-target edges from induced set
- `--subgraph` adds nodes and never reduces the union
- Expanded union includes all 1-hop neighbors of original union
- Induced edges are recomputed on expanded set
- Module has no IO implementation (strict rule compliance)

**FAIL boundaries**:
- If `buildResearchView` includes any IO or side effects, the test fails (architecture violation)
- If a node matched by terms ["phase", "work"] shows `discovered_by: ["work", "phase"]` (wrong order), the test fails
- If induced edges include edges targeting nodes outside the union, the test fails
- If `expandWithSeeds` removes a node from the original union, the test fails
- If empty results produce `rvMetadata` with `node_count` or `edge_count` that are not 0, the test fails

### Affected modules

- **New**: `src/Graphos/UseCase/Query/Research.hs`
- **New**: `test/Graphos/UseCase/Query/ResearchSpec.hs`
- **Imports from**: `src/Graphos/Domain/Query/Research.hs` (ResearchView, ResearchNode, etc.), `src/Graphos/UseCase/Query.hs` (queryGraphWithIndexScored, refineResponse), `src/Graphos/Domain/Graph/Core.hs` (Graph, NodeId, Edge, gEdges, gNodes), `src/Graphos/Domain/Graph/Query.hs` (communityOfNode), `src/Graphos/Domain/Community.hs` (CommunityId, CommunityComposition), `src/Graphos/UseCase/Query/Refine.hs` (RefineConfig)

### Prerequisites

- Task 1 (Domain types) must be implemented first
- Existing `queryGraphWithIndexScored`, `refineResponse`, `gEdges`, `gNodes`, `communityOfNode` must be available
- `RefineConfig` type must exist (from `query-noise-control` change or existing codebase)

### Risks

- **Medium**: Mocking `queryGraphWithIndexScored` in tests — the function has a complex signature with multiple parameters; may need a test wrapper or dependency injection via a typeclass (if not already present)
- **Medium**: `communityOfNode` and `gEdges` signatures must match; if the existing graph API uses FGL nids instead of `NodeId`, conversion is needed
- **Low**: The pure function approach is well-defined; most complexity is in test fixture construction
