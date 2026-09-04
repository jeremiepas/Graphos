# Task: Fix Query Relevance Scoring

## Goal

Fix the query relevance scoring algorithm to return more accurate and useful results for agent context selection.

## Score: 11.52 (P0) — Third highest priority

## Acceptance Criteria

- [ ] Query results ranked by relevance (not just keyword match)
- [ ] Semantic similarity considered (embedding-based)
- [ ] Community context boosts relevance
- [ ] Token budget respected in scoring
- [ ] Regression tests against existing query fixtures

## Dependencies

- None (build first)

## Blocks

- All query features (10 features)
- checkpoint-and-cluster-only-controls
- mcp-request-resilience
- enforce-query-token-budget
- fix-agent-skill-graphos-cli-tools
- deterministic-doc-code-edges
- openspec-view
- explorer-queries

## Implementation Plan

1. Review current query scoring in `src/Graphos/UseCase/Query/`
2. Identify scoring bugs (keyword-only, no semantic boost)
3. Implement hybrid scoring: keyword + semantic + community
4. Add token budget enforcement to scoring
5. Update query CLI contract if needed
6. Add regression tests

## Verification

- Run existing query tests
- Create test queries with known relevance order
- Verify semantic similarity boosts correct results
- Test token budget enforcement
