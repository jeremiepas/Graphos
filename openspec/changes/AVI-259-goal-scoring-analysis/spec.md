# Spec: Goal Scoring Analysis

## Purpose

Provide a comprehensive analysis of all Graphos features with:
- Wish scores for user value prioritization
- Dependency graphs showing what blocks what
- Multiple build path solutions
- Statistics for planning

## Scope

- All active specs under `openspec/specs/`
- All active changes under `openspec/changes/` (excluding archive/)
- Dependency mapping between features
- Score computation and tier assignment

## Wish Score Formula

```
WishScore = (U × W × R) / (C × D)
```

Where:
- U = User Value (1–10)
- W = Workflow Fit (1–10)
- R = Reusability (1–10)
- C = Complexity (1–10, inverted)
- D = Dependency Depth (1–10, inverted)

## Score Tiers

| Tier | Range | Action |
|------|-------|--------|
| P0 | 8.0+ | Build first — foundational |
| P1 | 6.0–7.9 | High priority — blocks major workflows |
| P2 | 4.0–5.9 | Medium priority — valuable but not blocking |
| P3 | <4.0 | Low priority — nice to have |

## Acceptance Criteria

- [ ] All 22 active changes scored and categorized
- [ ] Dependency graph with topological ordering
- [ ] 3 alternative build paths documented
- [ ] Statistics computed (distribution, by category, effort estimation)
- [ ] Child issues created for P0 features
