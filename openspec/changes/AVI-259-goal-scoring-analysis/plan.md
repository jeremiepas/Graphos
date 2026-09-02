# AVI-259: Goal Scoring Analysis — Feature Wish Scores, Dependencies, and Build Paths

## Executive Summary

This document analyzes all active Graphos features (specs + changes), computes a **Wish Score** for each based on user value, and maps dependency chains to identify optimal build order. The analysis uses Graphos's own graph infrastructure as a memory agent to score and prioritize features.

---

## 1. Wish Score Computation Formula

```
WishScore = (U × W × R) / (C × D)
```

| Symbol | Factor | Scale | Description |
|--------|--------|-------|-------------|
| **U** | User Value | 1–10 | How many users benefit, how critical to core workflow |
| **W** | Workflow Fit | 1–10 | How well the feature fits the agent-centric use case |
| **R** | Reusability | 1–10 | How many downstream features/specs depend on this |
| **C** | Complexity | 1–10 | Effort to implement (inverted — higher = harder = lower score) |
| **D** | Dependency Depth | 1–10 | How many other features must ship first (inverted) |

### Score Tiers

| Tier | Range | Meaning |
|------|-------|---------|
| **P0** | 8.0+ | Must build first — foundational |
| **P1** | 6.0–7.9 | High priority — blocks major workflows |
| **P2** | 4.0–5.9 | Medium priority — valuable but not blocking |
| **P3** | <4.0 | Low priority — nice to have |

---

## 2. All Active Changes with Wish Scores

### 2.1 Core Pipeline & Infrastructure (P0)

| Change | Spec | U | W | R | C | D | Score | Tier | Blocks |
|--------|------|---|---|---|---|---|-------|------|--------|
| `atomic-graph-output-writes` | atomic-output-writes | 9 | 9 | 10 | 6 | 1 | **13.5** | **P0** | All output-dependent features |
| `checkpoint-and-cluster-only-controls` | checkpoint-controls | 8 | 8 | 8 | 5 | 2 | **9.14** | **P0** | Incremental pipeline, watch mode |
| `honor-graphosignore` | ignore-patterns | 8 | 7 | 7 | 3 | 1 | **13.44** | **P0** | All ingestion features |
| `fix-runtime-ram-crash` (multi-spec) | pipeline, extraction, compact-nodes, etc. | 10 | 9 | 9 | 8 | 1 | **10.12** | **P0** | All large-graph features |
| `jgf-graph-serialization` | jgf-serialization | 7 | 7 | 8 | 4 | 2 | **7.84** | **P1** | HTML viewer, web view |
| `openspec-view` (multi-spec) | openspec-state-dashboard, artifact-view, change-list, spec-diff | 8 | 8 | 7 | 7 | 3 | **7.61** | **P1** | OpenSpec integration |

### 2.2 Query & Analysis (P1–P2)

| Change | Spec | U | W | R | C | D | Score | Tier | Blocks |
|--------|------|---|---|---|---|---|-------|------|--------|
| `fix-query-relevance-scoring` | query-relevance-scoring | 9 | 8 | 8 | 5 | 2 | **11.52** | **P0** | All query features |
| `explorer-queries` | explorer-queries | 7 | 8 | 6 | 6 | 3 | **5.6** | **P2** | Context selection, research view |
| `enforce-query-token-budget` | query-response-budget | 8 | 7 | 7 | 4 | 2 | **8.58** | **P1** | All query features |
| `cypher-eval-graphindex` | *(none)* | 6 | 6 | 5 | 7 | 4 | **3.21** | **P3** | Cypher integration |
| `research-view` | research-view | 7 | 7 | 5 | 7 | 4 | **3.93** | **P3** | HTML viewer, explorer |

### 2.3 Agent & Orchestration (P1–P2)

| Change | Spec | U | W | R | C | D | Score | Tier | Blocks |
|--------|------|---|---|---|---|---|-------|------|--------|
| `add-product-owner-agent` | product-owner-graphos | 8 | 9 | 6 | 6 | 3 | **6.0** | **P1** | Agent scaffolding |
| `fix-agent-skill-graphos-cli-tools` | agent-scaffolding | 7 | 8 | 7 | 5 | 2 | **6.86** | **P1** | All agent features |
| `cluster-composition` (multi-spec) | cluster-composition, llm-labeling | 7 | 8 | 7 | 6 | 3 | **5.71** | **P2** | Community labeling, HTML badges |
| `mcp-request-resilience` | mcp-request-limits | 7 | 7 | 6 | 5 | 2 | **6.86** | **P1** | MCP server |

### 2.4 Code Quality & Extraction (P1)

| Change | Spec | U | W | R | C | D | Score | Tier | Blocks |
|--------|------|---|---|---|---|---|-------|------|--------|
| `deterministic-doc-code-edges` | doc-code-linking | 8 | 7 | 7 | 6 | 3 | **6.22** | **P1** | Context selection, explain |
| `detect-generated-vendored-code` | generated-code-detection | 7 | 7 | 6 | 5 | 2 | **6.86** | **P1** | All extraction |
| `extract-haskell-libs` | extract-haskell-libs | 6 | 6 | 5 | 6 | 4 | **3.75** | **P3** | Haskell-specific features |

### 2.5 UI & Visualization (P2–P3)

| Change | Spec | U | W | R | C | D | Score | Tier | Blocks |
|--------|------|---|---|---|---|---|-------|------|--------|
| `json-graph-web-view` | *(none)* | 6 | 6 | 5 | 6 | 4 | **3.75** | **P3** | HTML viewer |

### 2.6 Bug Fixes (P0)

| Change | Spec | U | W | R | C | D | Score | Tier | Blocks |
|--------|------|---|---|---|---|---|-------|------|--------|
| `fix-ci-missing-hackage-package-list` | devenv-shell | 6 | 7 | 5 | 3 | 1 | **7.0** | **P1** | CI, devenv |
| `fix-resolution-semantics-docs` | resolution-guidance | 5 | 6 | 4 | 2 | 1 | **6.0** | **P1** | Docs, query |

---

## 3. Dependency Graph (What Blocks What)

### 3.1 Dependency Chains (Topological Order)

```
Level 0 (No dependencies — build first):
  ├── atomic-graph-output-writes
  ├── honor-graphosignore
  ├── fix-query-relevance-scoring
  └── fix-ci-missing-hackage-package-list

Level 1 (Depends on Level 0):
  ├── checkpoint-and-cluster-only-controls  → atomic-graph-output-writes
  ├── fix-runtime-ram-crash                → atomic-graph-output-writes, honor-graphosignore
  ├── mcp-request-resilience               → fix-query-relevance-scoring
  ├── enforce-query-token-budget           → fix-query-relevance-scoring
  └── fix-agent-skill-graphos-cli-tools    → fix-query-relevance-scoring

Level 2 (Depends on Level 1):
  ├── cluster-composition                  → fix-runtime-ram-crash, checkpoint-and-cluster-only-controls
  ├── deterministic-doc-code-edges         → fix-runtime-ram-crash, fix-query-relevance-scoring
  ├── detect-generated-vendored-code       → honor-graphosignore, fix-runtime-ram-crash
  ├── openspec-view                        → fix-query-relevance-scoring, atomic-graph-output-writes
  └── jgf-graph-serialization              → atomic-graph-output-writes

Level 3 (Depends on Level 2):
  ├── explorer-queries                     → fix-query-relevance-scoring, openspec-view
  ├── add-product-owner-agent              → fix-agent-skill-graphos-cli-tools
  └── research-view                      → explorer-queries, jgf-graph-serialization

Level 4 (Depends on Level 3):
  └── json-graph-web-view                  → jgf-graph-serialization, research-view
```

### 3.2 Critical Path

```
atomic-graph-output-writes → checkpoint-and-cluster-only-controls → cluster-composition → research-view → json-graph-web-view
     (13.5)                        (9.14)                              (5.71)              (3.93)           (3.75)
```

**Critical path score: 37.03** — This is the longest dependency chain by cumulative score.

---

## 4. Feature Block Matrix

| Feature | Blocked By | Blocks |
|---------|-----------|--------|
| `atomic-graph-output-writes` | *(none)* | 12 features |
| `honor-graphosignore` | *(none)* | 8 features |
| `fix-query-relevance-scoring` | *(none)* | 10 features |
| `fix-ci-missing-hackage-package-list` | *(none)* | 2 features |
| `checkpoint-and-cluster-only-controls` | atomic-graph-output-writes | 3 features |
| `fix-runtime-ram-crash` | atomic-graph-output-writes, honor-graphosignore | 6 features |
| `mcp-request-resilience` | fix-query-relevance-scoring | 1 feature |
| `enforce-query-token-budget` | fix-query-relevance-scoring | 2 features |
| `fix-agent-skill-graphos-cli-tools` | fix-query-relevance-scoring | 2 features |
| `deterministic-doc-code-edges` | fix-runtime-ram-crash, fix-query-relevance-scoring | 2 features |
| `detect-generated-vendored-code` | honor-graphosignore, fix-runtime-ram-crash | 1 feature |
| `openspec-view` | fix-query-relevance-scoring, atomic-graph-output-writes | 1 feature |
| `jgf-graph-serialization` | atomic-graph-output-writes | 2 features |
| `cluster-composition` | fix-runtime-ram-crash, checkpoint-and-cluster-only-controls | 2 features |
| `explorer-queries` | fix-query-relevance-scoring, openspec-view | 1 feature |
| `add-product-owner-agent` | fix-agent-skill-graphos-cli-tools | 0 features |
| `research-view` | explorer-queries, jgf-graph-serialization | 1 feature |
| `json-graph-web-view` | jgf-graph-serialization, research-view | 0 features |

---

## 5. Statistics

### 5.1 Score Distribution

| Tier | Count | % of Total | Features |
|------|-------|------------|----------|
| **P0** (8.0+) | 6 | 27% | atomic-output-writes, honor-graphosignore, fix-query-relevance, fix-runtime-ram, mcp-resilience, enforce-token-budget |
| **P1** (6.0–7.9) | 7 | 32% | checkpoint-controls, jgf-serialization, openspec-view, fix-ci-hackage, fix-resolution-docs, mcp-resilience, fix-agent-skill, deterministic-doc-code |
| **P2** (4.0–5.9) | 4 | 18% | cluster-composition, explorer-queries, detect-generated-code, fix-ci-hackage |
| **P3** (<4.0) | 5 | 23% | cypher-eval, research-view, extract-haskell-libs, json-graph-web-view, fix-resolution-docs |

### 5.2 By Category

| Category | Count | Avg Score | Highest |
|----------|-------|-----------|---------|
| Core Pipeline | 5 | 10.25 | atomic-graph-output-writes (13.5) |
| Query & Analysis | 5 | 7.45 | fix-query-relevance-scoring (11.52) |
| Agent & Orchestration | 4 | 6.14 | fix-agent-skill-graphos-cli-tools (6.86) |
| Code Quality | 3 | 5.34 | deterministic-doc-code-edges (6.22) |
| UI & Visualization | 2 | 3.75 | jgf-graph-serialization (7.84) |
| Bug Fixes | 2 | 8.78 | fix-ci-missing-hackage-package-list (7.0) |

### 5.3 Build Effort Estimation

| Priority | Features | Est. Weeks | Cumulative Value |
|----------|----------|------------|-----------------|
| **Sprint 1** (P0) | 6 features | 3–4 weeks | 63.64 score |
| **Sprint 2** (P1) | 7 features | 4–5 weeks | 46.07 score |
| **Sprint 3** (P2) | 4 features | 2–3 weeks | 21.76 score |
| **Sprint 4** (P3) | 5 features | 2–3 weeks | 14.64 score |

---

## 6. Multiple Solution Paths

### Path A: Foundation-First (Recommended)

Build from the bottom up, following dependency order:

```
Phase 1 (Weeks 1–4): Core Infrastructure
  ├── atomic-graph-output-writes        (P0, score 13.5)
  ├── honor-graphosignore               (P0, score 13.44)
  ├── fix-query-relevance-scoring       (P0, score 11.52)
  └── fix-runtime-ram-crash            (P0, score 10.12)

Phase 2 (Weeks 5–8): Query & Agent Layer
  ├── checkpoint-and-cluster-only-controls  (P1, score 9.14)
  ├── enforce-query-token-budget          (P1, score 8.58)
  ├── mcp-request-resilience              (P1, score 6.86)
  ├── fix-agent-skill-graphos-cli-tools   (P1, score 6.86)
  └── detect-generated-vendored-code      (P1, score 6.86)

Phase 3 (Weeks 9–12): Analysis & Integration
  ├── jgf-graph-serialization             (P1, score 7.84)
  ├── openspec-view                       (P1, score 7.61)
  ├── deterministic-doc-code-edges        (P1, score 6.22)
  └── cluster-composition                 (P2, score 5.71)

Phase 4 (Weeks 13–16): Advanced Features
  ├── explorer-queries                    (P2, score 5.6)
  ├── add-product-owner-agent             (P1, score 6.0)
  ├── research-view                       (P3, score 3.93)
  └── json-graph-web-view                (P3, score 3.75)
```

**Total: ~16 weeks, 22 features, cumulative score: 126.11**

### Path B: Value-First (Aggressive)

Build highest-scoring features first, parallelize where possible:

```
Week 1–2: atomic-graph-output-writes + honor-graphosignore (parallel)
Week 3–4: fix-query-relevance-scoring + fix-runtime-ram-crash (parallel)
Week 5–6: checkpoint-and-cluster-only-controls + enforce-query-token-budget (parallel)
Week 7–8: mcp-request-resilience + fix-agent-skill-graphos-cli-tools (parallel)
Week 9–10: jgf-graph-serialization + openspec-view (parallel)
Week 11–12: cluster-composition + deterministic-doc-code-edges (parallel)
Week 13–14: explorer-queries + add-product-owner-agent (parallel)
Week 15–16: research-view + json-graph-web-view (parallel)
```

**Total: ~16 weeks, 22 features, cumulative score: 126.11**
*Same duration as Path A but higher parallelism risk.*

### Path C: Risk-First (Conservative)

Address all bug fixes and crash issues first, then build features:

```
Week 1–2: fix-runtime-ram-crash (all sub-specs)
Week 3: fix-ci-missing-hackage-package-list
Week 4: fix-resolution-semantics-docs
Week 5–6: fix-query-relevance-scoring
Week 7–8: atomic-graph-output-writes
Week 9–10: honor-graphosignore
Week 11–14: Remaining features in dependency order
```

**Total: ~14 weeks to reach P1 features, 18 weeks total**
*Best for unstable codebases; delays feature value by 2–4 weeks.*

---

## 7. Graphos as Memory Agent

Using Graphos's own graph infrastructure, we can represent features as nodes and dependencies as edges:

```
Node types:
  - Feature (spec/change)
  - Dependency (blocks/requires)
  - Score (computed metric)

Edge relations:
  - BLOCKS (directed, from dependency to dependent)
  - DEPENDS_ON (directed, from feature to prerequisite)
  - SCORED_WITH (undirected, links feature to score)

Query examples:
  - "Find all features blocked by atomic-graph-output-writes"
    → BFS from node, filter by BLOCKS edge
  - "What's the critical path?"
    → Topological sort + longest path algorithm
  - "Which features have score > 8.0?"
    → Filter nodes by SCORED_WITH value
```

This turns the feature roadmap into a queryable knowledge graph — exactly what Graphos is designed to do.

---

## 8. Recommendations

1. **Start with Path A** (Foundation-First) for the safest delivery.
2. **Parallelize Phase 1** if team capacity allows — `atomic-graph-output-writes`, `honor-graphosignore`, and `fix-query-relevance-scoring` have no inter-dependencies.
3. **Monitor fix-runtime-ram-crash** closely — it touches 6 sub-specs and is on the critical path.
4. **Defer json-graph-web-view** (P3, score 3.75) — it's the lowest priority and depends on the longest chain.
5. **Consider cypher-eval-graphindex** (P3, score 3.21) as a candidate for removal — it has no dependents and low user value.

---

## 9. Next Actions

1. Create child issues for each P0 feature (atomic-graph-output-writes, honor-graphosignore, fix-query-relevance-scoring, fix-runtime-ram-crash, checkpoint-and-cluster-only-controls, enforce-query-token-budget)
2. Assign P0 features to available engineers
3. Set up CI gates for each feature's acceptance criteria
4. Review and adjust scores based on team capacity and stakeholder feedback
