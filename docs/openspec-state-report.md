# OpenSpec State Report — Graphos

> Generated 2026-10-07 for [AVI-952](/AVI/issues/93fcab74-0efe-45b6-b57b-fc223b2b75ae) (openspec view) · Source of truth: `openspec/` in the Graphos repo at commit `c24ec5e` (main). This is a point-in-time report; refresh with the embedded commands.

## 1. Workspace overview

| Metric | Value |
|---|---|
| Active changes | 23 |
| Archived changes | 66 (see `openspec/changes/archive/`, newest: `2026-10-06-wire-incremental-update`) |
| Main specs (`openspec/specs/`) | 104 |
| Total planned tasks (active changes) | 481 |
| Completed tasks (active changes) | 22 (4.6%) |
| Schema | `spec-driven` (artifacts: proposal → specs → design → tasks) |
| Validation | 17 of 23 changes pass `openspec validate`; 6 fail (§5) |

Aggregates: `openspec list --json` · specs: `openspec list --json --specs` · validation: `openspec validate --changes`

## 2. Change states

State classes:

- **In Progress** — tasks exist and work has started (any task checked).
- **Ready (planned, untouched)** — planning artifacts complete, tasks exist, 0 tasks done. These are the prioritizable backlog.
- **Blocked (planning incomplete)** — artifacts missing/empty; cannot be scheduled until planning finishes.
- **Superseded / Planning-scaffold** — see per-change notes.

| Change | State | Tasks (done/total) | Artifacts | Last commit activity | Delta specs |
|---|---|---|---|---|---|
| fix-agent-skill-graphos-cli-tools | 🟡 InProgress | **16/20** | proposal, design, tasks, specs | 2026-09-09 | agent-scaffolding |
| extract-haskell-libs | 🟡 InProgress | 2/60 | proposal, design, tasks, specs | 2026-09-02 | extract-haskell-libs |
| multi-source-graphs | 🟡 InProgress | 2/19 | proposal, design, tasks, specs | 2026-10-06 | 03-watch-mode, 15-config-init, community-label-cache, multi-source-graphs, node-schema |
| intent-graph-verification | 🟡 InProgress | 2/26 | proposal, design, tasks, specs | 2026-09-17 | change-impact-obligations, graph-json-contract, intent-code-graph, intent-spec-checks |
| explorer-queries | 🟢 Ready | 0/69 | proposal, design, tasks, specs | 2026-08-21 | explorer-queries, query-cli-contract |
| research-view | 🟢 Ready | 0/56 | proposal, design, tasks, specs | 2026-08-21 | research-view |
| progressive-graph-interface | 🟢 Ready | 0/34 | proposal, design, tasks, specs | 2026-09-11 | graph-slice-api, html-lod-viewer, live-render-tuning, progressive-graph-viewer, query-groups |
| viewer-navigation | 🟢 Ready | 0/34 | proposal, design, tasks, specs | 2026-09-10 | viewer-navigation |
| cluster-composition | 🟢 Ready | 0/40 | proposal, design, tasks, specs | 2026-08-31 | cluster-composition, llm-labeling |
| consumable-graph-outputs | 🟢 Ready | 0/29 | proposal, design, tasks, specs | 2026-09-11 | canvas-export, cli-subcommand-help, cypher-query, graph-diff-cli, subgraph-extraction-cli |
| openspec-view | 🟢 Ready | 0/13 | proposal, design, tasks, specs | 2026-09-01 | openspec-artifact-view, openspec-change-list, openspec-spec-diff, openspec-state-dashboard |
| capped: cap-oversized-communities | 🟢 Ready (⚠ superseded by community-shape-control) | 0/14 | proposal, design, tasks, specs | 2026-08-15 | community-size-cap |
| checkpoint-and-cluster-only-controls | 🟢 Ready | 0/12 | proposal, design, tasks, specs | 2026-08-15 | checkpoint-controls |
| mcp-request-resilience | 🟢 Ready | 0/14 | proposal, design, tasks, specs | 2026-08-15 | mcp-request-limits |
| bounded-embedding-memory | 🟢 Ready | 0/13 | proposal, design, tasks, specs | 2026-10-05 | bounded-embedding-memory, embedding, embedding-throughput, semantic-edge-inference |
| fix-resolution-semantics-docs | 🟢 Ready | 0/12 | proposal, design, tasks, specs | 2026-08-15 | resolution-guidance |
| cypher-eval-graphindex | 🟢 Ready | 0/10 | proposal, design, tasks | 2026-08-25 | (design doc only; no specs dir) |
| lean-proof-methodology | 🟢 Ready | 0/6 | proposal, tasks, specs | 2026-09-21 | embedding-throughput |
| community-shape-control | 🔶 PlanningIncomplete | — | proposal only | 2026-09-22 | (none yet; absorbs cap-oversized-communities) |
| json-graph-web-view | 🔶 PlanningIncomplete | — | proposal only | 2026-08-19 | (none) |
| AVI-259-goal-scoring-analysis | 🔶 Scaffold (non-standard) | — | plan.md, spec.md, tasks/ | 2026-02—09 | (see note) |
| fix-pipeline-e2e | 🔶 PlanningIncomplete | — | specs dir only | 2026-09-08 | pipeline-shutdown |
| streaming-embeddings | 🔶 PlanningIncomplete | — | lean/ scaffold only | 2026-10-05 | (none) |

Notes on non-standard entries:

- **AVI-259-goal-scoring-analysis** is a planning scaffold (Paperclip epic AVI-259), not a schema-conformant change. Its `tasks.md` is a scored child-issue roster (P0: atomic-graph-output-writes, honor-graphosignore, fix-query-relevance-scoring…). Most P0 children were completed and archived mid-2026; the epic is effectively spent and should be archived.
- **streaming-embeddings** and **bounded-embedding-memory** overlap closely (same last-commit hash `996c4f7`). streaming-embeddings contains only a `lean/` verification scaffold and no planning artifacts; bounded-embedding-memory carries the real proposal/design/tasks. Either finish streaming-embeddings' planning or fold it into bounded-embedding-memory.
- **cap-oversized-communities** is explicitly superseded by `community-shape-control` (per that proposal's Supersession section). Recommend archiving with a supersession marker.

## 3. Delta specs vs main specs

Only changes with a `specs/` dir ship deltas. 18 active changes carry 41 delta-spec folders across 39 distinct capabilities; 4 capabilities are entirely new (no main spec counterpart):

- `openspec-artifact-view`, `openspec-change-list`, `openspec-spec-diff`, `openspec-state-dashboard` (from the openspec-view change)
- Also no main-spec counterpart: `research-view`, `viewer-navigation`, `graph-slice-api`, `progressive-graph-viewer`, `canvas-export`, `graph-diff-cli`, `subgraph-extraction-cli`, `community-size-cap`

Modified-spec changes (deltas against existing capabilities): `multi-source-graphs` (03-watch-mode, 15-config-init, community-label-cache, node-schema), `intent-graph-verification` (graph-json-contract), `extract-haskell-libs`, `fix-pipeline-e2e` (pipeline-shutdown), `lean-proof-methodology` (embedding-throughput), `bounded-embedding-memory` (embedding, embedding-throughput, semantic-edge-inference), `cap-oversized-communities` (community-size-cap is new), `consumer changes` (cypher-query), etc.

Inspect any change's deltas: `openspec show <change> --json` (or `--deltas-only`).

## 4. Portfolio roll-up

- **Implementation is highly skewed**: one change (fix-agent-skill-graphos-cli-tools) holds 16 of 22 completed tasks (80%). Fix-stream is where execution actually happens.
- **Backlog mass is concentrated on UI/viewer work**: research-view + viewer-navigation + progressive-graph-interface + explorer-queries = 193 tasks (40% of all planned tasks) with zero progress.
- **Oldest untouched ready changes** (last touched 2026-08-15): cap-oversized-communities (superseded — archive it), checkpoint-and-cluster-only-controls, mcp-request-resilience, fix-resolution-semantics-docs. These are stalled-by-inertia candidates for a defer/re-prioritize decision.
- **Youngest active planning**: multi-source-graphs (2026-10-06) and bounded-embedding-memory (2026-10-05) are the freshest priorities by activity.

## 5. Validation failures (17 pass / 6 fail)

| Change | Primary failure |
|---|---|
| AVI-259-goal-scoring-analysis | non-standard scaffold; no proposal.md |
| community-shape-control | proposal-only; no specs/ deltas yet |
| json-graph-web-view | proposal-only; no deltas |
| multi-source-graphs | several MODIFIED blocks omit scenarios still present in main specs; multiple ADDED requirements exceed 500 chars |
| progressive-graph-interface | many ADDED requirement texts >500 chars; a MODIFIED block omits an existing scenario |
| streaming-embeddings | no proposal; no deltas |

Failures are mostly *planning-incompleteness*, except multi-source-graphs and progressive-graph-interface which are real delta hygiene issues that will refuse on `openspec archive` — needs a spec-fix pass before those changes complete.

## 6. How to refresh this report

```bash
openspec list --json                 # active changes + task progress
openspec list --json --specs         # 104 main specs + requirement counts
openspec status --change <name>      # artifact completion (proposal/specs/design/tasks)
openspec validate --changes          # 17 pass / 6 fail summary
openspec show <change> --json        # deltas per change
```