# Graphos — Review History, Current State & Improvement Proposal

**Author**: graphos-navigator agent (review session)
**Date**: 2026-08-10
**Status**: Proposal — awaiting owner decision
**Scope**: Graphos project (`~/Documents/perso/Graphos`), not solario-core

---

## 1. Purpose

This document synthesizes the **review history context** of the Graphos project
and defines **what needs to be improved** next. It is a proposal file, not a
spec change. It consolidates evidence from:

- Git history (62 commits, 2026-04-17 → 2026-08-09)
- `CHANGELOG.md` (Unreleased + 0.1.0.0)
- `PRD.md` (936 lines, 18 sections)
- 12 active OpenSpec changes + 19 archived changes
- 32 capability specs under `openspec/specs/`
- `graphos-out/GRAPH_REPORT.md` (self-graph run)
- Live `graphos` invocations performed during this review session

The goal is to give the owner a single, evidence-backed page that answers:
*"Where is Graphos today, and what should I improve next?"*

---

## 2. Project Identity (Recap)

| Field | Value |
|-------|-------|
| **Name** | Graphos |
| **Tagline** | Context graph builder — any input → knowledge graph → clustered communities |
| **Version** | 0.1.0.0 (1 tag: `v0.0.1`) |
| **Language** | Haskell (GHC 9.10) |
| **Build** | Cabal 3.0, devenv/Nix shell |
| **Architecture** | Clean Architecture: Domain ← UseCase ← Infrastructure |
| **Graph library** | FGL 5.8+ (Patricia Tree) |
| **Extraction** | LSP primary, tree-sitter fallback, stub last resort |
| **Clustering** | Leiden community detection (pure Haskell) |
| **Outputs** | `graph.json`, `graph.html`, `GRAPH_REPORT.md`, Obsidian, Neo4j, Memgraph, SVG, GraphML |
| **Agent surface** | MCP stdio server (11 tools) + CLI (`query`, `path`, `explain`, `neighbors`, `symbols`) |

---

## 3. Review History — What Has Been Built & Fixed

### 3.1 Arc by Phase (from git log)

| Phase | Window | What shipped |
|-------|--------|--------------|
| **Foundation** | 2026-04-17 → 2026-04-22 | Initial release: LSP extraction, graph build, Leiden, HTML/JSON/report, MCP stub, static HTTP server, GHC 9.6.7 CI, modular split of `Domain.Types` and `LSP.Client` |
| **Context & Memory** | 2026-04-19 → 2026-05-19 | LLM context selection (5 strategies), conversation memory (community 0), node metadata, incremental pipeline, graph index, merge, LLM labeling, observability stack (OTel + Tempo + Loki + Prometheus + Grafana), Memgraph exporter |
| **Multi-format ingest** | 2026-06-04 → 2026-07-16 | LLM embedding infra, ingest index, office/image extraction foundation, streaming-pipeline-memory (archived, 24 tasks) |
| **Hardening** | 2026-07-26 → 2026-08-04 | gitignore parsing fix, devenv migration (Nix shell → devenv), OpenSpec validation in CI, query scored results + verdict foundation |
| **Architecture refactor** | 2026-07-28 → 2026-08-05 | Ports & `AppEnv`, split god modules (`UseCase.Extract`, `UseCase.Pipeline`, `Domain.Config`) — 48/48 tasks complete |
| **Quality & scale** | 2026-08-08 → 2026-08-09 | PDF debug logging, configurable extraction granularity (fine/function/file), EdgeId dedup fix, LSP references + call hierarchy, MVar shutdown fix, ingest-config domain |

### 3.2 Archived OpenSpec Changes (19 — completed & folded into specs)

All 19 under `openspec/changes/archive/` are marked complete. Notable ones:

- `2026-07-17-fix-leiden-scalability` — Leiden 16× faster at 100k nodes
- `2026-07-16-streaming-pipeline-memory` — OOM fix, 24 tasks
- `2026-08-09-configurable-extraction-granularity` — `fine`/`function`/`file` levels
- `2026-08-09-fix-graph-quality-and-tracing` — report/export consistency, dedup surprising connections
- `2026-08-09-fix-mvar-shutdown-crash` — async-based shutdown
- `2026-08-09-add-ingest-config` — ingest config domain types, `--no-embed`

### 3.3 Active OpenSpec Changes (12 — mixed completion)

| Change | Tasks done | State |
|--------|-----------|-------|
| `fix-extraction-perf-and-missing-grammars` | 24/24 | ✅ Ready to archive |
| `fix-pipeline-e2e` | 20/20 | ✅ Ready to archive (MVar note in Act) |
| `refactor-architecture-ports-and-split-god-modules` | 48/48 | ✅ Ready to archive |
| `optimise-community-detection-large-graph` | 3/23 | 🚧 Just started (Step 5 unstubbed) |
| `fix-mcp-query-perf-and-correctness` | 4/32 | 🚧 Thread `GraphIndex` through MCP started |
| `refactor-html-large-graph-lod` | 4/28 | 🚧 Design revised to sigma.js + WASM-SQLite |
| `add-profondeur-view-selector` | 0/24 | 📋 Planned |
| `docker-otel-stack` | 0/28 | 📋 Planned |
| `fix-runtime-ram-crash` | 0/32 | 📋 Planned |
| `fix-community-labels-in-html` | 0/16 | 📋 Planned |
| `fix-agent-skill-graphos-cli-tools` | 0/20 | 📋 Planned |
| `install-global-opencode-skills` | 0/24 | 📋 Planned |

### 3.4 Live Session Evidence (2026-08-10, this review)

During this review I ran `graphos` against the solario-core repo (a real
codebase) to verify current behavior:

| Run | Granularity | Nodes | Edges | Communities | Time | Verdict |
|-----|-------------|-------|-------|-------------|------|---------|
| Default (`graphos .`) | `function` (default) | 3,972 | 4,069 | — | ~3 min | Mostly docs/headers; code symbols sparse |
| `--granularity file` | `file` | 159,330 | 185,860 | 17,754 | ~55 s extraction + 47 s cluster | ✅ Full code surface covered |

**Observed strengths**:
- `graphos query` returns a **scored verdict** (`strong`/`weak`/`none`) — the `improve-query-agent-ergonomics` work landed.
- `--update --no-viz` incremental path works and is fast.
- `--granularity file` produced a 159K-node graph and Leiden clustered it in ~47 s — the scalability fix is real.
- The `graphos query "graph implementation adjacency list"` run on the file-level graph immediately surfaced `src/infrastructure/structural-analysis/graph-builder.ts` with score 0.75–1.0 — the query engine finds real code.

**Observed weaknesses** (this session, file-backed):
1. **Default `function` granularity under-extracts on repos where LSP servers are not installed.** The first run produced only 5 `Function` nodes, 11 `Type` nodes — almost all nodes were `Header`/`Tag` from docs. The code surface was invisible until I forced `--granularity file`.
2. **`graphos explain <id>` returned the wrong node** when I passed a label-like ID. The explain command expects an exact node ID, but the query output prints human-readable labels first — the mapping from printed label → node ID is not obvious to the caller. (Navigator agent confusion, not a code bug, but it hurts ergonomics.)
3. **`graphos query` result ordering mixes docs and code** with no way to filter by `kind` or `file_type`. On a docs-heavy repo, code hits drown in headers.
4. **`GRAPH_REPORT.md` for the self-graph run shows `Communities: 0`** — the self-run was on the old default; the scale-fix change is not yet reflected in committed artifacts.

---

## 4. Current Gaps — What Needs to Be Improved

The gaps below are grouped by theme and prioritized. Each is backed by evidence
(git, OpenSpec change, live session, or PRD §18 future directions).

### 4.1 P0 — Correctness & Trust

| # | Gap | Evidence | Why P0 |
|---|-----|----------|--------|
| C1 | **`nidToInt` hash collisions silently drop nodes** in FGL conversion. Affects `shortestPath`, `articulationPoints`, `biconnectedComponents`, `dominators`, BFS/DFS. Wrong results, not just slow. | `fix-mcp-query-perf-and-correctness` proposal §1.4 | Silent data loss in a knowledge graph destroys trust in every downstream answer. |
| C2 | **MCP server rebuilds `GraphIndex` + `CachedFGL` on every tool call** and calls `queryGraph` 3× per request. | Same proposal §1.1–1.3 | MCP is the agent surface; if it appears hung, agents stop using Graphos. |
| C3 | **`community_id` not joined back onto `Node` records in export** for some code paths; `GRAPH_REPORT.md` self-run shows 0 communities. | `refactor-html-large-graph-lod` design Context §1; live session | A graph that claims 0 communities is not usable for LOD or community-aware context selection. |
| C4 | **Community labels from `--label` never reach `graph.html`** (hardcoded `"Community <id>"`). | `fix-community-labels-in-html` proposal | Users run expensive LLM labeling and see nothing in the viewer. |

### 4.2 P1 — Scale & Performance

| # | Gap | Evidence |
|---|-----|----------|
| S1 | **HTML viewer freezes above ~10K nodes**; current approach inlines 157 MB JSON. | `refactor-html-large-graph-lod` (revised to sigma.js v3 + `graph.sqlite` + OPFS) |
| S2 | **OOM on 100K+ file multi-language codebases** — LSP server processes multiply. | `fix-runtime-ram-crash` (0/32 tasks) |
| S3 | **Leiden still has quadratic spots**: `fromListWith (++)` grouping, multi-scan `bestCommunityFor`. | `optimise-community-detection-large-graph` tasks 2–5 (20 tasks pending) |
| S4 | **`buildLabelIndex` uses `(++)`** instead of `(:)` + reverse — O(N×hits) vs O(N). | `fix-mcp-query-perf-and-correctness` §"Switch `buildLabelIndex`" |

### 4.3 P1 — Agent & Query Ergonomics

| # | Gap | Evidence |
|---|-----|----------|
| E1 | **No `--kind` / `--file-type` filter on `graphos query`** — docs drown code hits. | Live session: `query "graph implementation"` returned 137 nodes, mostly `Header`/`Tag` |
| E2 | **`explain` expects exact node ID but query prints labels** — caller cannot easily map back. | Live session: `graphos explain` returned a wrong node |
| E3 | **Query result format is not machine-readable** (no `--json` for query/path/explain). | `--help` shows no JSON flag for query commands; only verdict has JSON-mode spec |
| E4 | **Semantic search is substring-only** — embeddings infrastructure exists but not wired to query. | PRD §18.1: "Semantic search (embeddings) — Partial (substring only) — High" |
| E5 | **`graphify` skill shadows `graphos` skill** — agents pick the wrong tool. | `fix-agent-skill-graphos-cli-tools` proposal |

### 4.4 P2 — Developer Experience & Operations

| # | Gap | Evidence |
|---|-----|----------|
| D1 | **No local observability stack** — `--otel` pushes to `localhost:4318` where nothing listens. | `docker-otel-stack` (0/28) |
| D2 | **opencode skill not discoverable** — wrong location/format for opencode v1.15. | `install-global-opencode-skills` (0/24) |
| D3 | **`graph.html` has only 2 depths** (overview/drilldown) — no full-graph or N-hop neighborhood view. | `add-profondeur-view-selector` (0/24) |
| D4 | **Working tree has uncommitted refactor work** (`UseCase/Pipeline/`, `UseCase/Extract/Core.hs`, etc.) not yet committed. | `git status` on 2026-08-10 |

### 4.5 P3 — Future (PRD §18, not yet started)

| # | Gap | PRD ref |
|---|-----|---------|
| F1 | Real-time graph mutation via MCP | §18.1 Critical |
| F2 | LLM-driven conversation summarization (infra exists, not wired) | §18.1 High |
| F3 | Temporal relevance (time decay) | §18.1 Medium |
| F4 | Incremental community update (changed nodes only) | §18.2 Future |
| F5 | Adaptive context learning (track good selections) | §18.3 Future |

---

## 5. Proposed Improvement Plan

A scoped, ordered plan that turns the gaps above into actionable work. Each
item references the existing OpenSpec change where one exists — this proposal
does **not** invent new change IDs.

### Phase A — Trust Repair (do first, ~1 week)

1. **Archive the 3 completed changes** (`fix-extraction-perf-and-missing-grammars`, `fix-pipeline-e2e`, `refactor-architecture-ports-and-split-god-modules`). They are 100% done but still in `openspec/changes/`, not `archive/`. Archiving folds their specs into `openspec/specs/` and clears the active backlog.
2. **Complete `fix-mcp-query-perf-and-correctness`** (28 tasks left). This closes C1 + C2 + S4 in one change: bijective FGL indices, threaded `GraphIndex`/`CachedFGL`, single-call `handleQueryGraph`. It is the single highest-leverage change — it fixes silent wrong answers AND makes MCP fast.
3. **Commit the uncommitted refactor work** (D4) on its branch before it rots. `git status` shows `UseCase/Pipeline/`, `UseCase/Extract/Core.hs`, `Image.hs`, `Markdown.hs`, `Office.hs` as untracked — these are the ports refactor continuation.

### Phase B — Scale Completion (~1–2 weeks)

4. **Continue `optimise-community-detection-large-graph`** (tasks 2–5, 20 left). Pure Domain work, parallel-safe with Phase A on different files. Closes S3.
5. **Start `fix-runtime-ram-crash`** (0/32). Closes S2 — bounds LSP concurrency, adds backpressure. Precondition for 1M-node vision.
6. **Start `refactor-html-large-graph-lod`** (24 left, design already revised). Closes S1 + C3 + C4 in one architectural pass: sigma.js v3, `graph.sqlite` + OPFS, community-join, community labels in HTML. The design is already updated for the 158K reality — implementation can begin.

### Phase C — Agent Ergonomics (~1 week)

7. **Add `--kind` / `--file-type` filters and `--json` output to `graphos query`/`path`/`explain`** (E1, E3). Small, high-impact. New OpenSpec change recommended (`query-result-filtering`).
8. **Make `explain` accept a label and disambiguate** when the ID is not an exact match (E2). Either print "did you mean…" or accept `--id` vs `--label` flags.
9. **Wire embeddings into query** (E4). The embedding infra and ingest index exist (archived `2026-08-05-default-ollama-headers-embedding`); query still does substring only. PRD §18.1 marks this High.
10. **Resolve `fix-agent-skill-graphos-cli-tools`** (E5, 0/20) — deconfuse the `graphify` vs `graphos` skill situation so agents stop running the wrong tool.

### Phase D — Operations & Viewer (~1 week, parallel with C)

11. **`docker-otel-stack`** (D1, 0/28) — one-command local Grafana/Tempo/Loki/Prometheus so `--otel` is visible.
12. **`install-global-opencode-skills`** (D2, 0/24) — make the Graphos skill discoverable in opencode.
13. **`add-profondeur-view-selector`** (D3, 0/24) — full/community/custom-depth selector. Depends on `refactor-html-large-graph-lod` landing first; can start design tasks in parallel.

### Phase E — Future (after B/C/D)

14. F1–F5 from PRD §18 — real-time mutation, summarization, temporal decay, incremental communities, adaptive learning. Each warrants its own OpenSpec change. Defer until Phase B+C+D land so the foundation is trustworthy.

---

## 6. Success Criteria (Measurable)

Borrowed from PRD §16.1 and the active changes' PDCA plans:

| Criterion | Target | Verified by |
|-----------|--------|-------------|
| FGL bijectivity | 0 collisions on 159K-node graph | `fix-mcp-query-perf` task property test |
| MCP `query_graph` latency | < 500ms on 159K nodes (PRD §16.1) | `--metrics` histogram |
| Leiden at 159K nodes | < 30s (PRD §16.1) | `span_cluster` trace |
| HTML initial load | < 3s on 158K nodes | `refactor-html-lod` Check |
| HTML browser memory | < 200 MB at 158K nodes | `refactor-html-lod` Check (revised) |
| `graph.json` community_id | non-null for all community members | `optimise-community` Check |
| `GRAPH_REPORT.md` totals | match `graph.json` counts | `report-consistency` spec |
| `cabal test` | 308+ examples, 0 failures | existing suite |
| `cabal build` | zero warnings (`-Werror`) | existing CI |

---

## 7. Risks & Guards

| Risk | Guard |
|------|-------|
| Phase A items block each other | A1 (archive) is pure file moves; A2 (MCP) touches `MCP.hs` + `Domain/Graph/*`; A3 (commit) is separate files — run in parallel branches |
| HTML rewrite (B6) is large | Design already revised; split into data-join pass (pure, testable) + viewer rewrite (infra, not unit-tested) per the change's own PDCA |
| Uncommitted refactor (A3) conflicts with B4 | Commit A3 first, then branch B4 from it |
| Skill deconfusion (C10) touches user-global files | Scope strictly to `~/.claude/skills/` and `.opencode/`; do not modify solario-core |

---

## 8. What This Proposal Does NOT Do

- It does not create any OpenSpec change or spec file.
- It does not modify Graphos source code.
- It does not archive any change (that is an owner action).
- It does not commit the uncommitted work (owner action per AGENTS.md §2/§3).
- It is not a PRD amendment — it references PRD § numbers, it does not change them.

---

## 9. Recommendation (One Line)

**Archive the 3 completed changes, then complete `fix-mcp-query-perf-and-correctness` next — it fixes silent wrong answers and makes the agent surface fast in a single pass.**

---

## Appendix A — Evidence Index

| Claim | Source |
|-------|--------|
| 62 commits, 2026-04-17 → 2026-08-09 | `git log --oneline` |
| 12 active / 19 archived OpenSpec changes | `openspec/changes/` listing |
| 32 capability specs | `openspec/specs/` listing |
| Leiden 16× faster | `CHANGELOG.md` Unreleased |
| `nidToInt` collision bug | `fix-mcp-query-perf-and-correctness` proposal §1.4 |
| HTML freezes at 10K, 158K reality | `refactor-html-large-graph-lod` design Context |
| Community labels not in HTML | `fix-community-labels-in-html` proposal |
| Self-graph: 0 communities | `graphos-out/GRAPH_REPORT.md` |
| Live session: 159K nodes in 47s | `graphos . --update --no-viz --granularity file` (this review) |
| Live session: default under-extracts code | `graphos query` first run (3,972 nodes, 5 Functions) |
| PRD §18 future gaps | `PRD.md` lines 888–933 |

---

*End of proposal. Owner: review Phase A ordering and approve or amend before any work begins.*