# Proposal: fix-graph-quality-and-tracing

## Why

A review of the pipeline traces (`traces/*.jsonl`) and the latest graph output (`graphos-out/`) uncovered five defects that undermine the core product promise (PRD §1: "any input → knowledge graph → clustered communities"):

1. **The graph is 165 disconnected file-islands.** All 7,459 `imports` edges resolve to per-directory stub nodes (e.g. `10424_( NodeId, CommunityI`) instead of the real module node in another directory. Zero cross-file edges exist in `graph.json`, defeating the purpose of a codebase knowledge graph (PRD §3 pipeline: build stage).
2. **~35% of nodes are junk.** 5,900/8,366 nodes have `kind: None`; 2,933 labels are hard-truncated at exactly 20 chars (`"  - from: " ++ T.un`); guard fragments (`| otherwise ->`), braces, and string literals are extracted as symbols (PRD §3.2 extract stage, node-schema spec).
3. **GRAPH_REPORT.md contradicts graph.json.** Report claims 47,900 edges / 41 communities / "0 articulation points, well-connected"; the export contains 9,307 edges / 276 communities / 165 components. The report is computed from the post-inference in-memory graph while `graph.json` is written pre-inference (PRD §12 export formats).
4. **Span timings are meaningless.** `span_build` and `span_cluster` report 40–600 *nanoseconds* because pure `let` bindings are timed without forcing evaluation — laziness pushes all work outside the timed window (PRD §10 observability, §16.1 performance targets are unverifiable).
5. **A `traces/` folder is created unconditionally.** `newDebugTraceEnv` calls `createDirectoryIfMissing` even when debug tracing is disabled, littering the working/output directory (PRD §10.4 debug trace).

Additionally, the last recorded run (Jun 10) produced 8,105 nodes but **1 edge / 1 community** — a silent edge-extraction collapse that no check caught.

## What Changes

- **Cross-file import resolution** (build stage): imported-module stub nodes are canonicalized by module name so `import Graphos.Domain.Config` links to the module node of `src/Graphos/Domain/Config.hs`, reconnecting the file-islands. Decl edges from module nodes use `contains`, not `imports`.
- **Extraction quality** (extract stage, Haskell stub extractor): only column-0 declarations are treated as top-level; lines that don't yield a valid identifier are skipped instead of emitting a 20-char truncated fragment; decl nodes get a `kind` (Function/Type/Class/Instance).
- **Report/export consistency**: `graph.json` nodes/edges/communities are written from the same enriched graph + final clustering that GRAPH_REPORT.md uses; "Surprising Connections" entries are deduplicated.
- **Accurate span timing**: build and cluster results are forced (`evaluate`/`deepseq`) inside the timed window so `span_build`/`span_cluster` durations reflect real work.
- **No unconditional traces folder**: the debug-trace directory is created lazily at flush time, only when tracing is enabled and events exist. No `traces/` folder appears otherwise.
- **Sanity guard**: the pipeline logs a prominent warning when a code-dominant run yields an implausibly low edge/node ratio (catches regressions like the Jun 10 `edges=1` run).

## Capabilities

### New Capabilities
- `import-resolution`: cross-file edge resolution at graph build time — import edges point to canonical module nodes, not per-directory stubs (workflow: 01-full-pipeline, 02-incremental-pipeline).
- `extraction-quality`: stub-extraction hygiene — no truncated/junk symbol nodes, kinds assigned to declarations (workflow: 01-full-pipeline).
- `report-consistency`: GRAPH_REPORT.md and graph.json derive from identical graph data (workflow: 01-full-pipeline, 04-query).

### Modified Capabilities
- `observability-consolidation`: span duration measurements MUST force evaluation of the measured computation; debug-trace directory MUST only be created when tracing is enabled and there are events to flush.

## Impact

- **Code**: `src/Graphos/UseCase/Extract/Haskell.hs` (stub parser), `src/Graphos/UseCase/Build.hs` / `Graphos.Domain` build path (import canonicalization), `src/Graphos/UseCase/Pipeline.hs` (write ordering, span forcing, sanity guard), `src/Graphos/Infrastructure/Observability/SDK.hs` (lazy dir creation), `src/Graphos/UseCase/Report.hs` (dedup).
- **Outputs**: `graph.json` gains cross-file edges and consistent community data; GRAPH_REPORT.md stats become trustworthy; no stray `traces/` folder.
- **No API/CLI changes**; no new dependencies. Existing `--debug-trace-dir` behavior preserved when explicitly enabled.
- **Tests**: new Hspec/QuickCheck coverage for stub parsing, import canonicalization, and trace-dir behavior.

## PDCA Cycle

- **Plan**: Hypothesis — the five defects above are the primary causes of low graph quality. Success measured against PRD §16: cross-file `imports` edges > 0 (target: connected component count drops from 165 to near number-of-projects); nodes with `kind: None` reduced by ≥ 80%; zero labels truncated at 20 chars; report totals equal export totals; `span_build`/`span_cluster` ≥ 1ms on this repo; no `traces/` dir on default runs.
- **Do**: Implement the six changes listed under "What Changes" (see design.md and tasks.md).
- **Check**: `cabal test` (new unit specs) + full pipeline run on this repo; verify with a JSON audit script that component count, edge relations, label lengths, kind coverage, and report/export parity meet the Plan targets; confirm no `traces/` folder is created.
- **Act**: If targets are met, archive the change and fold the audit script into CI as a regression gate; if imports still fail to resolve for some languages, open a follow-up change for per-language canonicalization.
