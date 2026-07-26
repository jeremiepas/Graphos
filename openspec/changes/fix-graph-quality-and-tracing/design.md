# Design: fix-graph-quality-and-tracing

## Context

The Haskell stub extractor (`Graphos.UseCase.Extract.Haskell`, UseCase layer) is the dominant node source for this repo (HLS fallback). It has three structural flaws:

| Flaw | Location | Effect |
|------|----------|--------|
| Node IDs prefixed with a per-directory hash (`dirHash_Name`) | `haskellStubNodes` | The same module gets different IDs per directory; import targets (`dirHash_import_X`) never match the imported module's real node (`otherDirHash_X`) → 0 cross-file edges, 165 components |
| `haskellStubEdges` links module → *all* other nodes with relation `Imports` | `haskellStubEdges` | Declarations appear as "imports"; relation semantics corrupted |
| `extractDeclName` falls back to `take 20 trimmed`; `isTopLevelDecl` accepts indented lines | `extractDeclName`, `isTopLevelDecl` | 2,933 20-char junk labels, guard/brace fragments as nodes, 5,900 kind-less nodes |

The pipeline (`Graphos.UseCase.Pipeline`, UseCase layer) has three more:

| Flaw | Location | Effect |
|------|----------|--------|
| `graph.json` nodes/edges written *before* edge inference + re-clustering; report generated *after* | steps 3–6 | Report (47,900 edges, 41 communities) contradicts export (9,307 edges, 276 communities) |
| Build/cluster wrapped in `let` between two `getCurrentTime` calls, never forced | steps 3–4 | `span_build`/`span_cluster` report nanoseconds (thunk creation, not work) |
| `newDebugTraceEnv` calls `createDirectoryIfMissing` before checking `dtEnabled` | `Graphos.Infrastructure.Observability.SDK` (Infrastructure layer) | A `traces/` folder is created on every run, even with tracing disabled |

## Goals / Non-Goals

**Goals:**
- Cross-file `imports` edges: connected-component count on this repo drops from 165 to a small number of genuine islands.
- No 20-char truncated labels; no guard/brace/string-fragment nodes; ≥80% reduction in `kind: None` nodes from the Haskell stub path.
- `graph.json` and `GRAPH_REPORT.md` computed from the same enriched graph and final community map; duplicate "Surprising Connections" removed.
- `span_build`/`span_cluster` durations reflect real work (≥1ms on this repo).
- No `traces/` directory created unless debug tracing is enabled *and* events were emitted.
- Loud warning when edge/node ratio is implausibly low on code-dominant runs.

**Non-Goals:**
- Replacing the stub extractor with full LSP/HLS extraction quality (separate concern).
- Per-language import canonicalization beyond Haskell stubs (follow-up cycle if needed).
- New edge relations (calls/uses) — semantic depth is out of scope here.
- Changing the JSONL trace format or the OTLP pipeline.

## Decisions

### D1 — Canonical module identity by module name (Domain/UseCase)

Module nodes and import targets share one canonical ID derived from the module name (`mod_<ModuleName>`), replacing the per-directory-hash prefix for module-kind nodes. Import edges then resolve naturally at build time when maps merge (`Graphos.UseCase.Build` already unions node maps by ID).

- **Alternatives considered:**
  - *Post-build resolution pass matching `_import_X` stubs to nodes labeled X* — keeps ID scheme but adds an O(N·M) reconciliation step in `Graphos.Domain.Graph` and leaves duplicate stubs to garbage-collect; rejected as more code for a worse invariant.
  - *Global content-hash IDs* — over-engineered; module names are already the natural key for `imports` semantics.
- **Layering:** ID derivation stays pure in UseCase (`Extract.Haskell`); no Infrastructure involvement. Decl node IDs keep a file-scoped prefix (decl names are not globally unique).

### D2 — Correct relation semantics in stub edges (UseCase)

`haskellStubEdges` emits `module —imports→ importNode` only for nodes parsed from import lines, and `module —contains→ declNode` for declarations.

- **Alternatives considered:** keep single-relation edges and reclassify at build time — rejected; the extractor is the only place that knows the provenance of each node.

### D3 — Skip instead of truncate; classify decl kinds (UseCase)

`isTopLevelDecl` requires column-0 lines starting with a letter or `(`; `extractDeclName` returns `Maybe` — `Nothing` (no valid identifier) means *no node is emitted*, removing the `take 20` fallback. Kind assignment table:

| Line prefix | nodeKind |
|-------------|----------|
| `data` / `newtype` / `type` | Type |
| `class` | Class |
| `instance` | Instance |
| identifier + `::` | Function |
| other identifier line | Function (binding) |

- **Alternatives considered:** keep fallback but filter at build time — rejected; junk should never enter the extraction, and filtering downstream can't recover the lost kind information.

### D4 — Single source of truth for export vs report (UseCase)

The incremental writer's node/edge sections are written *after* enrichment and final clustering (or rewritten if streaming constraints require an early pass), so `graph.json`, checkpoint stats and `generateReport` all consume the identical `(enrichedGraph, finalCommMap, analysis)` triple. Surprising-connection entries are deduplicated by (source, target, reason) before rendering.

- **Alternatives considered:**
  - *Compute the report from `graph.json` after export* — introduces an Infrastructure→UseCase read-back dependency and a large re-parse; rejected on clean-architecture grounds (Report must stay a pure UseCase over Domain values).
  - *Write both pre- and post-inference graphs* — doubles output size for no user value.
- **Trade-off:** the incremental writer loses its "stream nodes as soon as built" property for the enriched sections; acceptable because inference happens in-memory anyway.

### D5 — Force evaluation inside timed spans (UseCase, per compilation-optimisation conventions)

Build and cluster results are forced to normal form (`deepseq` for graph; `evaluate` on map sizes where NFData already applied) *between* the start and end timestamps, mirroring the existing pattern already used at the post-checkpoint GC boundary.

- **Alternatives considered:** wall-clock the whole step including export side effects — rejected; conflates IO with computation and breaks PRD §16.1 target attribution.

### D6 — Lazy trace-directory creation (Infrastructure)

`newDebugTraceEnv` no longer touches the filesystem. `flushDebugTrace` creates the directory (`createDirectoryIfMissing`) only when `dtEnabled` is true *and* the buffer is non-empty, immediately before writing the JSONL file.

- **Alternatives considered:** create the dir in `initObservability` behind the enabled flag — still creates an empty folder for enabled-but-quiet runs; rejected in favor of flush-time creation which guarantees "folder exists ⇔ trace file exists".

### D7 — Edge-collapse sanity guard (UseCase)

After the build step, if code files dominate the input and `edges/nodes < 0.05`, log a prominent warning (not a failure — some inputs legitimately have few edges). Threshold is a named constant.

- **Alternatives considered:** hard failure — rejected; would break legitimate sparse inputs (image/doc-only runs).

## Risks / Trade-offs

- [Module-name IDs collide across unrelated projects ingested together] → Two `Main` modules merge into one node. Mitigation: canonical IDs apply to Haskell stub modules only; `Main` keeps the directory-hash prefix as a documented exception.
- [Changing node IDs breaks existing checkpoints/caches] → Old `graph.checkpoint.json`/SHA256 cache reference stale IDs. Mitigation: cache entries are keyed by content hash and re-extraction is idempotent; document that the first run after upgrade performs a full rebuild.
- [Forcing evaluation in spans increases peak memory earlier] → The same forcing already happens post-checkpoint; moving it inside the span shifts timing, not total allocation.
- [Stricter `isTopLevelDecl` drops legitimate multi-line signatures] → A name-less first line loses one node. Acceptable: current behavior produces garbage for those lines anyway; QuickCheck property guards against regressions on valid decl forms.
- [Rewriting incremental export ordering slows large runs] → Bounded: nodes/edges are serialized once either way; only the write position moves.

## Verification Strategy (Check)

- **Unit (Hspec/QuickCheck, `cabal test`):**
  - `Extract.Haskell`: properties — no emitted label is a truncated prefix of its source line; every emitted node has a `kind`; indented/guard/brace lines yield no node; import lines yield `imports` edges, decl lines yield `contains` edges; module and import IDs are equal for identical module names.
  - `Observability.SDK`: with tracing disabled, no directory is created; enabled + zero events → no directory; enabled + events → directory and JSONL file exist.
  - `Report`: surprising connections list contains no duplicates; totals in the rendered report equal the graph passed in.
- **Integration (`cabal run graphos -- .` on this repo):** audit `graphos-out/graph.json` — connected components ≪ 165; cross-file `imports` edges > 0; zero labels of exactly 20 truncated chars; report node/edge/community counts equal export counts; `traces/` absent on a default run.
- **Build gate:** `cabal build` with dev flags (`-Wall -Werror`) clean.
- **Trace check:** run with debug tracing enabled; `span_build` and `span_cluster` durations ≥ 1ms.

## Iteration & Rollback (Act)

- **If Check fails on connectivity:** the canonicalization missed a path (qualified imports, package-qualified names) — extend `extractImportName` normalization in a follow-up task before archiving; the change stays unarchived until the component target is met.
- **Rollback:** all changes are pure-code and additive to output quality; reverting the commits restores prior behavior. Old outputs remain readable (schema unchanged).
- **Standardize:** promote the integration audit (component count, label hygiene, report/export parity, no-traces-folder) into a scripted regression check run in CI after `cabal test`; record the edge/node ratio threshold and module-ID exception (`Main`) in PRD §3/§10 on archive.
