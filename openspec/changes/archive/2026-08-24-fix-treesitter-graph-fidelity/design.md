# Design — fix-treesitter-graph-fidelity

## Context

Evidence corpus: `typescipt-repository`, a TypeScript repository with 1,291 files under `./src`,
extracted with the default tree-sitter configuration into a 104,101-node / 122,347-edge
`graph.json`. Measurements taken on that graph:

| Observation | Value |
|---|---|
| Edge relations present | `contains` 109,592 · `inferred` 10,000 · `references` 2,755 |
| `imports` edges | **0** |
| `Import` kind nodes | 12,164 |
| `Import` labels truncated past the specifier | 307 (2.5%) — 265 recoverable from disk, 42 not |
| Source files on disk vs in graph | 1,291 vs 1,205 → **86 missing** (85 under a directory named `build`, 1 unexplained) |
| Edges leaving a 43-file selected subsystem | 2 (both to a markdown file) |
| Loader failures when re-feeding a derived graph via `--graph` | 5 successive hard aborts |
| Import pairs recovered by the external harness | **203/203 — precision 1.00, recall 1.00** |

The graph is therefore structurally a forest: `makeNodeId`
(`Infrastructure/Extract/TreeSitter/Convert.hs:286–293`) is directory-hash + file-stem scoped,
so IDs cannot collide across files, and the only cross-file joining mechanism in the build stage
is NodeId collision (`Domain/Graph/Core.hs:97–104`). Without explicit import edges there is
nothing to connect.

## Goals / Non-Goals

**Goals**

- Tree-sitter extraction produces cross-file `imports` edges for path-based module systems.
- The module specifier survives label truncation and node-ID derivation.
- Directories named `build` inside a source tree are extracted.
- `graph.json` is versioned and can be re-read after being produced by another version or tool.
- Fidelity is measured by an in-tree oracle, not asserted.

**Non-Goals**

- A native pattern-based subgraph command. `research-view` already owns multi-query → induced
  subgraph → HTML; the `Subgraph` use-case module covers the path/taxonomy-driven case
  complementary to it.
- `tsconfig` path aliases, bundler aliases, dynamic `import()`, `require()` — measured and
  reported as residual classes, scoped as a follow-up if the residual exceeds the threshold.
- Fixing `--granularity` being ignored on the tree-sitter path
  (`UseCase/Extract/TreeSitter.hs:31`, `Infrastructure/Wiring.hs:180`) — a real defect found
  during investigation, filed separately to keep this change reviewable.
- LSP-based extraction paths, which already emit `References`/`Calls`
  (`Infrastructure/LSP/Extraction.hs:258`, `:405`).

## Decisions

### D1 — Resolve imports at extraction time, not in the build stage

*Alternatives*: (a) resolve in `UseCase/Build.hs` after all extractions are merged; (b) resolve
in `Convert.hs` during extraction; (c) post-process in `UseCase/Infer.hs`.

*Choice*: (b), with (a) as a fallback only for targets that resolve to files not yet extracted.

*Rationale*: the importing file's directory is the resolution base and is only naturally
available at extraction time. `buildGraph` drops edges with unknown endpoints
(`Domain/Graph/Core.hs:70–72`), so the extraction must also *materialize* the target node — the
same technique the Haskell stub extractor already uses via `canonicalModuleId`
(`UseCase/Extract/Haskell.hs:37–43`). Resolving in `Infer.hs` would misclassify a hard,
syntactically-known fact as an inference.

*Consequence*: two files that import each other each materialize the other's module node; the
merge in `Domain/Graph/Core.hs:97–104` deduplicates them by NodeId, which is precisely why the
target identity must be canonical (a normalized resolved path), not the specifier text.

### D2 — Canonical target identity is the resolved path, extension-normalized

TypeScript ESM source imports `'../x.js'` but the file on disk is `../x.ts`. The identity
function therefore rewrites `.js`/`.mjs`/`.cjs` to the source extension, then tries the literal
path, then `<path>/index.<ext>`. This ordering was validated empirically by the harness: it is
what took recall from 95% to 100% on the evidence corpus (the last 5% were barrel
re-exports plus one truncated label).

Package specifiers collapse to a single canonical external node per package
(`node:path`, `zod`, `@scope/pkg` — with subpaths folded into the package root), keeping fan-in
useful rather than exploding one node per import site.

### D3 — Elide the middle of long declarations, never the tail

`Core.hs:134` truncates raw text at 200 characters after newlines have been stripped, which
deletes exactly the semantically important tail (`from '<specifier>'`). Rather than raising the
budget — which inflates every node — the declaration is normalized and elided in the middle.
This keeps label size bounded while preserving both the declaration form and the specifier, and
it makes node IDs artifact-free (no trailing ellipsis in an identity).

### D4 — Ignore rules split into anchored and depth-independent classes

`build`, `out`, `target`, `dist`, `dist-newstyle`, `DerivedData`, `.build` become root-anchored;
`node_modules`, `.git`, `.stack-work`, `.cache`, `__pycache__` stay depth-independent. The
`Detect.hs:180` short-circuit (`elem` before any pattern evaluation) is replaced by
negation-first evaluation, so `.graphosignore` can override the built-in list — today it cannot,
which is why the 85 dropped files were unrecoverable without patching Graphos.

This intentionally inverts the existing test at
`tests/Graphos/Infrastructure/FileSystem/IgnoreSpec.hs:71–73`, which asserts `shouldIgnore …
"src/build" == True`. That assertion encodes the defect; the delta spec replaces it.

### D5 — Tolerant-by-default loading, strict on request

Producers want validation; consumers want to read what they were given. `--strict-graph`
restores the current fail-fast behaviour; the default degrades unknown enums (`relation` →
`inferred`, `file_type` → `code`) with counted warnings and skips individually malformed
nodes/edges. Combined with optional `communities`/`cohesion`/`god_nodes`, this also makes a
crashed run's partial output loadable, which the hand-rolled writer
(`Infrastructure/Export/IncrementalJSON.hs:27–38`) can currently produce but never read back.

### D6 — Port the harness into the Haskell codebase

*Alternatives*: vendor as-is in Python under `scripts/`; keep it out-of-tree; port into Haskell.

*Choice*: port into the Haskell codebase as Hspec spec modules and a pure `Subgraph` use-case
module.

*Rationale*: keeping the harness in the same language and build system as the code under test
ensures it is always compiled, type-checked, and run by the same `cabal test` invocation — no
separate interpreter, no dev dependency, no CI drift. The original concern that a shared
implementation could hide shared bugs is addressed by using independent parsing logic (direct
`Text` scanning for import specifiers rather than tree-sitter or the project's own extraction
modules) so the oracle remains algorithmically independent even within the same codebase. The
fidelity modules live under `tests/Graphos/Fidelity/` and `src/Graphos/UseCase/Subgraph.hs`,
compiling with `--flag dev` like everything else.

## Risks / Trade-offs

| Risk | Mitigation |
|---|---|
| Graph size grows: every import becomes an edge plus possibly an external node | External nodes are one per package, not per import site; measure node/edge delta on both corpora and record it in `tasks.md` |
| Extracting `src/**/build/**` increases extraction time and node count on repos that relied on the accidental pruning | Documented as an intended correction in the proposal; users can restore pruning with an explicit `.graphosignore` entry |
| Tolerant loading masks producer bugs | Degradations are counted and reported; `--strict-graph` is available and is what CI should use for Graphos' own output |
| Import resolution recall below threshold due to path aliases | The harness groups misses by class; a residual class becomes a scoped follow-up rather than an open-ended widening of this change |
| Middle-elision changes existing node IDs for long declarations | IDs already change whenever source changes; the cache is keyed by file SHA256, so the effect is a one-time re-extraction |

## Migration

- `graph.json` gains `schema_version` and `imports` edges. Readers that ignore unknown keys are
  unaffected; graphs without `schema_version` load as the baseline version.
- Existing caches remain valid — extraction output changes, so the SHA256 cache re-extracts
  affected files naturally on the next run.
- Repositories that want the old pruning behaviour add `src/**/build/**` to `.graphosignore`.

## Open Questions

1. Should package specifiers with subpaths (`@scope/pkg/sub`) collapse to the package root
   (chosen here) or keep the subpath? Fan-in usefulness vs precision — revisit if external nodes
   dominate hub rankings.
2. Should `imports` edges carry a confidence below 1.0 when the target was materialized but never
   independently extracted (a file outside the scan root)? Current position: confidence 1.0 with
   an `edgeExtra` marker, since the specifier is syntactically certain.
3. Does `graphos merge` need to reconcile differing `schema_version` values between its two
   inputs? Likely yes; out of scope here, flagged for the merge capability.
