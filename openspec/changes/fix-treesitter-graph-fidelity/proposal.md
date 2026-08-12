## Why

A real-world exercise — building a "configuration system" subgraph of the `solario-core`
TypeScript repository (1,291 `./src` files) from a freshly built `graph.json` (104,101 nodes,
122,347 edges) — could not be done with the graph Graphos produced. Four defects had to be
worked around by an external Python harness before any navigable subgraph existed.

1. **Tree-sitter extraction emits zero `imports` edges.** The relation histogram of the whole
   graph is `contains: 109,592`, `inferred: 10,000`, `references: 2,755` — and **no `imports`
   at all**. `Convert.hs:135–157` is the only edge producer on the tree-sitter path and it
   constructs `Contains` exclusively (`Convert.hs:146`). Imports are emitted as *nodes*
   (`kind: "Import"`, `Convert.hs:234–238`) and never resolved. Only the Haskell stub path
   emits `Imports` (`UseCase/Extract/Haskell.hs:121`, routed at
   `UseCase/Extract/TreeSitter.hs:30`), so `openspec/specs/import-resolution/spec.md`
   "Cross-file connectivity" is satisfied for `.hs` and silently unmet for every other
   grammar. Consequence: the 43 configuration files selected in `solario-core` induced
   1,106 nodes / 1,233 edges with exactly **2 edges leaving the selected set** — the code
   graph is a forest of per-file islands, and `makeNodeId` (`Convert.hs:286–293`, directory-
   hash + file-stem scoped) structurally guarantees IDs can never collide across files.

2. **Import labels are truncated before the module specifier.** `Core.hs:134` applies a bare
   literal `truncateText 200` and `Convert.hs:184` strips newlines instead of normalizing the
   declaration, so a multi-line `import { … } from './x.js'` collapses into one long line whose
   trailing `from '<specifier>'` is cut. 307 of 12,164 `Import` nodes (2.5%) lost their
   specifier; the harness recovered 265 by re-reading the source from disk, 42 were
   unrecoverable. The truncated text also feeds `makeNodeId` (`Convert.hs:166`), so node IDs
   inherit the truncation.

3. **Bare `build` is ignored at any depth, unconditionally.** `UseCase/Detect.hs:154` lists
   `"build"` in `hardcodedIgnoreDirNames` and `Detect.hs:180` tests it with `elem` on the
   directory *basename* before any pattern logic runs, so no `.graphosignore` negation can
   re-include it; `Infrastructure/FileSystem/Ignore.hs:209` repeats the pattern and
   `Ignore.hs:73–76` matches `ExactPattern` as a path *segment* anywhere. In `solario-core`,
   85 legitimate source files under `src/domain/build/`, `src/services/phase/build/` and
   `src/lib/build/` were dropped (86 of 1,291 files missing overall, 6.7%), silently removing
   real consumers from the graph.

4. **`graph.json` is strictly parsed and unversioned.** Feeding a derived graph back through
   `--graph` failed five times in a row, each an all-or-nothing `eitherDecode` abort with no
   node/edge-level recovery: unknown relation (`Edge.hs:48–52`), missing `file_type`
   (`Node.hs:127`), unknown file type value (`Node.hs:53–62`), `null` `source_file`
   (`Node.hs:128`), missing top-level `communities` (`Load.hs:93`). There is no
   `schema_version` field anywhere, and `community_aggregates` is written
   (`IncrementalJSON.hs:104–107`) but never read back (`Load.hs:89–97`) — an asymmetry that
   also makes a crashed or partially written run unloadable.

A harness written to work around all four is the evidence base for this change: after resolving
imports from `Import` node labels (plus disk fallback for the truncated 2.5% and the ignored
`build/` files), it reached **203/203 import pairs, 100% precision and 100% recall** against an
independent parse of all 1,291 source files. That oracle is exactly what Graphos needs as an
acceptance test, so this change ports it into the Haskell codebase as Hspec spec modules and a
pure `Subgraph` use-case module, and makes it the gate for the extraction fixes.

Related but **not duplicated**: `research-view` already covers multi-query → induced subgraph →
HTML rendering. This change adds a path/taxonomy-driven subgraph CLI subcommand
(`graphos subgraph`) complementary to `research-view`'s query-term-driven approach.

## What Changes

- **Tree-sitter import edges (all grammars).** `Convert.hs` gains import-declaration handling:
  parse the module specifier out of each `Import`/re-export declaration, materialize a
  canonical *import target* node for the resolved module, and emit an `Imports` edge from the
  file's module node to it. Path-based module systems (TS/JS/Python relative imports) resolve
  the specifier against the importing file's directory and the extension/`index` candidates;
  package specifiers (`node:fs`, `zod`, `@scope/pkg`) resolve to a canonical external module
  node. Targets are always materialized so `buildGraph` cannot silently drop the edge
  (`Domain/Graph/Core.hs:70–72`).
- **Re-export edges.** `export { x } from './y'` is an import for connectivity purposes and
  SHALL produce an `Imports` edge, tagged in `edgeExtra` so consumers can distinguish it.
- **Truncation policy.** Replace the literal `200` at `Core.hs:134` with a named exported
  constant, and normalize import/export declarations (collapse interior whitespace, keep the
  `from '<specifier>'` clause) *before* truncation so the specifier is never lost. Node IDs are
  derived from a normalized, specifier-bearing key.
- **Ignore scoping.** Build-output directory names (`build`, `out`, `target`, `dist`,
  `dist-newstyle`, `DerivedData`) are matched **only at the scan root**, not at arbitrary
  depth; `node_modules`, `.git`, `.stack-work`, etc. keep depth-independent matching. The
  `Detect.hs` fast path consults negation patterns before pruning, so `!src/**/build/**` in
  `.graphosignore` re-includes a pruned directory.
- **`graph.json` contract.** Add a top-level `schema_version`; make the loader tolerant —
  unknown `relation` and unknown `file_type` degrade to `inferred` / `code` with a warning
  instead of aborting; `source_file` becomes optional; `communities`, `cohesion` and
  `god_nodes` become optional with empty defaults; `community_aggregates` round-trips.
  Malformed *individual* nodes/edges are skipped and counted, not fatal.
- **Validation harness.** Port the three harness tools into the Haskell codebase as
  `src/Graphos/UseCase/Subgraph.hs` (pure subgraph extraction module exposed via `graphos
  subgraph` CLI subcommand), `tests/Graphos/Fidelity/ImportEdgesSpec.hs` (ground-truth oracle:
  parses every source file on disk and reports precision/recall of the graph's `imports` edges,
  fails the Hspec spec below threshold), `tests/Graphos/Fidelity/GraphCoverageSpec.hs` (files on
  disk vs files in the graph, grouped by ignore reason). All three compile as part of the
  standard `cabal test` / `graphos` build — no external interpreter required. Documented in
  `README.md` and used as the acceptance gate for the fixes above.
- **BREAKING**: none for consumers. `graph.json` gains fields and edges; existing readers that
  ignore unknown keys are unaffected. Repositories that previously skipped `src/**/build/**`
  will now extract those files, increasing node counts (an intended correction).

## Capabilities

### New Capabilities
- `graph-json-contract`: the versioned, tolerant read/write contract for `graph.json`
  (schema version, optional top-level sections, degradation rules, round-trip symmetry).
- `extraction-fidelity-harness`: the in-tree Haskell tooling (Hspec spec modules + a pure
  `Subgraph` use-case module) that measures import-edge precision/recall and file coverage
  against on-disk ground truth, and extracts pattern-selected subgraphs for inspection.

### Modified Capabilities
- `import-resolution`: "Cross-file connectivity" is extended from the Haskell stub extractor to
  every tree-sitter grammar, with new requirements for specifier extraction, canonical import
  target identity for path-based module systems, and re-export edges.
- `extraction-quality`: new requirements for a named truncation budget and for
  semantics-preserving normalization of import/export declarations before truncation.
- `gitignore-parsing`: new requirements anchoring build-output directory names to the scan root
  and making the hardcoded fast-path list negatable.

## Impact

- **Code**:
  - `src/Graphos/Infrastructure/Extract/TreeSitter/Convert.hs` — import-declaration parsing,
    canonical import-target nodes, `Imports` edge emission (`:120–121`, `:135–157`, `:164–178`,
    `:286–293`).
  - `src/Graphos/Infrastructure/Extract/TreeSitter/Core.hs` — named truncation constant and
    declaration normalization (`:134`, `:145–148`).
  - `src/Graphos/UseCase/Detect.hs` — root-anchored build-output names, negation-aware pruning
    (`:146–173`, `:177–182`).
  - `src/Graphos/Infrastructure/FileSystem/Ignore.hs` — anchored pattern class for build outputs
    (`:199–228`, `:73–76`).
  - `src/Graphos/UseCase/Load.hs` — tolerant `GraphFile` parsing, `schema_version`,
    `community_aggregates` (`:42–71`, `:89–97`).
  - `src/Graphos/Domain/Types/Edge.hs` (`:48–52`), `src/Graphos/Domain/Types/Node.hs`
    (`:53–62`, `:123–136`) — degrading enum parsers, optional `source_file`.
  - `src/Graphos/Infrastructure/Export/IncrementalJSON.hs` — emit `schema_version`, keep
    reader/writer key sets symmetric (`:27–38`, `:96–114`).
  - `src/Graphos/UseCase/Subgraph.hs` — new pure subgraph extraction module.
  - `app/Main.hs` / `src/Graphos/CLI/Parser.hs` — `graphos subgraph` CLI subcommand wiring.
  - `tests/Graphos/Fidelity/ImportEdgesSpec.hs`,
    `tests/Graphos/Fidelity/GraphCoverageSpec.hs`,
    `tests/Graphos/UseCase/SubgraphSpec.hs` — new Hspec spec modules.
  - `scripts/validate_import_edges.py`, `scripts/graph_coverage.py`,
    `scripts/subgraph_from_patterns.py` — removed (superseded by the Haskell modules above).
- **APIs**: `graph.json` gains `schema_version` and `imports` edges; no CLI flag removal. A
  `--strict-graph` flag is added to restore fail-fast loading.
- **Dependencies**: none new. The harness compiles as part of the standard `cabal build` /
  `cabal test` using already-listed dependencies (`aeson`, `directory`, `filepath`, `hspec`).
- **Tests**: `tests/Graphos/Infrastructure/Extract/TreeSitterSpec.hs`,
  `tests/Graphos/UseCase/DetectSpec.hs`,
  `tests/Graphos/Infrastructure/FileSystem/IgnoreSpec.hs` (the existing assertion at `:71–73`
  that `src/build` is ignored is intentionally inverted), `tests/Graphos/UseCase/LoadSpec.hs`,
  `tests/Graphos/Fidelity/ImportEdgesSpec.hs`,
  `tests/Graphos/Fidelity/GraphCoverageSpec.hs`,
  `tests/Graphos/UseCase/SubgraphSpec.hs`.
- **Specs/Docs**: PRD §3.2 (`PRD.md:102–112`) Detect/Extract/Build rows need the ignore-scoping
  and import-edge statements; PRD §13.2 (`PRD.md:712–733`) is missing `--graph`/`--granularity`
  and gains `--strict-graph`.
- **Known adjacent defect, deliberately out of scope**: `--granularity` has no effect on the
  tree-sitter path (`UseCase/Extract/TreeSitter.hs:31` discards `_gran`;
  `Infrastructure/Wiring.hs:180` hardcodes `GranularityFunction`). Filed as a follow-up so this
  change stays reviewable.

## PDCA Cycle

- **Plan**: Make a tree-sitter-extracted graph *connected and reloadable*, and prove it with an
  on-disk oracle rather than by inspection. Success is measured on two corpora: (a)
  `solario-core` (TypeScript, 1,291 source files) — `ImportEdgesSpec` reports ≥ 99%
  precision and ≥ 99% recall of `imports` edges, `GraphCoverageSpec` reports 0 files missing
  for reasons other than an explicit ignore rule (today: 86 missing, 0 import edges); (b) the
  Graphos repository itself — no regression in the Haskell stub path's existing `imports`
  edges. Reload success is measured by `graphos query --graph <derived-graph>` accepting a
  graph produced by `graphos subgraph` without schema errors (today: 5 successive
  hard failures).
- **Do**: Implement the six work items in `tasks.md` in dependency order — truncation policy
  first (it unblocks specifier extraction), then import edges, then ignore scoping, then the
  graph.json contract, then port the harness to Haskell, then run the two-corpus validation.
  Keep all IO in Infrastructure and all resolution logic pure (architecture-purity spec).
- **Check**: Hspec/QuickCheck units for specifier parsing, path resolution (relative, extension
  candidates, `index` files, package specifiers), root-anchored ignore matching, and tolerant
  JSON decoding; plus the two-corpus harness run recorded in `tasks.md`. `cabal build --flag
  dev` and `cabal test` green with `-Werror`.
- **Act**: If precision/recall clears the threshold on TypeScript, promote the specifier +
  resolution helper to the shared path for Python/Go/Rust grammars and open the follow-up for
  `--granularity`. If recall stalls below threshold because of dynamic imports or path aliases
  (`tsconfig` `paths`), record the residual class in `tasks.md` attempt history and scope an
  alias-resolution follow-up rather than widening this change.
